# pv_suggest(): the blank-page killer. Hand it a data frame and it looks
# at what is actually there - column types, cardinalities, repeated keys,
# coordinates, map ids - and prints a handful of runnable chart calls
# with the real column names filled in, each with one line of reasoning.
# Every suggestion is built once behind the scenes before it is offered,
# so nothing printed here can error when the user runs it.

# ---- what a data frame looks like ------------------------------------------

# Column names that mean "identifier", not "measurement". An id makes a
# fine join key or tooltip label, but plotting one on a value axis is
# always a mistake, so these never enter the measure pool.
suggest_id_name <- function(nm) {
  grepl("(^|[_.])(id|ids|nr|no|code|key)$", tolower(nm))
}

# Calendar parts (a bare year, month, weekday column) order a time axis
# but are not quantities in their own right, so they are kept out of the
# measure pool too.
suggest_calendar_name <- function(nm) {
  tolower(nm) %in% c("year", "yr", "jahr", "annee", "month", "monat",
                     "week", "day", "weekday", "doy", "quarter", "hour",
                     "minute", "date")
}

# How a measure should be folded when a suggestion needs one row per key:
# indices, rates, shares and temperatures average; counts and totals sum.
# Logical columns always sum - the count of TRUEs is the natural measure.
suggest_fun <- function(nm, kind) {
  if (identical(kind, "logical")) {
    return("sum")
  }
  ratey <- paste0("index|rate|share|pct|percent|ratio|per[_.]|_per|temp|",
                  "mean|avg|median|price|score|density")
  if (grepl(ratey, tolower(nm))) "mean" else "sum"
}

# The join keys of the bundled map layers: the feature ids as character
# vectors (ids are compared as strings on both sides, exactly as
# pv_choropleth does) and the feature names (what pv_flow_map matches
# endpoints against first). Each layer also records how it is asked for
# in a printed call: map_arg for pv_choropleth (whose default layer is
# Lucerne) and flow_arg for pv_flow_map (whose default is the cantons) -
# NULL where the layer is that chart's default and the argument can stay
# home. The fetched country-wide municipal layer is deliberately absent:
# a suggestion should never trigger a download.
suggest_map_layers <- function() {
  ids_of <- function(m) {
    v <- unlist(lapply(m$features, function(f) f$properties$id))
    as.character(v)
  }
  names_of <- function(m) {
    v <- unlist(lapply(m$features, function(f) f$properties$name))
    as.character(v)
  }
  list(
    lucerne = list(ids = ids_of(pv_lucerne_map),
                   names = names_of(pv_lucerne_map),
                   map_arg = NULL, flow_arg = "lucerne",
                   label = "Lucerne municipality"),
    cantons = list(ids = ids_of(pv_swiss_cantons),
                   names = names_of(pv_swiss_cantons),
                   map_arg = "cantons", flow_arg = NULL,
                   label = "canton"),
    districts = list(ids = ids_of(pv_swiss_districts),
                     names = names_of(pv_swiss_districts),
                     map_arg = "districts", flow_arg = "districts",
                     label = "district")
  )
}

# Does this column hold ids from one of the bundled map layers? The bar
# is deliberately high: nearly every value must be a known feature id
# (precision) and the column must cover a good share of the layer
# (recall), so a month column 1-12 never reads as canton numbers. The
# canton layer's tiny id space (1-26) gets an extra guard: near-full
# coverage, or a name that says canton.
suggest_match_layer <- function(nm, values) {
  vals <- unique(values[!is.na(values)])
  if (length(vals) < 5) {
    return(NULL)
  }
  vals <- as.character(vals)
  best <- NULL
  for (layer in names(suggest_the$layers)) {
    ids <- suggest_the$layers[[layer]]$ids
    precision <- mean(vals %in% ids)
    recall <- mean(ids %in% vals)
    if (precision < 0.9 || recall < 0.5) next
    if (layer == "cantons" && recall < 0.85 &&
        !grepl("canton|kanton", tolower(nm))) {
      next
    }
    if (is.null(best) || recall > best$recall) {
      best <- list(layer = layer, recall = recall)
    }
  }
  if (is.null(best)) NULL else best$layer
}

# One-time cache for the layer id sets - extracting them walks a few
# hundred features, which need not happen again within a session.
suggest_the <- new.env(parent = emptyenv())

# Everything the suggestion builders need to know about the data, worked
# out once: each column's kind and cardinality, the numeric measures (with
# their natural fold), the categorical columns, a time axis if one exists,
# a longitude/latitude pair, a map-id column, and any numeric list-columns
# fit for sparklines.
suggest_profile <- function(data) {
  if (is.null(suggest_the$layers)) {
    suggest_the$layers <- suggest_map_layers()
  }
  nms <- names(data)
  kind <- vapply(nms, function(nm) {
    v <- data[[nm]]
    if (is.list(v) && !is.data.frame(v)) {
      "list"
    } else if (inherits(v, "Date")) {
      "date"
    } else if (is.logical(v)) {
      "logical"
    } else if (is.numeric(v)) {
      "numeric"
    } else if (is.character(v) || is.factor(v)) {
      "category"
    } else {
      "other"
    }
  }, character(1))
  distinct <- vapply(seq_along(nms), function(i) {
    if (kind[[i]] %in% c("list", "other")) {
      return(NA_integer_)
    }
    v <- data[[nms[[i]]]]
    length(unique(v[!is.na(v)]))
  }, integer(1))

  # A longitude/latitude pair: numeric, named like coordinates, and
  # holding plausible WGS84 degrees. The bundled base maps show
  # Switzerland, so the profile also records whether every point falls
  # inside the country - only then is a bubble map offered.
  low <- tolower(nms)
  lon_i <- which(kind == "numeric" & low %in%
                   c("lon", "lng", "long", "longitude"))
  lat_i <- which(kind == "numeric" & low %in% c("lat", "latitude"))
  coords <- NULL
  if (length(lon_i) && length(lat_i)) {
    lonv <- data[[nms[[lon_i[[1]]]]]]
    latv <- data[[nms[[lat_i[[1]]]]]]
    lonv <- lonv[!is.na(lonv)]
    latv <- latv[!is.na(latv)]
    world <- length(lonv) > 0 && length(latv) > 0 &&
      all(abs(lonv) <= 180) && all(abs(latv) <= 90)
    if (world) {
      coords <- list(
        lon = nms[[lon_i[[1]]]], lat = nms[[lat_i[[1]]]],
        swiss = all(lonv >= 5.7 & lonv <= 10.8) &&
          all(latv >= 45.5 & latv <= 48.1))
    }
  }
  coord_cols <- if (is.null(coords)) character(0) else c(coords$lon,
                                                        coords$lat)

  # A column of ids matching one of the bundled map layers. Calendar
  # parts and coordinates never qualify, whatever their values.
  map <- NULL
  for (i in seq_along(nms)) {
    nm <- nms[[i]]
    if (nm %in% coord_cols || suggest_calendar_name(nm)) next
    v <- data[[nm]]
    candidate <- if (kind[[i]] == "numeric") {
      ok <- v[!is.na(v)]
      length(ok) > 0 && all(ok == trunc(ok))
    } else if (kind[[i]] == "category") {
      ok <- as.character(v[!is.na(v)])
      length(ok) > 0 && all(grepl("^[0-9]+$", ok))
    } else {
      FALSE
    }
    if (!candidate) next
    layer <- suggest_match_layer(nm, as.character(v))
    if (!is.null(layer)) {
      map <- list(col = nm, layer = layer)
      break
    }
  }

  # The measure pool: numeric or logical columns that read as quantities -
  # coordinates, ids, calendar parts, and the map join key stay out.
  measures <- list()
  for (i in seq_along(nms)) {
    nm <- nms[[i]]
    if (!kind[[i]] %in% c("numeric", "logical")) next
    if (nm %in% coord_cols || suggest_id_name(nm) ||
        suggest_calendar_name(nm)) {
      next
    }
    if (!is.null(map) && nm == map$col) next
    v <- data[[nm]]
    ok <- v[!is.na(v)]
    if (!length(ok)) next
    measures[[length(measures) + 1]] <- list(
      name = nm, kind = kind[[i]], fun = suggest_fun(nm, kind[[i]]),
      n_ok = length(ok), distinct = distinct[[i]],
      min = min(as.numeric(ok)), max = max(as.numeric(ok)))
  }

  # The grouping pool: character or factor columns with at least two
  # levels, skipping id-like names (a code makes a poor axis).
  cats <- list()
  for (i in seq_along(nms)) {
    nm <- nms[[i]]
    if (kind[[i]] != "category" || suggest_id_name(nm)) next
    if (is.na(distinct[[i]]) || distinct[[i]] < 2) next
    cats[[length(cats) + 1]] <- list(name = nm, distinct = distinct[[i]])
  }

  # A time axis: the first Date column with at least three moments, or
  # failing that a numeric column that both looks and is named like
  # calendar years - pv_line and pv_area take either happily.
  time <- NULL
  date_i <- which(kind == "date" & !is.na(distinct) & distinct >= 3)
  if (length(date_i)) {
    time <- list(col = nms[[date_i[[1]]]], kind = "date")
  } else {
    for (i in seq_along(nms)) {
      if (kind[[i]] != "numeric" || is.na(distinct[[i]]) ||
          distinct[[i]] < 3) {
        next
      }
      if (!grepl("^(year|yr|jahr|annee)s?$", low[[i]])) next
      v <- data[[nms[[i]]]]
      ok <- v[!is.na(v)]
      if (length(ok) && all(ok == trunc(ok)) && min(ok) >= 1500 &&
          max(ok) <= 2200) {
        time <- list(col = nms[[i]], kind = "year")
        break
      }
    }
  }

  # List-columns holding a numeric vector per row - sparkline material.
  list_cols <- nms[kind == "list"]
  sparkable <- list_cols[vapply(list_cols, function(nm) {
    all(vapply(data[[nm]], function(el) {
      is.numeric(el) && length(el) >= 2 && any(!is.na(el))
    }, logical(1)))
  }, logical(1))]

  list(data = data, n = nrow(data), n_cols = ncol(data), kind = kind,
       coords = coords, map = map, measures = measures, cats = cats,
       time = time, date_col = if (length(which(kind == "date"))) {
         nms[[which(kind == "date")[[1]]]]
       },
       list_cols = list_cols, spark_cols = sparkable)
}

# ---- shared bits of code-building ------------------------------------------

# A column name as it appears inside a call - quoted, with anything odd
# escaped so the printed code parses back exactly.
suggest_q <- function(x) {
  encodeString(x, quote = "\"")
}

# Whether a name can sit bare in an aggregate() formula. Suggestions that
# need aggregation are only offered when everything involved is a plain
# syntactic name - a formula full of backticks is not beginner code.
suggest_syntactic <- function(x) {
  all(grepl("^[a-zA-Z.][a-zA-Z0-9._]*$", x))
}

# The name the aggregated frame gets in the printed code: by_region,
# by_date_region, and so on.
suggest_agg_var <- function(keys) {
  paste0("by_", paste(gsub("[^A-Za-z0-9]", "_", keys), collapse = "_"))
}

# The aggregate() line itself, plus the note the reason carries so the
# aggregation is said out loud, not just shown.
suggest_agg <- function(measure, keys, nm, fun) {
  var <- suggest_agg_var(keys)
  list(
    var = var,
    line = sprintf("%s <- aggregate(%s ~ %s, %s, %s)",
                   var, measure, paste(keys, collapse = " + "), nm, fun),
    note = sprintf(" (%s to one row per %s first)",
                   if (identical(fun, "mean")) "averaged" else "summed",
                   paste(sprintf("`%s`", keys), collapse = "/")))
}

# Rows complete on the named columns - the same rows a chart would keep
# after its own missing-value dropping, so duplicate-key checks here
# agree with the checks inside the chart constructors.
suggest_complete <- function(data, cols) {
  keep <- rep(TRUE, nrow(data))
  for (col in cols) {
    keep <- keep & !is.na(data[[col]])
  }
  keep
}

# ---- one builder per chart shape -------------------------------------------
# Each returns a suggestion record - list(chart, code, reason, score) - or
# NULL when the shape is not there. The scores order the final list by
# fit: the specific shapes (a map join, coordinates, daily dates) beat the
# generic ones (a lone histogram, the fallback table).

suggest_choropleth <- function(p, nm) {
  if (is.null(p$map) || !length(p$measures)) {
    return(NULL)
  }
  m <- p$measures[[1]]
  layer <- suggest_the$layers[[p$map$layer]]
  id <- p$map$col
  map_part <- if (is.null(layer$map_arg)) "" else {
    sprintf(", map = %s", suggest_q(layer$map_arg))
  }
  keep <- suggest_complete(p$data, c(id, m$name))
  dup <- anyDuplicated(p$data[[id]][keep]) > 0
  note <- ""
  if (dup) {
    if (!suggest_syntactic(c(m$name, id))) {
      return(NULL)
    }
    agg <- suggest_agg(m$name, id, nm, m$fun)
    frame <- agg$var
    lines <- agg$line
    note <- agg$note
  } else {
    frame <- nm
    lines <- character(0)
  }
  call_line <- sprintf("pv_choropleth(%s%s, id = %s, value = %s)",
                       frame, map_part, suggest_q(id), suggest_q(m$name))
  list(chart = "pv_choropleth",
       code = paste(c(lines, call_line), collapse = "\n"),
       reason = sprintf(
         "the ids in `%s` are %s numbers, so `%s` can be painted by region%s",
         id, layer$label, m$name, note),
       score = 95)
}

# A category column holding the two-letter canton codes with one row
# per canton, plus a measure: pv_hexmap's home ground. The bar mirrors
# the choropleth's: full precision (every value must be a real code, so
# a country-code column with a stray "DE" never qualifies) and near-full
# coverage of the 26 - the equal-hexagon grid only earns its keep when
# (nearly) the whole country is there.
suggest_hexmap <- function(p, nm) {
  if (!length(p$measures)) {
    return(NULL)
  }
  m <- p$measures[[1]]
  codes <- pv_hexmap_layout$code
  # The code column is hunted among the raw category columns, not
  # p$cats: a join key is exactly the kind of id-named column ("code",
  # "canton_code") the grouping pool screens out.
  nms <- names(p$data)
  col <- NULL
  for (i in seq_along(nms)) {
    if (p$kind[[i]] != "category" || suggest_calendar_name(nms[[i]])) next
    v <- toupper(trimws(as.character(p$data[[nms[[i]]]])))
    vals <- unique(v[!is.na(v)])
    if (!length(vals) || !all(vals %in% codes)) next
    if (length(vals) < 0.9 * length(codes)) next
    col <- nms[[i]]
    break
  }
  if (is.null(col)) {
    return(NULL)
  }
  # One row per canton, as the chart itself demands - a messier frame
  # falls through to the other builders rather than guessing a fold.
  keep <- suggest_complete(p$data, c(col, m$name))
  if (!any(keep) || anyDuplicated(p$data[[col]][keep]) > 0) {
    return(NULL)
  }
  list(chart = "pv_hexmap",
       code = sprintf("pv_hexmap(%s, id = %s, value = %s)",
                      nm, suggest_q(col), suggest_q(m$name)),
       reason = sprintf(
         "`%s` holds the two-letter canton codes with one row per canton, so `%s` reads as a hex cartogram - every canton the same size, the small dense ones no longer invisible",
         col, m$name),
       score = 95)
}

suggest_bubble_map <- function(p, nm) {
  if (is.null(p$coords) || !isTRUE(p$coords$swiss)) {
    return(NULL)
  }
  size <- NULL
  for (m in p$measures) {
    if (m$kind == "numeric" && m$min >= 0 && m$max > 0) {
      size <- m
      break
    }
  }
  if (is.null(size)) {
    return(NULL)
  }
  label <- NULL
  for (cc in p$cats) {
    if (cc$distinct >= 0.9 * p$n) {
      label <- cc$name
      break
    }
  }
  label_part <- if (is.null(label)) "" else {
    sprintf(", label = %s", suggest_q(label))
  }
  list(chart = "pv_bubble_map",
       code = sprintf("pv_bubble_map(%s, lon = %s, lat = %s, size = %s%s)",
                      nm, suggest_q(p$coords$lon), suggest_q(p$coords$lat),
                      suggest_q(size$name), label_part),
       reason = sprintf(
         "`%s`/`%s` are Swiss longitude/latitude, so `%s` can sit as sized circles on the map",
         p$coords$lon, p$coords$lat, size$name),
       score = 94)
}

# Two category columns whose every value is a feature name on one
# bundled layer, plus a non-negative measure: the origin-destination
# shape, and pv_flow_map's home ground. The bar mirrors pv_flow_map's
# own rules - full precision (an endpoint matching no feature is an
# error there), no place flowing to itself, and at least three places in
# play so a two-place shuttle stays with the plainer forms.
suggest_flow_map <- function(p, nm) {
  m <- NULL
  for (cand in p$measures) {
    if (cand$kind == "numeric" && cand$min >= 0 && cand$max > 0) {
      m <- cand
      break
    }
  }
  if (is.null(m)) {
    return(NULL)
  }
  # An origin and a destination are two category columns whose every
  # value is a feature name on the SAME layer, so the search runs layer
  # by layer rather than column by column - a column holding only
  # "Luzern" belongs to whichever layer its partner column pins down.
  # The cantons come first: they are the layer whole-country flows
  # actually use, and the one pv_flow_map defaults to.
  nms <- names(p$data)
  candidates <- character(0)
  for (i in seq_along(nms)) {
    if (p$kind[[i]] != "category" || suggest_calendar_name(nms[[i]])) next
    candidates <- c(candidates, nms[[i]])
  }
  if (length(candidates) < 2) {
    return(NULL)
  }
  vals_of <- function(col) {
    v <- unique(as.character(p$data[[col]]))
    v[!is.na(v)]
  }
  pair <- NULL
  for (layer in c("cantons", "districts", "lucerne")) {
    layer_names <- suggest_the$layers[[layer]]$names
    hits <- candidates[vapply(candidates, function(col) {
      v <- vals_of(col)
      length(v) > 0 && all(v %in% layer_names)
    }, logical(1))]
    if (length(hits) >= 2) {
      pair <- list(cols = hits[1:2], layer = layer)
      break
    }
  }
  if (is.null(pair)) {
    return(NULL)
  }
  # Column order decides which end is which, unless the names say so
  # themselves - a column called "to" should not become the origin. The
  # words must stand alone or lead a compound ("to", "to_canton"), so a
  # column called "town" stays untouched.
  from <- pair$cols[[1]]
  to <- pair$cols[[2]]
  fromish <- grepl("^(from|origin|source|start|von|herkunft)([_.]|$)",
                   tolower(c(from, to)))
  toish <- grepl("^(to|dest|destination|target|ziel|nach)([_.]|$)",
                 tolower(c(from, to)))
  if ((toish[[1]] && !toish[[2]]) || (fromish[[2]] && !fromish[[1]])) {
    tmp <- from
    from <- to
    to <- tmp
  }
  keep <- suggest_complete(p$data, c(from, to, m$name))
  if (!any(keep)) {
    return(NULL)
  }
  fv <- as.character(p$data[[from]][keep])
  tv <- as.character(p$data[[to]][keep])
  # A place flowing to itself cannot be drawn, and fewer than three
  # places is not much of a map.
  if (any(fv == tv) || length(unique(c(fv, tv))) < 3) {
    return(NULL)
  }
  # One row per directed pair, exactly as pv_flow_map demands; repeated
  # pairs (or missing values) get the aggregate() step spelled out.
  dup <- anyDuplicated(paste(fv, tv, sep = "\r")) > 0
  nas <- anyNA(p$data[[m$name]])
  note <- ""
  if (dup || nas) {
    if (!suggest_syntactic(c(m$name, from, to))) {
      return(NULL)
    }
    agg <- suggest_agg(m$name, c(from, to), nm, m$fun)
    frame <- agg$var
    lines <- agg$line
    note <- agg$note
  } else {
    frame <- nm
    lines <- character(0)
  }
  layer <- suggest_the$layers[[pair$layer]]
  map_part <- if (is.null(layer$flow_arg)) "" else {
    sprintf(", map = %s", suggest_q(layer$flow_arg))
  }
  call_line <- sprintf("pv_flow_map(%s, from = %s, to = %s, value = %s%s)",
                       frame, suggest_q(from), suggest_q(to),
                       suggest_q(m$name), map_part)
  list(chart = "pv_flow_map",
       code = paste(c(lines, call_line), collapse = "\n"),
       reason = sprintf(
         "`%s` and `%s` hold %s names, so each row draws as a curved band from origin to destination%s",
         from, to, layer$label, note),
       score = 93)
}

suggest_calendar <- function(p, nm) {
  if (is.null(p$date_col)) {
    return(NULL)
  }
  m <- NULL
  for (cand in p$measures) {
    if (cand$kind == "numeric") {
      m <- cand
      break
    }
  }
  if (is.null(m)) {
    return(NULL)
  }
  dates <- p$data[[p$date_col]]
  ud <- sort(unique(dates[!is.na(dates)]))
  # "Daily" means many days sitting mostly one apart - a monthly series
  # would render as 12 lonely cells per year.
  if (length(ud) < 60 ||
      stats::median(as.numeric(diff(ud))) > 1.5) {
    return(NULL)
  }
  keep <- suggest_complete(p$data, c(p$date_col, m$name))
  dup <- anyDuplicated(dates[keep]) > 0
  note <- ""
  if (dup) {
    if (!suggest_syntactic(c(m$name, p$date_col))) {
      return(NULL)
    }
    agg <- suggest_agg(m$name, p$date_col, nm, m$fun)
    frame <- agg$var
    lines <- agg$line
    note <- agg$note
  } else {
    frame <- nm
    lines <- character(0)
  }
  # The calendar holds at most six year blocks; past that, the printed
  # call windows to the most recent six rather than error.
  yrs <- sort(unique(as.integer(format(ud, "%Y"))))
  years_part <- if (length(yrs) > 6) {
    sprintf(", years = %d:%d", max(yrs) - 5L, max(yrs))
  } else {
    ""
  }
  call_line <- sprintf("pv_calendar(%s, date = %s, value = %s%s)",
                       frame, suggest_q(p$date_col), suggest_q(m$name),
                       years_part)
  list(chart = "pv_calendar",
       code = paste(c(lines, call_line), collapse = "\n"),
       reason = sprintf(
         "`%s` is daily, so `%s` reads as a year-at-a-glance calendar - weekday rhythm and seasons jump out%s",
         p$date_col, m$name, note),
       score = 90)
}

suggest_line <- function(p, nm) {
  if (is.null(p$time)) {
    return(NULL)
  }
  x <- p$time$col
  m <- NULL
  for (cand in p$measures) {
    if (cand$kind == "numeric" && cand$n_ok >= 3) {
      m <- cand
      break
    }
  }
  if (is.null(m)) {
    return(NULL)
  }
  axis_word <- if (p$time$kind == "date") "date" else "year"

  # A series column that already gives one row per x per level draws
  # directly; otherwise the smallest grouping is aggregated over. With no
  # grouping at all, repeated x values are folded down to one row each.
  unique_maker <- NULL
  smallest <- NULL
  for (cc in p$cats) {
    keep <- suggest_complete(p$data, c(x, m$name, cc$name))
    if (!any(keep)) next
    if (anyDuplicated(paste(p$data[[x]][keep],
                            p$data[[cc$name]][keep], sep = "\r")) == 0) {
      if (is.null(unique_maker) || cc$distinct < unique_maker$distinct) {
        unique_maker <- cc
      }
    }
    if (is.null(smallest) || cc$distinct < smallest$distinct) {
      smallest <- cc
    }
  }
  if (!is.null(unique_maker)) {
    return(list(chart = "pv_line",
      code = sprintf("pv_line(%s, x = %s, y = %s, series = %s)",
                     nm, suggest_q(x), suggest_q(m$name),
                     suggest_q(unique_maker$name)),
      reason = sprintf(
        "a %s and a numeric per %s read naturally as lines",
        axis_word, unique_maker$name),
      score = 85))
  }
  if (!is.null(smallest) && suggest_syntactic(c(m$name, x, smallest$name))) {
    agg <- suggest_agg(m$name, c(x, smallest$name), nm, m$fun)
    return(list(chart = "pv_line",
      code = paste(c(agg$line,
        sprintf("pv_line(%s, x = %s, y = %s, series = %s)",
                agg$var, suggest_q(x), suggest_q(m$name),
                suggest_q(smallest$name))), collapse = "\n"),
      reason = sprintf(
        "a %s and a numeric per %s read naturally as lines%s",
        axis_word, smallest$name, agg$note),
      score = 85))
  }
  keep <- suggest_complete(p$data, c(x, m$name))
  if (!any(keep)) {
    return(NULL)
  }
  if (anyDuplicated(p$data[[x]][keep]) > 0) {
    if (!suggest_syntactic(c(m$name, x))) {
      return(NULL)
    }
    agg <- suggest_agg(m$name, x, nm, m$fun)
    return(list(chart = "pv_line",
      code = paste(c(agg$line,
        sprintf("pv_line(%s, x = %s, y = %s)",
                agg$var, suggest_q(x), suggest_q(m$name))), collapse = "\n"),
      reason = sprintf("`%s` over `%s` reads naturally as a line%s",
                       m$name, x, agg$note),
      score = 85))
  }
  list(chart = "pv_line",
       code = sprintf("pv_line(%s, x = %s, y = %s)",
                      nm, suggest_q(x), suggest_q(m$name)),
       reason = sprintf("`%s` over `%s` reads naturally as a line",
                        m$name, x),
       score = 85)
}

# Exactly two moments per group: the slope chart's home ground. The
# profile's time axis wants three points or more, so this builder hunts
# on its own for a Date or year column holding exactly two distinct
# values - too few for a line, exactly right for a slope. The signal is
# strong and distinctive, so it scores above pv_line.
suggest_slope <- function(p, nm) {
  nms <- names(p$data)
  low <- tolower(nms)
  x <- NULL
  axis_word <- NULL
  for (i in seq_along(nms)) {
    v <- p$data[[nms[[i]]]]
    if (p$kind[[i]] == "date") {
      ok <- v[!is.na(v)]
      if (length(ok) && length(unique(ok)) == 2) {
        x <- nms[[i]]
        axis_word <- "date"
        break
      }
    } else if (p$kind[[i]] == "numeric" &&
               grepl("^(year|yr|jahr|annee)s?$", low[[i]])) {
      ok <- v[!is.na(v)]
      if (length(ok) && length(unique(ok)) == 2 && all(ok == trunc(ok)) &&
          min(ok) >= 1500 && max(ok) <= 2200) {
        x <- nms[[i]]
        axis_word <- "year"
        break
      }
    }
  }
  if (is.null(x)) {
    return(NULL)
  }
  m <- NULL
  for (cand in p$measures) {
    if (cand$kind == "numeric") {
      m <- cand
      break
    }
  }
  if (is.null(m)) {
    return(NULL)
  }
  # The grouping: one row per position per level, with at least two
  # groups present at both ends - a slope wants clean pairs, so no
  # aggregate() escape hatch here; messier frames fall through to the
  # other builders.
  g <- NULL
  for (cc in p$cats) {
    keep <- suggest_complete(p$data, c(x, m$name, cc$name))
    if (!any(keep)) next
    key <- paste(p$data[[x]][keep], p$data[[cc$name]][keep], sep = "\r")
    if (anyDuplicated(key) > 0) next
    per <- table(as.character(p$data[[cc$name]][keep]))
    if (sum(per == 2) < 2) next
    if (is.null(g) || cc$distinct < g$distinct) {
      g <- cc
    }
  }
  if (is.null(g)) {
    return(NULL)
  }
  xv <- p$data[[x]]
  pos <- sort(unique(xv[!is.na(xv)]))
  pos <- if (inherits(xv, "Date")) {
    format(pos, "%Y-%m-%d")
  } else {
    format(pos, trim = TRUE)
  }
  list(chart = "pv_slope",
       code = sprintf("pv_slope(%s, x = %s, y = %s, group = %s)",
                      nm, suggest_q(x), suggest_q(m$name), suggest_q(g$name)),
       reason = sprintf(
         "`%s` holds exactly two %ss (%s and %s), so each `%s` draws one line whose slope is its change",
         x, axis_word, pos[[1]], pos[[2]], g$name),
       score = 88)
}

# A time axis, a measure, and a grouping with more levels than the
# palette holds: the spaghetti threshold. Past it a line chart drowns -
# every extra series is one more indistinguishable strand - so the
# horizon takes over and folds each series into its own compact shaded
# ribbon. The cap comes from pv_horizon's own height floor: the default
# 420px chart holds at most 26 rows at 12px each, and a suggestion must
# run exactly as printed.
suggest_horizon <- function(p, nm) {
  if (is.null(p$time)) {
    return(NULL)
  }
  x <- p$time$col
  m <- NULL
  for (cand in p$measures) {
    if (cand$kind == "numeric" && cand$n_ok >= 3) {
      m <- cand
      break
    }
  }
  if (is.null(m)) {
    return(NULL)
  }
  # The largest grouping that still fits the default height: the horizon
  # is the many-series form, so the more ribbons the better its case.
  slots <- theme_palette_slots()
  g <- NULL
  for (cc in p$cats) {
    if (cc$distinct <= slots || cc$distinct > 26) next
    if (is.null(g) || cc$distinct > g$distinct) {
      g <- cc
    }
  }
  if (is.null(g)) {
    return(NULL)
  }
  keep <- suggest_complete(p$data, c(x, m$name, g$name))
  if (!any(keep)) {
    return(NULL)
  }
  dup <- anyDuplicated(paste(p$data[[x]][keep],
                             p$data[[g$name]][keep], sep = "\r")) > 0
  note <- ""
  if (dup) {
    if (!suggest_syntactic(c(m$name, x, g$name))) {
      return(NULL)
    }
    agg <- suggest_agg(m$name, c(x, g$name), nm, m$fun)
    frame <- agg$var
    lines <- agg$line
    note <- agg$note
  } else {
    frame <- nm
    lines <- character(0)
  }
  call_line <- sprintf("pv_horizon(%s, x = %s, y = %s, series = %s)",
                       frame, suggest_q(x), suggest_q(m$name),
                       suggest_q(g$name))
  list(chart = "pv_horizon",
       code = paste(c(lines, call_line), collapse = "\n"),
       reason = sprintf(
         "%d `%s` series would tangle as spaghetti lines; the horizon folds each into its own compact shaded ribbon%s",
         g$distinct, g$name, note),
       score = 86)
}

# Exactly two numeric columns on one shared scale, one row per category:
# the wide before/after shape. It rides alongside the scatter - the
# scatter shows how the pair co-varies, the dumbbell shows each row's
# gap - and edges just ahead of it, being the more specific read.
suggest_dumbbell <- function(p, nm) {
  nums <- Filter(function(m) m$kind == "numeric" && m$distinct >= 2,
                 p$measures)
  if (length(nums) != 2) {
    return(NULL)
  }
  a <- nums[[1]]
  b <- nums[[2]]
  # Same-ish scale means the two value ranges overlap. Columns in
  # different units (counts against francs, say) share no axis, so they
  # stay a scatter.
  if (max(a$min, b$min) > min(a$max, b$max)) {
    return(NULL)
  }
  g <- NULL
  for (cc in p$cats) {
    keep <- suggest_complete(p$data, c(cc$name, a$name, b$name))
    k <- sum(keep)
    if (k < 2 || k > 40) next
    if (anyDuplicated(p$data[[cc$name]][keep]) > 0) next
    if (is.null(g) || cc$distinct > g$distinct) {
      g <- cc
    }
  }
  if (is.null(g)) {
    return(NULL)
  }
  list(chart = "pv_dumbbell",
       code = sprintf("pv_dumbbell(%s, y = %s, x1 = %s, x2 = %s)",
                      nm, suggest_q(g$name), suggest_q(a$name),
                      suggest_q(b$name)),
       reason = sprintf(
         "`%s` and `%s` sit on one scale with one row per `%s`, so each row reads as a pair with its gap in view",
         a$name, b$name, g$name),
       score = 73)
}

# The pyramid's naming signal: do these two column names read as the
# two poles of one opposing flow? Each entry pairs the words for one
# pole with the words for its opposite; a name carries a pole when one
# of its separator-split tokens is on that pole's word list, so
# "commuters_in" reads as inbound but "internal" never does. Returns
# the two names ordered left/right (first pole left, matching the
# population-pyramid convention of male-left/female-right), or NULL.
suggest_pyramid_sides <- function(a, b) {
  poles <- list(
    c("in inbound inflow incoming arrivals immigration einpendler zuzug",
      "out outbound outflow outgoing departures emigration auspendler wegzug"),
    c("male males men maenner hommes uomini",
      "female females women frauen femmes donne"),
    c("import imports importe importations",
      "export exports exporte exportations"),
    c("from origin source von herkunft",
      "to dest destination target nach ziel"))
  words_of <- function(s) strsplit(s, " ", fixed = TRUE)[[1]]
  tokens <- function(nm) strsplit(tolower(nm), "[^a-z]+")[[1]]
  ta <- tokens(a)
  tb <- tokens(b)
  for (pair in poles) {
    first <- words_of(pair[[1]])
    second <- words_of(pair[[2]])
    if (any(ta %in% first) && any(tb %in% second)) {
      return(list(left = a, right = b))
    }
    if (any(ta %in% second) && any(tb %in% first)) {
      return(list(left = b, right = a))
    }
  }
  NULL
}

# Exactly two non-negative numeric columns whose names read as opposing
# flows, one row per category: the pyramid's home ground. The same wide
# shape the dumbbell reads as before/after, but the naming signal says
# the two columns are two directions of one thing - so the pyramid is
# the more specific read and edges ahead of it.
suggest_pyramid <- function(p, nm) {
  nums <- Filter(function(m) m$kind == "numeric", p$measures)
  if (length(nums) != 2) {
    return(NULL)
  }
  a <- nums[[1]]
  b <- nums[[2]]
  # Mirrored bars grow outward from the spine; a negative value has no
  # side to sit on.
  if (a$min < 0 || b$min < 0 || (a$max <= 0 && b$max <= 0)) {
    return(NULL)
  }
  sides <- suggest_pyramid_sides(a$name, b$name)
  if (is.null(sides)) {
    return(NULL)
  }
  g <- NULL
  for (cc in p$cats) {
    keep <- suggest_complete(p$data, c(cc$name, a$name, b$name))
    k <- sum(keep)
    if (k < 2 || k > 40) next
    if (anyDuplicated(p$data[[cc$name]][keep]) > 0) next
    if (is.null(g) || cc$distinct > g$distinct) {
      g <- cc
    }
  }
  if (is.null(g)) {
    return(NULL)
  }
  list(chart = "pv_pyramid",
       code = sprintf("pv_pyramid(%s, y = %s, left = %s, right = %s)",
                      nm, suggest_q(g$name), suggest_q(sides$left),
                      suggest_q(sides$right)),
       reason = sprintf(
         "`%s` and `%s` name opposing flows, so each `%s` mirrors as a left/right pair from one centre spine",
         sides$left, sides$right, g$name),
       score = 75)
}

# A summable measure that mixes gains and losses over a handful of
# categories reads like contributions building toward a total. That is a
# guess - only the author knows whether the rows are steps of one story
# - so the reason says so out loud.
suggest_waterfall <- function(p, nm) {
  m <- NULL
  for (cand in p$measures) {
    if (cand$kind != "numeric" || !identical(cand$fun, "sum")) next
    v <- p$data[[cand$name]]
    ok <- v[!is.na(v)]
    if (!length(ok) || any(!is.finite(ok))) next
    neg <- mean(ok < 0)
    # Negatives must be a real minority: none means plain bars, half or
    # more means the "total" the bars build toward is not the story.
    if (neg >= 0.1 && neg < 0.5 && any(ok > 0)) {
      m <- cand
      break
    }
  }
  if (is.null(m)) {
    return(NULL)
  }
  fitting <- Filter(function(cc) cc$distinct >= 2 && cc$distinct <= 15,
                    p$cats)
  if (!length(fitting)) {
    return(NULL)
  }
  counts <- vapply(fitting, function(cc) cc$distinct, numeric(1))
  cat_col <- fitting[[which.max(counts)]]
  keep <- suggest_complete(p$data, c(cat_col$name, m$name))
  if (!any(keep)) {
    return(NULL)
  }
  dup <- anyDuplicated(p$data[[cat_col$name]][keep]) > 0
  note <- ""
  if (dup) {
    if (!suggest_syntactic(c(m$name, cat_col$name))) {
      return(NULL)
    }
    agg <- suggest_agg(m$name, cat_col$name, nm, "sum")
    frame <- agg$var
    lines <- agg$line
    note <- agg$note
  } else {
    frame <- nm
    lines <- character(0)
  }
  call_line <- sprintf("pv_waterfall(%s, x = %s, y = %s)",
                       frame, suggest_q(cat_col$name), suggest_q(m$name))
  list(chart = "pv_waterfall",
       code = paste(c(lines, call_line), collapse = "\n"),
       reason = sprintf(
         "`%s` mixes gains and losses - if the `%s` rows are steps of one story, a waterfall shows them building to the total (a guess: only you know whether they are)%s",
         m$name, cat_col$name, note),
       score = 58)
}

# Two category columns where one nests strictly inside the other - every
# child level belongs to exactly one parent level, and there are more
# children than parents - plus a summable non-negative measure: a
# genuine hierarchy.
suggest_sunburst <- function(p, nm) {
  m <- NULL
  for (cand in p$measures) {
    if (cand$kind == "numeric" && identical(cand$fun, "sum") &&
        cand$min >= 0) {
      m <- cand
      break
    }
  }
  if (is.null(m)) {
    return(NULL)
  }
  best <- NULL
  for (parent in p$cats) {
    if (parent$distinct < 2 || parent$distinct > theme_palette_slots()) {
      next
    }
    for (child in p$cats) {
      if (identical(child$name, parent$name)) next
      # Strictly more children than parents rules out 1:1 relabelings
      # (a code column and its name column); the cap keeps the rings
      # countable.
      if (child$distinct <= parent$distinct || child$distinct > 40) next
      keep <- suggest_complete(p$data, c(parent$name, child$name, m$name))
      if (!any(keep)) next
      pv <- as.character(p$data[[parent$name]][keep])
      cv <- as.character(p$data[[child$name]][keep])
      # Nested means each child level appears under exactly one parent:
      # as many distinct (child, parent) pairs as distinct children.
      if (length(unique(paste(cv, pv, sep = "\r"))) != length(unique(cv))) {
        next
      }
      if (is.null(best) || child$distinct > best$child$distinct) {
        best <- list(parent = parent, child = child)
      }
    }
  }
  if (is.null(best)) {
    return(NULL)
  }
  list(chart = "pv_sunburst",
       code = sprintf("pv_sunburst(%s, levels = c(%s, %s), value = %s)",
                      nm, suggest_q(best$parent$name),
                      suggest_q(best$child$name), suggest_q(m$name)),
       reason = sprintf(
         "every `%s` belongs to exactly one `%s`, so `%s` nests as rings - pv_icicle draws the same tree with readable labels",
         best$child$name, best$parent$name, m$name),
       score = 66)
}

suggest_area <- function(p, nm) {
  if (is.null(p$time)) {
    return(NULL)
  }
  x <- p$time$col
  m <- NULL
  for (cand in p$measures) {
    if (cand$kind == "numeric" && cand$n_ok >= 3 && cand$min >= 0) {
      m <- cand
      break
    }
  }
  if (is.null(m)) {
    return(NULL)
  }
  # Stacked bands need a small set of series - one palette slot each.
  series <- NULL
  for (cc in p$cats) {
    if (cc$distinct < 2 || cc$distinct > theme_palette_slots()) next
    if (is.null(series) || cc$distinct < series$distinct) {
      series <- cc
    }
  }
  if (is.null(series)) {
    return(NULL)
  }
  keep <- suggest_complete(p$data, c(x, m$name, series$name))
  if (!any(keep)) {
    return(NULL)
  }
  dup <- anyDuplicated(paste(p$data[[x]][keep],
                             p$data[[series$name]][keep], sep = "\r")) > 0
  note <- ""
  if (dup) {
    if (!suggest_syntactic(c(m$name, x, series$name))) {
      return(NULL)
    }
    agg <- suggest_agg(m$name, c(x, series$name), nm, m$fun)
    frame <- agg$var
    lines <- agg$line
    note <- agg$note
  } else {
    frame <- nm
    lines <- character(0)
  }
  call_line <- sprintf("pv_area(%s, x = %s, y = %s, series = %s)",
                       frame, suggest_q(x), suggest_q(m$name),
                       suggest_q(series$name))
  list(chart = "pv_area",
       code = paste(c(lines, call_line), collapse = "\n"),
       reason = sprintf(
         "the `%s` bands are non-negative, so stacking them also shows the total%s",
         series$name, note),
       score = 68)
}

suggest_heatmap <- function(p, nm) {
  if (!length(p$measures)) {
    return(NULL)
  }
  m <- p$measures[[1]]
  fitting <- Filter(function(cc) cc$distinct >= 2 && cc$distinct <= 30,
                    p$cats)
  if (length(fitting) < 2) {
    return(NULL)
  }
  ord <- order(vapply(fitting, function(cc) cc$distinct, numeric(1)))
  y <- fitting[[ord[[1]]]]$name
  x <- fitting[[ord[[2]]]]$name
  keep <- suggest_complete(p$data, c(x, y, m$name))
  if (!any(keep)) {
    return(NULL)
  }
  dup <- anyDuplicated(paste(p$data[[x]][keep],
                             p$data[[y]][keep], sep = "\r")) > 0
  note <- ""
  if (dup) {
    if (!suggest_syntactic(c(m$name, x, y))) {
      return(NULL)
    }
    agg <- suggest_agg(m$name, c(x, y), nm, m$fun)
    frame <- agg$var
    lines <- agg$line
    note <- agg$note
  } else {
    frame <- nm
    lines <- character(0)
  }
  call_line <- sprintf("pv_heatmap(%s, x = %s, y = %s, value = %s)",
                       frame, suggest_q(x), suggest_q(y), suggest_q(m$name))
  list(chart = "pv_heatmap",
       code = paste(c(lines, call_line), collapse = "\n"),
       reason = sprintf(
         "one value per `%s`/`%s` pair reads as a colour grid - patterns across the whole matrix at once%s",
         x, y, note),
       score = 74)
}

suggest_scatter <- function(p, nm) {
  nums <- Filter(function(m) m$kind == "numeric" && m$n_ok >= 3 &&
                   m$distinct >= 2, p$measures)
  if (length(nums) < 2) {
    return(NULL)
  }
  x <- nums[[1]]$name
  y <- nums[[2]]$name
  density <- p$n > 5000
  label <- NULL
  if (!density) {
    for (cc in p$cats) {
      if (cc$distinct >= 0.9 * p$n) {
        label <- cc$name
        break
      }
    }
  }
  extra <- if (density) {
    ", density = TRUE"
  } else if (!is.null(label)) {
    sprintf(", label = %s", suggest_q(label))
  } else {
    ""
  }
  reason <- if (density) {
    sprintf(
      "%s rows would smear as dots; density = TRUE draws the joint shape as filled contours instead (or density = \"hex\" as countable hexagon bins)",
      format(p$n, big.mark = ","))
  } else {
    sprintf("two numeric columns invite a look at how `%s` moves with `%s`",
            y, x)
  }
  list(chart = "pv_scatter",
       code = sprintf("pv_scatter(%s, x = %s, y = %s%s)",
                      nm, suggest_q(x), suggest_q(y), extra),
       reason = reason,
       score = 72)
}

suggest_pairs <- function(p, nm) {
  nums <- Filter(function(m) m$kind == "numeric" && m$n_ok >= 2 &&
                   m$distinct >= 2, p$measures)
  if (length(nums) < 4) {
    return(NULL)
  }
  cols <- utils::head(vapply(nums, function(m) m$name, character(1)), 6)
  list(chart = "pv_pairs",
       code = sprintf("pv_pairs(%s, columns = c(%s))",
                      nm, paste(suggest_q(cols), collapse = ", ")),
       reason = sprintf(
         "%d numeric columns pair up in one matrix - every relationship and distribution at a glance",
         length(cols)),
       score = 76)
}

suggest_parallel <- function(p, nm) {
  nums <- Filter(function(m) m$kind == "numeric" && m$distinct >= 2,
                 p$measures)
  if (length(nums) < 4 || p$n > 1500) {
    return(NULL)
  }
  cols <- utils::head(vapply(nums, function(m) m$name, character(1)), 6)
  if (sum(stats::complete.cases(p$data[cols])) < 1) {
    return(NULL)
  }
  list(chart = "pv_parallel",
       code = sprintf("pv_parallel(%s, columns = c(%s))",
                      nm, paste(suggest_q(cols), collapse = ", ")),
       reason = sprintf(
         "each row runs one line across the %d numeric axes, so clusters and trade-offs stand out",
         length(cols)),
       score = 56)
}

# Bar for a modest number of categories, sorted lollipops when there are
# too many bars to read but still few enough to rank.
suggest_category <- function(p, nm) {
  if (!length(p$measures)) {
    return(NULL)
  }
  m <- p$measures[[1]]
  barable <- Filter(function(cc) cc$distinct >= 2 && cc$distinct <= 12,
                    p$cats)
  lollable <- Filter(function(cc) cc$distinct >= 13 && cc$distinct <= 40,
                     p$cats)
  if (length(barable)) {
    counts <- vapply(barable, function(cc) cc$distinct, numeric(1))
    cat_col <- barable[[which.max(counts)]]
    chart <- "pv_bar"
    score <- 64
  } else if (length(lollable)) {
    counts <- vapply(lollable, function(cc) cc$distinct, numeric(1))
    cat_col <- lollable[[which.min(counts)]]
    chart <- "pv_lollipop"
    score <- 63
  } else {
    return(NULL)
  }
  keep <- suggest_complete(p$data, c(cat_col$name, m$name))
  if (!any(keep)) {
    return(NULL)
  }
  dup <- anyDuplicated(p$data[[cat_col$name]][keep]) > 0
  note <- ""
  if (dup) {
    if (!suggest_syntactic(c(m$name, cat_col$name))) {
      return(NULL)
    }
    agg <- suggest_agg(m$name, cat_col$name, nm, m$fun)
    frame <- agg$var
    lines <- agg$line
    note <- agg$note
  } else {
    frame <- nm
    lines <- character(0)
  }
  call_line <- sprintf("%s(%s, x = %s, y = %s)", chart, frame,
                       suggest_q(cat_col$name), suggest_q(m$name))
  reason <- if (chart == "pv_bar") {
    sprintf("one number per `%s` compares cleanly as bars%s",
            cat_col$name, note)
  } else {
    sprintf(
      "%d `%s` levels rank more readably as sorted lollipops than as bars%s",
      cat_col$distinct, cat_col$name, note)
  }
  list(chart = chart,
       code = paste(c(lines, call_line), collapse = "\n"),
       reason = reason, score = score)
}

# A donut only where shares of a whole make sense: few categories, a
# non-negative measure, and a measure that sums (an averaged index has no
# meaningful total to split).
suggest_donut <- function(p, nm) {
  if (!length(p$measures)) {
    return(NULL)
  }
  m <- p$measures[[1]]
  if (!identical(m$fun, "sum") || m$min < 0) {
    return(NULL)
  }
  fitting <- Filter(function(cc) cc$distinct >= 2 && cc$distinct <= 6,
                    p$cats)
  if (!length(fitting)) {
    return(NULL)
  }
  counts <- vapply(fitting, function(cc) cc$distinct, numeric(1))
  cat_col <- fitting[[which.max(counts)]]
  keep <- suggest_complete(p$data, c(cat_col$name, m$name))
  if (!any(keep)) {
    return(NULL)
  }
  # pv_donut refuses missing values and sums duplicates itself, but an
  # explicit aggregate() line makes the whole-and-its-parts idea visible.
  dup <- anyDuplicated(p$data[[cat_col$name]][keep]) > 0
  nas <- anyNA(p$data[[m$name]])
  note <- ""
  if (dup || nas) {
    if (!suggest_syntactic(c(m$name, cat_col$name))) {
      return(NULL)
    }
    agg <- suggest_agg(m$name, cat_col$name, nm, "sum")
    frame <- agg$var
    lines <- agg$line
    note <- agg$note
  } else {
    frame <- nm
    lines <- character(0)
  }
  call_line <- sprintf("pv_donut(%s, category = %s, value = %s)",
                       frame, suggest_q(cat_col$name), suggest_q(m$name))
  list(chart = "pv_donut",
       code = paste(c(lines, call_line), collapse = "\n"),
       reason = sprintf(
         "`%s` splits a non-negative total %d ways, so shares of the whole make sense%s",
         m$name, cat_col$distinct, note),
       score = 40)
}

# The waffle rides the donut's coat-tails: the same parts-of-a-whole
# test, deliberately mirrored condition for condition, offered as the
# alternative whose squares can actually be counted. It scores one below
# the donut so the pair always sits together in the list.
suggest_waffle <- function(p, nm) {
  if (!length(p$measures)) {
    return(NULL)
  }
  m <- p$measures[[1]]
  if (!identical(m$fun, "sum") || m$min < 0) {
    return(NULL)
  }
  fitting <- Filter(function(cc) cc$distinct >= 2 && cc$distinct <= 6,
                    p$cats)
  if (!length(fitting)) {
    return(NULL)
  }
  counts <- vapply(fitting, function(cc) cc$distinct, numeric(1))
  cat_col <- fitting[[which.max(counts)]]
  keep <- suggest_complete(p$data, c(cat_col$name, m$name))
  if (!any(keep)) {
    return(NULL)
  }
  # A waffle divides a total into squares, so the total must be there
  # to divide.
  if (sum(as.numeric(p$data[[m$name]][keep])) <= 0) {
    return(NULL)
  }
  # pv_waffle sums duplicate categories itself, but the donut beside
  # this suggestion prints the aggregate() step - showing the same code
  # here keeps the pair interchangeable.
  dup <- anyDuplicated(p$data[[cat_col$name]][keep]) > 0
  nas <- anyNA(p$data[[m$name]])
  note <- ""
  if (dup || nas) {
    if (!suggest_syntactic(c(m$name, cat_col$name))) {
      return(NULL)
    }
    agg <- suggest_agg(m$name, cat_col$name, nm, "sum")
    frame <- agg$var
    lines <- agg$line
    note <- agg$note
  } else {
    frame <- nm
    lines <- character(0)
  }
  call_line <- sprintf("pv_waffle(%s, category = %s, value = %s)",
                       frame, suggest_q(cat_col$name), suggest_q(m$name))
  list(chart = "pv_waffle",
       code = paste(c(lines, call_line), collapse = "\n"),
       reason = sprintf(
         "the same %d shares of `%s` as countable unit squares - a waffle reads more precisely than a donut%s",
         cat_col$distinct, m$name, note),
       score = 39)
}

# One distribution chart, picked by group cardinality: violins for two or
# three well-filled groups, ridgelines for four to eight, boxes when the
# groups are too thin for honest densities.
suggest_distribution <- function(p, nm) {
  m <- NULL
  for (cand in p$measures) {
    if (cand$kind == "numeric" && cand$distinct >= 5) {
      m <- cand
      break
    }
  }
  if (is.null(m)) {
    return(NULL)
  }
  fitting <- Filter(function(cc) cc$distinct >= 2 && cc$distinct <= 8,
                    p$cats)
  if (!length(fitting)) {
    return(NULL)
  }
  counts <- vapply(fitting, function(cc) cc$distinct, numeric(1))
  g <- fitting[[which.max(counts)]]
  keep <- suggest_complete(p$data, c(g$name, m$name))
  if (!any(keep)) {
    return(NULL)
  }
  per_group <- table(as.character(p$data[[g$name]][keep]))
  if (length(per_group) < 2 || min(per_group) < 5) {
    return(NULL)
  }
  k <- length(per_group)
  chart <- if (k <= 3 && min(per_group) >= 30) {
    "pv_violin"
  } else if (k >= 4 && min(per_group) >= 15) {
    "pv_ridgeline"
  } else {
    "pv_boxplot"
  }
  reason <- switch(chart,
    pv_violin = sprintf(
      "`%s` repeats across %d well-filled `%s` groups - violins show each distribution's full shape",
      m$name, k, g$name),
    pv_ridgeline = sprintf(
      "`%s` spreads across %d `%s` groups, which stack readably as ridgelines",
      m$name, k, g$name),
    pv_boxplot = sprintf(
      "`%s` repeats across %d `%s` groups; boxes compare the spreads",
      m$name, k, g$name))
  list(chart = chart,
       code = sprintf("%s(%s, value = %s, group = %s)", chart, nm,
                      suggest_q(m$name), suggest_q(g$name)),
       reason = reason, score = 60)
}

suggest_histogram <- function(p, nm) {
  m <- NULL
  for (cand in p$measures) {
    if (cand$kind == "numeric" && cand$distinct >= 20 && cand$n_ok >= 30) {
      m <- cand
      break
    }
  }
  if (is.null(m)) {
    return(NULL)
  }
  list(chart = "pv_histogram",
       code = sprintf("pv_histogram(%s, x = %s)", nm, suggest_q(m$name)),
       reason = sprintf("the spread of `%s` is worth a first look on its own",
                        m$name),
       score = 45)
}

suggest_spark_table <- function(p, nm) {
  # pv_table holds at most 5,000 rows; past that there is nothing to offer.
  if (!length(p$spark_cols) || p$n > 5000) {
    return(NULL)
  }
  spark_part <- sprintf(", spark = c(%s)",
                        paste(suggest_q(p$spark_cols), collapse = ", "))
  # Any list-column that cannot be a sparkline would make pv_table balk,
  # so those are left out via an explicit column list.
  other_lists <- setdiff(p$list_cols, p$spark_cols)
  cols_part <- if (length(other_lists)) {
    keep <- c(setdiff(names(p$data), p$list_cols), p$spark_cols)
    sprintf(", columns = c(%s)", paste(suggest_q(keep), collapse = ", "))
  } else {
    ""
  }
  list(chart = "pv_table",
       code = sprintf("pv_table(%s%s%s)", nm, cols_part, spark_part),
       reason = sprintf(
         "`%s` holds a numeric vector per row - a table draws them as inline sparklines",
         p$spark_cols[[1]]),
       score = 55)
}

# The last resort: when no chart shape stands out, at least the rows can
# be browsed and sorted.
suggest_fallback_table <- function(p, nm) {
  if (p$n > 5000) {
    return(NULL)
  }
  cols_part <- ""
  if (length(p$list_cols)) {
    keep <- setdiff(names(p$data), p$list_cols)
    if (!length(keep)) {
      return(NULL)
    }
    cols_part <- sprintf(", columns = c(%s)",
                         paste(suggest_q(keep), collapse = ", "))
  }
  list(chart = "pv_table",
       code = sprintf("pv_table(%s%s)", nm, cols_part),
       reason = "a sortable table shows the raw rows while you decide",
       score = 8)
}

# ---- the promise: nothing printed can fail ---------------------------------

# Every candidate is built once, silently, before it is offered: the code
# string is parsed and evaluated against the actual data, and anything
# that errors - or returns something other than a chart - is dropped.
# What survives is exactly what the user will run.
suggest_runs <- function(code, nm, data) {
  env <- new.env(parent = asNamespace("polyviz"))
  assign(nm, data, envir = env)
  w <- tryCatch(
    suppressMessages(suppressWarnings(eval(parse(text = code), env))),
    error = function(e) NULL)
  inherits(w, "htmlwidget")
}

#' Suggest charts for a data frame
#'
#' The blank-page killer: hand `pv_suggest()` a data frame and it prints
#' a short list of runnable chart calls with the real column names filled
#' in, each with one line of reasoning. Copy the call you like, run it,
#' and go from there.
#'
#' The suggestions come from what is actually in the data: column types
#' and cardinalities, whether keys repeat (which decides between a direct
#' call and an `aggregate()` step, printed as part of the code), a `Date`
#' or year column (lines; a calendar for daily dates; a slope chart when
#' it holds exactly two moments per group; a horizon chart when a
#' grouping holds more series than the palette has colours, the point
#' where a line chart turns to spaghetti), pairs of numeric columns
#' (scatter, with `density = TRUE` past a few thousand rows and the
#' reason naming `density = "hex"` as the countable alternative; a
#' dumbbell when exactly two share a scale with one row per category; a
#' pyramid when exactly two are non-negative and named as opposing
#' flows - in and out, male and female, imports and exports),
#' many numeric columns (a pairs matrix), longitude/latitude pairs (a
#' bubble map), id columns matching the bundled Swiss map layers (a
#' choropleth), a column of two-letter canton codes with one row per
#' canton (a hex cartogram), two place-name columns whose values are
#' feature names on
#' one bundled layer plus a non-negative measure (a flow map), grouped
#' numeric columns (boxplot, violin, or ridgeline, by group count), a
#' signed measure whose losses are a real minority (a waterfall, offered
#' as a guess), nested category columns (a sunburst, with the icicle
#' named as its readable-label twin), and parts of a whole (a donut,
#' with a waffle beside it as the precise-reading alternative). Every
#' candidate is built once behind the scenes before it is offered, so a
#' printed suggestion never errors on the data it was suggested for.
#'
#' @param data A data frame to inspect.
#' @param n Maximum number of suggestions to keep, best fit first
#'   (default 4).
#' @return An object of class `pv_suggestions`: a list with the data
#'   frame's name and dimensions plus a `suggestions` list, one record
#'   per suggestion with `chart` (the function's name), `code` (the
#'   runnable call, sometimes led by an `aggregate()` line), and
#'   `reason` (one plain-English line). Its print method renders the
#'   numbered list; the object itself is returned for programmatic use.
#' @examples
#' # Daily weather readings: a calendar and a line top the list.
#' pv_suggest(pv_weather)
#'
#' # A date, two categories and two measures - note the aggregate() step
#' # spelled out where a chart needs one row per key.
#' pv_suggest(pv_sales)
#'
#' # Municipality ids that match the bundled Lucerne map layer.
#' fiscal25 <- subset(pv_fiscal, year == 2025)
#' pv_suggest(fiscal25)
#'
#' # Exactly two census years per city: the slope chart's home ground.
#' census <- subset(pv_city_population, year %in% c(1930, 2024))
#' pv_suggest(census)
#'
#' # 26 cantons over 21 years - past the spaghetti threshold, so a
#' # horizon chart is offered where a line chart would drown.
#' pv_suggest(pv_tourism)
#'
#' # Two columns of canton names and a count: origin-destination flows.
#' pendler <- data.frame(
#'   from = c("Aargau", "Luzern", "Schwyz", "Zug"),
#'   to = c("Zug", "Zug", "Zug", "Luzern"),
#'   commuters = c(4905, 11251, 4576, 4925))
#' pv_suggest(pendler)
#' @export
pv_suggest <- function(data, n = 4) {
  check_columns(data, list())
  if (!nrow(data)) {
    rlang::abort("`data` has no rows; nothing to suggest.")
  }
  if (!is.numeric(n) || length(n) != 1 || is.na(n) || n < 1 ||
      n != trunc(n)) {
    rlang::abort("`n` must be a single whole number, 1 or more.")
  }
  n <- as.integer(n)
  # The printed calls use the caller's own name for the data frame, so
  # they run as-is. Anything that isn't a plain name (a pipe, a subset()
  # inline) is shown as `data`, and the print method says so.
  nm <- paste(deparse(substitute(data)), collapse = " ")
  placeholder <- !grepl("^[a-zA-Z.][a-zA-Z0-9._]*$", nm)
  if (placeholder) {
    nm <- "data"
  }

  p <- suggest_profile(data)
  builders <- list(
    suggest_choropleth, suggest_hexmap, suggest_bubble_map,
    suggest_flow_map,
    suggest_calendar, suggest_slope, suggest_horizon, suggest_line,
    suggest_pairs, suggest_heatmap, suggest_pyramid, suggest_dumbbell,
    suggest_scatter,
    suggest_area, suggest_sunburst, suggest_category,
    suggest_distribution, suggest_waterfall, suggest_parallel,
    suggest_spark_table, suggest_histogram, suggest_donut,
    suggest_waffle, suggest_fallback_table)
  cands <- Filter(Negate(is.null), lapply(builders, function(b) b(p, nm)))
  if (length(cands)) {
    cands <- cands[order(-vapply(cands, function(s) s$score, numeric(1)))]
  }

  # Best fit first, one suggestion per chart, each proven to run before
  # it is kept - and no more than n of them.
  kept <- list()
  seen <- character(0)
  for (s in cands) {
    if (length(kept) >= n) break
    if (s$chart %in% seen) next
    if (!suggest_runs(s$code, nm, data)) next
    seen <- c(seen, s$chart)
    kept[[length(kept) + 1]] <- s
  }

  structure(
    list(data_name = nm, placeholder = placeholder,
         n_rows = nrow(data), n_cols = ncol(data),
         suggestions = kept),
    class = "pv_suggestions")
}

#' @export
print.pv_suggestions <- function(x, ...) {
  cat(sprintf("<pv_suggestions> %s: %s rows, %s columns\n",
              x$data_name, format(x$n_rows, big.mark = ","), x$n_cols))
  if (isTRUE(x$placeholder)) {
    cat("(your data frame is shown as `data`)\n")
  }
  if (!length(x$suggestions)) {
    cat("No chart suggestions - none of the package's chart shapes fit",
        "these columns.\n")
    return(invisible(x))
  }
  for (i in seq_along(x$suggestions)) {
    s <- x$suggestions[[i]]
    cat(sprintf("\n%d) %s - %s\n", i, s$chart, s$reason))
    for (line in strsplit(s$code, "\n", fixed = TRUE)[[1]]) {
      cat("   ", line, "\n", sep = "")
    }
  }
  cat("\n")
  invisible(x)
}
