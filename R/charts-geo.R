# Geographic charts: the choropleth and the bubble map. Like every chart
# file, this one only shapes the payload - the drawing lives in
# inst/htmlwidgets/lib/pv-renderers/geo.js.

# ---- map plumbing shared by the geo charts ---------------------------------

# The bundled (or fetched) layer each map name stands for. "lucerne" is
# the historical default layer; "municipalities" is fetched and cached on
# first use (R/fetch-map.R) because the country-wide municipal polygons
# are too big to ship inside the package.
pv_map_names <- c("lucerne", "cantons", "districts", "municipalities")

pv_map_shape_error <- function() {
  rlang::abort(paste(
    "`map` must be a GeoJSON FeatureCollection stored as an R list -",
    "a `type` of \"FeatureCollection\" and a non-empty `features` -",
    "like `pv_lucerne_map`; or a layer name (\"lucerne\", \"cantons\",",
    "\"districts\", \"municipalities\"); or an sf object."))
}

# Turns whatever the `map` argument holds into the FeatureCollection list
# the renderer draws: a layer name, an sf object (or a data frame carrying
# an sf geometry column), or an already-shaped list. Returns
# list(geo = <FeatureCollection>, layer = <name or NA>).
pv_resolve_map <- function(map, join_id = NULL) {
  from_sf <- FALSE
  layer <- NA_character_
  if (is.character(map)) {
    if (length(map) != 1 || is.na(map) || !map %in% pv_map_names) {
      rlang::abort(sprintf(
        "Unknown map layer %s. The available layers are: %s - or pass a FeatureCollection list or an sf object.",
        if (length(map) == 1 && !is.na(map)) paste0('"', map, '"') else
          "(not a single string)",
        paste0('"', pv_map_names, '"', collapse = ", ")))
    }
    layer <- map
    map <- switch(map,
      lucerne = pv_lucerne_map,
      cantons = pv_swiss_cantons,
      districts = pv_swiss_districts,
      municipalities = pv_fetch_map("municipalities"))
  } else if (inherits(map, "sf") ||
             (is.data.frame(map) &&
                any(vapply(map, inherits, logical(1), what = "sfc")))) {
    map <- pv_map_from_sf(map, join_id)
    from_sf <- TRUE
  } else if (is.data.frame(map)) {
    rlang::abort(paste(
      "`map` is a data frame without a geometry column. Pass an sf object",
      "(or a data frame carrying an sf geometry column), a layer name,",
      "or a FeatureCollection list like `pv_lucerne_map`."))
  }
  # join_id picks a column while converting an sf map; on every other form
  # the join key is fixed as `properties$id`, so a stray join_id is a
  # misunderstanding worth flagging rather than ignoring.
  if (!is.null(join_id) && !from_sf) {
    rlang::abort(paste(
      "`join_id` only applies to an sf (or geometry data frame) `map`;",
      "list maps always join on each feature's `properties$id`."))
  }
  # The result must at least look like a FeatureCollection before it is
  # sent across - d3 would otherwise fail with something unreadable.
  if (!is.list(map) || !identical(map$type, "FeatureCollection") ||
      !length(map$features)) {
    pv_map_shape_error()
  }
  list(geo = map, layer = layer)
}

# Converts an sf polygon layer to the renderer's FeatureCollection layout:
# reprojected to the WGS84 lon/lat d3 expects, one feature per row, with
# `properties$id` from the join_id column (a column named "id" by default)
# and `properties$name` from a "name" column when there is one. User
# coordinates are kept at full precision - only the fetched country-wide
# layers get rounded.
pv_map_from_sf <- function(x, join_id = NULL) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    rlang::abort(paste(
      "Handing `map` an sf object needs the sf package;",
      'install it with `install.packages("sf")`.'))
  }
  if (!inherits(x, "sf")) {
    x <- sf::st_as_sf(x)
  }
  if (is.na(sf::st_crs(x))) {
    rlang::abort(paste(
      "The sf `map` has no coordinate reference system, so it cannot be",
      "reprojected to longitude/latitude. Set one first, e.g.",
      "`sf::st_crs(map) <- 2056` for Swiss LV95 coordinates."))
  }
  types <- unique(as.character(sf::st_geometry_type(x)))
  if (!all(types %in% c("POLYGON", "MULTIPOLYGON"))) {
    rlang::abort(sprintf(
      "The sf `map` must hold polygons; it has %s geometry.",
      paste(setdiff(types, c("POLYGON", "MULTIPOLYGON")), collapse = ", ")))
  }
  id_col <- join_id %||% (if ("id" %in% names(x)) "id")
  if (!is.null(id_col)) {
    if (!is.character(id_col) || length(id_col) != 1 || is.na(id_col)) {
      rlang::abort("`join_id` must be a single column name.")
    }
    if (!id_col %in% names(x)) {
      rlang::abort(sprintf(
        "`join_id` column `%s` is not in the sf `map`. Available: %s.",
        id_col, paste(setdiff(names(x), attr(x, "sf_column")),
                      collapse = ", ")))
    }
  }
  ids <- if (!is.null(id_col)) as.character(x[[id_col]])
  nms <- if ("name" %in% names(x)) as.character(x[["name"]])

  # The geometry goes out through the GeoJSON writer and straight back in
  # as the plain nested lists the payload uses - the same round trip the
  # bundled layers took in data-raw/process-boundaries.R. Features come
  # back in row order, so ids and names pair up by position.
  x <- sf::st_transform(x[, character(0)], 4326)
  tmp <- tempfile(fileext = ".geojson")
  on.exit(unlink(tmp), add = TRUE)
  sf::st_write(x, tmp, quiet = TRUE)
  raw <- jsonlite::fromJSON(tmp, simplifyVector = FALSE)
  feats <- lapply(seq_along(raw$features), function(i) {
    props <- list()
    if (!is.null(ids) && !is.na(ids[[i]])) props$id <- ids[[i]]
    if (!is.null(nms) && !is.na(nms[[i]])) props$name <- nms[[i]]
    list(type = "Feature", properties = props,
         geometry = raw$features[[i]]$geometry)
  })
  list(type = "FeatureCollection", features = feats)
}

# The lon/lat bounding box of a FeatureCollection, as c(lon_min, lon_max,
# lat_min, lat_max). GeoJSON nests rings arbitrarily deep, but the
# innermost elements are always [lon, lat] pairs, so a flat unlist
# alternates strictly between the two.
pv_map_bbox <- function(geo) {
  v <- unlist(lapply(geo$features, function(f) f$geometry$coordinates),
              use.names = FALSE)
  if (!length(v)) {
    return(c(NA_real_, NA_real_, NA_real_, NA_real_))
  }
  lon <- v[c(TRUE, FALSE)]
  lat <- v[c(FALSE, TRUE)]
  c(range(lon), range(lat))
}

# Whether the lakes layer joins the payload. TRUE and FALSE are the
# user's word; "auto" switches the water on for maps that show (most of)
# Switzerland - the named country-wide layers, or anything spanning over
# 3 degrees of longitude - and leaves regional maps alone, since those
# (like the bundled Lucerne layer) usually carry their own lakes.
pv_resolve_lakes <- function(lakes, geo, layer) {
  check_flag(lakes, "lakes")
  if (isFALSE(lakes)) {
    return(NULL)
  }
  if (identical(lakes, "auto")) {
    wide <- if (!is.na(layer)) {
      layer %in% c("cantons", "districts", "municipalities")
    } else {
      bb <- pv_map_bbox(geo)
      isTRUE(bb[2] - bb[1] >= 3)
    }
    if (!wide) {
      return(NULL)
    }
  }
  pv_swiss_lakes
}

# ---- choropleth ------------------------------------------------------------

#' Interactive D3 choropleth map
#'
#' Regions of a map coloured by a numeric value — the chart for showing
#' how a measure varies across space. The map is a GeoJSON
#' FeatureCollection stored as a plain R list (the bundled
#' [pv_lucerne_map] is the template: each feature carries
#' `properties$id` and `properties$name`), and `data` joins onto it by
#' matching the `id` column against the features' `properties$id`.
#'
#' `palette = "sequential"` maps magnitude onto the theme's single-hue
#' ramp over the data range. `palette = "diverging"` is for values that
#' sit around a meaningful reference point — an index of 100, a change of
#' zero — and needs that point as `center`: the neutral midpoint colour is
#' pinned there and the colour domain is made symmetric around it, so
#' equal distances above and below carry equally strong colour.
#'
#' Regions with no data row keep a neutral fill and a dashed outline —
#' absence stays visible instead of masquerading as a low value. Hovering
#' a region outlines it and shows its name and exact value, and a compact
#' colour-scale legend sits in the header, with the centre marked for
#' diverging palettes.
#'
#' @param data A data frame with at most one row per region.
#' @param map The map to colour. Either a layer name — `"lucerne"` (the
#'   79 Lucerne municipalities, the default layer), `"cantons"` (the 26
#'   cantons, joining on the BFS canton number), `"districts"` (the 143
#'   districts, joining on the BFS district number), or
#'   `"municipalities"` (all Swiss municipalities, downloaded and cached
#'   on first use via [pv_fetch_map()], joining on the BFS municipality
#'   number) — or a GeoJSON FeatureCollection stored as an R list with
#'   each feature carrying `properties$id` (join key) and
#'   `properties$name` (shown in tooltips), like [pv_lucerne_map] and
#'   the bundled Swiss layers; or an sf polygon object (needs the sf
#'   package), which is reprojected to WGS84 and joined on its `id`
#'   column — name a different column with `join_id`.
#' @param id Name of the column joining `data` to the map's feature ids.
#'   Ids that match no feature are dropped with a warning that counts
#'   them and shows a sample; if none match at all, that is an error.
#' @param value Name of the numeric column mapped to colour. Rows with a
#'   missing value are dropped with a warning — their regions read as
#'   "no data".
#' @param palette `"sequential"` (default, for magnitudes) or
#'   `"diverging"` (for values around a reference point, which needs
#'   `center`).
#' @param center The reference value the diverging palette's neutral
#'   midpoint stands for (e.g. `100` for an index, `0` for a change).
#'   Required — and only allowed — when `palette = "diverging"`.
#' @param lakes Draw the major Swiss lakes ([pv_swiss_lakes]) in a muted
#'   water tone? The country-wide layers' polygons include their lake
#'   surfaces, so the water paints over the region fills — the map
#'   convention of the boundaries' own publisher. `TRUE` always, `FALSE`
#'   never; `"auto"` (default) draws them exactly when the map shows the
#'   whole of Switzerland — the `"cantons"`, `"districts"`, and
#'   `"municipalities"` layers, or any map spanning over 3 degrees of
#'   longitude — and leaves regional maps (which usually carry their own
#'   water, like the Lucerne layer) alone.
#' @param join_id Only for an sf `map`: name of the column holding each
#'   feature's id. Defaults to a column named `id`.
#' @inheritParams pv_bar
#' @return An htmlwidget.
#' @examples
#' fiscal25 <- subset(pv_fiscal, year == 2025)
#' pv_choropleth(fiscal25, id = "municipality_id", value = "resource_index",
#'               palette = "diverging", center = 100,
#'               title = "Fiscal strength of Lucerne municipalities")
#' # A Switzerland-wide map joins on the canton number (1-26):
#' cantons <- data.frame(canton = 1:26, share = runif(26, 20, 60))
#' pv_choropleth(cantons, map = "cantons", id = "canton", value = "share")
#' @export
pv_choropleth <- function(data, map = pv_lucerne_map, id, value,
                          palette = c("sequential", "diverging"),
                          center = NULL, lakes = "auto", join_id = NULL,
                          title = NULL, subtitle = NULL, mode = "auto",
                          duration = 500, source = NULL, width = NULL,
                          height = NULL, elementId = NULL) {
  check_columns(data, list(id, value))
  check_nonempty(data)
  check_value_column(data, value)
  palette <- match.arg(palette)
  resolved <- pv_resolve_map(map, join_id)
  map <- resolved$geo
  lakes <- pv_resolve_lakes(lakes, map, resolved$layer)
  # The join keys the map offers. Ids are compared as strings on both
  # sides (here and in JavaScript), so 1001 and "1001" are the same
  # region. Background features without an id (lakes, in the bundled
  # Lucerne map) simply never take part in the join.
  map_ids <- unlist(lapply(map$features, function(f) {
    v <- f$properties$id
    if (is.null(v)) NULL else as.character(v)
  }))
  if (!length(map_ids)) {
    rlang::abort(paste(
      "`map` has no feature with a `properties$id` to join on.",
      "For an sf `map`, name the id column with `join_id`."))
  }

  df <- data.frame(id = as.character(data[[id]]),
                   value = as.numeric(data[[value]]))
  df <- drop_missing(df, is.na(df$value), value)
  if (anyDuplicated(df$id)) {
    rlang::abort(
      "`data` has more than one row per region id; aggregate it first.")
  }

  # A join that silently matches nothing draws an all-grey map, so ids
  # that miss the map are worth a warning - and missing with every single
  # id means the wrong column or the wrong map, which is an error.
  unmatched <- setdiff(df$id, map_ids)
  if (length(unmatched) == nrow(df)) {
    rlang::abort(sprintf(
      "No value in `%s` matches any feature id on the map (map ids look like: %s). Wrong column, or wrong map?",
      id, paste(utils::head(map_ids, 3), collapse = ", ")))
  }
  if (length(unmatched)) {
    shown <- utils::head(unmatched, 5)
    extra <- length(unmatched) - length(shown)
    rlang::warn(sprintf(
      "%d id(s) in `%s` match no map feature and will not be drawn: %s%s.",
      length(unmatched), id, paste(shown, collapse = ", "),
      if (extra > 0) sprintf(" and %d more", extra) else ""))
  }

  # The colour domain is a statistic, so it is decided here: the data
  # range for sequential; symmetric around `center` for diverging so the
  # midpoint colour always means exactly the reference value.
  if (palette == "diverging") {
    if (!is.numeric(center) || length(center) != 1 || is.na(center)) {
      rlang::abort(paste(
        '`palette = "diverging"` needs a numeric `center` - the value the',
        "neutral midpoint colour stands for (e.g. 100 for an index)."))
    }
    center <- as.numeric(center)
    m <- max(abs(range(df$value) - center))
    domain <- center + c(-m, m)
  } else {
    if (!is.null(center)) {
      rlang::abort('`center` only applies to `palette = "diverging"`.')
    }
    domain <- range(df$value)
  }
  # All-equal values would collapse the scale; give it a token width.
  if (domain[1] >= domain[2]) domain <- domain[1] + c(-1, 1)

  pv_widget("choropleth", c(list(
    data = df, map = map, lakes = lakes, palette = palette, domain = domain,
    obs = range(df$value),
    center = if (palette == "diverging") center,
    vlab = value
  ), chart_opts(title, subtitle, mode, duration, source)),
  width, height, elementId)
}

# ---- bubble map ------------------------------------------------------------

#' Interactive D3 bubble map
#'
#' Circles on a map, sized by a value — the chart for quantities anchored
#' to places rather than regions: city populations, plant capacities,
#' event counts. The base map is drawn in a quiet neutral fill (with the
#' Swiss lakes over it, for country-wide layers) and each data row
#' becomes a circle at its longitude/latitude whose *area* encodes the
#' value.
#'
#' Circle area scales with the value (radius with its square root — the
#' honest encoding for sizes), up to a maximum radius that adapts to the
#' map's pixel size. Crowded maps get lighter fills so overlapping
#' circles stay readable, every circle wears a 2px surface-coloured ring,
#' and hovering one shows its name and exact value. A compact size legend
#' with two or three reference circles sits in the map's corner. An
#' optional `color` column maps up to three categories onto the palette,
#' with the usual legend row in the header.
#'
#' @param data A data frame with one row per circle. Rows with a missing
#'   longitude, latitude, or size are dropped with a warning, as are
#'   points falling outside the map.
#' @param lon,lat Names of the numeric coordinate columns, in WGS84
#'   degrees (longitude east, latitude north — [pv_city_coords] is the
#'   bundled template).
#' @param size Name of the numeric column mapped to circle area. Must be
#'   non-negative.
#' @param color Optional name of a categorical column (max 3 distinct
#'   values keeps every pair distinguishable; more will error).
#' @param label Optional name of a column naming each point in tooltips.
#' @param map The base map: a layer name (`"cantons"`, the default;
#'   `"districts"`; `"municipalities"`; `"lucerne"`), a FeatureCollection
#'   list like [pv_swiss_cantons], or an sf polygon object — the same
#'   forms [pv_choropleth()] takes. Feature ids are not needed here; the
#'   polygons are background.
#' @param lakes Draw the Swiss lakes beneath the circles? `TRUE`,
#'   `FALSE`, or `"auto"` (default: only on maps showing the whole of
#'   Switzerland) — exactly as in [pv_choropleth()].
#' @param legend Show the colour legend row above the chart? `TRUE`
#'   always (when a `color` mapping exists), `FALSE` hides it; `"auto"`
#'   (default) shows it exactly when a `color` mapping exists. The circle
#'   size legend on the map is always drawn.
#' @inheritParams pv_bar
#' @return An htmlwidget.
#' @examples
#' pop24 <- subset(pv_city_population, year == 2024)
#' cities <- merge(pv_city_coords, pop24, by = "city")
#' pv_bubble_map(cities, lon = "lon", lat = "lat", size = "population",
#'               label = "city", title = "Where urban Switzerland lives")
#' @export
pv_bubble_map <- function(data, lon, lat, size, color = NULL, label = NULL,
                          map = "cantons", lakes = "auto", legend = "auto",
                          title = NULL, subtitle = NULL, mode = "auto",
                          duration = 500, source = NULL, width = NULL,
                          height = NULL, elementId = NULL) {
  check_columns(data, list(lon, lat, size, color, label))
  check_nonempty(data)
  check_value_column(data, lon)
  check_value_column(data, lat)
  check_value_column(data, size)
  check_flag(legend, "legend")
  if (!is.null(color)) {
    n_levels <- length(unique(data[[color]]))
    if (n_levels > 3) {
      rlang::abort(paste(
        "`color` has more than 3 levels; with all circle pairs potentially",
        "adjacent, only 3 colours stay reliably distinguishable. Facet or",
        "group the variable instead."))
    }
  }
  resolved <- pv_resolve_map(map)
  geo <- resolved$geo
  lakes <- pv_resolve_lakes(lakes, geo, resolved$layer)

  df <- data.frame(lon = as.numeric(data[[lon]]),
                   lat = as.numeric(data[[lat]]),
                   size = as.numeric(data[[size]]))
  if (!is.null(color)) df$series <- as.character(data[[color]])
  if (!is.null(label)) df$label <- as.character(data[[label]])
  df <- drop_missing(df, is.na(df$lon), lon)
  df <- drop_missing(df, is.na(df$lat), lat)
  df <- drop_missing(df, is.na(df$size), size)
  if (!is.null(color)) df <- drop_missing(df, is.na(df$series), color)

  # A circle's area cannot encode a negative quantity, and a map of
  # all-zero sizes has nothing to draw.
  if (any(df$size < 0)) {
    rlang::abort(sprintf(
      "`%s` has negative values; a circle's area cannot be negative.",
      size))
  }
  if (max(df$size) <= 0) {
    rlang::abort(sprintf(
      "`%s` has no positive values; nothing to draw.", size))
  }

  # Coordinates outside the physically possible range almost always mean
  # the two columns are swapped - say so instead of drawing nonsense.
  if (any(abs(df$lat) > 90) || any(abs(df$lon) > 180)) {
    rlang::abort(sprintf(paste(
      "`%s`/`%s` hold values outside the possible longitude/latitude",
      "ranges. Are the two columns swapped?"), lon, lat))
  }
  # Points beyond the map's extent would draw outside the plot (or off in
  # the margins), so they are dropped - loudly, with a sample, because a
  # systematic miss usually means the wrong map. The small tolerance
  # keeps shoreline points that round just past the boundary.
  bb <- pv_map_bbox(geo)
  tol <- 0.25
  out <- df$lon < bb[1] - tol | df$lon > bb[2] + tol |
    df$lat < bb[3] - tol | df$lat > bb[4] + tol
  if (all(out)) {
    rlang::abort(paste(
      "No point falls on the map. Wrong map, or are `lon` and `lat`",
      "not WGS84 degrees?"))
  }
  if (any(out)) {
    who <- if (!is.null(df$label)) df$label[out] else
      sprintf("(%g, %g)", df$lon[out], df$lat[out])
    shown <- utils::head(who, 5)
    extra <- sum(out) - length(shown)
    rlang::warn(sprintf(
      "%d point(s) fall outside the map and will not be drawn: %s%s.",
      sum(out), paste(shown, collapse = ", "),
      if (extra > 0) sprintf(" and %d more", extra) else ""))
    df <- df[!out, , drop = FALSE]
    rownames(df) <- NULL
  }

  pv_widget("bubblemap", c(list(
    data = df, map = geo, lakes = lakes, sizelab = size,
    showLegend = !is.null(color), legend = legend
  ), chart_opts(title, subtitle, mode, duration, source)),
  width, height, elementId)
}
