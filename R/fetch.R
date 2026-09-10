# Live fetchers for the Swiss open-data portals the bundled datasets come
# from, plus the shared download cache they and R/opendata.R run through.
# Every fetch tells the user where the data comes from and under which
# licence terms - that message is not optional decoration, it is how the
# attribution requirements of the sources travel with the data.

# ---- download cache --------------------------------------------------------

# One flat directory under the user's R cache, one pair of files per URL:
# <md5-of-url>.dat holds the downloaded bytes, <md5-of-url>.json remembers
# which URL they came from and when.
pv_cache_dir <- function(create = TRUE) {
  dir <- tools::R_user_dir("polyviz", "cache")
  if (create && !dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
  dir
}

# base R has no string hasher, but tools::md5sum hashes files just fine.
pv_cache_key <- function(url) {
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  writeLines(url, tmp)
  unname(tools::md5sum(tmp))
}

# Downloads a URL, serving repeats from the cache. Returns list(path, cached).
# The download lands in a temp file first and is only renamed into the cache
# on success, so a dropped connection can never leave a poisoned entry
# behind. With cache = FALSE (API calls whose answers shouldn't be reused)
# the bytes go to a session temp file instead. Extra request headers (an
# Accept header for content negotiation, say) ride along via `headers`;
# the cache keys on the URL alone, which is fine as long as one URL is
# always requested with the same headers.
pv_download <- function(url, refresh = FALSE, cache = TRUE, headers = NULL) {
  if (cache) {
    key <- pv_cache_key(url)
    dat <- file.path(pv_cache_dir(), paste0(key, ".dat"))
    if (!refresh && file.exists(dat)) {
      return(list(path = dat, cached = TRUE))
    }
    tmp <- paste0(dat, ".tmp")
    # a failed or interrupted download must not leave a stray staging file
    on.exit(unlink(tmp), add = TRUE)
  } else {
    # uncached downloads live in the session tempdir; R cleans that up
    dat <- tmp <- tempfile(fileext = ".dat")
  }

  # Slow government servers plus multi-megabyte extracts outgrow R's
  # default 60-second timeout.
  old <- options(timeout = max(300, getOption("timeout")))
  on.exit(options(old), add = TRUE)

  status <- tryCatch(
    utils::download.file(url, tmp, quiet = TRUE, mode = "wb",
                         headers = c(`User-Agent` = pv_user_agent, headers)),
    error = function(e) conditionMessage(e),
    warning = function(w) conditionMessage(w)
  )
  if (!identical(status, 0L) || !file.exists(tmp)) {
    reason <- if (is.character(status)) sub("\\s+$", "", status) else
      "download returned a non-zero status"
    rlang::abort(
      sprintf(paste0(
        "Could not download %s\n(%s)\n",
        "Fetching live open data needs internet access; ",
        "earlier downloads are served from the cache (see `pv_cache_status()`)."),
        url, reason),
      class = "polyviz_download_error")
  }
  if (cache) {
    file.rename(tmp, dat)
    jsonlite::write_json(
      list(url = url, fetched = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
           size = file.size(dat)),
      file.path(pv_cache_dir(), paste0(key, ".json")),
      auto_unbox = TRUE)
  }
  list(path = dat, cached = FALSE)
}

pv_user_agent <- "polyviz R package (https://github.com/jastephan63/polyviz)"

#' Inspect polyviz's open-data download cache
#'
#' Downloads made by [pv_fetch_bfs()], [pv_fetch_lustat()],
#' [pv_fetch_eurostat()], and [pv_fetch_opendata()] are kept under
#' `tools::R_user_dir("polyviz", "cache")` and reused on repeat calls (pass
#' `refresh = TRUE` to a fetcher to force a fresh download).
#' `pv_cache_status()` lists what is currently cached; `pv_cache_clear()`
#' deletes all of it.
#'
#' @return `pv_cache_status()`: a data frame with one row per cached
#'   download (`url`, `size_kb`, `fetched`, `file`), zero rows when the
#'   cache is empty. `pv_cache_clear()`: the number of entries removed,
#'   invisibly.
#' @examples
#' pv_cache_status()
#' @export
pv_cache_status <- function() {
  # read-only: looking at the cache must not create it
  dir <- pv_cache_dir(create = FALSE)
  metas <- list.files(dir, pattern = "\\.json$", full.names = TRUE)
  rows <- lapply(metas, function(m) {
    dat <- sub("\\.json$", ".dat", m)
    if (!file.exists(dat)) {
      return(NULL)
    }
    info <- jsonlite::read_json(m)
    data.frame(url = info$url %||% NA_character_,
               size_kb = round(file.size(dat) / 1024, 1),
               fetched = info$fetched %||% NA_character_,
               file = dat)
  })
  out <- do.call(rbind, rows)
  if (is.null(out)) {
    out <- data.frame(url = character(), size_kb = numeric(),
                      fetched = character(), file = character())
  }
  out[order(out$fetched), , drop = FALSE]
}

#' @rdname pv_cache_status
#' @export
pv_cache_clear <- function() {
  dir <- pv_cache_dir(create = FALSE)
  n <- length(list.files(dir, pattern = "\\.dat$"))
  unlink(list.files(dir, full.names = TRUE))
  message(sprintf("Removed %d cached download(s) from %s", n, dir))
  invisible(n)
}

# ---- shared parsing and provenance -----------------------------------------

# Reads a CSV whose separator may be ";" (LUSTAT and many Swiss portals) or
# ",": whichever occurs more often in the header line wins. Character
# columns that are secretly numbers are converted.
pv_read_swiss_csv <- function(path) {
  header <- readLines(path, n = 1, warn = FALSE)
  n_semi <- length(regmatches(header, gregexpr(";", header, fixed = TRUE))[[1]])
  n_comma <- length(regmatches(header, gregexpr(",", header, fixed = TRUE))[[1]])
  sep <- if (n_semi > n_comma) ";" else ","
  d <- utils::read.csv(path, sep = sep, check.names = FALSE,
                       fileEncoding = "UTF-8", stringsAsFactors = FALSE)
  names(d)[1] <- sub("^\ufeff", "", names(d)[1])
  d[] <- lapply(d, function(v) {
    if (is.character(v)) {
      utils::type.convert(v, as.is = TRUE, na.strings = c("NA", ""))
    } else {
      v
    }
  })
  d
}

# Parses a stats.swiss SDMX "csv with labels" export. After four fixed
# columns (STRUCTURE, STRUCTURE_ID, STRUCTURE_NAME, ACTION) every component
# arrives as a code column immediately followed by its human-readable label
# column. Per component this keeps the label under the lower-cased SDMX id
# and the code under "<id>_code"; components without labels (OBS_VALUE,
# dates) keep just their value. All-empty columns and code columns that
# merely repeat the component id (text attributes such as DATASET_COMMENT
# do that) are dropped.
pv_parse_sdmx_labels <- function(path, url = "the download") {
  x <- utils::read.csv(path, check.names = FALSE, fileEncoding = "UTF-8",
                       stringsAsFactors = FALSE)
  fixed <- c("STRUCTURE", "STRUCTURE_ID", "STRUCTURE_NAME", "ACTION")
  if (ncol(x) < 6 || !identical(names(x)[1:4], fixed) ||
      (ncol(x) - 4) %% 2 != 0) {
    rlang::abort(sprintf(paste0(
      "%s is not an SDMX csv-with-labels export ",
      "(expected %s followed by code/label column pairs)."),
      url, paste(fixed, collapse = ", ")))
  }
  empty <- function(v) is.na(v) | v == ""
  out <- list()
  for (i in seq(5, ncol(x), by = 2)) {
    id <- tolower(names(x)[i])
    code <- x[[i]]
    label <- x[[i + 1]]
    if (all(empty(code)) && all(empty(label))) next
    if (all(empty(label))) {
      out[[id]] <- code
      next
    }
    out[[id]] <- label
    if (!all(empty(code) | code == names(x)[i])) {
      out[[paste0(id, "_code")]] <- code
    }
  }
  out <- lapply(out, function(v) {
    if (is.character(v)) {
      utils::type.convert(v, as.is = TRUE, na.strings = c("NA", ""))
    } else {
      v
    }
  })
  d <- data.frame(out, check.names = FALSE)
  attr(d, "pv_dataflow") <- x$STRUCTURE_ID[1]
  d
}

# Parses a plain SDMX-CSV export (Eurostat's dissemination API serves
# these): DATAFLOW and LAST UPDATE up front, then one code column per
# dimension, TIME_PERIOD, OBS_VALUE, and attribute columns such as
# OBS_FLAG. No label columns here - the codes are the data. Column names
# are lower-cased to match the other fetchers, the two header columns
# become the pv_dataflow attribute, and all-empty attribute columns are
# dropped.
pv_parse_sdmx_csv <- function(path, url = "the download") {
  # everything comes in as character; read.csv's own guessing would turn
  # a code column of "T" values (sex: total) into logical TRUE, and a
  # geo column holding only Namibia ("NA") into missing values
  x <- utils::read.csv(path, check.names = FALSE, fileEncoding = "UTF-8",
                       stringsAsFactors = FALSE, colClasses = "character",
                       na.strings = character())
  names(x)[1] <- sub("^\ufeff", "", names(x)[1])
  fixed <- c("DATAFLOW", "LAST UPDATE")
  if (ncol(x) < 4 || !identical(names(x)[1:2], fixed)) {
    rlang::abort(sprintf(paste0(
      "%s is not an SDMX-CSV export ",
      "(expected %s followed by the dimension columns)."),
      url, paste(fixed, collapse = ", ")))
  }
  flow <- if (nrow(x)) x$DATAFLOW[1] else NA_character_
  x <- x[setdiff(names(x), fixed)]
  names(x) <- tolower(names(x))
  keep <- !vapply(x, function(v) all(is.na(v) | v == ""), logical(1))
  x <- x[keep]
  # a conversion only sticks when it produces actual numbers; code
  # columns stay character, with empty cells as NA
  x[] <- lapply(x, function(v) {
    conv <- utils::type.convert(v, as.is = TRUE, na.strings = "")
    if (is.numeric(conv)) conv else replace(v, !nzchar(v), NA)
  })
  attr(x, "pv_dataflow") <- flow
  x
}

# The source/licence message every fetch prints, and the matching
# attributes that keep the provenance attached to the returned data.
pv_deliver <- function(d, source, licence, url) {
  message("Source: ", source, "\nLicence: ", licence)
  attr(d, "pv_source") <- source
  attr(d, "pv_licence") <- licence
  attr(d, "pv_url") <- url
  attr(d, "pv_fetched") <- Sys.time()
  d
}

pv_licence_bfs <- paste0(
  'opendata.swiss "OPEN BY" terms - free use, source citation required ',
  '("Quelle: Bundesamt f\u00fcr Statistik").')

pv_licence_lustat <- paste0(
  'opendata.swiss "OPEN BY ASK" terms - free use with source citation ',
  '("Quelle: LUSTAT Statistik Luzern"); commercial use requires the data ',
  "owner's permission.")

pv_licence_eurostat <- paste0(
  "Eurostat's standard reuse policy - Creative Commons Attribution 4.0 ",
  '(CC BY 4.0), free use with source citation ("Source: Eurostat").')

# ---- the fetchers ----------------------------------------------------------

# Builds the data URL for one stats.swiss dataflow reference. Without a
# filter the whole flow comes down through the /all path; with one, the
# SDMX data key takes that spot, and start/end bound the time axis via
# the startPeriod/endPeriod query parameters.
pv_bfs_url <- function(ref, filter = NULL, start = NULL, end = NULL) {
  url <- sprintf(
    "https://disseminate.stats.swiss/rest/data/%s/%s?format=csvfilewithlabels",
    ref, if (is.null(filter)) "all" else filter)
  if (!is.null(start)) {
    url <- paste0(url, "&startPeriod=", start)
  }
  if (!is.null(end)) {
    url <- paste0(url, "&endPeriod=", end)
  }
  url
}

# start/end name a year or period; plain numbers are as natural as
# strings for years, so both are accepted and numbers are formatted.
pv_bfs_period <- function(x, arg) {
  if (is.null(x)) {
    return(NULL)
  }
  if (is.numeric(x) && length(x) == 1 && !is.na(x)) {
    x <- format(x, scientific = FALSE, trim = TRUE)
  }
  if (!is.character(x) || length(x) != 1 || is.na(x) || !nzchar(x) ||
      grepl("[[:space:]/?&=]", x)) {
    rlang::abort(sprintf(
      "`%s` must be a single year or period, e.g. \"2015\".", arg))
  }
  x
}

# A named-list filter needs the dataflow's dimension order, which only the
# data structure definition knows. The structure endpoint speaks SDMX-JSON
# when asked for it; the plain URL would return XML.
pv_bfs_structure_url <- function(ref) {
  parts <- strsplit(ref, ",", fixed = TRUE)[[1]]
  sprintf(
    "https://disseminate.stats.swiss/rest/dataflow/%s/%s/latest?references=all",
    parts[1], parts[2])
}

pv_accept_sdmx_json <- c(
  Accept = "application/vnd.sdmx.structure+json;version=1.0")

# Pulls the dimension ids, in key order, out of a downloaded SDMX-JSON
# structure message. The key order is the dimensionList's position order;
# the time dimension lives outside the key (start/end handle it).
pv_bfs_dimension_ids <- function(path, ref) {
  j <- tryCatch(jsonlite::read_json(path), error = function(e) NULL)
  dsds <- j$data$dataStructures
  dims <- if (length(dsds)) {
    dsds[[1]]$dataStructureComponents$dimensionList$dimensions
  }
  ids <- vapply(dims, function(d) {
    if (is.character(d$id) && length(d$id) == 1) d$id else NA_character_
  }, character(1))
  if (!length(ids) || anyNA(ids)) {
    rlang::abort(sprintf(paste0(
      "The structure message for %s did not contain a readable ",
      "dimension list; pass `filter` as a dot-separated SDMX data key ",
      "instead (e.g. \"LU....\")."), ref))
  }
  pos <- vapply(dims, function(d) {
    p <- d$position
    if (is.numeric(p) && length(p) == 1) as.integer(p) else NA_integer_
  }, integer(1))
  if (!anyNA(pos)) {
    ids <- ids[order(pos)]
  }
  ids
}

# Fetches (and caches) a dataflow's structure and returns its dimension ids
# in key order. A portal that refuses the request would leave a named-list
# filter unresolvable, so the failure says how to work without the lookup.
pv_bfs_dimensions <- function(ref, refresh = FALSE) {
  url <- pv_bfs_structure_url(ref)
  dl <- tryCatch(
    pv_download(url, refresh = refresh, headers = pv_accept_sdmx_json),
    polyviz_download_error = function(e) {
      rlang::abort(sprintf(paste0(
        "Could not fetch the data structure of %s, which a named-list ",
        "`filter` needs to learn the dimension order.\n(%s)\n",
        "A `filter` given as a dot-separated SDMX data key ",
        "(e.g. \"LU....\") works without this lookup."),
        ref, conditionMessage(e)),
        class = "polyviz_download_error")
    })
  pv_bfs_dimension_ids(dl$path, ref)
}

# Turns a named-list filter into the dot-separated data key: named
# dimensions get their values ('+' joins several), everything else stays
# an empty segment. A name the dataflow does not have aborts with the real
# dimension ids in order - that list is exactly what someone guessing at
# names needs to see.
pv_bfs_key <- function(filter, dims, ref) {
  if (!length(filter)) {
    rlang::abort("An empty list is not a `filter`; use NULL for no filter.")
  }
  nms <- names(filter)
  if (is.null(nms) || any(!nzchar(nms))) {
    rlang::abort(paste0(
      "A list `filter` must have every element named after a dimension, ",
      "e.g. filter = list(GR_KT_GDE = c(\"LU\", \"ZH\"))."))
  }
  if (anyDuplicated(nms)) {
    rlang::abort(sprintf("`filter` names a dimension twice: %s.",
                         paste0('"', unique(nms[duplicated(nms)]), '"',
                                collapse = ", ")))
  }
  bad <- setdiff(nms, dims)
  if (length(bad)) {
    rlang::abort(sprintf(paste0(
      "Unknown dimension(s) in `filter`: %s.\n",
      "Dataflow %s keys its data by these dimensions, in this order:\n",
      "  %s"),
      paste0('"', bad, '"', collapse = ", "), ref,
      paste(dims, collapse = ".")))
  }
  segments <- vapply(dims, function(dim) {
    if (!dim %in% nms) {
      return("")
    }
    v <- filter[[dim]]
    if (is.numeric(v)) {
      v <- vapply(v, format, character(1), scientific = FALSE, trim = TRUE)
    }
    if (!is.character(v) || !length(v) || anyNA(v) || !all(nzchar(v)) ||
        any(grepl("[[:space:]./?&=+]", v))) {
      rlang::abort(sprintf(paste0(
        "`filter$%s` must be one or more dimension values without ",
        "whitespace or SDMX key punctuation, e.g. c(\"LU\", \"ZH\")."), dim))
    }
    paste(v, collapse = "+")
  }, character(1))
  paste(segments, collapse = ".")
}

#' Fetch a dataset from the Swiss Federal Statistical Office (stats.swiss)
#'
#' Downloads an SDMX dataflow from the Bundesamt für Statistik's
#' dissemination API behind <https://stats.swiss> as a labelled CSV export
#' and parses it into a tidy data frame. Downloads are cached (see
#' [pv_cache_status()]).
#'
#' Each SDMX component becomes a column named by its lower-cased id holding
#' the human-readable label, with the underlying code kept in a matching
#' `<id>_code` column; the observation itself is `obs_value`. The dataflow
#' reference, source, and licence travel along as attributes
#' (`pv_dataflow`, `pv_source`, `pv_licence`, `pv_url`).
#'
#' Two quirks of the "Statistik der Schweizer Städte" city dataflows
#' (`DF_SSV_*`) are worth knowing: the pseudo-city with code `"_ST"` is the
#' all-cities total, and the population indicator mixes explicit census
#' years (`pop_ref_period_1930` ...) with current-period codes
#' (`pop_ref_period`, `pop_ref_period-10`) whose label carries the year -
#' plus density and percent-change variants. Rows whose indicator code
#' starts with `"pop_ref_period"` and whose label is a bare four-digit year
#' are the population counts.
#'
#' Some dataflows are enormous when pulled whole - the vacancy dataflow
#' `DF_LWZ_1` and the dwelling register `DF_GWS_REG5`, for instance, run to
#' hundreds of megabytes unfiltered. `filter` narrows the download on the
#' server with an SDMX data key: one value per dimension, separated by
#' dots, in the dataflow's dimension order, where an empty segment keeps
#' every value of that dimension and `+` combines alternatives within one.
#' The dimension order comes from the dataflow's data structure - request
#' `https://disseminate.stats.swiss/rest/dataflow/<agency>/<id>/latest?references=all`
#' to discover it. For `"CH1.LWZ,DF_LWZ_1"` the order is
#' `GR_KT_GDE.WOHN_ANZAHL.LEERWOHN_TYP.MEASURE_DIMENSION.FREQ`, so the key
#' `"LU...."` fixes the region to canton Lucerne and leaves the other four
#' dimensions open. `start` and `end` bound the time axis the same way.
#' Because the download cache keys on the full request URL, differently
#' filtered pulls of one dataflow cache independently of each other and of
#' the complete flow.
#'
#' Keeping that dimension order straight by hand is fiddly, so `filter`
#' also takes a named list: `list(GR_KT_GDE = c("LU", "ZH"),
#' MEASURE_DIMENSION = "PC")` names the dimensions to pin (several values
#' become a `+` alternative) and leaves every unnamed dimension open.
#' polyviz then fetches the dataflow's data structure once (cached like
#' any download), reads the dimension order from it, and builds the dot
#' key itself - misspell a dimension and the error lists the dataflow's
#' real dimension ids in order, which doubles as the discovery step.
#' Should the structure lookup itself fail (no connection, a throttling
#' portal), fall back to the dot-separated character key, which needs no
#' lookup.
#'
#' One quirk of filtered exports: the server names their time column
#' `TIME_PERIOD` where the complete `/all` exports say `PERIOD`, so a
#' filtered result carries a `time_period` column where an unfiltered one
#' has `period`. Everything else parses identically.
#'
#' BFS publishes stats.swiss data under the opendata.swiss "OPEN BY" terms:
#' free use with source citation ("Quelle: Bundesamt für Statistik").
#' Check the dataset page on stats.swiss should a dataflow state different
#' terms.
#'
#' @param id Dataflow id, e.g. `"DF_SSV_POP_1930"`. The owning agency is
#'   derived from the theme code in the id (`DF_SSV_*` belongs to
#'   `CH1.SSV`); pass `agency`, or a full reference such as
#'   `"CH1.SSV,DF_SSV_POP_1930"`, when that guess is wrong.
#' @param agency SDMX agency id owning the dataflow, e.g. `"CH1.SSV"`.
#'   Defaults to `"CH1.<theme>"` derived from `id`.
#' @param filter A single SDMX data key restricting the download to a
#'   slice of the dataflow, e.g. `"LU...."` - dimension values in the
#'   dataflow's dimension order, separated by dots, where an empty segment
#'   keeps all values of a dimension and `+` combines several. Or a named
#'   list mapping dimension ids to the values to keep, e.g.
#'   `list(GR_KT_GDE = c("LU", "ZH"))`, from which the key is built after
#'   a lookup of the dataflow's dimension order. The default `NULL`
#'   downloads the complete flow. See Details.
#' @param start,end First / last period to download, a single year or
#'   period such as `"2015"` (sent as the `startPeriod` / `endPeriod`
#'   query parameters). The default `NULL` places no bound.
#' @param refresh Set to TRUE to bypass the cache and download again.
#' @return A data frame; source and licence are attached as attributes and
#'   printed on every fetch.
#' @seealso [pv_fetch_lustat()], [pv_fetch_eurostat()],
#'   [pv_search_opendata()], [pv_cache_status()]
#' @examples
#' \dontrun{
#' pop <- pv_fetch_bfs("DF_SSV_POP_1930")
#' cities <- pop[startsWith(pop$ssv_pop_1930_code, "pop_ref_period") &
#'                 grepl("^\\d{4}$", pop$ssv_pop_1930) &
#'                 pop$ssv_swiss_city_code != "_ST", ]
#'
#' # canton Lucerne's vacancy rate only - the complete DF_LWZ_1 flow runs
#' # to hundreds of megabytes, this slice is a few kilobytes
#' lwz <- pv_fetch_bfs("CH1.LWZ,DF_LWZ_1", filter = "LU._T._T.PC.A",
#'                     start = "2015")
#'
#' # the same slice without memorising the dimension order: name the
#' # dimensions, polyviz builds the "LU._T._T.PC.A" key from the
#' # dataflow's structure (and a misspelt name lists the real ones)
#' lwz <- pv_fetch_bfs("CH1.LWZ,DF_LWZ_1",
#'                     filter = list(GR_KT_GDE = "LU", WOHN_ANZAHL = "_T",
#'                                   LEERWOHN_TYP = "_T",
#'                                   MEASURE_DIMENSION = "PC", FREQ = "A"),
#'                     start = "2015")
#' }
#' @export
pv_fetch_bfs <- function(id, agency = NULL, filter = NULL, start = NULL,
                         end = NULL, refresh = FALSE) {
  if (!is.character(id) || length(id) != 1 || is.na(id) || !nzchar(id)) {
    rlang::abort("`id` must be a single dataflow id, e.g. \"DF_SSV_POP_1930\".")
  }
  if (!is.null(filter) && !is.list(filter)) {
    if (!is.character(filter) || length(filter) != 1 || is.na(filter) ||
        !nzchar(filter) || grepl("[[:space:]]", filter)) {
      rlang::abort(paste0(
        "`filter` must be a single SDMX data key without whitespace, ",
        "e.g. \"LU....\" (dimension values in the dataflow's dimension ",
        "order, separated by dots), or a named list such as ",
        "list(GR_KT_GDE = \"LU\")."))
    }
    if (grepl("[/?]", filter)) {
      rlang::abort(paste0(
        "`filter` must not contain \"/\" or \"?\" - pass just the ",
        "dot-separated data key, not a URL."))
    }
  }
  start <- pv_bfs_period(start, "start")
  end <- pv_bfs_period(end, "end")
  if (!isTRUE(refresh) && !isFALSE(refresh)) {
    rlang::abort("`refresh` must be TRUE or FALSE.")
  }
  ref <- sub(":", ",", id, fixed = TRUE)
  if (!grepl(",", ref, fixed = TRUE)) {
    if (is.null(agency)) {
      theme <- regmatches(ref, regexec("^DF_([A-Z0-9]+)_", ref))[[1]][2]
      if (is.na(theme)) {
        rlang::abort(sprintf(paste0(
          "Cannot derive the owning agency from `id` \"%s\"; ",
          "pass `agency` (e.g. \"CH1.SSV\")."), id))
      }
      agency <- paste0("CH1.", theme)
    }
    ref <- paste(agency, ref, sep = ",")
  }
  if (is.list(filter)) {
    # the dot key needs the dataflow's dimension order, so this costs one
    # extra (cached) structure download the first time around
    filter <- pv_bfs_key(filter, pv_bfs_dimensions(ref, refresh = refresh),
                         ref)
  }
  url <- pv_bfs_url(ref, filter = filter, start = start, end = end)
  dl <- pv_download(url, refresh = refresh)
  d <- pv_parse_sdmx_labels(dl$path, url = url)
  pv_deliver(
    d,
    source = sprintf(
      "Bundesamt f\u00fcr Statistik, dataflow %s, via stats.swiss",
      attr(d, "pv_dataflow") %||% ref),
    licence = pv_licence_bfs,
    url = url)
}

#' Fetch a dataset from LUSTAT Statistik Luzern
#'
#' Downloads one of the open-government CSV files LUSTAT Statistik Luzern
#' publishes on <https://www.data.lustat.ch> (the resources its
#' opendata.swiss datasets point at) and parses it into a data frame.
#' Downloads are cached (see [pv_cache_status()]).
#'
#' LUSTAT data is published under the opendata.swiss "OPEN BY ASK" terms:
#' free use with source citation ("Quelle: LUSTAT Statistik Luzern"), and
#' commercial use requires the data owner's permission. Every fetch prints
#' these terms and attaches them to the result (`pv_source`, `pv_licence`,
#' `pv_url`).
#'
#' @param id File name of the dataset on data.lustat.ch, with or without
#'   the `.csv` suffix - e.g. `"fa-lu-ra"` (municipal fiscal equalization)
#'   or `"grwahlen-lu"` (municipal council elections).
#' @inheritParams pv_fetch_bfs
#' @return A data frame; source and licence are attached as attributes and
#'   printed on every fetch.
#' @seealso [pv_fetch_bfs()], [pv_fetch_eurostat()], [pv_search_opendata()],
#'   [pv_cache_status()]
#' @examples
#' \dontrun{
#' fiscal <- pv_fetch_lustat("fa-lu-ra")
#' str(fiscal)
#' }
#' @export
pv_fetch_lustat <- function(id, refresh = FALSE) {
  if (!is.character(id) || length(id) != 1 || is.na(id) || !nzchar(id)) {
    rlang::abort("`id` must be a single dataset name, e.g. \"fa-lu-ra\".")
  }
  if (!isTRUE(refresh) && !isFALSE(refresh)) {
    rlang::abort("`refresh` must be TRUE or FALSE.")
  }
  id <- sub("\\.csv$", "", id, ignore.case = TRUE)
  url <- sprintf("https://www.data.lustat.ch/%s.csv", id)
  dl <- pv_download(url, refresh = refresh)
  d <- pv_read_swiss_csv(dl$path)
  pv_deliver(
    d,
    source = sprintf(
      "LUSTAT Statistik Luzern, dataset \"%s\" (data.lustat.ch)", id),
    licence = pv_licence_lustat,
    url = url)
}

# Builds the data URL for one Eurostat dataset. Unlike stats.swiss there
# is no /all path: the bare dataset id means the whole thing, and an SDMX
# data key appended as a path segment narrows it. compressed=false keeps
# the response plain CSV; the default would be gzip, which
# utils::download.file saves without decompressing.
pv_eurostat_url <- function(id, filter = NULL, start = NULL, end = NULL) {
  url <- sprintf(
    paste0("https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/",
           "data/%s%s?format=SDMX-CSV&compressed=false"),
    id, if (is.null(filter)) "" else paste0("/", filter))
  if (!is.null(start)) {
    url <- paste0(url, "&startPeriod=", start)
  }
  if (!is.null(end)) {
    url <- paste0(url, "&endPeriod=", end)
  }
  url
}

#' Fetch a dataset from Eurostat
#'
#' Downloads a dataset from Eurostat's SDMX dissemination API
#' (<https://ec.europa.eu/eurostat/web/main/data/database>) as an SDMX-CSV
#' export and parses it into a tidy data frame. Downloads are cached (see
#' [pv_cache_status()]).
#'
#' Eurostat's SDMX-CSV carries codes, not labels: each dimension becomes a
#' lower-cased code column (`freq`, `unit`, `geo`, ...), the observation is
#' `obs_value`, and the time axis is `time_period`. Flag columns such as
#' `obs_flag` stay when any row carries a flag and are dropped when empty.
#' The dataflow reference, source, and licence travel along as attributes
#' (`pv_dataflow`, `pv_source`, `pv_licence`, `pv_url`).
#'
#' Many Eurostat tables are enormous when pulled whole, so `filter` narrows
#' the download on the server exactly as in [pv_fetch_bfs()]: a dot-separated
#' SDMX data key in the dataset's dimension order, where an empty segment
#' keeps every value of a dimension and `+` combines alternatives. For
#' `"demo_pjan"` the order is `freq.unit.age.sex.geo`, so
#' `"A.NR.TOTAL.T.LU"` is Luxembourg's total population. The dimension
#' order and codes are listed on each dataset's page in Eurostat's data
#' browser. Unlike [pv_fetch_bfs()] the named-list form is not available
#' here - Eurostat's structure endpoint does not serve the SDMX-JSON this
#' package reads, so the key must be spelled out. `start` and `end` bound
#' the time axis via `startPeriod`/`endPeriod`, as on stats.swiss.
#'
#' Eurostat publishes under its standard reuse policy, the Creative
#' Commons Attribution 4.0 licence (CC BY 4.0): free use with source
#' citation ("Source: Eurostat"). A few datasets integrating third-party
#' data state additional terms on their dataset page.
#'
#' @param id Dataset code as Eurostat's data browser shows it, e.g.
#'   `"demo_pjan"` (population on 1 January) or `"nama_10_gdp"` (GDP and
#'   main components).
#' @param filter A single SDMX data key restricting the download to a
#'   slice of the dataset, e.g. `"A.NR.TOTAL.T.LU"` - dimension values in
#'   the dataset's dimension order, separated by dots, where an empty
#'   segment keeps all values of a dimension and `+` combines several. The
#'   default `NULL` downloads the complete dataset. See Details.
#' @inheritParams pv_fetch_bfs
#' @return A data frame; source and licence are attached as attributes and
#'   printed on every fetch.
#' @seealso [pv_fetch_bfs()], [pv_fetch_lustat()], [pv_cache_status()]
#' @examples
#' \dontrun{
#' # Luxembourg's population on 1 January, 2015 onwards - the key is
#' # freq.unit.age.sex.geo, so the unfiltered dataset would be every
#' # age/sex/country combination since 1960
#' lux <- pv_fetch_eurostat("demo_pjan", filter = "A.NR.TOTAL.T.LU",
#'                          start = "2015")
#'
#' # GDP at market prices for two countries
#' gdp <- pv_fetch_eurostat("nama_10_gdp", filter = "A.CP_MEUR.B1GQ.CH+LU")
#' }
#' @export
pv_fetch_eurostat <- function(id, filter = NULL, start = NULL, end = NULL,
                              refresh = FALSE) {
  if (!is.character(id) || length(id) != 1 || is.na(id) || !nzchar(id) ||
      grepl("[[:space:]/?]", id)) {
    rlang::abort("`id` must be a single dataset code, e.g. \"demo_pjan\".")
  }
  if (!is.null(filter)) {
    if (is.list(filter)) {
      rlang::abort(paste0(
        "Named-list filters are a pv_fetch_bfs() feature; Eurostat's ",
        "structure endpoint does not serve the dimension order this ",
        "package can read, so pass `filter` as a dot-separated SDMX ",
        "data key, e.g. \"A.NR.TOTAL.T.LU\"."))
    }
    if (!is.character(filter) || length(filter) != 1 || is.na(filter) ||
        !nzchar(filter) || grepl("[[:space:]]", filter)) {
      rlang::abort(paste0(
        "`filter` must be a single SDMX data key without whitespace, ",
        "e.g. \"A.NR.TOTAL.T.LU\" (dimension values in the dataset's ",
        "dimension order, separated by dots)."))
    }
    if (grepl("[/?]", filter)) {
      rlang::abort(paste0(
        "`filter` must not contain \"/\" or \"?\" - pass just the ",
        "dot-separated data key, not a URL."))
    }
  }
  start <- pv_bfs_period(start, "start")
  end <- pv_bfs_period(end, "end")
  if (!isTRUE(refresh) && !isFALSE(refresh)) {
    rlang::abort("`refresh` must be TRUE or FALSE.")
  }
  url <- pv_eurostat_url(id, filter = filter, start = start, end = end)
  dl <- pv_download(url, refresh = refresh)
  d <- pv_parse_sdmx_csv(dl$path, url = url)
  pv_deliver(
    d,
    source = sprintf("Eurostat, dataset %s", id),
    licence = pv_licence_eurostat,
    url = url)
}
