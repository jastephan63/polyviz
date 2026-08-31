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
# the bytes go to a session temp file instead.
pv_download <- function(url, refresh = FALSE, cache = TRUE) {
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
                         headers = c(`User-Agent` = pv_user_agent)),
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
#' Downloads made by [pv_fetch_bfs()], [pv_fetch_lustat()], and
#' [pv_fetch_opendata()] are kept under
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

# ---- the fetchers ----------------------------------------------------------

#' Fetch a dataset from the Swiss Federal Statistical Office (stats.swiss)
#'
#' Downloads an SDMX dataflow from the Bundesamt fuer Statistik's
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
#' Two quirks of the "Statistik der Schweizer Staedte" city dataflows
#' (`DF_SSV_*`) are worth knowing: the pseudo-city with code `"_ST"` is the
#' all-cities total, and the population indicator mixes explicit census
#' years (`pop_ref_period_1930` ...) with current-period codes
#' (`pop_ref_period`, `pop_ref_period-10`) whose label carries the year -
#' plus density and percent-change variants. Rows whose indicator code
#' starts with `"pop_ref_period"` and whose label is a bare four-digit year
#' are the population counts.
#'
#' BFS publishes stats.swiss data under the opendata.swiss "OPEN BY" terms:
#' free use with source citation ("Quelle: Bundesamt fuer Statistik").
#' Check the dataset page on stats.swiss should a dataflow state different
#' terms.
#'
#' @param id Dataflow id, e.g. `"DF_SSV_POP_1930"`. The owning agency is
#'   derived from the theme code in the id (`DF_SSV_*` belongs to
#'   `CH1.SSV`); pass `agency`, or a full reference such as
#'   `"CH1.SSV,DF_SSV_POP_1930"`, when that guess is wrong.
#' @param agency SDMX agency id owning the dataflow, e.g. `"CH1.SSV"`.
#'   Defaults to `"CH1.<theme>"` derived from `id`.
#' @param refresh Set to TRUE to bypass the cache and download again.
#' @return A data frame; source and licence are attached as attributes and
#'   printed on every fetch.
#' @seealso [pv_fetch_lustat()], [pv_search_opendata()], [pv_cache_status()]
#' @examples
#' \dontrun{
#' pop <- pv_fetch_bfs("DF_SSV_POP_1930")
#' cities <- pop[startsWith(pop$ssv_pop_1930_code, "pop_ref_period") &
#'                 grepl("^\\d{4}$", pop$ssv_pop_1930) &
#'                 pop$ssv_swiss_city_code != "_ST", ]
#' }
#' @export
pv_fetch_bfs <- function(id, agency = NULL, refresh = FALSE) {
  if (!is.character(id) || length(id) != 1 || is.na(id) || !nzchar(id)) {
    rlang::abort("`id` must be a single dataflow id, e.g. \"DF_SSV_POP_1930\".")
  }
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
  url <- sprintf(
    "https://disseminate.stats.swiss/rest/data/%s/all?format=csvfilewithlabels",
    ref)
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
#' @seealso [pv_fetch_bfs()], [pv_search_opendata()], [pv_cache_status()]
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
