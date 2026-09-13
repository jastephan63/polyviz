# The public-transport engine: opens the Swiss national GTFS timetable
# (opentransportdata.swiss) through DuckDB and answers earliest-arrival
# queries over it. The feed is polyviz's largest download by far - a
# ~240 MB ZIP holding ~4 GB of text, 35 million stop_times rows among it -
# so nothing here ever loads a file into R wholesale: DuckDB reads the
# extracted CSVs in place, and only the one day's worth of connections a
# query needs crosses over into R.

# ---- source and licence ----------------------------------------------------

# opentransportdata.swiss is run by SBB on behalf of the Federal Office of
# Transport; its open-data terms ("ODMCH") allow free use with source
# citation. Printed and attached on every fetch, like the other fetchers.
pv_licence_opentransport <- paste0(
  'opentransportdata.swiss open-data ("ODMCH") terms - free use, ',
  'source citation required ("Quelle: opentransportdata.swiss").')

# The CKAN dataset on opendata.swiss that catalogues the weekly GTFS
# exports of the national timetable ("Fahrplan 2026").
pv_gtfs_dataset <- "fahrplan-2026-gtfs2020"

# ---- resolving and fetching the feed ---------------------------------------

# Finds the newest weekly ZIP among the dataset's resources. Unlike the
# pv_ckan() lookups this catalogue query goes through the download cache
# on purpose: the answer decides which 240 MB ZIP a machine downloads and
# extracts, and that choice should stay put between sessions instead of
# chasing the weekly export - `refresh = TRUE` moves both together.
pv_gtfs_latest_resource <- function(refresh = FALSE) {
  url <- paste0(ckan_api, "package_show?id=", pv_gtfs_dataset)
  dl <- pv_download(url, refresh = refresh)
  ans <- jsonlite::fromJSON(dl$path, simplifyVector = FALSE)
  if (!isTRUE(ans$success)) {
    rlang::abort(sprintf(
      'The opendata.swiss lookup of dataset "%s" failed.', pv_gtfs_dataset))
  }
  resources <- ans$result$resources %||% list()
  urls <- vapply(resources, function(r) {
    as.character(r$download_url %||% r$url %||% "")
  }, character(1))
  sizes <- vapply(resources, function(r) {
    as.numeric(r$byte_size %||% NA_real_)
  }, numeric(1))
  # the weekly exports are named GTFS_FP2026_YYYYMMDD.zip; the stamp in
  # the file name, not the resource order, decides which is newest
  stamps <- suppressWarnings(as.integer(
    sub("^.*_(\\d{8})\\.zip$", "\\1", tolower(basename(urls)))))
  ok <- which(!is.na(stamps) & nzchar(urls))
  if (!length(ok)) {
    rlang::abort(sprintf(paste(
      'No "*_YYYYMMDD.zip" resource found in dataset "%s" -',
      "the catalogue layout may have changed; please report this at %s."),
      pv_gtfs_dataset, "https://github.com/jastephan63/polyviz/issues"))
  }
  sel <- ok[which.max(stamps[ok])]
  list(url = urls[sel], stamp = stamps[sel], name = basename(urls[sel]),
       size = sizes[sel])
}

# Unpacks the .txt members of a GTFS zip flat into exdir.
pv_gtfs_extract <- function(zip, exdir) {
  entries <- utils::unzip(zip, list = TRUE)$Name
  txt <- grep("\\.txt$", entries, value = TRUE)
  if (!length(txt)) {
    rlang::abort(paste(
      "The downloaded file is not a GTFS feed - it contains no .txt",
      "members. The catalogue may have changed; please report this at",
      "https://github.com/jastephan63/polyviz/issues."))
  }
  dir.create(exdir, recursive = TRUE, showWarnings = FALSE)
  utils::unzip(zip, files = txt, exdir = exdir, junkpaths = TRUE)
  invisible(exdir)
}

#' Fetch the Swiss national public-transport timetable (GTFS)
#'
#' Downloads the newest weekly GTFS export of the complete Swiss timetable
#' ("Fahrplan 2026") from opentransportdata.swiss, extracts it into the
#' polyviz download cache, and opens it as a live DuckDB session with one
#' view per GTFS file (`stops`, `trips`, `stop_times`, `calendar`, ...).
#' The returned handle is what [pv_transit_times()] runs on; close it with
#' [pv_gtfs_close()] when done.
#'
#' Be warned about the size before the first call: the ZIP is about
#' 240 MB - by far the largest download in this package - and unpacks to
#' roughly 4 GB of text (35 million `stop_times` rows alone). Both the ZIP
#' and the extracted files stay in the cache
#' (`tools::R_user_dir("polyviz", "cache")`), so later calls open the
#' feed in a couple of seconds without any network. Nothing is loaded
#' into R: DuckDB reads the extracted files in place, which is why the
#' handle needs the duckdb package (a Suggests dependency).
#'
#' Which weekly export counts as "newest" is resolved through a catalogue
#' lookup that is itself cached, so an installed feed stays put between
#' sessions; `refresh = TRUE` re-resolves the newest week, downloads it,
#' and re-extracts.
#'
#' The feed is published by SBB on behalf of the Federal Office of
#' Transport under the opentransportdata.swiss open-data terms ("ODMCH"):
#' free use, source citation required ("Quelle:
#' opentransportdata.swiss"). Every fetch prints these terms and attaches
#' them to the handle (`pv_source`, `pv_licence`, `pv_url`).
#'
#' @param refresh Set to TRUE to re-resolve the newest weekly export,
#'   download it again, and rebuild the extracted copy.
#' @return A handle of class `pv_gtfs`: a list carrying the extract
#'   directory (`dir`), the open DuckDB connection (`con`, one view per
#'   GTFS file), the file names found (`files`), and the feed's
#'   `feed_info` row. Printing it shows the feed's validity window and a
#'   few counts.
#' @seealso [pv_transit_times()], [pv_gtfs_close()], [pv_cache_status()]
#' @examples
#' \dontrun{
#' gtfs <- pv_fetch_gtfs() # first call: ~240 MB download + extraction
#' gtfs
#' times <- pv_transit_times(gtfs, "Luzern", "2026-09-15")
#' pv_gtfs_close(gtfs)
#' }
#' @export
pv_fetch_gtfs <- function(refresh = FALSE) {
  if (!isTRUE(refresh) && !isFALSE(refresh)) {
    rlang::abort("`refresh` must be TRUE or FALSE.")
  }
  sql_need_duckdb("Working with the national GTFS timetable")

  res <- pv_gtfs_latest_resource(refresh = refresh)
  dat <- file.path(pv_cache_dir(),
                   paste0(pv_cache_key(res$url), ".dat"))
  if (refresh || !file.exists(dat)) {
    # nothing else in the package downloads anywhere near this much, so
    # nothing else warns first either - this one does
    size <- if (is.na(res$size)) "about 240 MB" else
      sprintf("%.0f MB", res$size / 1e6)
    message(sprintf(paste0(
      "Downloading %s (%s) from data.opentransportdata.swiss - the ",
      "complete national timetable, by far polyviz's largest download. ",
      "This can take several minutes; the file is then kept in the ",
      "download cache (see pv_cache_status())."), res$name, size))
  }
  zip <- pv_download(res$url, refresh = refresh)$path

  exdir <- file.path(pv_cache_dir(), paste0("gtfs-", res$stamp))
  marker <- file.path(exdir, ".pv-extracted")
  if (refresh || !file.exists(marker)) {
    # the marker only appears after a complete extraction, so an
    # interrupted one is redone rather than trusted
    unlink(exdir, recursive = TRUE)
    message(sprintf(
      "Extracting %s (roughly 4 GB uncompressed) ...", res$name))
    pv_gtfs_extract(zip, exdir)
    file.create(marker)
  }

  handle <- pv_gtfs_open(exdir, url = res$url)
  pv_deliver(
    handle,
    source = sprintf(paste0(
      "opentransportdata.swiss (ODMCH), Fahrplan 2026, %s ",
      "(SBB for the Federal Office of Transport)"), res$name),
    licence = pv_licence_opentransport,
    url = res$url)
}

# ---- the handle ------------------------------------------------------------

# The view definition for one GTFS file. stop_times clock times must stay
# text: GTFS times run past midnight ("25:10:00" is ten past one the next
# morning, still the same service day), and DuckDB's sampling type
# inference would call the column TIME on a file whose first rows are all
# ordinary times, then choke on the first late-night row.
pv_gtfs_view_sql <- function(name, path) {
  esc <- gsub("'", "''", path, fixed = TRUE)
  types <- if (name == "stop_times") {
    ", types = {'arrival_time': 'VARCHAR', 'departure_time': 'VARCHAR'}"
  } else {
    ""
  }
  sprintf(
    "CREATE OR REPLACE VIEW %s AS SELECT * FROM read_csv_auto('%s', header = true%s)",
    name, esc, types)
}

# Opens a handle on an already-extracted feed directory. Split off from
# pv_fetch_gtfs so the unit tests can run the whole engine on a tiny local
# fixture feed without any network.
pv_gtfs_open <- function(dir, url = NA_character_) {
  sql_need_duckdb("Working with the national GTFS timetable")
  if (!is.character(dir) || length(dir) != 1 || is.na(dir) ||
      !dir.exists(dir)) {
    rlang::abort("`dir` must be an existing directory of GTFS .txt files.")
  }
  files <- list.files(dir, pattern = "\\.txt$")
  tables <- sub("\\.txt$", "", files)
  # only well-behaved GTFS file names become view names; anything else in
  # the directory is ignored rather than interpolated into SQL
  keep <- grepl("^[a-z][a-z_]*$", tables)
  files <- files[keep]
  tables <- tables[keep]

  required <- c("stops", "trips", "stop_times")
  missing <- setdiff(required, tables)
  if (length(missing)) {
    rlang::abort(sprintf(
      "%s is not a usable GTFS feed: it is missing %s.",
      dir, paste0(missing, ".txt", collapse = ", ")))
  }
  if (!any(c("calendar", "calendar_dates") %in% tables)) {
    rlang::abort(sprintf(paste(
      "%s has neither calendar.txt nor calendar_dates.txt,",
      "so no service day can ever be resolved."), dir))
  }

  con <- DBI::dbConnect(duckdb::duckdb())
  ok <- FALSE
  on.exit(if (!ok) pv_db_disconnect(con), add = TRUE)
  for (i in seq_along(tables)) {
    DBI::dbExecute(con, pv_gtfs_view_sql(tables[i], file.path(dir, files[i])))
  }
  feed_info <- if ("feed_info" %in% tables) {
    tryCatch(DBI::dbGetQuery(con, "SELECT * FROM feed_info LIMIT 1"),
             error = function(e) NULL)
  }
  ok <- TRUE
  structure(
    list(dir = normalizePath(dir), con = con, files = sort(tables),
         feed_info = feed_info, url = url,
         # per-handle scratch: lazily built lookup tables and the paths of
         # per-date connection caches live here, not in the list itself,
         # so they survive the copy-on-modify of the handle
         cache = new.env(parent = emptyenv())),
    class = "pv_gtfs")
}

# Shared guard: everything that runs on a handle calls this first.
pv_gtfs_check <- function(gtfs) {
  if (!inherits(gtfs, "pv_gtfs")) {
    rlang::abort("`gtfs` must be a handle from pv_fetch_gtfs().")
  }
  if (!DBI::dbIsValid(gtfs$con)) {
    rlang::abort(paste(
      "This pv_gtfs handle has been closed;",
      "open the feed again with pv_fetch_gtfs()."))
  }
  invisible(gtfs)
}

# Column names of one of the handle's views, for building SQL that adapts
# to the optional GTFS columns a feed may or may not carry.
pv_gtfs_cols <- function(gtfs, table) {
  names(DBI::dbGetQuery(gtfs$con, sprintf("SELECT * FROM %s LIMIT 0", table)))
}

#' @export
print.pv_gtfs <- function(x, ...) {
  if (!DBI::dbIsValid(x$con)) {
    cat("<pv_gtfs> (connection closed)\n")
    return(invisible(x))
  }
  cat(sprintf("<pv_gtfs> GTFS feed at %s\n", x$dir))
  fi <- x$feed_info
  if (!is.null(fi) && nrow(fi)) {
    cat(sprintf("  validity: %s to %s (version %s)\n",
                fi$feed_start_date %||% "?", fi$feed_end_date %||% "?",
                fi$feed_version %||% "?"))
  }
  # only counts that are cheap on the national feed: stops.txt is small,
  # and the other small files barely register - stop_times (gigabytes)
  # is deliberately not counted here
  counts <- tryCatch({
    n_stops <- DBI::dbGetQuery(x$con, "SELECT count(*) AS n FROM stops")$n
    n_stations <- if ("parent_station" %in% pv_gtfs_cols(x, "stops")) {
      DBI::dbGetQuery(x$con, paste(
        "SELECT count(DISTINCT coalesce(parent_station, stop_id)) AS n",
        "FROM stops"))$n
    } else {
      n_stops
    }
    n_routes <- if ("routes" %in% x$files) {
      DBI::dbGetQuery(x$con, "SELECT count(*) AS n FROM routes")$n
    }
    sprintf("  %s stops in %s stations%s\n",
            format(n_stops, big.mark = ","),
            format(n_stations, big.mark = ","),
            if (is.null(n_routes)) "" else
              sprintf(" | %s routes", format(n_routes, big.mark = ",")))
  }, error = function(e) NULL)
  if (!is.null(counts)) {
    cat(counts)
  }
  cat(sprintf("  tables: %s\n", paste(x$files, collapse = ", ")))
  invisible(x)
}

#' Close a GTFS handle
#'
#' Shuts down the DuckDB connection a [pv_fetch_gtfs()] handle holds. The
#' extracted feed and any per-date connection caches stay on disk, so
#' reopening later is quick.
#'
#' @param gtfs A handle from [pv_fetch_gtfs()].
#' @return `TRUE`, invisibly.
#' @seealso [pv_fetch_gtfs()]
#' @export
pv_gtfs_close <- function(gtfs) {
  if (!inherits(gtfs, "pv_gtfs")) {
    rlang::abort("`gtfs` must be a handle from pv_fetch_gtfs().")
  }
  if (DBI::dbIsValid(gtfs$con)) {
    pv_db_disconnect(gtfs$con)
  }
  invisible(TRUE)
}

# ---- argument parsing ------------------------------------------------------

# A service day as the feed spells it: the YYYYMMDD integer the calendar
# files use, plus the matching weekday column name. The weekday comes from
# POSIXlt's numeric wday, never from locale-dependent day names.
pv_gtfs_date <- function(date) {
  d <- if (inherits(date, "Date") && length(date) == 1 && !is.na(date)) {
    date
  } else if (is.character(date) && length(date) == 1 && !is.na(date) &&
             grepl("^\\d{4}-\\d{2}-\\d{2}$", date)) {
    tryCatch(as.Date(date), error = function(e) NULL)
  }
  if (is.null(d) || is.na(d)) {
    rlang::abort(
      '`date` must be a Date or a "YYYY-MM-DD" string, e.g. "2026-09-15".')
  }
  list(date = d,
       int = as.integer(format(d, "%Y%m%d")),
       weekday = c("sunday", "monday", "tuesday", "wednesday", "thursday",
                   "friday", "saturday")[as.POSIXlt(d)$wday + 1])
}

# "HH:MM" or "HH:MM:SS" to seconds. Hours may run past 24 in GTFS style:
# depart = "24:30" asks for the service day's own half past midnight.
pv_gtfs_seconds <- function(x, arg = "depart") {
  if (is.character(x) && length(x) == 1 && !is.na(x) &&
      grepl("^\\d{1,2}:\\d{2}(:\\d{2})?$", x)) {
    p <- as.integer(strsplit(x, ":", fixed = TRUE)[[1]])
    if (length(p) == 2) {
      p <- c(p, 0L)
    }
    if (p[2] <= 59 && p[3] <= 59) {
      return(p[1] * 3600L + p[2] * 60L + p[3])
    }
  }
  rlang::abort(sprintf(
    '`%s` must be a clock time like "08:00" or "08:00:30".', arg))
}

# ---- station tables --------------------------------------------------------

# Builds (once per handle) the station lookup: every stop grouped under
# its parent station, so travel times come out per station rather than
# per platform, plus a dense integer index the scan works in. Returns the
# stations data frame, cached on the handle.
pv_gtfs_stations <- function(gtfs) {
  cached <- get0("stations", envir = gtfs$cache)
  if (!is.null(cached)) {
    return(cached)
  }
  cols <- pv_gtfs_cols(gtfs, "stops")
  parent <- if ("parent_station" %in% cols) {
    "coalesce(parent_station, stop_id)"
  } else {
    "stop_id"
  }
  # within a station, the parent row (if any) supplies name and
  # coordinates; the CASE key makes arg_min pick it, deterministically,
  # over any of its platforms
  pref <- if ("parent_station" %in% cols) {
    "CASE WHEN parent_station IS NULL THEN '' ELSE CAST(stop_id AS VARCHAR) END"
  } else {
    "CAST(stop_id AS VARCHAR)"
  }
  DBI::dbExecute(gtfs$con, sprintf(
    "CREATE OR REPLACE TEMP TABLE pv_stations AS
     SELECT station_id, station, lat, lon,
            CAST(row_number() OVER (ORDER BY station_id) AS INTEGER) AS idx
     FROM (
       SELECT %s AS station_id,
              arg_min(stop_name, %s) AS station,
              arg_min(stop_lat, %s) AS lat,
              arg_min(stop_lon, %s) AS lon
       FROM stops GROUP BY 1
     )", parent, pref, pref, pref))
  parent_q <- if ("parent_station" %in% cols) {
    "coalesce(s.parent_station, s.stop_id)"
  } else {
    "s.stop_id"
  }
  DBI::dbExecute(gtfs$con, sprintf(
    "CREATE OR REPLACE TEMP TABLE pv_stop_station AS
     SELECT s.stop_id, p.idx AS station_idx
     FROM stops s JOIN pv_stations p ON p.station_id = %s", parent_q))
  stations <- DBI::dbGetQuery(gtfs$con, paste(
    "SELECT idx, station_id, station, lat, lon FROM pv_stations",
    "ORDER BY idx"))
  assign("stations", stations, envir = gtfs$cache)
  stations
}

# Turns whatever the user passed as `origin` into a station index:
# a station id, a platform/stop id, or a station name matched
# case-insensitively - with near-misses listed when nothing (or more than
# one station) matches.
pv_gtfs_resolve_origin <- function(gtfs, origin, stations) {
  if (!is.character(origin) || length(origin) != 1 || is.na(origin) ||
      !nzchar(trimws(origin))) {
    rlang::abort(
      '`origin` must be a single station name (or stop id), e.g. "Luzern".')
  }
  origin <- trimws(origin)

  hit <- which(as.character(stations$station_id) == origin)
  if (length(hit) == 1) {
    return(stations$idx[hit])
  }
  # a platform id resolves to the station that owns it
  child <- tryCatch(
    DBI::dbGetQuery(gtfs$con, paste(
      "SELECT station_idx FROM pv_stop_station",
      "WHERE CAST(stop_id AS VARCHAR) = ? LIMIT 1"),
      params = list(origin)),
    error = function(e) NULL)
  if (!is.null(child) && nrow(child) == 1) {
    return(child$station_idx[1])
  }

  lo <- tolower(origin)
  hit <- which(tolower(stations$station) == lo)
  if (length(hit) == 1) {
    return(stations$idx[hit])
  }
  if (length(hit) > 1) {
    rlang::abort(sprintf(paste(
      '"%s" names %d different stations in the feed; pass one of their',
      "stop ids instead: %s."),
      origin, length(hit),
      paste0('"', as.character(stations$station_id[hit]), '"',
             collapse = ", ")))
  }

  # no station of that name: suggest by containment first, edit distance
  # as the fallback, so the error doubles as the discovery step
  nms <- unique(stations$station)
  near <- nms[grepl(lo, tolower(nms), fixed = TRUE)]
  if (!length(near)) {
    near <- nms[order(utils::adist(lo, tolower(nms)))][
      seq_len(min(5, length(nms)))]
  }
  rlang::abort(sprintf(
    'No station in the feed is named "%s". Did you mean: %s?',
    origin,
    paste0('"', utils::head(near, 8), '"', collapse = ", ")))
}

# ---- the day's connections -------------------------------------------------

# Builds (and caches as a Parquet file next to the feed) the complete
# elementary-connection table of one service day: which services run that
# day, their trips' stop_times joined and paired into consecutive
# station-to-station hops with departure/arrival seconds, sorted by
# departure. A second query on the same date - any origin - reuses the
# file and skips the ~20 s scan over stop_times.
pv_gtfs_connections <- function(gtfs, d) {
  key <- sprintf("connections-%d", d$int)
  path <- get0(key, envir = gtfs$cache)
  if (!is.null(path) && file.exists(path)) {
    return(path)
  }
  path <- file.path(gtfs$dir, sprintf("pv-connections-%d-v1.parquet", d$int))
  if (file.exists(path)) {
    assign(key, path, envir = gtfs$cache)
    return(path)
  }
  # a read-only extract directory (or one that vanishes) falls back to a
  # session temp file - the cache is then per-session instead of on disk
  if (file.access(gtfs$dir, mode = 2) != 0) {
    path <- tempfile(fileext = ".parquet")
  }

  # which services run that day: the weekday flag within the calendar
  # validity window, plus the day's added exceptions, minus its removed
  # ones (calendar_dates exception_type 1 and 2)
  parts <- character()
  if ("calendar" %in% gtfs$files) {
    parts <- sprintf(
      "SELECT service_id FROM calendar
       WHERE %s = 1 AND start_date <= %d AND end_date >= %d",
      d$weekday, d$int, d$int)
  }
  if ("calendar_dates" %in% gtfs$files) {
    parts <- c(parts, sprintf(
      "SELECT service_id FROM calendar_dates
       WHERE date = %d AND exception_type = 1", d$int))
  }
  services <- paste0("(", paste(parts, collapse = " UNION "), ")")
  if ("calendar_dates" %in% gtfs$files) {
    services <- sprintf(
      "%s EXCEPT SELECT service_id FROM calendar_dates
       WHERE date = %d AND exception_type = 2", services, d$int)
  }

  # frequencies-based trips (headway service, in the Swiss feed a handful
  # of on-demand lines) have no fixed departures and are left out
  freq <- if ("frequencies" %in% gtfs$files) {
    "AND trip_id NOT IN (SELECT trip_id FROM frequencies)"
  } else {
    ""
  }

  st_cols <- pv_gtfs_cols(gtfs, "stop_times")
  pick <- if ("pickup_type" %in% st_cols) {
    "coalesce(st.pickup_type, 0) <> 1"
  } else {
    "TRUE"
  }
  drop <- if ("drop_off_type" %in% st_cols) {
    "coalesce(st.drop_off_type, 0) <> 1"
  } else {
    "TRUE"
  }
  secs <- function(col) {
    sprintf(paste(
      "CAST(split_part(st.%s, ':', 1) AS INTEGER) * 3600 +",
      "CAST(split_part(st.%s, ':', 2) AS INTEGER) * 60 +",
      "CAST(split_part(st.%s, ':', 3) AS INTEGER)"), col, col, col)
  }

  message(sprintf(
    "Building the connection table for %s (kept for later queries on this date) ...",
    format(d$date)))
  tmp <- paste0(path, ".tmp")
  on.exit(unlink(tmp), add = TRUE)
  # `prev` chains each hop to the previous one of the same trip, so the
  # scan can stay seated on a vehicle without paying the change buffer;
  # `rn` is the global departure order the chain refers to
  DBI::dbExecute(gtfs$con, sprintf(
    "COPY (
       WITH pv_services AS (%s),
       pv_day_trips AS (
         SELECT trip_id FROM trips
         WHERE service_id IN (SELECT service_id FROM pv_services) %s
       ),
       pv_events AS (
         SELECT st.trip_id, st.stop_sequence, ss.station_idx,
                %s AS arr_s,
                %s AS dep_s,
                %s AS pick_ok,
                %s AS drop_ok
         FROM stop_times st
         JOIN pv_day_trips USING (trip_id)
         JOIN pv_stop_station ss ON ss.stop_id = st.stop_id
         WHERE st.arrival_time IS NOT NULL AND st.departure_time IS NOT NULL
       ),
       pv_conn AS (
         SELECT trip_id, stop_sequence,
                station_idx AS from_idx, dep_s AS dep, pick_ok,
                lead(station_idx) OVER w AS to_idx,
                lead(arr_s) OVER w AS arr,
                lead(drop_ok) OVER w AS drop_ok
         FROM pv_events
         WINDOW w AS (PARTITION BY trip_id ORDER BY stop_sequence)
         QUALIFY lead(station_idx) OVER w IS NOT NULL
                 AND lead(station_idx) OVER w <> station_idx
       ),
       pv_numbered AS (
         SELECT *, CAST(row_number()
                  OVER (ORDER BY dep, trip_id, stop_sequence) AS INTEGER) AS rn
         FROM pv_conn
       )
       SELECT rn, dep, arr, from_idx, to_idx, pick_ok, drop_ok,
              lag(rn) OVER (PARTITION BY trip_id ORDER BY stop_sequence) AS prev
       FROM pv_numbered
       ORDER BY rn
     ) TO '%s' (FORMAT PARQUET)",
    services, freq, secs("arrival_time"), secs("departure_time"),
    pick, drop, gsub("'", "''", tmp, fixed = TRUE)))
  file.rename(tmp, path)
  assign(key, path, envir = gtfs$cache)
  path
}

# The station-to-station minimum transfer times of transfers.txt, grouped
# to parent stations and filtered (in DuckDB) to the stations that appear
# in the given day's connections. Self-entries become that station's
# change buffer; entries between different stations become footpaths.
pv_gtfs_transfers <- function(gtfs, parquet) {
  empty <- data.frame(from_idx = integer(), to_idx = integer(),
                      walk = integer())
  if (!"transfers" %in% gtfs$files) {
    return(empty)
  }
  if (!isTRUE(get0("transfers_built", envir = gtfs$cache))) {
    cols <- pv_gtfs_cols(gtfs, "transfers")
    if (!all(c("from_stop_id", "to_stop_id", "min_transfer_time") %in%
             cols)) {
      return(empty)
    }
    type <- if ("transfer_type" %in% cols) {
      "coalesce(tr.transfer_type, 0) = 2"
    } else {
      "TRUE"
    }
    # the Swiss feed extends transfers.txt with route- and trip-pinned
    # rows; only the plain station-to-station ones express a walking time
    # that holds for every journey
    extra <- intersect(
      c("from_trip_id", "to_trip_id", "from_route_id", "to_route_id"), cols)
    pin <- if (length(extra)) {
      paste(" AND", paste(sprintf("tr.%s IS NULL", extra),
                          collapse = " AND "))
    } else {
      ""
    }
    DBI::dbExecute(gtfs$con, sprintf(
      "CREATE OR REPLACE TEMP TABLE pv_transfers_all AS
       SELECT f.station_idx AS from_idx, t.station_idx AS to_idx,
              CAST(min(tr.min_transfer_time) AS INTEGER) AS walk
       FROM transfers tr
       JOIN pv_stop_station f ON f.stop_id = tr.from_stop_id
       JOIN pv_stop_station t ON t.stop_id = tr.to_stop_id
       WHERE %s AND tr.min_transfer_time IS NOT NULL%s
       GROUP BY 1, 2", type, pin))
    assign("transfers_built", TRUE, envir = gtfs$cache)
  }
  esc <- gsub("'", "''", parquet, fixed = TRUE)
  DBI::dbExecute(gtfs$con, sprintf(
    "CREATE OR REPLACE TEMP TABLE pv_day_stations AS
     SELECT DISTINCT from_idx AS idx FROM read_parquet('%s')
     UNION
     SELECT DISTINCT to_idx FROM read_parquet('%s')", esc, esc))
  DBI::dbGetQuery(gtfs$con, paste(
    "SELECT tt.from_idx, tt.to_idx, tt.walk FROM pv_transfers_all tt",
    "JOIN pv_day_stations a ON a.idx = tt.from_idx",
    "JOIN pv_day_stations b ON b.idx = tt.to_idx"))
}

# ---- the scan --------------------------------------------------------------

# The Connection Scan Algorithm over the day's connections, vectorised in
# blocks. A plain R loop over ~3 million connections would take minutes;
# instead the departure-sorted connections are processed in fixed-size
# blocks, and within each block a small fixpoint iteration does the work
# with whole-vector operations: station arrivals enable boardings, the
# `prev` chain carries a boarded trip forward past intermediate stops, and
# footpaths fan arrivals out. A block spans only a few minutes of
# departures, so its internal chains are short and the fixpoint settles
# after a handful of passes.
pv_gtfs_scan <- function(cn, n_stations, origin, depart_s, buffer, fp,
                         max_legs, block) {
  st_time <- rep(Inf, n_stations) # earliest arrival, seconds
  st_legs <- rep(Inf, n_stations) # vehicles boarded to achieve it
  bt <- rep(Inf, n_stations)      # earliest a NEW trip can be boarded
  st_time[origin] <- depart_s
  st_legs[origin] <- 0
  bt[origin] <- depart_s          # the first boarding needs no buffer

  # footpath edges in CSR layout: for a set of just-improved stations,
  # push their arrival + walking time out to the neighbours, repeatedly,
  # until the walking closure settles (chains of footpaths are rare and
  # short). Walking does not board anything, so legs carry over and the
  # walk time itself already covers the change buffer.
  relax <- function(stations) {
    while (length(stations) && length(fp$from)) {
      lo <- fp$ptr[stations]
      hi <- fp$ptr[stations + 1L] - 1L
      has <- hi >= lo
      if (!any(has)) {
        return(invisible(NULL))
      }
      e <- sequence(hi[has] - lo[has] + 1L, from = lo[has])
      u <- fp$from[e]
      v <- fp$to[e]
      cand <- st_time[u] + fp$walk[e]
      cl <- st_legs[u]
      o <- order(cand, cl)
      vo <- v[o]
      first <- !duplicated(vo)
      vv <- vo[first]
      cc <- cand[o][first]
      ll <- cl[o][first]
      # a walk can open an earlier boarding even when a vehicle already
      # got there sooner (its change buffer may outlast the walk), so the
      # boarding clock improves independently of the arrival label
      imp_bt <- cc < bt[vv]
      if (any(imp_bt)) {
        bt[vv[imp_bt]] <<- cc[imp_bt]
      }
      imp <- cc < st_time[vv] | (cc == st_time[vv] & ll < st_legs[vv])
      if (!any(imp)) {
        return(invisible(NULL))
      }
      vv <- vv[imp]
      st_time[vv] <<- cc[imp]
      st_legs[vv] <<- ll[imp]
      stations <- vv
    }
    invisible(NULL)
  }
  relax(origin)

  n <- length(cn$dep)
  ride <- rep(Inf, n) # fewest boardings with which each hop can be ridden
  i1 <- 1L
  while (i1 <= n) {
    i2 <- min(i1 + block - 1L, n)
    I <- i1:i2
    dI <- cn$dep[I]
    aI <- cn$arr[I]
    fI <- cn$from[I]
    tI <- cn$to[I]
    pkI <- cn$pick[I]
    drI <- cn$drop[I]
    pvI <- cn$prev[I]
    pv_ok <- !is.na(pvI)
    pv_idx <- ifelse(pv_ok, pvI, 1L)
    repeat {
      changed <- FALSE
      # board where a station arrival (plus its buffer) makes the
      # departure, within the transfer allowance
      bl <- st_legs[fI] + 1
      bl[!(pkI & bt[fI] <= dI & bl <= max_legs)] <- Inf
      # ... and stay seated: a hop is ridable with however few boardings
      # its trip's previous hop was ridable with. One pass moves the
      # chain one hop, so iterate it to rest.
      repeat {
        pr <- ride[pv_idx]
        pr[!pv_ok] <- Inf
        new_ride <- pmin(bl, pr)
        upd <- new_ride < ride[I]
        if (!any(upd)) {
          break
        }
        ride[I[upd]] <- new_ride[upd]
        changed <- TRUE
      }
      # arrivals: per station the earliest (fewest boardings on a tie)
      # candidate this block offers, applied where it improves
      rI <- ride[I]
      act <- which(is.finite(rI) & drI)
      if (length(act)) {
        va <- tI[act]
        aa <- aI[act]
        la <- rI[act]
        o <- order(aa, la)
        vo <- va[o]
        first <- !duplicated(vo)
        vv <- vo[first]
        a2 <- aa[o][first]
        l2 <- la[o][first]
        imp <- a2 < st_time[vv] | (a2 == st_time[vv] & l2 < st_legs[vv])
        if (any(imp)) {
          vv <- vv[imp]
          st_time[vv] <- a2[imp]
          st_legs[vv] <- l2[imp]
          bt[vv] <- pmin(bt[vv], a2[imp] + buffer[vv])
          relax(vv)
          changed <- TRUE
        }
      }
      if (!changed) {
        break
      }
    }
    i1 <- i2 + 1L
  }
  list(time = st_time, legs = st_legs)
}

# ---- pv_transit_times ------------------------------------------------------

#' Earliest arrival at every Swiss station
#'
#' Computes, for one departure from `origin` on one service day, the
#' earliest possible arrival time at every station in the feed - the raw
#' material of a travel-time (isochrone) map. Journeys may chain any
#' vehicles the timetable offers, changing where the feed's minimum
#' transfer times (or a two-minute default) allow, up to `max_transfers`
#' changes and `max_minutes` of travel.
#'
#' The engine is a Connection Scan: DuckDB assembles the chosen day's
#' timetable - the services running that day (weekday flags plus
#' `calendar_dates` exceptions), their trips' consecutive stop pairs as
#' departure/arrival events, everything grouped to parent stations so
#' platforms count as one station - sorts it by departure, and caches it
#' as a Parquet file next to the extracted feed. R then sweeps the
#' connections once in departure order in vectorised blocks. On the
#' national feed the first query for a date spends about 20 seconds
#' building that day's connection table (roughly 3 million rows); the
#' sweep itself - a three-hour window pulls some 50 MB of connections
#' into R - takes a few seconds more. Later queries on the same date,
#' from any origin, skip the build and finish in a few seconds.
#'
#' The details worth knowing:
#' * Times past `"24:00"` are real GTFS times (the service day's own
#'   after-midnight departures) and are included; the search does not
#'   continue into the next day's services.
#' * Station-to-station entries of `transfers.txt` (type 2) supply
#'   minimum change times; where a station has no entry, changing
#'   vehicles there costs two minutes. Stops flagged as no-boarding or
#'   no-alighting are honoured.
#' * Frequency-based trips (`frequencies.txt`, a handful of on-demand
#'   services in the Swiss feed) are left out.
#' * The answers are exactly what the feed says, closures included: a
#'   station whose line is shut for engineering work that day really is
#'   unreachable by rail, with the replacement buses arriving at their
#'   own bus-stop station (e.g. `"Stans, Bahnhof"` rather than
#'   `"Stans"`).
#' * The scan keeps one label per station - the earliest arrival, with
#'   the fewest changes achieving it - which honours `max_transfers`
#'   without being a full multi-criteria search; in rare corners a
#'   station reachable only by trading a later arrival for fewer changes
#'   can come out `NA` or later than that trade would allow.
#'
#' @param gtfs A handle from [pv_fetch_gtfs()].
#' @param origin The starting station, matched case-insensitively against
#'   the feed's station names (e.g. `"Luzern"`); a station or platform id
#'   works too. A miss aborts with the closest names; a name shared by
#'   several stations aborts listing their ids.
#' @param date The service day, a `Date` or `"YYYY-MM-DD"` string.
#' @param depart Departure time at `origin`, `"HH:MM"` or `"HH:MM:SS"`;
#'   hours past 24 address the service day's own small hours, GTFS style.
#' @param max_transfers Maximum number of vehicle changes allowed.
#' @param max_minutes Travel-time horizon in minutes; stations not
#'   reachable within it come back `NA`.
#' @return A data frame with one row per station: `station` (name),
#'   `stop_id` (the parent station's id), `lat`, `lon`, `minutes` (travel
#'   time from `origin`, `NA` when unreachable within the limits), and
#'   `transfers` (vehicle changes on that earliest journey). Rows are
#'   sorted by `minutes`; the feed's source and licence ride along as
#'   attributes.
#' @seealso [pv_fetch_gtfs()], [pv_gtfs_close()]
#' @examples
#' \dontrun{
#' gtfs <- pv_fetch_gtfs()
#' reach <- pv_transit_times(gtfs, "Luzern", "2026-09-15")
#' head(reach, 20)
#' pv_gtfs_close(gtfs)
#' }
#' @export
pv_transit_times <- function(gtfs, origin, date, depart = "08:00",
                             max_transfers = 4, max_minutes = 180) {
  pv_gtfs_check(gtfs)
  d <- pv_gtfs_date(date)
  depart_s <- pv_gtfs_seconds(depart)
  if (!is.numeric(max_transfers) || length(max_transfers) != 1 ||
      is.na(max_transfers) || max_transfers < 0 ||
      max_transfers != round(max_transfers)) {
    rlang::abort("`max_transfers` must be a single whole number >= 0.")
  }
  if (!is.numeric(max_minutes) || length(max_minutes) != 1 ||
      is.na(max_minutes) || max_minutes <= 0) {
    rlang::abort("`max_minutes` must be a single number of minutes > 0.")
  }

  stations <- pv_gtfs_stations(gtfs)
  origin_idx <- pv_gtfs_resolve_origin(gtfs, origin, stations)
  parquet <- pv_gtfs_connections(gtfs, d)

  esc <- gsub("'", "''", parquet, fixed = TRUE)
  n_day <- DBI::dbGetQuery(gtfs$con, sprintf(
    "SELECT count(*) AS n FROM read_parquet('%s')", esc))$n
  if (n_day == 0) {
    warning(sprintf(
      "No services run on %s in this feed (its calendar may not cover that date).",
      format(d$date)), call. = FALSE)
  }

  # only the window a journey could possibly use crosses into R: nothing
  # departing before the traveller, nothing arriving past the horizon
  limit_s <- depart_s + max_minutes * 60
  cn <- DBI::dbGetQuery(gtfs$con, sprintf(
    "SELECT rn, dep, arr, from_idx, to_idx, pick_ok, drop_ok, prev
     FROM read_parquet('%s')
     WHERE dep >= %d AND arr <= %d
     ORDER BY rn", esc, depart_s, as.integer(ceiling(limit_s))))
  # re-point the trip chains at row positions within the window; a chain
  # link departing before the traveller could never be ridden anyway
  if (nrow(cn)) {
    pos <- integer(max(cn$rn))
    pos[cn$rn] <- seq_len(nrow(cn))
    prev <- pos[cn$prev]
    prev[!is.na(prev) & prev == 0L] <- NA_integer_
  } else {
    prev <- integer()
  }

  tr <- pv_gtfs_transfers(gtfs, parquet)
  buffer <- rep(120, nrow(stations))
  self <- tr$from_idx == tr$to_idx
  buffer[tr$from_idx[self]] <- tr$walk[self]
  fp_from <- tr$from_idx[!self]
  fp_to <- tr$to_idx[!self]
  fp_walk <- tr$walk[!self]
  o <- order(fp_from)
  fp <- list(from = fp_from[o], to = fp_to[o], walk = fp_walk[o],
             ptr = cumsum(c(1L, tabulate(fp_from, nrow(stations)))))

  res <- pv_gtfs_scan(
    list(dep = cn$dep, arr = cn$arr, from = cn$from_idx, to = cn$to_idx,
         pick = cn$pick_ok, drop = cn$drop_ok, prev = prev),
    n_stations = nrow(stations), origin = origin_idx, depart_s = depart_s,
    buffer = buffer, fp = fp, max_legs = max_transfers + 1,
    block = max(1L, as.integer(getOption("polyviz.gtfs_block", 50000L))))

  reached <- is.finite(res$time) & res$time <= limit_s
  minutes <- ifelse(reached, round((res$time - depart_s) / 60, 1), NA_real_)
  transfers <- as.integer(ifelse(reached, pmax(res$legs - 1, 0), NA))

  out <- data.frame(
    station = stations$station,
    stop_id = as.character(stations$station_id),
    lat = stations$lat, lon = stations$lon,
    minutes = minutes, transfers = transfers,
    stringsAsFactors = FALSE)
  out <- out[order(out$minutes, out$station), , drop = FALSE]
  rownames(out) <- NULL
  message(sprintf(
    "%s %s %s: %s of %s stations reachable within %s min (max %s transfer(s)).",
    stations$station[stations$idx == origin_idx], format(d$date), depart,
    format(sum(reached), big.mark = ","),
    format(nrow(out), big.mark = ","),
    format(max_minutes), format(max_transfers)))
  attr(out, "pv_source") <- attr(gtfs, "pv_source")
  attr(out, "pv_licence") <- attr(gtfs, "pv_licence")
  attr(out, "pv_url") <- attr(gtfs, "pv_url")
  attr(out, "pv_origin") <- stations$station[stations$idx == origin_idx]
  attr(out, "pv_date") <- d$date
  attr(out, "pv_depart") <- depart
  out
}
