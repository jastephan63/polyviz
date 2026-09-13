# The GTFS engine is tested end to end on a tiny handcrafted feed
# (fixtures/gtfs-mini): two connecting lines, one transfer point with a
# transfers.txt minimum change time, platforms grouped under parent
# stations, a calendar_dates added and a removed service, a frequencies
# trip that must be ignored, and a route running past midnight. Every
# arrival minute below is hand-computed from those files.
#
# There is deliberately NO live-download test of pv_fetch_gtfs: the real
# feed is a ~240 MB ZIP, and a pull of that size has no place in a test
# suite. The download/extract plumbing is exercised against a mocked
# pv_download instead, and the query engine runs on the fixture through
# the same internal constructor pv_fetch_gtfs itself uses.

# Opens a handle on a throwaway copy of the fixture feed, so the per-date
# connection caches the engine writes land in the tempdir, never in
# fixtures/.
local_gtfs_mini <- function(drop = character(), env = parent.frame()) {
  skip_if_not_installed("duckdb")
  dir <- withr::local_tempdir(.local_envir = env)
  src <- list.files(test_path("fixtures", "gtfs-mini"), full.names = TRUE)
  src <- src[!basename(src) %in% drop]
  file.copy(src, dir)
  g <- suppressMessages(pv_gtfs_open(dir))
  withr::defer(pv_gtfs_close(g), envir = env)
  g
}

# Minutes / transfers for one station out of a result frame.
mins <- function(res, station) res$minutes[match(station, res$station)]
tfrs <- function(res, station) res$transfers[match(station, res$station)]

tt <- function(g, ...) suppressMessages(pv_transit_times(g, ...))

test_that("pv_gtfs_open builds a handle with one view per file", {
  g <- local_gtfs_mini()
  expect_s3_class(g, "pv_gtfs")
  expect_true(all(c("stops", "trips", "stop_times", "calendar",
                    "transfers", "frequencies") %in% g$files))
  expect_identical(as.character(g$feed_info$feed_version), "test-1")
  n <- pv_query(g$con, "SELECT count(*) AS n FROM stops")$n
  expect_equal(n, 18)
})

test_that("the handle prints feed dates and cheap counts", {
  g <- local_gtfs_mini()
  # the method is called directly so the test does not depend on the
  # S3 registration that only lands in NAMESPACE at document time
  expect_output(print.pv_gtfs(g), "<pv_gtfs>")
  expect_output(print.pv_gtfs(g), "20260901 to 20261031")
  expect_output(print.pv_gtfs(g), "version test-1")
  expect_output(print.pv_gtfs(g), "18 stops in 9 stations")
  expect_output(print.pv_gtfs(g), "6 routes")
})

test_that("pv_gtfs_close shuts the connection and later use errors cleanly", {
  skip_if_not_installed("duckdb")
  dir <- withr::local_tempdir()
  file.copy(list.files(test_path("fixtures", "gtfs-mini"),
                       full.names = TRUE), dir)
  g <- suppressMessages(pv_gtfs_open(dir))
  expect_invisible(pv_gtfs_close(g))
  expect_output(print.pv_gtfs(g), "closed")
  expect_error(pv_transit_times(g, "Alpdorf", "2026-09-15"), "closed")
  # closing twice is fine
  expect_invisible(pv_gtfs_close(g))
})

test_that("unusable feed directories are refused with names", {
  skip_if_not_installed("duckdb")
  expect_error(pv_gtfs_open(file.path(tempdir(), "nope-gtfs")),
               "existing directory")
  dir <- withr::local_tempdir()
  file.copy(list.files(test_path("fixtures", "gtfs-mini"),
                       full.names = TRUE), dir)
  unlink(file.path(dir, "stops.txt"))
  expect_error(pv_gtfs_open(dir), "stops.txt")
  # a feed needs at least one of the two calendar files
  dir2 <- withr::local_tempdir()
  file.copy(list.files(test_path("fixtures", "gtfs-mini"),
                       full.names = TRUE), dir2)
  unlink(file.path(dir2, c("calendar.txt", "calendar_dates.txt")))
  expect_error(pv_gtfs_open(dir2), "neither calendar")
})

test_that("the duckdb doorway guards the GTFS engine", {
  local_mocked_bindings(sql_has_duckdb = function() FALSE)
  expect_error(pv_fetch_gtfs(), "duckdb")
  expect_error(pv_gtfs_open(test_path("fixtures", "gtfs-mini")), "duckdb")
})

# ---- pv_transit_times on the fixture ---------------------------------------

# Hand-computed picture for Tuesday 2026-09-15, departing Alpdorf 08:00.
# T1 (runs daily): Alpdorf 08:00 -> Brugghausen 08:10 -> Casteln 08:25.
# Changing at Brugghausen costs 300 s (transfers.txt B1->B2, grouped to
# the parent), so T2 (dep 08:20 -> Dorfli 08:30 -> Escherwil 08:40) is
# catchable but T3 (dep 08:13 -> Dorfli 08:21) is not - with the default
# two-minute buffer instead, Dorfli would wrongly come out at 21.
# Changing at Casteln has no transfers.txt entry, so the two-minute
# default applies: T6 (dep 08:28, runs ONLY this day via a calendar_dates
# addition) is catchable and reaches Dorfli 08:29, one minute before T2.
# T5 to Furtal would run on Tuesdays but is REMOVED this day by a
# calendar_dates exception, and T7 (08:01 -> Furtal 08:20) is a
# frequencies-based trip the engine leaves out: Furtal is unreachable.
# T4 (Casteln 23:50 -> Grat 24:20) is far past the default horizon.
test_that("earliest arrivals on the fixture are exactly right", {
  g <- local_gtfs_mini()
  res <- tt(g, "Alpdorf", "2026-09-15")

  expect_identical(names(res),
                   c("station", "stop_id", "lat", "lon", "minutes",
                     "transfers"))
  expect_equal(nrow(res), 9) # one row per parent station, not per platform
  expect_setequal(res$stop_id, paste0("P", c("A", "B", "C", "D", "E", "F",
                                             "G", "H", "I")))

  expect_equal(mins(res, "Alpdorf"), 0)
  expect_equal(tfrs(res, "Alpdorf"), 0L)
  expect_equal(mins(res, "Brugghausen"), 10)
  expect_equal(tfrs(res, "Brugghausen"), 0L)
  expect_equal(mins(res, "Casteln"), 25)
  expect_equal(tfrs(res, "Casteln"), 0L)
  expect_equal(mins(res, "Dorfli"), 29)
  expect_equal(tfrs(res, "Dorfli"), 1L)
  expect_equal(mins(res, "Escherwil"), 40)
  expect_equal(tfrs(res, "Escherwil"), 1L)
  expect_true(is.na(mins(res, "Furtal")))
  expect_true(is.na(tfrs(res, "Furtal")))
  expect_true(is.na(mins(res, "Grat")))
  expect_true(is.na(mins(res, "Zweidorf")))

  # sorted by minutes, unreached stations last
  expect_equal(res$station[1], "Alpdorf")
  expect_false(is.unsorted(res$minutes, na.rm = TRUE))
  expect_true(all(is.na(res$minutes[6:9])))

  expect_equal(attr(res, "pv_origin"), "Alpdorf")
  expect_equal(attr(res, "pv_date"), as.Date("2026-09-15"))
})

test_that("calendar exceptions cut both ways on another Tuesday", {
  g <- local_gtfs_mini()
  # 2026-09-22: S_EXC runs (Furtal via T5), S_ADD does not (Dorfli falls
  # back to T2's 08:30)
  res <- tt(g, "Alpdorf", as.Date("2026-09-22"))
  expect_equal(mins(res, "Furtal"), 35)
  expect_equal(tfrs(res, "Furtal"), 0L)
  expect_equal(mins(res, "Dorfli"), 30)
  expect_equal(tfrs(res, "Dorfli"), 1L)
  expect_equal(mins(res, "Escherwil"), 40)
})

test_that("times past 24:00 are the service day's own small hours", {
  g <- local_gtfs_mini()
  res <- tt(g, "Alpdorf", "2026-09-15", max_minutes = 1200)
  # T4 leaves Casteln 23:50 and arrives Grat 24:20 - 980 minutes after
  # the 08:00 departure, one change
  expect_equal(mins(res, "Grat"), 980)
  expect_equal(tfrs(res, "Grat"), 1L)
})

test_that("max_transfers and max_minutes cut the search off", {
  g <- local_gtfs_mini()
  res <- tt(g, "Alpdorf", "2026-09-15", max_transfers = 0)
  expect_equal(mins(res, "Brugghausen"), 10)
  expect_equal(mins(res, "Casteln"), 25)
  expect_true(is.na(mins(res, "Dorfli")))
  expect_true(is.na(mins(res, "Escherwil")))

  res <- tt(g, "Alpdorf", "2026-09-15", max_minutes = 20)
  expect_equal(mins(res, "Brugghausen"), 10)
  expect_true(is.na(mins(res, "Casteln")))
})

test_that("depart shifts the clock", {
  g <- local_gtfs_mini()
  res <- tt(g, "Alpdorf", "2026-09-15", depart = "07:30")
  expect_equal(mins(res, "Brugghausen"), 40) # still the 08:10 arrival
  expect_equal(mins(res, "Casteln"), 55)
})

test_that("origin accepts names case-insensitively and station/stop ids", {
  g <- local_gtfs_mini()
  a <- tt(g, "Alpdorf", "2026-09-15")
  for (o in c("alpdorf", "ALPDORF", "  Alpdorf ", "PA", "A2")) {
    b <- tt(g, o, "2026-09-15")
    expect_equal(b$minutes, a$minutes)
    expect_equal(b$transfers, a$transfers)
  }
})

test_that("origin misses and ambiguity abort helpfully", {
  g <- local_gtfs_mini()
  # a partial name lists the stations containing it
  err <- expect_error(tt(g, "Brugg", "2026-09-15"), "Did you mean")
  expect_match(conditionMessage(err), "Brugghausen")
  # a nonsense name still suggests the closest ones
  expect_error(tt(g, "Xyzzy", "2026-09-15"), "Did you mean")
  # two stations share the name Zweidorf: the error lists their ids
  err <- expect_error(tt(g, "Zweidorf", "2026-09-15"), "2 different stations")
  expect_match(conditionMessage(err), "PH")
  expect_match(conditionMessage(err), "PI")
  expect_error(tt(g, 42, "2026-09-15"), "single station name")
  expect_error(tt(g, c("A", "B"), "2026-09-15"), "single station name")
})

test_that("dates, times, and limits are validated", {
  g <- local_gtfs_mini()
  expect_error(tt(g, "Alpdorf", "15.09.2026"), "YYYY-MM-DD")
  expect_error(tt(g, "Alpdorf", "2026-02-30"), "YYYY-MM-DD")
  expect_error(tt(g, "Alpdorf", 42), "YYYY-MM-DD")
  expect_error(tt(g, "Alpdorf", "2026-09-15", depart = "8am"), "clock time")
  expect_error(tt(g, "Alpdorf", "2026-09-15", depart = "08:70"), "clock time")
  expect_error(tt(g, "Alpdorf", "2026-09-15", max_transfers = -1),
               "whole number")
  expect_error(tt(g, "Alpdorf", "2026-09-15", max_transfers = 1.5),
               "whole number")
  expect_error(tt(g, "Alpdorf", "2026-09-15", max_minutes = 0),
               "minutes > 0")
  expect_error(pv_transit_times(mtcars, "Alpdorf", "2026-09-15"),
               "pv_fetch_gtfs")
})

test_that("a day outside the calendar warns and returns only the origin", {
  g <- local_gtfs_mini()
  expect_warning(res <- tt(g, "Alpdorf", "2026-11-03"), "No services")
  expect_equal(mins(res, "Alpdorf"), 0)
  expect_equal(sum(!is.na(res$minutes)), 1)
})

test_that("the day's connections are cached and reused per date", {
  g <- local_gtfs_mini()
  res1 <- tt(g, "Alpdorf", "2026-09-15")
  p <- file.path(g$dir, "pv-connections-20260915-v1.parquet")
  expect_true(file.exists(p))
  m1 <- file.mtime(p)
  # a second origin on the same date rides on the cached table
  res2 <- tt(g, "Casteln", "2026-09-15")
  expect_identical(file.mtime(p), m1)
  # T6 leaves Casteln 08:28 and reaches Dorfli 08:29, no change needed
  expect_equal(mins(res2, "Dorfli"), 29)
  expect_equal(tfrs(res2, "Dorfli"), 0L)
  # a different date builds its own cache
  tt(g, "Alpdorf", "2026-09-22")
  expect_true(file.exists(
    file.path(g$dir, "pv-connections-20260922-v1.parquet")))
})

test_that("a read-only extract falls back to a session cache", {
  g <- local_gtfs_mini()
  Sys.chmod(g$dir, "0555")
  withr::defer(Sys.chmod(g$dir, "0755"))
  res <- tt(g, "Alpdorf", "2026-09-15")
  expect_equal(mins(res, "Escherwil"), 40)
  expect_false(file.exists(
    file.path(g$dir, "pv-connections-20260915-v1.parquet")))
})

test_that("the chunked scan gives the same answer at any block size", {
  g <- local_gtfs_mini()
  a <- tt(g, "Alpdorf", "2026-09-15", max_minutes = 1200)
  withr::local_options(polyviz.gtfs_block = 2)
  b <- tt(g, "Alpdorf", "2026-09-15", max_minutes = 1200)
  expect_equal(a$minutes, b$minutes)
  expect_equal(a$transfers, b$transfers)
  expect_equal(a$station, b$station)
})

# ---- pv_fetch_gtfs plumbing (mocked, no network) ---------------------------

test_that("pv_fetch_gtfs resolves the newest weekly ZIP, warns, extracts", {
  skip_if_not_installed("duckdb")
  skip_if(!nzchar(Sys.which("zip")), "no zip binary")
  withr::local_envvar(c(R_USER_CACHE_DIR = withr::local_tempdir()))

  # a fake catalogue answer: two weekly exports, the newer one NOT first,
  # plus an unrelated resource that must be ignored
  base <- "https://example.org/download/"
  ckan <- list(success = TRUE, result = list(resources = list(
    list(download_url = paste0(base, "gtfs_mini_20260901.zip"),
         byte_size = 236000000),
    list(download_url = paste0(base, "readme.md")),
    list(download_url = paste0(base, "gtfs_mini_20260908.zip"),
         byte_size = 236000000))))
  json <- withr::local_tempfile(fileext = ".json")
  jsonlite::write_json(ckan, json, auto_unbox = TRUE)

  zip <- withr::local_tempfile(fileext = ".zip")
  withr::with_dir(test_path("fixtures", "gtfs-mini"),
                  utils::zip(zip, list.files(), flags = "-q"))

  seen <- character()
  local_mocked_bindings(pv_download = function(url, ...) {
    seen[[length(seen) + 1]] <<- url
    if (grepl("package_show", url, fixed = TRUE)) {
      list(path = json, cached = FALSE)
    } else {
      list(path = zip, cached = FALSE)
    }
  })

  msgs <- capture_messages(g <- pv_fetch_gtfs())
  withr::defer(pv_gtfs_close(g))
  expect_match(msgs, "largest download", all = FALSE)
  expect_match(msgs, "Licence", all = FALSE)
  # the newest stamp won, not the first-listed resource
  expect_match(seen[2], "20260908", fixed = TRUE)
  expect_match(attr(g, "pv_source"), "opentransportdata.swiss")
  expect_match(attr(g, "pv_licence"), "Quelle: opentransportdata.swiss")
  expect_true(dir.exists(file.path(pv_cache_dir(), "gtfs-20260908")))

  # the extracted feed actually answers queries
  res <- tt(g, "Alpdorf", "2026-09-15")
  expect_equal(mins(res, "Casteln"), 25)

  # once the ZIP sits in the cache and the extract exists, a second call
  # neither warns about the size nor re-extracts
  writeLines("x", file.path(pv_cache_dir(),
                            paste0(pv_cache_key(seen[2]), ".dat")))
  msgs <- capture_messages(g2 <- pv_fetch_gtfs())
  withr::defer(pv_gtfs_close(g2))
  expect_false(any(grepl("largest download", msgs)))
  expect_false(any(grepl("Extracting", msgs)))
})

test_that("a reshuffled catalogue without weekly ZIPs aborts clearly", {
  skip_if_not_installed("duckdb")
  ckan <- list(success = TRUE, result = list(resources = list(
    list(download_url = "https://example.org/other.csv"))))
  json <- withr::local_tempfile(fileext = ".json")
  jsonlite::write_json(ckan, json, auto_unbox = TRUE)
  local_mocked_bindings(
    pv_download = function(url, ...) list(path = json, cached = FALSE))
  expect_error(pv_fetch_gtfs(), "catalogue layout may have changed")
  expect_error(pv_fetch_gtfs(refresh = "yes"), "TRUE or FALSE")
})
