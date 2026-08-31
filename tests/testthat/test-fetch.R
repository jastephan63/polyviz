# The parsing and caching are covered offline on fixture files; only the
# last few tests talk to the live portals, and they skip cleanly on CRAN
# or without a connection.

fetch_skip_if_offline <- function() {
  skip_on_cran()
  online <- if (requireNamespace("curl", quietly = TRUE)) {
    curl::has_internet()
  } else {
    withr::local_options(timeout = 10)
    tryCatch({
      con <- url("https://opendata.swiss", open = "rb")
      close(con)
      TRUE
    }, error = function(e) FALSE, warning = function(w) FALSE)
  }
  skip_if(!isTRUE(online), "no internet connection")
}

# Every cache-touching test gets its own throwaway cache directory;
# tools::R_user_dir honours R_USER_CACHE_DIR.
local_fetch_cache <- function(env = parent.frame()) {
  withr::local_envvar(
    c(R_USER_CACHE_DIR = withr::local_tempdir(.local_envir = env)),
    .local_envir = env)
}

file_url <- function(path) paste0("file://", normalizePath(path))

test_that("pv_parse_sdmx_labels tidies the csv-with-labels format", {
  d <- polyviz:::pv_parse_sdmx_labels(
    test_path("fixtures", "sdmx-city-pop.csv"))
  expect_s3_class(d, "data.frame")
  expect_equal(nrow(d), 9)

  # code/label pairs become <id> (label) plus <id>_code columns
  expect_true(all(c("ssv_swiss_city", "ssv_swiss_city_code",
                    "ssv_pop_1930", "ssv_pop_1930_code",
                    "obs_value", "obs_status", "period") %in% names(d)))
  expect_type(d$obs_value, "double")
  expect_equal(d$ssv_swiss_city[d$ssv_swiss_city_code == "117"], "Hinwil")

  # the SDMX header machinery is gone
  expect_false(any(c("STRUCTURE", "STRUCTURE_ID", "ACTION") %in% names(d)))
  # the dataset-comment attribute repeats its own id as "code"; that column
  # says nothing and is dropped, the comment text itself stays
  expect_false("dataset_comment_code" %in% names(d))
  expect_true("dataset_comment" %in% names(d))
  expect_match(d$dataset_comment[1], "Quellen: BFS")

  attr_flow <- attr(d, "pv_dataflow")
  expect_match(attr_flow, "DF_SSV_POP_1930")
})

test_that("the SSV quirks are filterable from the tidy output", {
  d <- polyviz:::pv_parse_sdmx_labels(
    test_path("fixtures", "sdmx-city-pop.csv"))
  # population counts = pop_ref_period* codes whose label is a bare year;
  # this keeps census and current-period rows but drops the density and
  # percent-change variants and the "_ST" all-cities total
  keep <- startsWith(d$ssv_pop_1930_code, "pop_ref_period") &
    grepl("^\\d{4}$", d$ssv_pop_1930) & d$ssv_swiss_city_code != "_ST"
  pop <- d[keep, ]
  expect_equal(nrow(pop), 5)
  expect_false(any(pop$ssv_swiss_city_code == "_ST"))
  expect_false(any(grepl("^(vpop|dens)", pop$ssv_pop_1930_code)))
  # the current-period row carries its year in the label, not the code
  expect_true("pop_ref_period-10" %in% pop$ssv_pop_1930_code)
  expect_true(all(grepl("^\\d{4}$", pop$ssv_pop_1930)))
})

test_that("pv_parse_sdmx_labels rejects files that are not SDMX exports", {
  expect_error(
    polyviz:::pv_parse_sdmx_labels(
      test_path("fixtures", "lustat-fiscal.csv")),
    "SDMX")
})

test_that("pv_read_swiss_csv sniffs the separator and converts types", {
  d <- polyviz:::pv_read_swiss_csv(test_path("fixtures", "lustat-fiscal.csv"))
  expect_equal(nrow(d), 3)
  expect_true(all(c("fa_jahr", "gnr", "gname", "rp_pEinw", "ra") %in%
                    names(d)))
  expect_type(d$fa_jahr, "integer")
  expect_type(d$rp_pEinw, "double")
  expect_equal(d$gname[1], "Doppleschwand")

  comma <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("name,value", "a,1", "b,2"), comma)
  d2 <- polyviz:::pv_read_swiss_csv(comma)
  expect_equal(names(d2), c("name", "value"))
  expect_equal(d2$value, c(1L, 2L))
})

test_that("downloads are cached and refresh bypasses the cache", {
  local_fetch_cache()
  src <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("a,b", "1,2"), src)
  url <- file_url(src)

  d1 <- polyviz:::pv_download(url)
  expect_false(d1$cached)
  expect_equal(readLines(d1$path)[2], "1,2")

  # the source changes, but the second fetch is served from the cache
  writeLines(c("a,b", "9,9"), src)
  d2 <- polyviz:::pv_download(url)
  expect_true(d2$cached)
  expect_equal(readLines(d2$path)[2], "1,2")

  # refresh = TRUE goes back to the source
  d3 <- polyviz:::pv_download(url, refresh = TRUE)
  expect_false(d3$cached)
  expect_equal(readLines(d3$path)[2], "9,9")

  status <- pv_cache_status()
  expect_equal(nrow(status), 1)
  expect_equal(status$url, url)

  expect_message(n <- pv_cache_clear(), "Removed 1 cached")
  expect_equal(n, 1)
  expect_equal(nrow(pv_cache_status()), 0)
})

test_that("a failed download errors clearly and leaves no cache entry", {
  local_fetch_cache()
  url <- file_url(tempdir())  # a directory that exists, but no such file
  url <- paste0(url, "/polyviz-does-not-exist.csv")
  expect_error(polyviz:::pv_download(url), class = "polyviz_download_error")
  expect_error(polyviz:::pv_download(url), "Could not download")
  expect_equal(nrow(pv_cache_status()), 0)
  expect_length(list.files(polyviz:::pv_cache_dir()), 0)
})

test_that("uncached downloads never write to the cache", {
  local_fetch_cache()
  src <- withr::local_tempfile(fileext = ".txt")
  writeLines("hello", src)
  d <- polyviz:::pv_download(file_url(src), cache = FALSE)
  expect_false(d$cached)
  expect_length(list.files(polyviz:::pv_cache_dir()), 0)
})

test_that("fetcher arguments are validated", {
  expect_error(pv_fetch_bfs(1), "single dataflow id")
  expect_error(pv_fetch_bfs("DF_SSV_POP_1930", refresh = "yes"),
               "TRUE or FALSE")
  # no DF_<theme>_ shape to derive the owning agency from
  expect_error(pv_fetch_bfs("NOPE"), "agency")
  expect_error(pv_fetch_lustat(c("a", "b")), "single dataset")
  expect_error(pv_fetch_lustat("fa-lu-ra", refresh = NA), "TRUE or FALSE")
})

test_that("pv_fetch_bfs downloads and tidies a live dataflow", {
  fetch_skip_if_offline()
  local_fetch_cache()

  expect_message(d <- pv_fetch_bfs("DF_SSV_POP_1930"), "OPEN BY")
  expect_s3_class(d, "data.frame")
  expect_gt(nrow(d), 1000)
  expect_true(all(c("ssv_swiss_city", "ssv_swiss_city_code", "ssv_pop_1930",
                    "ssv_pop_1930_code", "obs_value") %in% names(d)))
  expect_match(attr(d, "pv_source"), "stats.swiss")
  expect_match(attr(d, "pv_licence"), "OPEN BY")

  # the second fetch is served from the cache, message included
  entry <- pv_cache_status()
  expect_equal(nrow(entry), 1)
  before <- file.mtime(entry$file)
  expect_message(d2 <- pv_fetch_bfs("DF_SSV_POP_1930"), "stats.swiss")
  expect_equal(dim(d2), dim(d))
  expect_equal(file.mtime(entry$file), before)
})

test_that("pv_fetch_lustat downloads live data and states the terms", {
  fetch_skip_if_offline()
  local_fetch_cache()

  expect_message(d <- pv_fetch_lustat("fa-lu-ra"), "OPEN BY ASK")
  expect_message(pv_fetch_lustat("fa-lu-ra"), "commercial use")
  expect_s3_class(d, "data.frame")
  expect_true(all(c("fa_jahr", "gnr", "gname", "ra") %in% names(d)))
  expect_type(d$fa_jahr, "integer")
  expect_match(attr(d, "pv_source"), "LUSTAT Statistik Luzern")
  expect_match(attr(d, "pv_licence"), "owner's permission")
  expect_equal(nrow(pv_cache_status()), 1)
})
