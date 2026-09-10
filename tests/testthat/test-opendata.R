# Search-result parsing, resource selection, and the licence gate run
# offline against CKAN fixture metadata; only the last tests hit the live
# opendata.swiss catalogue.

opendata_skip_if_offline <- function() {
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

local_opendata_cache <- function(env = parent.frame()) {
  withr::local_envvar(
    c(R_USER_CACHE_DIR = withr::local_tempdir(.local_envir = env)),
    .local_envir = env)
}

read_ckan_fixture <- function(name) {
  jsonlite::fromJSON(test_path("fixtures", name),
                     simplifyVector = FALSE)$result
}

test_that("pv_licence_info tells open terms from restricted ones", {
  by_ask <- polyviz:::pv_licence_info(
    "https://opendata.swiss/terms-of-use#terms_by_ask")
  expect_true(by_ask$open)
  expect_true(by_ask$by)
  expect_true(by_ask$ask)
  expect_equal(by_ask$label, "OPEN BY ASK")

  by <- polyviz:::pv_licence_info(
    "https://opendata.swiss/terms-of-use#terms_by")
  expect_true(by$open && by$by && !by$ask)

  ask <- polyviz:::pv_licence_info(
    "https://opendata.swiss/terms-of-use#terms_ask")
  expect_true(ask$open && !ask$by && ask$ask)

  open <- polyviz:::pv_licence_info(
    "https://opendata.swiss/terms-of-use#terms_open")
  expect_true(open$open && !open$by && !open$ask)

  expect_true(polyviz:::pv_licence_info(
    "http://www.opendefinition.org/licenses/cc-zero")$open)
  expect_true(polyviz:::pv_licence_info(
    "NonCommercialAllowed-CommercialAllowed-ReferenceRequired")$by)
  expect_true(polyviz:::pv_licence_info(
    "NonCommercialAllowed-CommercialWithPermission-ReferenceRequired")$ask)

  expect_false(polyviz:::pv_licence_info(
    "NonCommercialAllowed-CommercialNotAllowed-ReferenceRequired")$open)
  expect_false(polyviz:::pv_licence_info(
    "https://creativecommons.org/licenses/by-nc/4.0/")$open)
  expect_false(polyviz:::pv_licence_info("ClosedData")$open)
  expect_false(polyviz:::pv_licence_info(NULL)$open)
  expect_equal(polyviz:::pv_licence_info("")$label, "no licence stated")
})

test_that("pick_lang prefers English, then the national languages", {
  expect_equal(polyviz:::pick_lang(list(de = "Hallo", en = "Hello")), "Hello")
  expect_equal(polyviz:::pick_lang(list(de = "Hallo", fr = "Bonjour")),
               "Hallo")
  expect_equal(polyviz:::pick_lang(list(it = "Ciao")), "Ciao")
  expect_equal(polyviz:::pick_lang("plain"), "plain")
  expect_true(is.na(polyviz:::pick_lang(NULL)))
  expect_true(is.na(polyviz:::pick_lang(list(de = ""))))
})

test_that("pv_search_opendata builds the compact result frame", {
  fixture <- read_ckan_fixture("ckan-search.json")
  local_mocked_bindings(pv_ckan = function(action, params) fixture)

  expect_message(out <- pv_search_opendata("Finanzausgleich"),
                 "2 dataset\\(s\\)")
  expect_equal(names(out),
               c("title", "id", "organisation", "licence", "formats"))
  expect_equal(out$id,
               c("finanzausgleich-kanton-luzern", "zuger-finanzausgleich"))
  expect_equal(out$title[1], "Financial equalization canton of Lucerne")
  expect_equal(out$organisation, c("LUSTAT Statistics Lucerne", "Kanton Zug"))
  expect_equal(out$licence, c("OPEN BY ASK", "OPEN BY"))
  expect_equal(out$formats, c("CSV", "CSV, JSON"))
})

test_that("pv_search_opendata validates its arguments", {
  expect_error(pv_search_opendata(""), "non-empty search string")
  expect_error(pv_search_opendata("x", rows = 0), "between 1 and 100")
  expect_error(pv_search_opendata("x", rows = 1:2), "between 1 and 100")
})

test_that("resource selection picks CSVs by position, number, and name", {
  local_opendata_cache()
  pkg <- read_ckan_fixture("ckan-dataset-open.json")
  # point the CSV resources at local copies (with the original file names
  # kept apart) so no network is involved
  dir <- withr::local_tempdir()
  fixture <- test_path("fixtures", "lustat-fiscal.csv")
  file.copy(fixture, file.path(dir, "fa-lu-tla.csv"))
  file.copy(fixture, file.path(dir, "fa-lu-ra.csv"))
  pkg$resources[[2]]$url <- paste0("file://",
                                   file.path(normalizePath(dir),
                                             "fa-lu-tla.csv"))
  pkg$resources[[3]]$url <- paste0("file://",
                                   file.path(normalizePath(dir),
                                             "fa-lu-ra.csv"))

  # NULL takes the first CSV resource, skipping the HTML landing page
  expect_message(
    d <- polyviz:::pv_deliver_ckan_resource(pkg),
    "Topographic cost compensation")
  expect_s3_class(d, "data.frame")
  expect_true("fa_jahr" %in% names(d))

  # a number counts CSV resources only
  expect_message(
    polyviz:::pv_deliver_ckan_resource(pkg, resource = 2),
    "Resource equalization")
  expect_error(polyviz:::pv_deliver_ckan_resource(pkg, resource = 5),
               "no number 5")

  # strings match resource ids and file names
  expect_message(
    polyviz:::pv_deliver_ckan_resource(
      pkg, resource = "0e13ec9c-9686-4e19-94c5-14f2a9a1efdd"),
    "Resource equalization")
  expect_message(
    polyviz:::pv_deliver_ckan_resource(pkg, resource = "fa-lu-ra.csv"),
    "Resource equalization")
  # ".csv" may be left off
  expect_message(
    polyviz:::pv_deliver_ckan_resource(pkg, resource = "fa-lu-ra"),
    "Resource equalization")
  expect_error(polyviz:::pv_deliver_ckan_resource(pkg, resource = "nope"),
               "No resource")
})

test_that("delivery states the OPEN BY ASK terms and attaches provenance", {
  local_opendata_cache()
  pkg <- read_ckan_fixture("ckan-dataset-open.json")
  pkg$resources[[2]]$url <- paste0(
    "file://", normalizePath(test_path("fixtures", "lustat-fiscal.csv")))

  expect_message(d <- polyviz:::pv_deliver_ckan_resource(pkg),
                 "OPEN BY ASK")
  expect_message(polyviz:::pv_deliver_ckan_resource(pkg),
                 "commercial use requires the data owner's permission")
  expect_message(polyviz:::pv_deliver_ckan_resource(pkg),
                 "Quelle: LUSTAT Statistics Lucerne")
  expect_match(attr(d, "pv_source"), "opendata.swiss")
  expect_match(attr(d, "pv_licence"), "OPEN BY ASK")
})

test_that("resources without an open licence are refused", {
  local_opendata_cache()
  closed <- read_ckan_fixture("ckan-dataset-closed.json")
  expect_error(polyviz:::pv_deliver_ckan_resource(closed),
               "not published under an open licence")
  # nothing was downloaded or cached on the way to the refusal
  expect_equal(nrow(pv_cache_status()), 0)

  # a dataset that states no licence at all is refused too
  pkg <- read_ckan_fixture("ckan-dataset-open.json")
  for (i in seq_along(pkg$resources)) {
    pkg$resources[[i]]$rights <- NULL
    pkg$resources[[i]]$license <- NULL
  }
  expect_error(polyviz:::pv_deliver_ckan_resource(pkg),
               "no licence stated")
})

test_that("datasets without a CSV resource error clearly", {
  pkg <- read_ckan_fixture("ckan-dataset-open.json")
  pkg$resources <- pkg$resources[1]  # only the HTML landing page remains
  expect_error(polyviz:::pv_deliver_ckan_resource(pkg),
               "no CSV resource")
  expect_error(polyviz:::pv_deliver_ckan_resource(pkg), "HTML")
})

test_that("pv_fetch_opendata validates its arguments", {
  expect_error(pv_fetch_opendata(1), "single dataset slug")
  expect_error(pv_fetch_opendata("x", refresh = "yes"), "TRUE or FALSE")
})

test_that("pv_search_opendata finds live datasets", {
  opendata_skip_if_offline()
  # a quoted phrase reaches CKAN as a phrase query and pins the result down
  expect_message(
    out <- pv_search_opendata('"Finanzausgleich Kanton Luzern"', rows = 5),
    "opendata.swiss")
  expect_s3_class(out, "data.frame")
  expect_gt(nrow(out), 0)
  expect_true("finanzausgleich-kanton-luzern" %in% out$id)
})

test_that("pv_fetch_opendata downloads live data through the cache", {
  opendata_skip_if_offline()
  local_opendata_cache()

  expect_message(
    d <- pv_fetch_opendata("finanzausgleich-kanton-luzern",
                           resource = "fa-lu-ra.csv"),
    "OPEN BY ASK")
  expect_s3_class(d, "data.frame")
  # The portal's bot protection sometimes hands cloud runners a page
  # that is not the dataset; skip on an unrecognisable shape (the
  # parser is covered on fixtures).
  if (!all(c("fa_jahr", "gnr") %in% names(d))) {
    skip("data.lustat.ch served an unexpected shape from this runner")
  }
  expect_true(all(c("fa_jahr", "gnr", "gname") %in% names(d)))
  expect_match(attr(d, "pv_licence"), "owner's permission")

  # the data file (not the catalogue lookup) landed in the cache, and the
  # second fetch reuses it
  entry <- pv_cache_status()
  expect_equal(nrow(entry), 1)
  before <- file.mtime(entry$file)
  expect_message(
    pv_fetch_opendata("finanzausgleich-kanton-luzern",
                      resource = "fa-lu-ra.csv"),
    "OPEN BY ASK")
  expect_equal(file.mtime(entry$file), before)

  expect_error(pv_fetch_opendata("polyviz-no-such-dataset-xyz"),
               "Could not look up")
})
