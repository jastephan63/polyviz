expect_pvchart <- function(w, type) {
  expect_s3_class(w, "htmlwidget")
  expect_equal(attr(w, "package"), "polyviz")
  expect_equal(w$x$type, type)
  invisible(w)
}

fiscal25 <- function() {
  f <- pv_fiscal[pv_fiscal$year == 2025, ]
  rownames(f) <- NULL
  f
}

test_that("choropleth passes the map through untouched and joins on id", {
  f <- fiscal25()
  w <- expect_pvchart(
    pv_choropleth(f, id = "municipality_id", value = "resource_index"),
    "choropleth")
  # The map travels in the payload exactly as it came in.
  expect_identical(w$x$map, pv_lucerne_map)
  # Ids go across as strings (the JS side compares them as strings too).
  expect_equal(w$x$data$id, as.character(f$municipality_id))
  expect_equal(w$x$data$value, f$resource_index)
  expect_equal(w$x$vlab, "resource_index")
  # Sequential is the default, its domain the plain data range.
  expect_equal(w$x$palette, "sequential")
  expect_equal(w$x$domain, range(f$resource_index))
  expect_null(w$x$center)
})

test_that("choropleth diverging needs a center and sends a symmetric domain", {
  f <- fiscal25()
  expect_error(
    pv_choropleth(f, id = "municipality_id", value = "resource_index",
                  palette = "diverging"),
    "center")
  expect_error(
    pv_choropleth(f, id = "municipality_id", value = "resource_index",
                  palette = "diverging", center = "hundred"),
    "center")
  w <- pv_choropleth(f, id = "municipality_id", value = "resource_index",
                     palette = "diverging", center = 100)
  expect_equal(w$x$center, 100)
  m <- max(abs(range(f$resource_index) - 100))
  expect_equal(w$x$domain, 100 + c(-m, m))
  # And a center without the diverging palette is refused, not ignored.
  expect_error(
    pv_choropleth(f, id = "municipality_id", value = "resource_index",
                  center = 100),
    "diverging")
})

test_that("choropleth validates its columns and the map object", {
  f <- fiscal25()
  expect_error(pv_choropleth(f, id = "nope", value = "resource_index"),
               "not in `data`")
  expect_error(pv_choropleth(f, id = "municipality_id", value = "nope"),
               "not in `data`")
  expect_error(
    pv_choropleth(f, map = list(type = "Polygon"),
                  id = "municipality_id", value = "resource_index"),
    "FeatureCollection")
  # A collection whose features have no properties$id cannot be joined.
  bare <- list(type = "FeatureCollection",
               features = list(list(type = "Feature",
                                    properties = list(name = "X"),
                                    geometry = NULL)))
  expect_error(
    pv_choropleth(f, map = bare, id = "municipality_id",
                  value = "resource_index"),
    "properties")
})

test_that("choropleth warns on partly-missing ids and aborts on none", {
  f <- fiscal25()
  # Seven ids off the map: the warning names the first five, counts the rest.
  f$municipality_id[1:7] <- 90001:90007
  expect_warning(
    w <- pv_choropleth(f, id = "municipality_id",
                       value = "resource_index"),
    "7 id\\(s\\).*90001, 90002, 90003, 90004, 90005 and 2 more")
  # The unmatched rows still travel; the map simply has no feature for them.
  expect_equal(nrow(w$x$data), nrow(f))
  # No overlap at all is an error, not an empty grey map.
  g <- fiscal25()
  g$municipality_id <- seq_len(nrow(g)) + 90000
  expect_error(
    pv_choropleth(g, id = "municipality_id", value = "resource_index"),
    "No value in `municipality_id` matches")
})

test_that("choropleth drops missing values with a warning and refuses duplicate ids", {
  f <- fiscal25()
  f$resource_index[3] <- NA
  expect_warning(
    w <- pv_choropleth(f, id = "municipality_id", value = "resource_index"),
    "Dropped 1 row\\(s\\) with missing `resource_index` values")
  expect_equal(nrow(w$x$data), nrow(f) - 1)
  expect_false(as.character(f$municipality_id[3]) %in% w$x$data$id)
  # The NA row's domain influence goes with it.
  expect_equal(w$x$domain, range(f$resource_index, na.rm = TRUE))

  dup <- rbind(fiscal25(), fiscal25()[1, ])
  expect_error(
    pv_choropleth(dup, id = "municipality_id", value = "resource_index"),
    "aggregate")

  allna <- fiscal25()
  allna$resource_index <- NA
  expect_error(
    pv_choropleth(allna, id = "municipality_id", value = "resource_index"),
    "non-missing")
})

test_that("choropleth widens an all-equal colour domain", {
  f <- fiscal25()
  f$resource_index <- 100
  w <- pv_choropleth(f, id = "municipality_id", value = "resource_index")
  expect_equal(w$x$domain, c(99, 101))
  wd <- pv_choropleth(f, id = "municipality_id", value = "resource_index",
                      palette = "diverging", center = 100)
  expect_equal(wd$x$domain, c(99, 101))
})
