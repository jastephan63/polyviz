# Same helper as test-widgets.R — each test file stands alone.
expect_pvchart <- function(w, type) {
  expect_s3_class(w, "htmlwidget")
  expect_equal(attr(w, "package"), "polyviz")
  expect_equal(w$x$type, type)
  invisible(w)
}

test_that("area widget builds from real data and validates columns", {
  agglo <- subset(pv_city_population,
                  city %in% c("Luzern", "Emmen", "Kriens", "Horw", "Ebikon"))
  w <- expect_pvchart(pv_area(agglo, "year", "population", series = "city"),
                      "area")
  expect_equal(w$x$offset, "stacked")
  expect_equal(w$x$xtype, "number")
  expect_setequal(w$x$series, unique(agglo$city))
  expect_equal(nrow(w$x$data), nrow(agglo))
  expect_error(pv_area(agglo, "nope", "population"), "not in `data`")
})

test_that("area keeps series in first-appearance order", {
  df <- data.frame(t = rep(1:3, 2), v = 1:6,
                   s = rep(c("late", "early"), each = 3))
  w <- pv_area(df, "t", "v", series = "s")
  expect_equal(w$x$series, c("late", "early"))
})

test_that("area refuses duplicate series/x rows", {
  dup <- data.frame(t = c(1, 1, 2), v = c(10, 99, 5), s = "a")
  expect_error(pv_area(dup, "t", "v", series = "s"), "aggregate")
  # Single-series charts hit the same pivot, so the same rule applies.
  expect_error(pv_area(dup, "t", "v"), "aggregate")
})

test_that("area caps series at the 8 palette slots", {
  expect_error(pv_area(pv_city_population, "year", "population",
                       series = "city"), "Other")
})

test_that("area understands offsets and x types", {
  agglo <- subset(pv_city_population, city %in% c("Luzern", "Emmen"))
  expect_equal(pv_area(agglo, "year", "population", series = "city",
                       offset = "percent")$x$offset, "percent")
  expect_equal(pv_area(agglo, "year", "population", series = "city",
                       offset = "stream")$x$offset, "stream")
  expect_error(pv_area(agglo, "year", "population", offset = "wavy"))
  dated <- data.frame(d = as.Date(c("2024-01-01", "2024-02-01")), v = 1:2)
  w <- pv_area(dated, "d", "v")
  expect_equal(w$x$xtype, "date")
  expect_equal(w$x$data$x, c("2024-01-01", "2024-02-01"))
  expect_false(w$x$showLegend)
  expect_equal(w$x$series, "value")
})

test_that("heatmap builds and picks its colour domain in R", {
  emp <- subset(pv_city_sectors,
                city %in% c("Luzern", "Winterthur", "Bern"))
  w <- expect_pvchart(pv_heatmap(emp, "city", "sector", "share"), "heatmap")
  expect_equal(w$x$palette, "sequential")
  expect_equal(w$x$domain, range(emp$share))
  div <- data.frame(a = rep(c("p", "q"), 2), b = rep(c("r", "s"), each = 2),
                    v = c(-2, 5, 1, -4))
  wd <- pv_heatmap(div, "a", "b", "v", palette = "diverging")
  expect_equal(wd$x$domain, c(-5, 5))
})

test_that("heatmap validates cells and drops missing values", {
  dup <- data.frame(a = c("p", "p"), b = c("q", "q"), v = 1:2)
  expect_error(pv_heatmap(dup, "a", "b", "v"), "aggregate")
  bad <- data.frame(a = "p", b = "q", v = 1)
  expect_error(pv_heatmap(bad, "a", "b", "nope"), "not in `data`")
  nas <- data.frame(a = c("p", "q"), b = c("r", "r"), v = c(1, NA))
  expect_equal(nrow(pv_heatmap(nas, "a", "b", "v")$x$data), 1)
  all_na <- data.frame(a = c("p", "q"), b = "r", v = NA_real_)
  expect_error(pv_heatmap(all_na, "a", "b", "v"), "non-missing")
})

test_that("heatmap widens an all-equal colour domain", {
  flat <- data.frame(a = c("p", "q"), b = c("r", "r"), v = c(3, 3))
  expect_equal(pv_heatmap(flat, "a", "b", "v")$x$domain, c(2, 4))
})
