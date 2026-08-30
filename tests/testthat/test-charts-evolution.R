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
  expect_equal(w$x$legend, "auto")
  expect_equal(w$x$series, "value")
})

test_that("area sends the legend flag as-is and validates its shape", {
  agglo <- subset(pv_city_population, city %in% c("Luzern", "Emmen"))
  # "auto" travels unresolved: the JavaScript side decides at render time.
  expect_equal(pv_area(agglo, "year", "population",
                       series = "city")$x$legend, "auto")
  expect_true(pv_area(agglo, "year", "population", series = "city",
                      legend = TRUE)$x$legend)
  expect_false(pv_area(agglo, "year", "population", series = "city",
                       legend = FALSE)$x$legend)
  expect_error(pv_area(agglo, "year", "population", legend = "yes"),
               "TRUE, FALSE")
  expect_error(pv_area(agglo, "year", "population", legend = NA),
               "TRUE, FALSE")
})

test_that("area resolves axis-title overrides in the payload", {
  agglo <- subset(pv_city_population, city %in% c("Luzern", "Emmen"))
  w <- pv_area(agglo, "year", "population", series = "city")
  expect_equal(w$x$xlab, "year")
  expect_equal(w$x$ylab, "population")
  # Percent and stream offsets drop the default y title (their y axes
  # don't show raw values), but an explicit one survives.
  expect_equal(pv_area(agglo, "year", "population", series = "city",
                       offset = "percent")$x$ylab, "")
  expect_equal(pv_area(agglo, "year", "population", series = "city",
                       offset = "percent", ylab = "Share")$x$ylab, "Share")
  # NA and "" both suppress; a string replaces the column name.
  w2 <- pv_area(agglo, "year", "population", series = "city",
                xlab = NA, ylab = "")
  expect_equal(w2$x$xlab, "")
  expect_equal(w2$x$ylab, "")
  expect_equal(pv_area(agglo, "year", "population", series = "city",
                       xlab = "Census year")$x$xlab, "Census year")
  expect_error(pv_area(agglo, "year", "population", series = "city",
                       xlab = c("a", "b")), "single string")
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

test_that("heatmap sends the cell-value flag as-is and validates its shape", {
  emp <- subset(pv_city_sectors, city %in% c("Luzern", "Bern"))
  expect_equal(pv_heatmap(emp, "city", "sector", "share")$x$cellValues,
               "auto")
  expect_true(pv_heatmap(emp, "city", "sector", "share",
                         cell_values = TRUE)$x$cellValues)
  expect_false(pv_heatmap(emp, "city", "sector", "share",
                          cell_values = FALSE)$x$cellValues)
  expect_error(pv_heatmap(emp, "city", "sector", "share",
                          cell_values = "sometimes"), "TRUE, FALSE")
})

test_that("heatmap validates the label character budget", {
  emp <- subset(pv_city_sectors, city %in% c("Luzern", "Bern"))
  expect_equal(pv_heatmap(emp, "city", "sector", "share")$x$truncateLabels,
               24L)
  expect_equal(pv_heatmap(emp, "city", "sector", "share",
                          truncate_labels = 12)$x$truncateLabels, 12L)
  expect_error(pv_heatmap(emp, "city", "sector", "share",
                          truncate_labels = 0), "positive")
  expect_error(pv_heatmap(emp, "city", "sector", "share",
                          truncate_labels = "lots"), "positive")
})

test_that("heatmap draws axis titles only when set explicitly", {
  emp <- subset(pv_city_sectors, city %in% c("Luzern", "Bern"))
  w <- pv_heatmap(emp, "city", "sector", "share")
  expect_equal(w$x$xtitle, "")
  expect_equal(w$x$ytitle, "")
  # xlab/ylab keep carrying the column names — the tooltip contract.
  expect_equal(w$x$xlab, "city")
  expect_equal(w$x$ylab, "sector")
  w2 <- pv_heatmap(emp, "city", "sector", "share",
                   xlab = "City", ylab = "Economic sector")
  expect_equal(w2$x$xtitle, "City")
  expect_equal(w2$x$ytitle, "Economic sector")
  expect_equal(pv_heatmap(emp, "city", "sector", "share",
                          xlab = NA)$x$xtitle, "")
})
