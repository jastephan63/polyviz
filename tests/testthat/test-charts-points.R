expect_pvchart <- function(w, type) {
  expect_s3_class(w, "htmlwidget")
  expect_equal(attr(w, "package"), "polyviz")
  expect_equal(w$x$type, type)
  invisible(w)
}

fiscal25 <- function() {
  f25 <- subset(pv_fiscal, year == 2025)
  f25$side <- ifelse(f25$equalization_chf > 0,
                     "receives equalization", "contributes")
  f25
}

test_that("beeswarm packs value, group, and label columns", {
  f25 <- fiscal25()
  w <- expect_pvchart(
    pv_beeswarm(f25, "resource_index", group = "side",
                label = "municipality"), "beeswarm")
  expect_equal(nrow(w$x$data), nrow(f25))
  expect_equal(w$x$data$value, f25$resource_index)
  expect_equal(w$x$data$group, f25$side)
  expect_equal(w$x$data$label, f25$municipality)
  expect_equal(w$x$xlab, "resource_index")
  # without group/label the payload carries neither column
  w2 <- pv_beeswarm(f25, "resource_index")
  expect_equal(names(w2$x$data), "value")
})

test_that("beeswarm drops missing values with a warning and validates columns", {
  f25 <- fiscal25()
  f25$resource_index[3] <- NA
  expect_warning(
    w <- pv_beeswarm(f25, "resource_index", group = "side"),
    "Dropped 1 row\\(s\\) with missing `resource_index` values")
  expect_equal(nrow(w$x$data), nrow(f25) - 1)
  # a missing group drops the row too - a dot needs a lane
  f25b <- fiscal25()
  f25b$side[5] <- NA
  expect_warning(
    w2 <- pv_beeswarm(f25b, "resource_index", group = "side"),
    "Dropped 1 row\\(s\\) with missing `side` values")
  expect_equal(nrow(w2$x$data), nrow(f25b) - 1)
  expect_error(pv_beeswarm(f25, "municipality"), "numeric")
  expect_error(pv_beeswarm(f25, "nope"), "not in `data`")
  allna <- data.frame(v = c(NA_real_, NA_real_))
  expect_error(pv_beeswarm(allna, "v"), "non-missing")
})

test_that("beeswarm refuses more than 800 dots, advising aggregation", {
  big <- data.frame(v = seq_len(801) / 10)
  expect_error(pv_beeswarm(big, "v"), "800")
  expect_error(pv_beeswarm(big, "v"), "Aggregate")
  # missing values don't count toward the cap
  big$v[1:2] <- NA
  expect_warning(w <- pv_beeswarm(big, "v"), "Dropped 2 row")
  expect_pvchart(w, "beeswarm")
  expect_equal(nrow(w$x$data), 799)
})

test_that("beeswarm caps groups at the 8 palette slots", {
  expect_error(pv_beeswarm(pv_fiscal, "resource_index",
                           group = "municipality"), "Other")
  # 8 group levels (the years) squeak through
  w <- pv_beeswarm(pv_fiscal, "resource_index", group = "year")
  expect_equal(length(unique(w$x$data$group)), 8)
})

test_that("beeswarm axis title follows the shared override rule", {
  f25 <- fiscal25()
  w <- pv_beeswarm(f25, "resource_index", xlab = "Index (average = 100)")
  expect_equal(w$x$xlab, "Index (average = 100)")
  w2 <- pv_beeswarm(f25, "resource_index", xlab = NA)
  expect_null(w2$x$xlab)
  expect_error(pv_beeswarm(f25, "resource_index", xlab = c("a", "b")),
               "single string")
})
