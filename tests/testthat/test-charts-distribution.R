expect_pvchart <- function(w, type) {
  expect_s3_class(w, "htmlwidget")
  expect_equal(attr(w, "package"), "polyviz")
  expect_equal(w$x$type, type)
  invisible(w)
}

test_that("histogram bins with R's rules and respects bins=", {
  f25 <- subset(pv_fiscal, year == 2025)
  w <- expect_pvchart(pv_histogram(f25, "resource_per_capita"), "histogram")
  expect_true(all(c("x0", "x1", "count") %in% names(w$x$data)))
  expect_equal(sum(w$x$data$count), nrow(f25))
  expect_null(w$x$density)
  w10 <- pv_histogram(f25, "resource_per_capita", bins = 10)
  expect_equal(nrow(w10$x$data), 10)
  expect_error(pv_histogram(f25, "municipality"), "numeric")
  expect_error(pv_histogram(f25, "nope"), "not in `data`")
})

test_that("histogram density curve is rescaled to count space", {
  f25 <- subset(pv_fiscal, year == 2025)
  w <- pv_histogram(f25, "resource_per_capita", bins = 20, density = TRUE)
  kde <- pv_kde(f25$resource_per_capita)
  bw <- w$x$data$x1[1] - w$x$data$x0[1]
  expect_equal(w$x$density$y, kde$y * nrow(f25) * bw)
  # the curve's peak must sit on the same scale as the tallest bar
  expect_lt(max(w$x$density$y), 2 * max(w$x$data$count))
})

test_that("boxplot computes Tukey stats per group", {
  w <- expect_pvchart(
    pv_boxplot(pv_fiscal, "resource_per_capita", group = "year"), "boxplot")
  expect_equal(length(w$x$boxes), 8)
  b <- w$x$boxes[[1]]
  s <- pv_boxstats(pv_fiscal$resource_per_capita[pv_fiscal$year == 2020])
  expect_equal(b$group, "2020")
  expect_equal(b$median, s$median)
  expect_equal(b$q1, s$q1)
  expect_equal(b$hi, s$hi)
  expect_equal(b$n, s$n)
  expect_null(w$x$points)
})

test_that("boxplot points are capped at 400 with a fixed seed", {
  # no group: one box over all 638 values, so the cap must kick in
  w <- pv_boxplot(pv_fiscal, "resource_per_capita", points = TRUE)
  expect_equal(length(w$x$boxes), 1)
  expect_equal(w$x$boxes[[1]]$n, nrow(pv_fiscal))
  expect_equal(nrow(w$x$points), 400)
  expect_true(all(w$x$points$value %in% pv_fiscal$resource_per_capita))
  # same call, same sample - and the caller's RNG stream is untouched
  set.seed(1); before <- runif(1)
  w2 <- pv_boxplot(pv_fiscal, "resource_per_capita", points = TRUE)
  set.seed(1); after <- runif(1)
  expect_equal(w$x$points$value, w2$x$points$value)
  expect_equal(before, after)
})

test_that("boxplot points accepts auto and ships the sample accordingly", {
  # "auto" with more than 600 values in total: no point payload at all -
  # the JavaScript side would hide the dots anyway, so R skips the sample
  w <- pv_boxplot(pv_fiscal, "resource_per_capita", group = "year")
  expect_equal(w$x$showPoints, "auto")
  expect_null(w$x$points)
  # "auto" with at most 600 values: the sample ships, ready to draw
  two <- subset(pv_fiscal, year %in% c(2024, 2025))
  w2 <- pv_boxplot(two, "resource_per_capita", group = "year")
  expect_equal(w2$x$showPoints, "auto")
  expect_equal(nrow(w2$x$points), nrow(two))
  # explicit TRUE/FALSE still behave exactly as before, flag passed as-is
  w3 <- pv_boxplot(pv_fiscal, "resource_per_capita", points = TRUE)
  expect_true(w3$x$showPoints)
  expect_equal(nrow(w3$x$points), 400)
  w4 <- pv_boxplot(two, "resource_per_capita", points = FALSE)
  expect_false(w4$x$showPoints)
  expect_null(w4$x$points)
  # anything else is refused up front
  expect_error(pv_boxplot(two, "resource_per_capita", points = "yes"),
               "TRUE, FALSE, or \"auto\"")
})

test_that("axis titles: NULL keeps defaults, strings override, NA/\"\" drop", {
  f25 <- subset(pv_fiscal, year == 2025)
  w <- pv_histogram(f25, "resource_per_capita")
  expect_equal(w$x$xlab, "resource_per_capita")
  expect_equal(w$x$ylab, "count")
  w2 <- pv_histogram(f25, "resource_per_capita",
                     xlab = "CHF per resident", ylab = NA)
  expect_equal(w2$x$xlab, "CHF per resident")
  expect_null(w2$x$ylab)
  w3 <- pv_boxplot(pv_fiscal, "resource_per_capita", group = "year",
                   xlab = "", ylab = "CHF")
  expect_null(w3$x$xlab)
  expect_equal(w3$x$ylab, "CHF")
  w4 <- pv_ridgeline(pv_fiscal, "resource_index", group = "year",
                     xlab = "Index (average = 100)", ylab = "Year")
  expect_equal(w4$x$xlab, "Index (average = 100)")
  expect_equal(w4$x$ylab, "Year")
  # ridgeline has no y title by default; ridges are labelled directly
  w5 <- pv_ridgeline(pv_fiscal, "resource_index", group = "year")
  expect_equal(w5$x$xlab, "resource_index")
  expect_null(w5$x$ylab)
  expect_error(pv_histogram(f25, "resource_per_capita",
                            xlab = c("a", "b")), "single string")
})

test_that("boxplot refuses more groups than palette colours", {
  expect_error(pv_boxplot(pv_fiscal, "resource_per_capita",
                          group = "municipality"), "Other")
  expect_error(pv_boxplot(pv_fiscal, "municipality", group = "year"),
               "numeric")
})

test_that("violin computes density plus box stats per group, in order", {
  w <- expect_pvchart(
    pv_violin(pv_fiscal, "resource_per_capita", group = "year"), "violin")
  expect_equal(length(w$x$violins), 8)
  v <- w$x$violins[[1]]
  expect_equal(v$group, "2020")
  vals20 <- pv_fiscal$resource_per_capita[pv_fiscal$year == 2020]
  s <- pv_boxstats(vals20)
  expect_equal(v$median, s$median)
  expect_equal(v$q1, s$q1)
  expect_equal(v$q3, s$q3)
  expect_equal(v$lo, s$lo)
  expect_equal(v$hi, s$hi)
  expect_equal(v$n, s$n)
  expect_true(all(c("x", "y") %in% names(v$density)))
  expect_equal(v$density$y, pv_kde(vals20)$y)
})

test_that("violin box and points flags are tri-state and shape the payload", {
  # defaults: box on, points off, nothing shipped for the cloud
  w <- pv_violin(pv_fiscal, "resource_per_capita", group = "year")
  expect_true(w$x$showBox)
  expect_false(w$x$showPoints)
  expect_null(w$x$points)
  # "auto" travels as-is, and every pv_fiscal year holds well under 200
  # values, so the jitter sample ships ready for the JavaScript side
  w2 <- pv_violin(pv_fiscal, "resource_per_capita", group = "year",
                  box = "auto", points = "auto")
  expect_equal(w2$x$showBox, "auto")
  expect_equal(w2$x$showPoints, "auto")
  expect_equal(nrow(w2$x$points), nrow(pv_fiscal))
  # points = TRUE ships the jitter sample, capped at 400 per group
  w3 <- pv_violin(pv_fiscal, "resource_per_capita", group = "year",
                  points = TRUE)
  expect_equal(nrow(w3$x$points), nrow(pv_fiscal))
  expect_true(all(w3$x$points$value %in% pv_fiscal$resource_per_capita))
  # anything else is refused up front
  expect_error(pv_violin(pv_fiscal, "resource_per_capita", group = "year",
                         box = "yes"), "TRUE, FALSE, or \"auto\"")
})

test_that("violin auto points hide for big groups; TRUE thins to 400", {
  # one group over the 200-value auto threshold: no payload at all -
  # the JavaScript side would hide the dots anyway
  big <- data.frame(v = as.numeric(1:500),
                    g = rep(c("a", "b"), c(300, 200)))
  w <- pv_violin(big, "v", group = "g", points = "auto")
  expect_equal(w$x$showPoints, "auto")
  expect_null(w$x$points)
  # forced on, the big group is thinned to a fixed-seed sample of 400
  w2 <- pv_violin(rbind(big, big), "v", group = "g", points = TRUE)
  perg <- table(w2$x$points$group)
  expect_equal(as.integer(perg[["a"]]), 400)
  expect_equal(as.integer(perg[["b"]]), 400)
  # same call, same sample - and the caller's RNG stream is untouched
  set.seed(1); before <- runif(1)
  w3 <- pv_violin(rbind(big, big), "v", group = "g", points = TRUE)
  set.seed(1); after <- runif(1)
  expect_equal(w2$x$points$value, w3$x$points$value)
  expect_equal(before, after)
})

test_that("violin validates groups like its distribution siblings", {
  expect_error(pv_violin(pv_fiscal, "resource_per_capita",
                         group = "municipality"), "Other")
  expect_error(pv_violin(pv_fiscal, "municipality", group = "year"),
               "numeric")
  tiny <- data.frame(v = c(1, 2, 3), g = c("a", "a", "b"))
  expect_error(pv_violin(tiny, "v", group = "g"), "at least 2 values")
  allna <- data.frame(v = c(NA_real_, NA_real_), g = c("a", "b"))
  expect_error(pv_violin(allna, "v", group = "g"), "non-missing")
  # axis titles default to the mapped column names
  w <- pv_violin(pv_fiscal, "resource_per_capita", group = "year")
  expect_equal(w$x$xlab, "year")
  expect_equal(w$x$ylab, "resource_per_capita")
  w2 <- pv_violin(pv_fiscal, "resource_per_capita", group = "year",
                  xlab = NA, ylab = "CHF per resident")
  expect_null(w2$x$xlab)
  expect_equal(w2$x$ylab, "CHF per resident")
})

test_that("ridgeline orders groups by median, descending", {
  w <- expect_pvchart(
    pv_ridgeline(pv_fiscal, "resource_index", group = "year"), "ridgeline")
  expect_equal(length(w$x$ridges), 8)
  meds <- vapply(w$x$ridges, `[[`, numeric(1), "median")
  expect_true(!is.unsorted(rev(meds)))
  r <- w$x$ridges[[1]]
  expect_true(all(c("x", "y") %in% names(r$points)))
  expect_gt(nrow(r$points), 10)
  expect_equal(r$n, sum(pv_fiscal$year == as.integer(r$group)))
})

test_that("ridgeline validates group count and size", {
  expect_error(pv_ridgeline(pv_fiscal, "resource_index",
                            group = "municipality"), "Other")
  tiny <- data.frame(v = c(1, 2, 3), g = c("a", "a", "b"))
  expect_error(pv_ridgeline(tiny, "v", group = "g"), "at least 2 values")
  # all-missing values must error, not return a widget with zero ridges
  allna <- data.frame(v = c(NA_real_, NA_real_), g = c("a", "b"))
  expect_error(pv_ridgeline(allna, "v", group = "g"), "non-missing")
})
