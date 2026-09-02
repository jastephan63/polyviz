test_that("R profiling engine matches base R", {
  prof <- pv_profile(airquality, engine = "r")
  expect_s3_class(prof, "data.frame")
  expect_equal(attr(prof, "engine"), "r")
  ozone <- prof[prof$variable == "Ozone", ]
  expect_equal(ozone$n_missing, sum(is.na(airquality$Ozone)))
  expect_equal(ozone$mean, mean(airquality$Ozone, na.rm = TRUE))
  expect_equal(ozone$median, median(airquality$Ozone, na.rm = TRUE))
})

test_that("profile shape columns match hand-computed moments", {
  prof <- pv_profile(airquality, engine = "r")
  expect_true(all(c("skewness", "kurtosis", "jb_stat", "jb_p") %in%
                    names(prof)))
  # Recompute the moment estimators longhand for one column and check the
  # profile row against them.
  x <- airquality$Wind
  n <- length(x)
  m2 <- mean((x - mean(x))^2)
  skew <- mean((x - mean(x))^3) / m2^1.5
  kurt <- mean((x - mean(x))^4) / m2^2 - 3
  jb <- n / 6 * (skew^2 + kurt^2 / 4)
  wind <- prof[prof$variable == "Wind", ]
  expect_equal(wind$skewness, skew)
  expect_equal(wind$kurtosis, kurt)
  expect_equal(wind$jb_stat, jb)
  expect_equal(wind$jb_p, stats::pchisq(jb, df = 2, lower.tail = FALSE))
})

test_that("profile shape columns give known answers", {
  # A symmetric sample has zero skewness.
  sym <- pv_profile(data.frame(x = c(1, 2, 3, 5, 7, 8, 9)), engine = "r")
  expect_equal(sym$skewness, 0, tolerance = 1e-12)
  # A uniform sample is flat-topped: negative excess kurtosis.
  unif <- pv_profile(data.frame(x = seq(0, 1, length.out = 200)),
                     engine = "r")
  expect_lt(unif$kurtosis, 0)
  # Normal draws should not be rejected by the Jarque-Bera test.
  set.seed(42)
  norm <- pv_profile(data.frame(x = rnorm(500)), engine = "r")
  expect_gt(norm$jb_p, 0.05)
  # A heavily skewed sample should be rejected decisively.
  set.seed(42)
  skewed <- pv_profile(data.frame(x = rexp(500)), engine = "r")
  expect_gt(skewed$skewness, 1)
  expect_lt(skewed$jb_p, 0.001)
})

test_that("profile shape columns guard small n and zero variance", {
  edge <- data.frame(tiny = c(1, 2, 3, NA, NA),
                     flat = c(5, 5, 5, 5, 5))
  for (engine in c("r", if (pv_py_available()) "python")) {
    prof <- pv_profile(edge, engine = engine)
    # Three observed values is too few, and a constant column has no
    # shape; both must come back NA, not an error.
    for (col in c("skewness", "kurtosis", "jb_stat", "jb_p")) {
      expect_true(all(is.na(prof[[col]])))
    }
    # The classical summary columns are untouched by the guard.
    expect_equal(prof$mean, c(2, 5))
    expect_equal(prof$sd, c(1, 0))
  }
})

test_that("R outlier engines behave", {
  x <- c(rnorm(100), 50)
  flags <- pv_outliers(x, engine = "r")
  expect_true(flags[101])
  expect_length(flags, 101)
  expect_false(any(pv_outliers(c(NA, 1, 2, NA, 3), engine = "r")))
  expect_false(any(pv_outliers(rep(5, 10), method = "zscore", engine = "r")))
})

test_that("python engine agrees with R engine when available", {
  skip_if_not(pv_py_available(), "Python backend not available")
  prof_py <- pv_profile(airquality, engine = "python")
  prof_r <- pv_profile(airquality, engine = "r")
  expect_equal(attr(prof_py, "engine"), "python")
  attr(prof_py, "engine") <- attr(prof_r, "engine") <- NULL
  expect_equal(names(prof_py), names(prof_r))
  expect_equal(prof_py, prof_r, tolerance = 1e-10)
  # The shape columns come from different arithmetic on each side, so
  # check them by name as well as through the frame-wide comparison.
  for (col in c("skewness", "kurtosis", "jb_stat", "jb_p")) {
    expect_equal(prof_py[[col]], prof_r[[col]], tolerance = 1e-10)
  }

  x <- c(rnorm(200), -30, 30, NA)
  expect_identical(pv_outliers(x, engine = "python"),
                   pv_outliers(x, engine = "r"))
  expect_identical(pv_outliers(x, method = "zscore", engine = "python"),
                   pv_outliers(x, method = "zscore", engine = "r"))
})

test_that("engine resolution errors are clear", {
  expect_error(pv_profile(data.frame(a = letters)), "numeric")
})
