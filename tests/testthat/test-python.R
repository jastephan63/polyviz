test_that("R profiling engine matches base R", {
  prof <- pv_profile(airquality, engine = "r")
  expect_s3_class(prof, "data.frame")
  expect_equal(attr(prof, "engine"), "r")
  ozone <- prof[prof$variable == "Ozone", ]
  expect_equal(ozone$n_missing, sum(is.na(airquality$Ozone)))
  expect_equal(ozone$mean, mean(airquality$Ozone, na.rm = TRUE))
  expect_equal(ozone$median, median(airquality$Ozone, na.rm = TRUE))
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
  expect_equal(prof_py, prof_r, tolerance = 1e-10)

  x <- c(rnorm(200), -30, 30, NA)
  expect_identical(pv_outliers(x, engine = "python"),
                   pv_outliers(x, engine = "r"))
  expect_identical(pv_outliers(x, method = "zscore", engine = "python"),
                   pv_outliers(x, method = "zscore", engine = "r"))
})

test_that("engine resolution errors are clear", {
  expect_error(pv_profile(data.frame(a = letters)), "numeric")
})
