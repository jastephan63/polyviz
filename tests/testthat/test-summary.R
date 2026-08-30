test_that("pv_summary counts correctly", {
  s <- pv_summary(airquality)
  expect_s3_class(s, "pv_summary")
  expect_equal(s$n_rows, 153)
  expect_equal(s$n_cols, 6)
  ozone <- s$columns[s$columns$variable == "Ozone", ]
  expect_equal(ozone$n_missing, 37)
  expect_equal(ozone$pct_missing, round(100 * 37 / 153, 1))
})

test_that("pv_summary carries SAS labels", {
  df <- pv_set_labels(mtcars, c(mpg = "Miles per gallon"))
  s <- pv_summary(df)
  expect_equal(s$columns$label[s$columns$variable == "mpg"],
               "Miles per gallon")
})

test_that("print method runs", {
  expect_output(print(pv_summary(mtcars)), "pv_summary")
})
