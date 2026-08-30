test_that("SAS xpt round trip preserves data and labels", {
  df <- data.frame(subj = 1:5, weight = c(70.2, 81.5, NA, 65.0, 90.1))
  df <- pv_set_labels(df, c(subj = "Subject ID", weight = "Weight (kg)"))
  path <- withr::local_tempfile(fileext = ".xpt")
  pv_write_sas(df, path)
  back <- pv_read_sas(path)
  expect_equal(as.numeric(back$weight), as.numeric(df$weight))
  expect_equal(unname(pv_labels(back)[["weight"]]), "Weight (kg)")
})

test_that("bundled demo xpt reads with labels", {
  demo <- system.file("extdata", "demo_sales.xpt", package = "polyviz")
  skip_if(demo == "", "demo file not built yet")
  df <- pv_read_sas(demo)
  expect_true(all(c("region", "product", "revenue") %in% names(df)))
  expect_equal(unname(pv_labels(df)[["revenue"]]), "Net revenue, EUR")
})

test_that("label helpers validate input", {
  expect_error(pv_set_labels(mtcars, c(nope = "x")), "Unknown columns")
  expect_true(all(is.na(pv_labels(mtcars))))
})

test_that("unsupported extensions error", {
  expect_error(pv_read_sas("file.sav"), "Unsupported")
  expect_error(pv_write_sas(mtcars, "file.parquet"), "Unsupported")
})
