test_that("pv_palette returns n colours in fixed order", {
  expect_length(pv_palette(3), 3)
  expect_identical(pv_palette(3), pv_palette(8)[1:3])
  expect_identical(pv_palette(1, mode = "dark"),
                   pv_colors$categorical$dark[1])
})

test_that("pv_palette refuses more than 8 series", {
  expect_error(pv_palette(9), "Other")
  expect_error(pv_palette(0))
})

test_that("theme and scales build", {
  expect_s3_class(theme_polyviz(), "theme")
  expect_s3_class(scale_fill_pv(), "ScaleDiscrete")
})
