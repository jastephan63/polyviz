# pv_demo() wraps the Shiny gallery in inst/shiny/app.R. The widgets on
# its pages are the same constructors the chart tests and the render
# suite already prove, so these tests cover the wiring: the app object,
# the shiny guard, the theme/locale sandbox every build runs in, and one
# server-side round trip through shiny::testServer.

test_that("pv_demo(launch = FALSE) returns the app object", {
  skip_if_not_installed("shiny")
  app <- pv_demo(launch = FALSE)
  expect_s3_class(app, "shiny.appobj")
})

test_that("the app file ships and parses", {
  path <- system.file("shiny", "app.R", package = "polyviz")
  expect_true(file.exists(path))
  expect_no_error(parse(path))
})

test_that("a clear error names shiny when it is missing", {
  local_mocked_bindings(demo_has_shiny = function() FALSE)
  expect_error(pv_demo(launch = FALSE), "needs the shiny package")
})

test_that("launch must be TRUE or FALSE", {
  expect_error(pv_demo(launch = NA), "`launch` must be TRUE or FALSE")
  expect_error(pv_demo(launch = "yes"), "`launch` must be TRUE or FALSE")
})

test_that("demo builds run under the demo settings", {
  w <- polyviz:::demo_with_settings("paper", "de-CH", function() {
    pv_bar(aggregate(revenue ~ region, pv_sales, sum),
           x = "region", y = "revenue")
  })
  # the widget carries the paper theme's pure-white surface and the
  # requested locale
  expect_identical(w$x$theme$ink$light$surface,
                   pv_theme_paper()$ink$light$surface)
  expect_identical(w$x$locale$tag, "de-CH")
})

test_that("demo builds never leak their settings into the session", {
  # give the session its own settings first, as a user might have
  pv_locale("it-CH")
  suppressWarnings(pv_set_theme(colors = pv_palette(8), check = FALSE))
  on.exit({
    pv_reset_theme()
    pv_locale(NULL)
  })

  polyviz:::demo_with_settings("paper", "de-CH", function() {
    pv_bar(aggregate(revenue ~ region, pv_sales, sum),
           x = "region", y = "revenue")
  })

  # a widget built afterwards still wears the session's own settings
  after <- pv_bar(aggregate(revenue ~ region, pv_sales, sum),
                  x = "region", y = "revenue")
  expect_identical(after$x$locale$tag, "it-CH")
  expect_identical(after$x$theme$ink$light$surface,
                   pv_colors$ink$light$surface)

  # and a build that errors restores just the same
  expect_error(
    polyviz:::demo_with_settings("paper", "de-CH",
                                 function() stop("boom")),
    "boom")
  again <- pv_bar(aggregate(revenue ~ region, pv_sales, sum),
                  x = "region", y = "revenue")
  expect_identical(again$x$locale$tag, "it-CH")
})

test_that("unknown locale choices fall back to no locale", {
  w <- polyviz:::demo_with_settings("default", "none", function() {
    pv_bar(aggregate(revenue ~ region, pv_sales, sum),
           x = "region", y = "revenue")
  })
  expect_null(w$x$locale)
})

test_that("flipping the theme input re-renders a widget", {
  skip_if_not_installed("shiny")
  app <- pv_demo(launch = FALSE)
  shiny::testServer(app, {
    # before any inputs exist the render falls back to the defaults
    first <- output$cmp_bar
    expect_true(nzchar(first))

    session$setInputs(theme = "paper", locale = "de-CH")
    second <- output$cmp_bar
    expect_false(identical(first, second))
    # the rebuilt payload carries the paper surface and the locale
    expect_match(second, "#ffffff", fixed = TRUE)
    expect_match(second, "de-CH", fixed = TRUE)

    # and back again: the default look returns
    session$setInputs(theme = "default", locale = "none")
    third <- output$cmp_bar
    expect_match(third, "#fbf9f5", fixed = TRUE)
  })
})
