# pv_locale(): the R side stores a locale definition and pv_widget()
# attaches it to every payload built while it is set; the JavaScript
# side does the actual formatting. The payload plumbing is tested
# directly, and the formatting end-to-end - a real headless Chrome
# renders the chart and the axis text is read back out of the DOM, so
# these tests see exactly what a reader of the chart would.

test_that("pv_locale validates its argument", {
  expect_error(pv_locale("de-DE"), "de-CH")
  expect_error(pv_locale(c("de-CH", "fr-CH")), "one of")
  expect_error(pv_locale(1), "one of")
  expect_error(pv_locale(NA_character_), "one of")
})

test_that("the payload carries the locale while one is set, and resets", {
  withr::defer(pv_locale(NULL))
  sales <- aggregate(revenue ~ region, pv_sales, sum)

  before <- pv_bar(sales, "region", "revenue")
  expect_null(before$x$locale)

  pv_locale("de-CH")
  localised <- pv_bar(sales, "region", "revenue")
  expect_identical(localised$x$locale$tag, "de-CH")
  expect_identical(localised$x$locale$number$thousands, "'")
  expect_identical(localised$x$locale$number$decimal, ".")
  expect_identical(localised$x$locale$time$months[[3]], "M\u00e4rz")
  # d3.formatLocale needs `grouping` to arrive as a JSON array; a bare
  # scalar would be auto-unboxed and break it.
  expect_identical(localised$x$locale$number$grouping, list(3L))

  pv_locale(NULL)
  after <- pv_bar(sales, "region", "revenue")
  expect_null(after$x$locale)
  # Resetting restores byte-identical payloads - the locale field is
  # absent, not merely empty.
  expect_identical(after$x, before$x)
})

test_that("pv_locale returns the active definition invisibly", {
  withr::defer(pv_locale(NULL))
  out <- withVisible(pv_locale("it-CH"))
  expect_false(out$visible)
  expect_identical(out$value$tag, "it-CH")
  expect_identical(out$value$time$months[[1]], "Gennaio")
  reset <- withVisible(pv_locale(NULL))
  expect_false(reset$visible)
  expect_null(reset$value)
})

# Renders a widget in headless Chrome exactly the way pv_save() stages
# its captures (light mode, no entrance animation), waits until the
# chart has settled, asserts the page raised no JavaScript errors, and
# returns the value of one JavaScript expression evaluated on the live
# page.
locale_render_eval <- function(widget, js) {
  w <- widget
  w$x$mode <- "light"
  w$x$duration <- 0
  w$width <- NULL
  w$height <- NULL
  w$sizingPolicy$browser$fill <- TRUE
  w$sizingPolicy$browser$padding <- 0
  stage <- tempfile("pv-locale-")
  dir.create(stage)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  page <- file.path(stage, "chart.html")
  htmlwidgets::saveWidget(w, page, selfcontained = FALSE, libdir = "lib")

  b <- chromote::ChromoteSession$new(width = 700, height = 460)
  on.exit(try(b$close(), silent = TRUE), add = TRUE)
  errors <- polyviz:::export_watch_errors(b)
  loaded <- b$Page$loadEventFired(wait_ = FALSE)
  b$Page$navigate(utils::URLencode(paste0("file://", normalizePath(page))),
                  wait_ = FALSE)
  b$wait_for(loaded)
  polyviz:::export_wait_settled(b, errors, 0)
  expect_identical(errors$msgs, character())
  b$Runtime$evaluate(js, returnByValue = TRUE)$result$value
}

# Every piece of text in the rendered plot's svg, joined with pipes -
# axis ticks included - plus a probe of the d3.format default locale,
# which is what the renderers' own formatting calls (tooltips, value
# labels) go through.
locale_text_js <- paste0(
  "(function () {",
  " var t = Array.prototype.map.call(",
  "   document.querySelectorAll('.pvchart svg text'),",
  "   function (n) { return n.textContent; }).join('|');",
  " return t + '||' + d3.format(',.2~f')(12345.6);",
  " })()")

test_that("a de-CH bar chart draws apostrophe-grouped axis ticks", {
  render_skip_if_no_chrome()
  withr::defer(pv_locale(NULL))
  df <- data.frame(canton = c("LU", "ZG", "SZ", "OW"),
                   pendler = c(12000, 28000, 33000, 45000))
  pv_locale("de-CH")
  w <- pv_bar(df, "canton", "pendler")
  pv_locale(NULL)
  txt <- locale_render_eval(w, locale_text_js)
  # Swiss grouping on the y axis, and no US-style output anywhere.
  expect_match(txt, "10'000", fixed = TRUE)
  expect_match(txt, "40'000", fixed = TRUE)
  expect_false(grepl("10,000", txt, fixed = TRUE))
  expect_false(grepl("10k", txt, fixed = TRUE))
  # The default d3.format the tooltips route through groups the same way.
  expect_match(txt, "12'345.6", fixed = TRUE)
})

test_that("a fr-CH line chart labels its months in French", {
  render_skip_if_no_chrome()
  withr::defer(pv_locale(NULL))
  df <- data.frame(
    mois = seq(as.Date("2024-01-01"), by = "month", length.out = 12),
    visiteurs = c(210, 190, 240, 260, 310, 340,
                  380, 360, 300, 270, 230, 220))
  pv_locale("fr-CH")
  w <- pv_line(df, "mois", "visiteurs")
  pv_locale(NULL)
  txt <- locale_render_eval(w, locale_text_js)
  expect_match(txt, "f\u00e9vrier", fixed = TRUE)
  expect_match(txt, "ao\u00fbt", fixed = TRUE)
  expect_false(grepl("February", txt, fixed = TRUE))
  # And the number side of fr-CH: decimal comma, non-breaking space
  # grouping.
  expect_match(txt, "12\u00a0345,6", fixed = TRUE)
})

test_that("without a locale the stock US-style output is untouched", {
  render_skip_if_no_chrome()
  df <- data.frame(canton = c("LU", "ZG", "SZ", "OW"),
                   pendler = c(12000, 28000, 33000, 45000))
  w <- pv_bar(df, "canton", "pendler")
  txt <- locale_render_eval(w, locale_text_js)
  # Exactly today's ticks: compact above 10k, never grouped.
  expect_match(txt, "10k", fixed = TRUE)
  expect_false(grepl("10'000", txt, fixed = TRUE))
  expect_match(txt, "12,345.6", fixed = TRUE)
})
