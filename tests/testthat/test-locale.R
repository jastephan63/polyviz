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

# The calendar's letter rows, read back in draw order: the twelve
# month initials along the top of the year block, then the weekday
# hints beside it. They are the only single-character <text> nodes in
# the plot, so filtering on length keeps exactly them.
calendar_letters_js <- paste0(
  "(function () {",
  " return Array.prototype.map.call(",
  "   document.querySelectorAll('.pvchart svg text'),",
  "   function (n) { return n.textContent; })",
  "   .filter(function (s) { return s.length === 1; }).join('');",
  " })()")

calendar_letters_df <- data.frame(
  date = as.Date(c("2024-01-15", "2024-04-02", "2024-07-19",
                   "2024-10-05", "2024-12-24")),
  reading = c(12, 18, 31, 14, 3))

test_that("a fr-CH calendar derives its letter rows from the locale", {
  render_skip_if_no_chrome()
  withr::defer(pv_locale(NULL))
  pv_locale("fr-CH")
  w <- pv_calendar(calendar_letters_df, date = "date", value = "reading")
  pv_locale(NULL)
  letters_seen <- locale_render_eval(w, calendar_letters_js)
  # The month row spells the initials of janvier through decembre -
  # which happen to be the same twelve letters as English - and then
  # the weekday hints give the locale away: lundi, mercredi, vendredi
  # read L, M, V where English shows M, W, F.
  expect_identical(letters_seen, "JFMAMJJASONDLMV")
})

test_that("an it-CH calendar swaps in the Italian month initials", {
  render_skip_if_no_chrome()
  withr::defer(pv_locale(NULL))
  pv_locale("it-CH")
  w <- pv_calendar(calendar_letters_df, date = "date", value = "reading")
  pv_locale(NULL)
  letters_seen <- locale_render_eval(w, calendar_letters_js)
  # Gennaio, Giugno, and Luglio break from the English row, so this is
  # the render that proves the initials are computed, not hardcoded.
  expect_identical(letters_seen, "GFMAMGLASONDLMV")
})

test_that("a de-CH race stamps its time label through the locale", {
  render_skip_if_no_chrome()
  withr::defer(pv_locale(NULL))
  df <- data.frame(
    stadt = rep(c("Luzern", "Zug", "Schwyz"), times = 3),
    monat = rep(as.Date(c("2024-01-01", "2024-02-01", "2024-03-01")),
                each = 3),
    besucher = seq_len(9) * 100)
  pv_locale("de-CH")
  w <- pv_race(df, time = "monat", id = "stadt", value = "besucher",
               top_n = 3)
  pv_locale(NULL)
  txt <- locale_render_eval(w, locale_text_js)
  # With no animation the race draws its final keyframe, so the big
  # corner readout shows the last time point - through de-CH's month
  # names: "Mar 2024" in English.
  expect_match(txt, "M\u00e4r 2024", fixed = TRUE)
  expect_false(grepl("Mar 2024", txt, fixed = TRUE))
})

test_that("without a locale the calendar keeps its English letter rows", {
  render_skip_if_no_chrome()
  w <- pv_calendar(calendar_letters_df, date = "date", value = "reading")
  letters_seen <- locale_render_eval(w, calendar_letters_js)
  expect_identical(letters_seen, "JFMAMJJASONDMWF")
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
