# pv_forecast: the forecast fan on single-series line charts. The
# statistics are pinned against the stats:: calls the roxygen documents
# (and against the written random-walk formula for the naive baseline);
# the drawing end goes through the same headless-Chrome pipeline as the
# other render tests.

# A regular monthly widget on the bundled electricity data - the
# canonical seasonal series most tests below share.
forecast_monthly <- function() {
  tot <- aggregate(gwh ~ date, pv_electricity, sum)
  list(data = tot, w = pv_line(tot, x = "date", y = "gwh"))
}

test_that("the payload carries points, nested bands, and the model spec", {
  m <- forecast_monthly()
  w <- pv_forecast(m$w, horizon = 24)
  fc <- w$x$forecast
  expect_identical(fc$method, "ets")
  expect_identical(fc$spec, "Holt-Winters additive, period 12")
  expect_equal(as.numeric(fc$levels), c(50, 80, 95))
  p <- fc$points
  expect_equal(nrow(p), 24)
  expect_identical(names(p),
                   c("x", "y", "lo50", "hi50", "lo80", "hi80",
                     "lo95", "hi95"))
  # The bands nest: each wider level brackets the narrower one, and the
  # point estimate sits inside them all.
  expect_true(all(p$lo95 <= p$lo80 & p$lo80 <= p$lo50))
  expect_true(all(p$hi50 <= p$hi80 & p$hi80 <= p$hi95))
  expect_true(all(p$lo50 <= p$y & p$y <= p$hi50))
  # Monthly dates continue month by month past the data, as ISO strings.
  expect_true(all(grepl("^\\d{4}-\\d{2}-\\d{2}$", p$x)))
  expect_identical(
    p$x, format(seq(max(m$data$date), by = "month",
                    length.out = 25)[-1], "%Y-%m-%d"))
})

test_that("levels are sorted ascending and a single level stays an array", {
  m <- forecast_monthly()
  w <- pv_forecast(m$w, 6, levels = c(95, 50))
  expect_equal(as.numeric(w$x$forecast$levels), c(50, 95))
  expect_identical(names(w$x$forecast$points),
                   c("x", "y", "lo50", "hi50", "lo95", "hi95"))
  # One level must still reach JavaScript as an array to loop over.
  w1 <- pv_forecast(forecast_monthly()$w, 3, levels = 90)
  expect_s3_class(w1$x$forecast$levels, "AsIs")
})

test_that("naive intervals match the random-walk formula exactly", {
  m <- forecast_monthly()
  w <- pv_forecast(m$w, horizon = 12, method = "naive")
  p <- w$x$forecast$points
  y <- m$data$gwh
  # Last value carried forward...
  expect_equal(p$y, rep(y[length(y)], 12))
  # ...with half-width qnorm((1 + level/100) / 2) * sd(diff(y)) * sqrt(h),
  # the formula the roxygen promises, at every level.
  sig <- stats::sd(diff(y))
  h <- sqrt(seq_len(12))
  for (lev in c(50, 80, 95)) {
    half <- stats::qnorm((1 + lev / 100) / 2) * sig * h
    expect_equal(p[[paste0("hi", lev)]] - p$y, half)
    expect_equal(p$y - p[[paste0("lo", lev)]], half)
  }
  expect_identical(w$x$forecast$spec,
                   "last value carried forward, random-walk intervals")
})

test_that("ets point forecasts match the documented HoltWinters call", {
  m <- forecast_monthly()
  w <- pv_forecast(m$w, horizon = 24)
  # The documented fit: seasonal HoltWinters with the defaults, retried
  # with start.periods = 3 when the optimiser fails (it does on this
  # series - which is exactly why the retry exists).
  fit <- tryCatch(
    stats::HoltWinters(stats::ts(m$data$gwh, frequency = 12)),
    error = function(e) stats::HoltWinters(
      stats::ts(m$data$gwh, frequency = 12), start.periods = 3))
  expect_equal(w$x$forecast$points$y,
               as.numeric(stats::predict(fit, n.ahead = 24)))
  # And the 95% band is predict()'s own prediction interval.
  pr <- stats::predict(fit, n.ahead = 24, prediction.interval = TRUE,
                       level = 0.95)
  expect_equal(w$x$forecast$points$hi95, as.numeric(pr[, "upr"]))
  expect_equal(w$x$forecast$points$lo95, as.numeric(pr[, "lwr"]))
})

test_that("a non-seasonal series gets the plain HoltWinters fit", {
  # Regular yearly data: no seasonal period to imply.
  set.seed(4)
  yearly <- data.frame(year = 2000:2024,
                       value = cumsum(rnorm(25, mean = 3)))
  w <- pv_line(yearly, "year", "value") |> pv_forecast(5)
  expect_identical(w$x$forecast$spec, "Holt-Winters, non-seasonal")
  fit <- stats::HoltWinters(stats::ts(yearly$value), gamma = FALSE)
  expect_equal(w$x$forecast$points$y,
               as.numeric(stats::predict(fit, n.ahead = 5)))
  # Numeric x extends by the series' own step.
  expect_equal(w$x$forecast$points$x, 2025:2029)
})

test_that("arima point forecasts match stats::arima at the chosen order", {
  m <- forecast_monthly()
  w <- pv_forecast(m$w, horizon = 12, method = "arima")
  ord <- as.integer(w$x$forecast$order)
  expect_length(ord, 3)
  expect_identical(w$x$forecast$spec,
                   sprintf("ARIMA(%d,%d,%d)", ord[1], ord[2], ord[3]))
  fit <- suppressWarnings(stats::arima(m$data$gwh, order = ord))
  pr <- stats::predict(fit, n.ahead = 12)
  p <- w$x$forecast$points
  expect_equal(p$y, as.numeric(pr$pred))
  # Intervals are the documented gaussian half-widths on predict()'s se.
  expect_equal(p$hi80 - p$y,
               stats::qnorm(0.9) * as.numeric(pr$se))
})

test_that("irregular spacing warns and proceeds on the median step", {
  pop <- subset(pv_city_population, city == "Luzern")
  expect_warning(
    w <- pv_line(pop, "year", "population") |> pv_forecast(3, "naive"),
    "irregularly spaced")
  # Census years step 10, 14, 40 years apart; the median step is 10.
  expect_equal(w$x$forecast$points$x, c(2034, 2044, 2054))
})

test_that("pv_forecast refuses the charts and arguments it must", {
  m <- forecast_monthly()
  monthly <- aggregate(revenue ~ month + region, pv_sales, sum)
  expect_error(
    pv_line(monthly, "month", "revenue", series = "region") |>
      pv_forecast(6),
    "Facet")
  expect_error(
    pv_line(aggregate(revenue ~ month, pv_sales, sum),
            "month", "revenue") |> pv_forecast(6),
    "category axis")
  expect_error(
    pv_bar(aggregate(revenue ~ region, pv_sales, sum),
           "region", "revenue") |> pv_forecast(6),
    "line")
  expect_error(pv_forecast(data.frame(x = 1), 6), "polyviz chart")
  expect_error(pv_forecast(m$w, 0), "horizon")
  expect_error(pv_forecast(m$w, 2.5), "horizon")
  expect_error(pv_forecast(m$w, c(2, 3)), "horizon")
  expect_error(pv_forecast(m$w, NA), "horizon")
  expect_error(pv_forecast(m$w, 6, method = "prophet"), "ets")
  expect_error(pv_forecast(m$w, 6, levels = 0), "levels")
  expect_error(pv_forecast(m$w, 6, levels = 100), "levels")
  expect_error(pv_forecast(m$w, 6, levels = c(50, 50)), "duplicate")
  expect_error(pv_forecast(m$w, 6, levels = numeric()), "levels")
  # One fan per chart.
  expect_error(pv_forecast(pv_forecast(m$w, 6), 6),
               "already carries a forecast")
  # Too short to fit anything honestly.
  tiny <- data.frame(x = 1:3, y = c(1, 2, 3))
  expect_error(pv_line(tiny, "x", "y") |> pv_forecast(2, "naive"),
               "at least 4")
})

# Stages a forecast chart the way pv_save() does (light mode, no
# entrance animation) and hands back the live Chrome session - the same
# staging test-widgets.R uses for its interaction tests.
forecast_page_session <- function(w, width = 700, height = 460) {
  w$x$mode <- "light"
  w$x$duration <- 0
  w$width <- NULL
  w$height <- NULL
  w$sizingPolicy$browser$fill <- TRUE
  w$sizingPolicy$browser$padding <- 0
  stage <- tempfile("pv-forecast-page-")
  dir.create(stage)
  page <- file.path(stage, "chart.html")
  htmlwidgets::saveWidget(w, page, selfcontained = FALSE, libdir = "lib")
  b <- chromote::ChromoteSession$new(width = width, height = height)
  errors <- polyviz:::export_watch_errors(b)
  loaded <- b$Page$loadEventFired(wait_ = FALSE)
  b$Page$navigate(utils::URLencode(paste0("file://", normalizePath(page))),
                  wait_ = FALSE)
  b$wait_for(loaded)
  polyviz:::export_wait_settled(b, errors, 0,
                                polyviz:::export_settle_count_js(w))
  list(b = b, errors = errors)
}

test_that("the crosshair over the fan reads estimate and intervals", {
  render_skip_if_no_chrome()
  m <- forecast_monthly()
  s <- forecast_page_session(pv_forecast(m$w, horizon = 24))
  withr::defer(try(s$b$close(), silent = TRUE))
  expect_identical(s$errors$msgs, character())
  res <- s$b$Runtime$evaluate("
    (function () {
      /* The plot svg is the big one (the download control also owns a
         tiny svg icon). */
      var svg = null;
      document.querySelectorAll('.pvchart svg').forEach(function (el) {
        if (!svg || el.clientWidth > svg.clientWidth) { svg = el; }
      });
      var fans = svg.querySelectorAll('path.pv-fan').length;
      var dashed = svg.querySelector('path.pv-fan-line');
      var r = svg.getBoundingClientRect();
      /* 90% across the plot is deep inside the projected two years. */
      var cx = r.left + r.width * 0.9, cy = r.top + r.height / 2;
      var target = document.elementFromPoint(cx, cy);
      target.dispatchEvent(new PointerEvent('pointermove',
        { clientX: cx, clientY: cy, bubbles: true }));
      return JSON.stringify({
        fans: fans,
        dash: dashed ? dashed.getAttribute('stroke-dasharray') : null,
        tip: document.querySelector('.pv-tooltip').innerHTML
      });
    })()", returnByValue = TRUE)$result$value
  got <- jsonlite::fromJSON(res)
  # One band per level, and the continuation really is dashed.
  expect_equal(got$fans, 3)
  expect_match(got$dash, "5,4", fixed = TRUE)
  # The tooltip reads the point estimate and all three intervals, at a
  # date beyond the observed data.
  expect_match(got$tip, "forecast: <b>")
  expect_match(got$tip, "50%:")
  expect_match(got$tip, "80%:")
  expect_match(got$tip, "95%:")
  expect_match(got$tip, "202[67]")
})

test_that("an SVG export carries the fan bands and the dashed line", {
  render_skip_if_no_chrome()
  m <- forecast_monthly()
  w <- pv_forecast(m$w, horizon = 24)
  path <- tempfile(fileext = ".svg")
  withr::defer(unlink(path))
  expect_no_warning(pv_save(w, path, quiet = TRUE))
  svg <- readChar(path, file.size(path), useBytes = TRUE)
  expect_equal(lengths(regmatches(svg,
    gregexpr('class="pv-fan"', svg, fixed = TRUE))), 3)
  expect_match(svg, 'class="pv-fan-line"', fixed = TRUE)
  expect_match(svg, 'class="pv-fan-rule"', fixed = TRUE)
})

test_that("forecasts render cleanly alongside zoom, trend, and annotation", {
  render_skip_if_no_chrome()
  set.seed(5)
  yearly <- data.frame(year = 1990:2024,
                       value = cumsum(rnorm(35, mean = 2)))
  w <- pv_line(yearly, "year", "value", zoom = TRUE) |>
    pv_trend("lm") |>
    pv_annotate(pv_vline(2020, label = "break")) |>
    pv_forecast(10, "arima")
  path <- tempfile(fileext = ".png")
  withr::defer(unlink(path))
  expect_no_warning(pv_save(w, path, quiet = TRUE))
  expect_gt(file.size(path), 20000)
})
