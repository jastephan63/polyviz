# The time-series tools (R/timeseries.R): pv_decompose(), which rides
# pv_line() + pv_facet(), and pv_changepoints(), which rides
# pv_annotate(). The statistics are verified against what they claim -
# components that re-sum to the observed series, planted level shifts
# recovered exactly, noise left alone - and the payloads against the
# shapes the existing renderers read.

# Same helper as test-widgets.R - each test file stands alone.
expect_pvchart <- function(w, type) {
  expect_s3_class(w, "htmlwidget")
  expect_equal(attr(w, "package"), "polyviz")
  expect_equal(w$x$type, type)
  invisible(w)
}

# Six years of synthetic monthly data with known parts: a gentle linear
# trend, a fixed seasonal shape, and small seeded noise.
monthly_fixture <- function() {
  months <- seq(as.Date("2019-01-01"), by = "month", length.out = 72)
  set.seed(11)
  data.frame(
    date = months,
    value = 100 + 0.5 * seq_len(72) +
      8 * sin(2 * pi * seq_len(72) / 12) + rnorm(72, sd = 1))
}

# Ninety points with level shifts planted after points 30 and 60. Seed 2
# is a representative draw where BIC keeps exactly the planted shifts
# (it does so on about 5 draws in 6 - the docs say as much).
shifted_fixture <- function() {
  set.seed(2)
  data.frame(t = 1:90,
             v = c(rnorm(30, 0, 0.5), rnorm(30, 3, 0.5),
                   rnorm(30, 1.2, 0.5)))
}

panel_named <- function(w, name) {
  Filter(function(p) p$name == name, w$x$panels)[[1]]$data
}

# ---- pv_decompose: payload shape ----

test_that("decompose builds a one-column facet of four aligned line panels", {
  w <- pv_decompose(monthly_fixture(), x = "date", y = "value",
                    title = "Parts of a series")
  expect_pvchart(w, "facet")
  expect_equal(w$x$subtype, "line")
  expect_equal(w$x$ncol, 1L)
  expect_equal(vapply(w$x$panels, `[[`, character(1), "name"),
               c("Observed", "Trend", "Seasonal", "Remainder"))
  # share_y = FALSE is the decompose convention: the seasonal swing and
  # the remainder are a fraction of the observed level, and one shared
  # scale would flatten them. The x limits ARE shared - that is what
  # keeps features vertically aligned.
  expect_null(w$x$ylim)
  expect_equal(w$x$xlim, c("2019-01-01", "2024-12-01"))
  # The zero reference line rides the annotation machinery once, at the
  # top level, and the facet renderer clones it into every panel.
  expect_length(w$x$annotations, 1)
  expect_equal(w$x$annotations[[1]]$type, "hline")
  expect_equal(w$x$annotations[[1]]$at, 0)
  # Panels label themselves; no legend row.
  expect_false(w$x$legend)
  # Method and frequency travel in the payload for anyone downstream.
  expect_equal(w$x$decompose, list(method = "stl", frequency = 12L))
  # The y title falls back to the caller's column name; the x title is
  # dropped on a date axis (each panel would repeat it under its own
  # date ticks) but kept for a numeric axis, whose index needs naming.
  # The raised default height gives four stacked panels their room.
  expect_equal(w$x$xlab, "")
  expect_equal(w$x$ylab, "value")
  expect_equal(w$height, 720)
  num <- data.frame(t = 1:40, value = rep(c(1, 5), 20))
  wn <- pv_decompose(num, "t", "value", frequency = 2)
  expect_equal(wn$x$xlab, "t")
})

test_that("decompose alt text says what the chart is, in facts", {
  w <- pv_decompose(monthly_fixture(), x = "date", y = "value")
  expect_match(w$x$alt, "seasonal decomposition")
  expect_match(w$x$alt, "stl, frequency 12")
  expect_match(w$x$alt, "observed, trend, seasonal, remainder")
})

# ---- pv_decompose: the statistics ----

test_that("stl components re-sum to the observed series everywhere", {
  w <- pv_decompose(monthly_fixture(), x = "date", y = "value")
  obs <- panel_named(w, "Observed")
  tr <- panel_named(w, "Trend")
  se <- panel_named(w, "Seasonal")
  re <- panel_named(w, "Remainder")
  # stl defines its trend to both ends, so all four panels are full
  # length and line up row for row.
  expect_equal(nrow(obs), 72)
  expect_equal(nrow(tr), 72)
  expect_equal(tr$x, obs$x)
  expect_lt(max(abs(obs$y - (tr$y + se$y + re$y))), 1e-8)
  # And the parts found should resemble the parts planted: the trend
  # rises about half a unit per month, the seasonal swing is about 8.
  expect_gt(stats::cor(tr$y, seq_len(72)), 0.99)
  expect_equal(max(se$y), 8, tolerance = 0.15)
})

test_that("classical components re-sum exactly and lose the trend edges", {
  w <- pv_decompose(monthly_fixture(), x = "date", y = "value",
                    method = "classical")
  expect_equal(w$x$decompose$method, "classical")
  obs <- panel_named(w, "Observed")
  tr <- panel_named(w, "Trend")
  re <- panel_named(w, "Remainder")
  # The centred moving average has no value in the first and last half
  # cycle: 6 points gone from each end of trend and remainder.
  expect_equal(nrow(obs), 72)
  expect_equal(nrow(tr), 72 - 12)
  expect_equal(nrow(re), 72 - 12)
  expect_equal(tr$x[1], obs$x[7])
  # Where the parts are defined, the identity is exact - the remainder
  # is literally observed minus seasonal minus trend.
  se <- panel_named(w, "Seasonal")
  m <- merge(merge(tr, re, by = "x", suffixes = c(".tr", ".re")),
             merge(obs, se, by = "x", suffixes = c(".obs", ".se")),
             by = "x")
  expect_equal(m$y.re, m$y.obs - m$y.se - m$y.tr, tolerance = 1e-12)
})

test_that("the classical seasonal shape repeats identically every cycle", {
  w <- pv_decompose(monthly_fixture(), x = "date", y = "value",
                    method = "classical")
  se <- panel_named(w, "Seasonal")$y
  expect_equal(se[1:60], se[13:72], tolerance = 1e-12)
})

# ---- pv_decompose: frequency inference ----

test_that("frequency is read off monthly, quarterly, and daily dates", {
  m <- pv_decompose(monthly_fixture(), "date", "value")
  expect_equal(m$x$decompose$frequency, 12L)

  q <- data.frame(
    date = seq(as.Date("2018-01-01"), by = "3 months", length.out = 24),
    value = rep(c(1, 4, 6, 2), 6) + seq_len(24) / 10)
  expect_equal(pv_decompose(q, "date", "value")$x$decompose$frequency, 4L)

  d <- data.frame(
    date = seq(as.Date("2025-01-06"), by = "day", length.out = 28),
    value = rep(c(5, 5, 5, 5, 5, 9, 9), 4) + seq_len(28) / 50)
  expect_equal(pv_decompose(d, "date", "value")$x$decompose$frequency, 7L)

  # Month-end dates wobble between 28 and 31 days apart but are still
  # plainly monthly.
  ends <- seq(as.Date("2019-02-01"), by = "month", length.out = 72) - 1
  me <- monthly_fixture()
  me$date <- ends
  expect_equal(pv_decompose(me, "date", "value")$x$decompose$frequency,
               12L)
})

test_that("unreadable spacing is refused plainly, never guessed", {
  irr <- data.frame(
    date = as.Date("2020-01-01") + cumsum(c(0, rep(c(20, 40), 15))),
    value = rnorm(31))
  expect_error(pv_decompose(irr, "date", "value"),
               "Could not infer a seasonal frequency")
  # Even with a frequency in hand, irregular spacing breaks the model's
  # one-observation-per-step assumption.
  expect_error(pv_decompose(irr, "date", "value", frequency = 12),
               "not evenly spaced")
  # Numbers carry no calendar; the caller must say the cycle length.
  num <- data.frame(t = 1:40, value = rnorm(40))
  expect_error(pv_decompose(num, "t", "value"), "no calendar")
  w <- pv_decompose(transform(num, value = rep(c(1, 5), 20)), "t",
                    "value", frequency = 2)
  expect_equal(w$x$decompose$frequency, 2L)
  # An explicit frequency wins over what the dates would have said.
  w6 <- pv_decompose(monthly_fixture(), "date", "value", frequency = 6)
  expect_equal(w6$x$decompose$frequency, 6L)
  expect_error(pv_decompose(num, "t", "value", frequency = 1.5),
               "whole number of at least 2")
})

# ---- pv_decompose: refusals and pass-through ----

test_that("incomplete, repeated, or too-short series abort plainly", {
  m <- monthly_fixture()
  m$value[10] <- NA
  expect_error(pv_decompose(m, "date", "value"), "complete series")
  m$value[10] <- Inf
  expect_error(pv_decompose(m, "date", "value"), "complete series")
  dup <- monthly_fixture()
  dup$date[2] <- dup$date[1]
  expect_error(pv_decompose(dup, "date", "value"), "repeated values")
  short <- monthly_fixture()[1:24, ]
  # stl needs one point beyond two full cycles; classical is content
  # with the two cycles themselves.
  expect_error(pv_decompose(short, "date", "value"), "at least 25")
  expect_pvchart(pv_decompose(short, "date", "value",
                              method = "classical"), "facet")
  cat_x <- data.frame(m = month.abb, value = rnorm(12))
  expect_error(pv_decompose(cat_x, "m", "value"), "dates or numbers")
  expect_error(pv_decompose(monthly_fixture(), "date", "value",
                            s_window = 8),
               "odd whole number")
  expect_error(pv_decompose(monthly_fixture(), "date", "value",
                            method = "loess"))
})

test_that("dots go to pv_line, minus the arguments decompose owns", {
  w <- pv_decompose(monthly_fixture(), "date", "value",
                    title = "T", subtitle = "S", xlab = "when",
                    ylab = "how much", height = 900)
  expect_equal(w$x$title, "T")
  expect_equal(w$x$subtitle, "S")
  expect_equal(w$x$xlab, "when")
  expect_equal(w$x$ylab, "how much")
  expect_equal(w$height, 900)
  expect_error(pv_decompose(monthly_fixture(), "date", "value",
                            series = "value"),
               "cannot be passed")
  # An unnamed straggler can only reach `...` once every named formal
  # before it is filled - and it is still refused there.
  expect_error(pv_decompose(monthly_fixture(), "date", "value", 12,
                            "stl", "periodic", "stray"),
               "must be named")
})

test_that("the same series always decomposes to the same widget", {
  a <- pv_decompose(monthly_fixture(), "date", "value")
  b <- pv_decompose(monthly_fixture(), "date", "value")
  expect_identical(a$x, b$x)
})

# ---- pv_changepoints: the statistics ----

test_that("two planted level shifts are found exactly, and marked", {
  w <- pv_line(shifted_fixture(), "t", "v") |> pv_changepoints()
  # The marks stand on the first point of each new level.
  expect_equal(w$x$changepoints$index, c(31L, 61L))
  expect_equal(w$x$changepoints$x, c("31", "61"))
  expect_length(w$x$annotations, 2)
  expect_equal(vapply(w$x$annotations, `[[`, character(1), "type"),
               c("vline", "vline"))
  expect_equal(vapply(w$x$annotations, `[[`, numeric(1), "at"),
               c(31, 61))
  expect_equal(vapply(w$x$annotations, `[[`, character(1), "label"),
               c("31", "61"))
  expect_match(w$x$alt, "binary segmentation with a BIC stopping rule")
  expect_match(w$x$alt, "2 sustained shifts")
})

test_that("pure noise earns no marks, a message, and an untouched payload", {
  set.seed(7)
  noise <- data.frame(t = 1:100, v = rnorm(100))
  w0 <- pv_line(noise, "t", "v")
  expect_message(w1 <- pv_changepoints(w0), "no sustained level shifts")
  expect_identical(w1$x, w0$x)
  expect_null(w1$x$annotations)
})

test_that("levels = TRUE adds one mean line per stretch", {
  df <- shifted_fixture()
  w <- pv_line(df, "t", "v") |> pv_changepoints(levels = TRUE)
  types <- vapply(w$x$annotations, `[[`, character(1), "type")
  expect_equal(types, c("vline", "vline", "hline", "hline", "hline"))
  hl <- vapply(w$x$annotations[types == "hline"], `[[`, numeric(1), "at")
  expect_equal(hl, c(mean(df$v[1:30]), mean(df$v[31:60]),
                     mean(df$v[61:90])))
  expect_equal(w$x$changepoints$mean, hl)
  expect_match(w$x$annotations[[3]]$label, "^mean ")
  expect_match(w$x$alt, "mean")
})

test_that("date axes ship epoch milliseconds the d3 time scale reads", {
  # The data rows carry ISO strings the renderer parses itself, but the
  # annotation values feed the scale directly, and a d3 time scale
  # coerces with +x - so a date mark travels as a number, keeping the
  # readable date in the label.
  df <- shifted_fixture()
  df$d <- as.Date("2024-01-01") + (df$t - 1)
  w <- pv_line(df, "d", "v") |> pv_changepoints()
  expect_equal(vapply(w$x$annotations, `[[`, numeric(1), "at"),
               as.numeric(as.Date(c("2024-01-31", "2024-03-01"))) *
                 86400000)
  expect_equal(vapply(w$x$annotations, `[[`, character(1), "label"),
               c("2024-01-31", "2024-03-01"))
  expect_equal(w$x$changepoints$x, c("2024-01-31", "2024-03-01"))
})

test_that("max_changes caps the search at the largest shifts", {
  set.seed(1)
  df <- data.frame(t = 1:100,
                   v = c(rnorm(25, 0, 0.4), rnorm(25, 4, 0.4),
                         rnorm(25, 1, 0.4), rnorm(25, 5, 0.4)))
  full <- pv_line(df, "t", "v") |> pv_changepoints()
  expect_equal(full$x$changepoints$index, c(26L, 51L, 76L))
  capped <- pv_line(df, "t", "v") |> pv_changepoints(max_changes = 2)
  # Greedy order: the two biggest jumps (into 4 and into 5) come first.
  expect_equal(capped$x$changepoints$index, c(26L, 76L))
})

test_that("marks append to annotations already on the chart", {
  w <- pv_line(shifted_fixture(), "t", "v") |>
    pv_annotate(pv_hline(0, label = "zero")) |>
    pv_changepoints()
  expect_length(w$x$annotations, 3)
  expect_equal(w$x$annotations[[1]]$label, "zero")
  expect_equal(w$x$annotations[[2]]$type, "vline")
})

test_that("the same chart always earns the same marks", {
  a <- pv_line(shifted_fixture(), "t", "v") |> pv_changepoints(levels = TRUE)
  b <- pv_line(shifted_fixture(), "t", "v") |> pv_changepoints(levels = TRUE)
  expect_identical(a$x, b$x)
})

# ---- pv_changepoints: refusals ----

test_that("only a single-series ordered line chart is accepted", {
  f25 <- subset(pv_fiscal, year == 2025)
  sc <- pv_scatter(f25, "resource_index", "equalization_chf")
  expect_error(pv_changepoints(sc), "line chart")
  pop <- subset(pv_city_population,
                city %in% c("Luzern", "Emmen", "Kriens", "Zug"))
  multi <- pv_line(pop, "year", "population", series = "city")
  expect_error(pv_changepoints(multi), "one series")
  cat_x <- data.frame(m = paste0("p", 1:30), v = rnorm(30))
  wc <- pv_line(cat_x, "m", "v")
  expect_error(pv_changepoints(wc), "category axis")
  few <- data.frame(t = 1:19, v = rnorm(19))
  expect_error(pv_changepoints(pv_line(few, "t", "v")),
               "at least 20 points")
  inf <- shifted_fixture()
  inf$v[5] <- Inf
  expect_error(pv_changepoints(pv_line(inf, "t", "v")), "finite")
  # A decomposed (faceted) widget is not a line chart any more.
  fac <- pv_decompose(monthly_fixture(), "date", "value")
  expect_error(pv_changepoints(fac), "facet chart")
  expect_error(pv_changepoints(data.frame(x = 1)), "polyviz chart")
})

test_that("option validation stays plain", {
  w <- pv_line(shifted_fixture(), "t", "v")
  expect_error(pv_changepoints(w, levels = "auto"),
               "`levels` must be TRUE or FALSE")
  expect_error(pv_changepoints(w, max_changes = 0), "from 1 to 5")
  expect_error(pv_changepoints(w, max_changes = 6), "from 1 to 5")
  expect_error(pv_changepoints(w, max_changes = 2.5), "from 1 to 5")
})

# ---- end to end through the real renderer ----

test_that("both tools render through the existing JavaScript unchanged", {
  render_skip_if_no_chrome()
  dir <- tempfile("pv-timeseries-")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  monthly <- aggregate(gwh ~ date, pv_electricity, sum)
  dec <- pv_decompose(monthly, "date", "gwh", title = "Decomposed")
  p1 <- file.path(dir, "decompose.png")
  expect_no_warning(pv_save(dec, p1, quiet = TRUE))
  expect_gt(file.size(p1), 20000)
  df <- shifted_fixture()
  df$d <- as.Date("2024-01-01") + (df$t - 1)
  cp <- pv_line(df, "d", "v", title = "Shifted") |>
    pv_changepoints(levels = TRUE)
  p2 <- file.path(dir, "changepoints.png")
  expect_no_warning(pv_save(cp, p2, quiet = TRUE))
  expect_gt(file.size(p2), 20000)
})
