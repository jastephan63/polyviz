# Payload-level tests for the pipe-able modifiers: annotations
# (R/modifiers.R), trends (R/trends.R), and crosstalk linking
# (R/crosstalk.R). Everything is checked on w$x - the payload the
# JavaScript layer reads - without rendering anything.

scatter_fixture <- function() {
  f25 <- subset(pv_fiscal, year == 2025)
  pv_scatter(f25, x = "resource_index", y = "equalization_chf")
}

# ---- annotation helpers ----

test_that("annotation helpers build the shapes pv.drawAnnotations reads", {
  h <- pv_hline(100, label = "average")
  expect_s3_class(h, "pv_annotation")
  expect_equal(unclass(h), list(type = "hline", at = 100, label = "average"))

  v <- pv_vline("Luzern", color = "#db444b")
  expect_equal(v$type, "vline")
  expect_equal(v$at, "Luzern")
  expect_equal(v$color, "#db444b")
  expect_null(v$label)

  b <- pv_band(y0 = 0, y1 = 50, label = "receiving zone")
  expect_equal(b$type, "band")
  expect_equal(b$y0, 0)
  expect_equal(b$y1, 50)
  expect_null(b$x0)

  n <- pv_note(3, 4, "look here", dx = 12)
  expect_equal(unclass(n), list(type = "label", x = 3, y = 4,
                                text = "look here", dx = 12, dy = 0))
})

test_that("annotation positions given as Dates become ISO strings", {
  expect_equal(pv_vline(as.Date("2021-06-15"))$at, "2021-06-15")
  b <- pv_band(x0 = as.Date("2020-01-01"), x1 = as.Date("2020-12-31"))
  expect_equal(b$x0, "2020-01-01")
  expect_equal(b$x1, "2020-12-31")
  expect_equal(pv_note(as.Date("2020-03-01"), 10, "lockdown")$x,
               "2020-03-01")
})

test_that("a band insists on exactly one complete axis pair", {
  expect_error(pv_band(), "exactly one axis pair")
  expect_error(pv_band(x0 = 1, x1 = 2, y0 = 3, y1 = 4),
               "exactly one axis pair")
  expect_error(pv_band(x0 = 1), "both `x0` and `x1`")
  expect_error(pv_band(y1 = 4), "both `y0` and `y1`")
})

test_that("helpers refuse vectors, missings, and empty text", {
  expect_error(pv_hline(c(1, 2)), "single non-missing")
  expect_error(pv_vline(NA), "single non-missing")
  expect_error(pv_hline(1, label = c("a", "b")), "single string")
  expect_error(pv_note(1, 2, ""), "non-empty")
  expect_error(pv_note(1, 2, "x", dx = "far"), "single number")
})

# ---- pv_annotate ----

test_that("pv_annotate attaches annotations and repeated calls append", {
  w <- scatter_fixture() |>
    pv_annotate(pv_vline(100, label = "cantonal average"),
                pv_band(x0 = 36, x1 = 100))
  expect_length(w$x$annotations, 2)
  expect_equal(w$x$annotations[[1]]$type, "vline")
  expect_equal(w$x$annotations[[2]]$type, "band")

  w <- pv_annotate(w, pv_note(65.9, 23278722, "Emmen"))
  expect_length(w$x$annotations, 3)
  expect_equal(w$x$annotations[[3]]$text, "Emmen")
  # The appended entries are plain lists, ready to serialise as JSON
  # objects.
  expect_false(inherits(w$x$annotations[[1]], "pv_annotation"))
})

test_that("pv_annotate works across the cartesian chart families", {
  agg <- aggregate(revenue ~ region, pv_sales, sum)
  w <- pv_bar(agg, "region", "revenue") |> pv_annotate(pv_hline(500000))
  expect_equal(w$x$annotations[[1]]$at, 500000)
  monthly <- aggregate(revenue ~ month, pv_sales, sum)
  w2 <- pv_line(monthly, "month", "revenue") |>
    pv_annotate(pv_hline(0))
  expect_length(w2$x$annotations, 1)
})

test_that("pv_annotate rejects non-cartesian charts and bad arguments", {
  expect_error(pv_annotate(pv_chord(pv_flows), pv_hline(1)),
               "x/y axes")
  expect_error(
    pv_annotate(pv_sunburst(pv_sales, c("region", "product"), "revenue"),
                pv_vline(1)),
    "sunburst")
  w <- scatter_fixture()
  expect_error(pv_annotate(w), "Nothing to annotate")
  expect_error(pv_annotate(w, list(type = "hline", at = 1)),
               "pv_hline\\(\\)")
  expect_error(pv_annotate(data.frame(x = 1), pv_hline(1)),
               "polyviz chart")
})

# ---- pv_trend ----

test_that("lm trend recovers a known slope over 80 monotone points", {
  set.seed(42)
  df <- data.frame(x = runif(60, 0, 10))
  df$y <- 2 * df$x + rnorm(60, sd = 0.1)
  w <- pv_scatter(df, "x", "y") |> pv_trend("lm")
  expect_length(w$x$trends, 1)
  tr <- w$x$trends[[1]]
  expect_equal(tr$slot, 2L)
  expect_false(tr$dash)
  p <- tr$points
  expect_equal(nrow(p), 80)
  expect_true(all(diff(p$x) > 0))
  slope <- (p$y[80] - p$y[1]) / (p$x[80] - p$x[1])
  expect_equal(slope, 2, tolerance = 0.05)
  # The ribbon brackets the fitted line.
  expect_true(all(c("lo", "hi") %in% names(p)))
  expect_true(all(p$lo <= p$y & p$y <= p$hi))
})

test_that("level = NA drops the ribbon columns entirely", {
  set.seed(1)
  df <- data.frame(x = 1:50, y = rnorm(50))
  w <- pv_scatter(df, "x", "y") |> pv_trend("lm", level = NA)
  expect_false(any(c("lo", "hi") %in% names(w$x$trends[[1]]$points)))
  w2 <- pv_scatter(df, "x", "y") |> pv_trend("loess", level = NA)
  expect_false(any(c("lo", "hi") %in% names(w2$x$trends[[1]]$points)))
})

test_that("a date line chart gets ISO string x back, in order", {
  daily <- aggregate(revenue ~ date, pv_sales, sum)
  w <- pv_line(daily, "date", "revenue") |> pv_trend("loess")
  p <- w$x$trends[[1]]$points
  expect_true(is.character(p$x))
  expect_true(all(grepl("^\\d{4}-\\d{2}-\\d{2}$", p$x)))
  # ISO date strings sort chronologically, so monotone means monotone.
  expect_false(is.unsorted(p$x, strictly = TRUE))
  expect_equal(p$x[1], format(min(daily$date), "%Y-%m-%d"))
})

test_that("repeated pv_trend calls append, each with its own slot", {
  set.seed(7)
  df <- data.frame(x = 1:40, y = rnorm(40))
  w <- pv_scatter(df, "x", "y") |>
    pv_trend("loess") |>
    pv_trend("lm", slot = 3)
  expect_length(w$x$trends, 2)
  expect_equal(w$x$trends[[1]]$slot, 2L)
  expect_equal(w$x$trends[[2]]$slot, 3L)
})

test_that("pv_trend refuses multi-series, category axes, and bad inputs", {
  monthly <- aggregate(revenue ~ month + region, pv_sales, sum)
  expect_error(
    pv_line(monthly, "month", "revenue", series = "region") |> pv_trend(),
    "Facet")
  mt <- mtcars
  mt$g <- rep(c("a", "b"), 16)
  expect_error(pv_scatter(mt, "wt", "mpg", color = "g") |> pv_trend(),
               "Facet")
  m1 <- aggregate(revenue ~ month, pv_sales, sum)
  expect_error(pv_line(m1, "month", "revenue") |> pv_trend(),
               "category axis")
  expect_error(pv_bar(aggregate(revenue ~ region, pv_sales, sum),
                      "region", "revenue") |> pv_trend(),
               "scatter or line")
  w <- scatter_fixture()
  expect_error(pv_trend(w, "splines"), "loess")
  expect_error(pv_trend(w, level = 2), "level")
  expect_error(pv_trend(w, slot = 99), "slot")
  expect_error(pv_trend(w, span = -1), "span")
  expect_error(pv_trend(data.frame(x = 1)), "polyviz chart")
  tiny <- data.frame(x = c(1, 2, 3), y = c(1, 2, 3))
  expect_error(pv_scatter(tiny, "x", "y") |> pv_trend("lm"),
               "at least 4")
})

# ---- pv_downloads ----

test_that("pv_downloads stores the flag the JavaScript side reads", {
  w <- scatter_fixture()
  # No flag in a fresh payload - the JavaScript default (enabled) rules.
  expect_null(w$x$downloads)
  expect_true(pv_downloads(w)$x$downloads)
  expect_false(pv_downloads(w, FALSE)$x$downloads)
  expect_equal(pv_downloads(w, "auto")$x$downloads, "auto")
  # A later call replaces the flag - the last word in a pipe wins.
  expect_false(pv_downloads(pv_downloads(w), FALSE)$x$downloads)
})

test_that("pv_downloads refuses bad flags and non-charts", {
  w <- scatter_fixture()
  expect_error(pv_downloads(w, "yes"), 'TRUE, FALSE, or "auto"')
  expect_error(pv_downloads(w, NA), 'TRUE, FALSE, or "auto"')
  expect_error(pv_downloads(w, c(TRUE, FALSE)), 'TRUE, FALSE, or "auto"')
  expect_error(pv_downloads(data.frame(x = 1)), "polyviz chart")
})

# ---- pv_textures ----

test_that("pv_textures stores the flag the JavaScript side reads", {
  mix <- aggregate(revenue ~ region + product, pv_sales, sum)
  w <- pv_bar(mix, x = "region", y = "revenue", series = "product",
              stack = "stack")
  # No flag in a fresh payload - the JavaScript default (solid fills)
  # rules, so every chart built before the flag existed stays identical.
  expect_null(w$x$textures)
  expect_true(pv_textures(w)$x$textures)
  expect_false(pv_textures(w, FALSE)$x$textures)
  # A later call replaces the flag - the last word in a pipe wins.
  expect_false(pv_textures(pv_textures(w), FALSE)$x$textures)
})

test_that("pv_textures attaches to every textured chart family", {
  agg <- aggregate(revenue ~ region, pv_sales, sum)
  expect_true(pv_textures(pv_bar(agg, "region", "revenue"))$x$textures)
  monthly <- aggregate(revenue ~ month + region, pv_sales, sum)
  expect_true(pv_textures(
    pv_area(monthly, "month", "revenue", series = "region"))$x$textures)
  expect_true(pv_textures(
    pv_donut(agg, category = "region", value = "revenue"))$x$textures)
  expect_true(pv_textures(
    pv_treemap(pv_sales, levels = c("region", "product"),
               value = "revenue"))$x$textures)
})

test_that("charts without filled series marks accept the flag quietly", {
  # Textures mean nothing on strokes, dots, or value ramps; the modifier
  # still attaches without complaint and the renderer ignores it.
  expect_true(pv_textures(scatter_fixture())$x$textures)
  monthly <- aggregate(revenue ~ month, pv_sales, sum)
  expect_true(pv_textures(
    pv_line(monthly, "month", "revenue"))$x$textures)
})

test_that("pv_textures refuses bad flags and non-charts", {
  w <- scatter_fixture()
  # A plain on/off switch: unlike the tri-state options, "auto" has no
  # data-driven decision to defer, so it is refused too.
  expect_error(pv_textures(w, "auto"), "TRUE or FALSE")
  expect_error(pv_textures(w, NA), "TRUE or FALSE")
  expect_error(pv_textures(w, c(TRUE, FALSE)), "TRUE or FALSE")
  expect_error(pv_textures(data.frame(x = 1)), "polyviz chart")
})

# ---- pv_link ----

test_that("pv_link attaches keys, group name, and the crosstalk libs", {
  f25 <- subset(pv_fiscal, year == 2025)
  sd <- crosstalk::SharedData$new(f25)
  w <- pv_scatter(f25, "resource_index", "equalization_chf") |>
    pv_link(sd)
  expect_equal(as.character(w$x$ctKeys), sd$key())
  expect_length(w$x$ctKeys, nrow(f25))
  expect_equal(w$x$ctGroup, sd$groupName())
  dep_names <- vapply(w$dependencies, function(d) d$name, character(1))
  expect_true("crosstalk" %in% dep_names)
})

test_that("pv_link matches force charts against their nodes", {
  sd <- crosstalk::SharedData$new(pv_network$nodes)
  w <- pv_force(pv_network$nodes, pv_network$links, group = "group") |>
    pv_link(sd)
  expect_length(w$x$ctKeys, nrow(pv_network$nodes))
  expect_equal(w$x$ctGroup, sd$groupName())
})

test_that("pv_link aborts on row mismatches and non-SharedData input", {
  f25 <- subset(pv_fiscal, year == 2025)
  w <- pv_scatter(f25, "resource_index", "equalization_chf")
  sd_short <- crosstalk::SharedData$new(f25[1:10, ])
  expect_error(pv_link(w, sd_short), "same number of rows")
  expect_error(pv_link(w, f25), "SharedData")
  expect_error(pv_link(f25, crosstalk::SharedData$new(f25)),
               "polyviz chart")
})
