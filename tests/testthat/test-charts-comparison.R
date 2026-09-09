# The comparison family: slope, dumbbell, waterfall, bullet. These tests
# cover the R side of each constructor - validation, the statistics
# computed before shipping, and the payload shape - plus a headless
# render, an SVG export, and a real hover per the family's contract.

expect_pvchart <- function(w, type) {
  expect_s3_class(w, "htmlwidget")
  expect_equal(attr(w, "package"), "polyviz")
  expect_equal(w$x$type, type)
  invisible(w)
}

# The two-moment city slice most slope tests draw from.
slope_pop <- function(cities = c("Luzern", "Emmen", "Kriens", "Zug")) {
  pv_city_population[pv_city_population$city %in% cities &
                       pv_city_population$year %in% c(1930, 2024), ]
}

# ---- slope -----------------------------------------------------------------

test_that("slope folds long rows into one row per group with both ends", {
  w <- expect_pvchart(
    pv_slope(slope_pop(), x = "year", y = "population", group = "city",
             highlight = "Zug"), "slope")
  expect_equal(sort(names(w$x$data)), c("group", "y1", "y2"))
  expect_equal(nrow(w$x$data), 4)
  expect_equal(w$x$xlevels, c("1930", "2024"))
  expect_equal(w$x$highlight, list("Zug"))
  # The ends land on the right sides: y1 is 1930, y2 is 2024.
  lu <- w$x$data[w$x$data$group == "Luzern", ]
  pop <- pv_city_population
  expect_equal(lu$y1, pop$population[pop$city == "Luzern" &
                                       pop$year == 1930])
  expect_equal(lu$y2, pop$population[pop$city == "Luzern" &
                                       pop$year == 2024])
})

test_that("slope insists on exactly two x positions, naming what it found", {
  err <- expect_error(
    pv_slope(pv_city_population[pv_city_population$city == "Luzern", ],
             x = "year", y = "population", group = "city"),
    "exactly two x positions")
  expect_match(conditionMessage(err), "`year` has 7")
  one <- slope_pop()[slope_pop()$year == 1930, ]
  expect_error(pv_slope(one, "year", "population", "city"),
               "has 1 \\(1930\\)")
})

test_that("slope orders numeric positions as numbers, not strings", {
  df <- data.frame(t = c(10, 9, 10, 9), v = c(4, 1, 6, 3),
                   g = c("a", "a", "b", "b"))
  w <- pv_slope(df, x = "t", y = "v", group = "g")
  expect_equal(w$x$xlevels, c("9", "10"))
  expect_equal(w$x$data$y1, c(1, 3))
  expect_equal(w$x$data$y2, c(4, 6))
})

test_that("slope refuses duplicate group/x rows and unknown highlights", {
  dup <- data.frame(t = c(1, 1, 2), v = 1:3, g = "a")
  expect_error(pv_slope(dup, "t", "v", "g"), "aggregate it first")
  ok <- data.frame(t = c(1, 2), v = 1:2, g = "a")
  expect_error(pv_slope(ok, "t", "v", "g", highlight = "nope"),
               "unknown group")
  expect_error(pv_slope(ok, "t", "v", "g", highlight = 1),
               "character vector")
})

test_that("slope drops half-present groups with a warning, refuses none left", {
  df <- data.frame(t = c(1, 2, 1), v = c(1, 2, 5),
                   g = c("both", "both", "only-start"))
  expect_warning(w <- pv_slope(df, "t", "v", "g"), "only-start")
  expect_equal(w$x$data$group, "both")
  lone <- data.frame(t = c(1, 2), v = 1:2, g = c("a", "b"))
  expect_error(suppressWarnings(pv_slope(lone, "t", "v", "g")),
               "both x positions")
})

test_that("slope drops missing rows with a warning and validates columns", {
  df <- data.frame(t = c(1, 2, 1, 2), v = c(1, 2, NA, 4),
                   g = c("a", "a", "b", "b"))
  # Losing its 1930 value costs group b its slope, so the missing-value
  # drop is followed by the half-present-group drop.
  expect_warning(expect_warning(pv_slope(df, "t", "v", "g"),
                                "missing `v`"),
                 "one of the two x positions")
  w <- suppressWarnings(pv_slope(df, "t", "v", "g"))
  expect_equal(w$x$data$group, "a")
  expect_error(pv_slope(df, "nope", "v", "g"), "not in `data`")
  chr <- data.frame(t = c(1, 2), v = c("x", "y"), g = "a")
  expect_error(pv_slope(chr, "t", "v", "g"), "not numeric")
})

test_that("slope axis titles default to none and honour overrides", {
  w <- pv_slope(slope_pop(), "year", "population", "city")
  expect_equal(w$x$xlab, "")
  expect_equal(w$x$ylab, "")
  w2 <- pv_slope(slope_pop(), "year", "population", "city",
                 xlab = "census year", ylab = "residents")
  expect_equal(w2$x$xlab, "census year")
  expect_equal(w2$x$ylab, "residents")
})

# ---- dumbbell --------------------------------------------------------------

# The first-vs-latest fiscal slice the dumbbell tests draw from.
dumbbell_fiscal <- function(n = 12) {
  f20 <- pv_fiscal[pv_fiscal$year == 2020,
                   c("municipality", "resource_index")]
  f27 <- pv_fiscal[pv_fiscal$year == 2027,
                   c("municipality", "resource_index")]
  both <- merge(f20, f27, by = "municipality",
                suffixes = c("_2020", "_2027"))
  head(both[order(-both$resource_index_2027), ], n)
}

test_that("dumbbell builds, defaults its labels, and sorts by gap", {
  top <- dumbbell_fiscal()
  w <- expect_pvchart(
    pv_dumbbell(top, y = "municipality", x1 = "resource_index_2020",
                x2 = "resource_index_2027"), "dumbbell")
  expect_equal(nrow(w$x$data), 12)
  expect_equal(w$x$labels, c("resource_index_2020", "resource_index_2027"))
  gaps <- w$x$data$x2 - w$x$data$x1
  expect_true(!is.unsorted(rev(gaps)))
  w2 <- pv_dumbbell(top, "municipality", "resource_index_2020",
                    "resource_index_2027", labels = c("2020", "2027"))
  expect_equal(w2$x$labels, c("2020", "2027"))
})

test_that("dumbbell sorts by either end or not at all", {
  df <- data.frame(m = c("a", "b", "c"), v1 = c(3, 1, 2), v2 = c(1, 9, 5))
  by1 <- pv_dumbbell(df, "m", "v1", "v2", sort = "x1")
  expect_equal(by1$x$data$y, c("a", "c", "b"))
  by2 <- pv_dumbbell(df, "m", "v1", "v2", sort = "x2")
  expect_equal(by2$x$data$y, c("b", "c", "a"))
  keep <- pv_dumbbell(df, "m", "v1", "v2", sort = FALSE)
  expect_equal(keep$x$data$y, c("a", "b", "c"))
  expect_error(pv_dumbbell(df, "m", "v1", "v2", sort = "up"),
               '"gap", "x1", "x2", or FALSE')
  expect_error(pv_dumbbell(df, "m", "v1", "v2", sort = TRUE),
               '"gap", "x1", "x2", or FALSE')
})

test_that("dumbbell validates labels, duplicates, caps, and missing rows", {
  df <- data.frame(m = c("a", "b"), v1 = c(1, 2), v2 = c(3, 4))
  expect_error(pv_dumbbell(df, "m", "v1", "v2", labels = "one"),
               "two non-empty strings")
  expect_error(pv_dumbbell(df, "m", "v1", "v2", labels = c("a", NA)),
               "two non-empty strings")
  dup <- data.frame(m = c("a", "a"), v1 = c(1, 2), v2 = c(3, 4))
  expect_error(pv_dumbbell(dup, "m", "v1", "v2"), "aggregate it first")
  many <- data.frame(m = as.character(1:41), v1 = 1:41, v2 = 2:42)
  expect_error(pv_dumbbell(many, "m", "v1", "v2"), "limit is 40")
  hole <- data.frame(m = c("a", "b"), v1 = c(1, NA), v2 = c(3, 4))
  expect_warning(w <- pv_dumbbell(hole, "m", "v1", "v2"), "missing `v1`")
  expect_equal(w$x$data$y, "a")
  chr <- data.frame(m = "a", v1 = "x", v2 = 1)
  expect_error(pv_dumbbell(chr, "m", "v1", "v2"), "not numeric")
})

# ---- waterfall -------------------------------------------------------------

# Lucerne's population change, period by period - the canonical
# waterfall on the bundled data.
waterfall_steps <- function() {
  lu <- pv_city_population[pv_city_population$city == "Luzern", ]
  lu <- lu[order(lu$year), ]
  list(steps = data.frame(
    period = paste(head(lu$year, -1), lu$year[-1], sep = "\u2013"),
    change = diff(lu$population)), start = lu$population[[1]])
}

test_that("waterfall computes the running levels and closes with a total", {
  wf <- waterfall_steps()
  w <- expect_pvchart(
    pv_waterfall(wf$steps, x = "period", y = "change", start = wf$start),
    "waterfall")
  df <- w$x$data
  # A non-zero start opens the sequence, the total closes it.
  expect_equal(df$kind[[1]], "start")
  expect_equal(df$kind[[nrow(df)]], "total")
  expect_equal(df$x[[nrow(df)]], "Total")
  deltas <- df[df$kind == "delta", ]
  # Every bar floats from where the one before it ended.
  expect_equal(deltas$y0, head(c(wf$start, deltas$y1), -1))
  expect_equal(deltas$y1 - deltas$y0, wf$steps$change)
  # The total is an amount from zero, equal to the final running level.
  expect_equal(df$y1[[nrow(df)]], wf$start + sum(wf$steps$change))
  expect_equal(df$y0[[nrow(df)]], 0)
})

test_that("waterfall start and total options behave", {
  df <- data.frame(step = c("a", "b"), v = c(5, -2))
  w <- pv_waterfall(df, "step", "v")
  # A zero start earns no start bar.
  expect_equal(w$x$data$kind, c("delta", "delta", "total"))
  expect_equal(w$x$data$y1, c(5, 3, 3))
  w2 <- pv_waterfall(df, "step", "v", total = FALSE)
  expect_equal(w2$x$data$kind, c("delta", "delta"))
  w3 <- pv_waterfall(df, "step", "v", total = "Bestand")
  expect_equal(w3$x$data$x[[3]], "Bestand")
  expect_error(pv_waterfall(df, "step", "v", total = NA),
               "TRUE, FALSE, or a single string")
  expect_error(pv_waterfall(df, "step", "v", start = Inf),
               "single finite number")
  expect_error(pv_waterfall(df, "step", "v", start = "0"),
               "single finite number")
})

test_that("waterfall refuses what it cannot draw", {
  inf <- data.frame(step = c("a", "b"), v = c(1, Inf))
  expect_error(pv_waterfall(inf, "step", "v"), "infinite")
  dup <- data.frame(step = c("a", "a"), v = c(1, 2))
  expect_error(pv_waterfall(dup, "step", "v"), "aggregate it first")
  chr <- data.frame(step = "a", v = "x")
  expect_error(pv_waterfall(chr, "step", "v"), "not numeric")
  hole <- data.frame(step = c("a", "b"), v = c(1, NA))
  expect_warning(w <- pv_waterfall(hole, "step", "v"), "missing `v`")
  expect_equal(nrow(w$x$data), 2)  # the surviving delta plus the total
})

test_that("waterfall value_labels flag validates and rides in the payload", {
  df <- data.frame(step = c("a", "b"), v = c(1, 2))
  expect_equal(pv_waterfall(df, "step", "v")$x$valueLabels, "auto")
  expect_true(pv_waterfall(df, "step", "v",
                           value_labels = TRUE)$x$valueLabels)
  expect_error(pv_waterfall(df, "step", "v", value_labels = "yes"),
               'TRUE, FALSE, or "auto"')
})

# ---- bullet ----------------------------------------------------------------

# Revenue against a synthesised target - the canonical bullet on the
# bundled sales data.
bullet_sales <- function() {
  rev <- aggregate(revenue ~ region, pv_sales, sum)
  rev$target <- round(1.08 * mean(rev$revenue), -4)
  rev
}

test_that("bullet builds with shared numeric bands, sorted ascending", {
  rev <- bullet_sales()
  w <- expect_pvchart(
    pv_bullet(rev, label = "region", value = "revenue", target = "target",
              bands = c(3e6, 1e6, 2e6)), "bullet")
  expect_equal(w$x$bands, c(1e6, 2e6, 3e6))
  expect_equal(w$x$bandCount, 3)
  expect_true(w$x$shared)
  expect_equal(w$x$vlab, "revenue")
  expect_equal(w$x$tlab, "target")
  expect_equal(sort(names(w$x$data)), c("label", "target", "value"))
})

test_that("bullet reads per-row bands from columns and sorts each row", {
  df <- data.frame(m = c("a", "b"), v = c(5, 6), t = c(7, 8),
                   lo = c(9, 2), hi = c(4, 11))
  w <- pv_bullet(df, "m", "v", "t", bands = c("lo", "hi"))
  expect_null(w$x$bands)
  expect_equal(w$x$bandCount, 2)
  # Row "a" arrived with its thresholds reversed; they ship ascending.
  expect_equal(w$x$data$b1, c(4, 2))
  expect_equal(w$x$data$b2, c(9, 11))
})

test_that("bullet validates bands, signs, duplicates, and the row cap", {
  df <- data.frame(m = c("a", "b"), v = c(1, 2), t = c(2, 3))
  expect_error(pv_bullet(df, "m", "v", "t", bands = 1:4), "at most 3")
  expect_error(pv_bullet(df, "m", "v", "t", bands = c(-1, 2)),
               "non-negative")
  expect_error(pv_bullet(df, "m", "v", "t", bands = TRUE),
               "numeric thresholds")
  expect_error(pv_bullet(df, "m", "v", "t", shared = "auto"),
               "TRUE or FALSE")
  neg <- data.frame(m = "a", v = -1, t = 2)
  expect_error(pv_bullet(neg, "m", "v", "t"), "grows from zero")
  negt <- data.frame(m = "a", v = 1, t = -2)
  expect_error(pv_bullet(negt, "m", "v", "t"), "zero-based")
  dup <- data.frame(m = c("a", "a"), v = c(1, 2), t = c(2, 3))
  expect_error(pv_bullet(dup, "m", "v", "t"), "aggregate it first")
  many <- data.frame(m = as.character(1:16), v = 1:16, t = 1:16)
  expect_error(pv_bullet(many, "m", "v", "t"), "limit is 15")
})

test_that("bullet drops missing rows with a warning, band columns included", {
  df <- data.frame(m = c("a", "b"), v = c(1, NA), t = c(2, 3))
  expect_warning(w <- pv_bullet(df, "m", "v", "t"), "missing `v`")
  expect_equal(w$x$data$label, "a")
  bf <- data.frame(m = c("a", "b"), v = c(1, 2), t = c(2, 3),
                   b = c(4, NA))
  expect_warning(w2 <- pv_bullet(bf, "m", "v", "t", bands = "b"),
                 "missing `b`")
  expect_equal(w2$x$data$label, "a")
})

# ---- family contract: alt text, rendering, export, hover -------------------

# One canonical widget per comparison type, on the bundled Swiss data.
comparison_charts <- function() {
  wf <- waterfall_steps()
  top <- dumbbell_fiscal()
  list(
    slope = pv_slope(slope_pop(), x = "year", y = "population",
                     group = "city", highlight = "Zug",
                     title = "A century of urban growth"),
    dumbbell = pv_dumbbell(top, y = "municipality",
                           x1 = "resource_index_2020",
                           x2 = "resource_index_2027",
                           labels = c("2020", "2027"),
                           title = "Tax strength, first vs latest year"),
    waterfall = pv_waterfall(wf$steps, x = "period", y = "change",
                             start = wf$start,
                             title = "How Lucerne's population moved"),
    bullet = pv_bullet(bullet_sales(), label = "region",
                       value = "revenue", target = "target",
                       bands = round(max(bullet_sales()$revenue) *
                                       c(0.5, 0.8, 1.1), -4),
                       title = "Revenue against target")
  )
}

test_that("every comparison chart carries generated alt text", {
  for (w in comparison_charts()) {
    txt <- pv_alt_text(w)
    expect_true(is.character(txt) && length(txt) == 1 && nzchar(txt))
    expect_identical(w$x$alt, txt)
  }
})

test_that("the standalone HTML export carries the comparison renderers", {
  # The R-only export path: no browser needed, and the bundle must name
  # each renderer or the page could never draw it.
  f <- file.path(withr::local_tempdir(), "slope.html")
  pv_save(comparison_charts()$slope, f)
  html <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"),
                collapse = "\n")
  for (type in c("slope", "dumbbell", "waterfall", "bullet")) {
    expect_match(html, paste0("pvRenderers.", type), fixed = TRUE)
  }
})

test_that("slope renders without JavaScript errors", {
  render_skip_if_no_chrome()
  path <- tempfile(fileext = ".png")
  expect_no_warning(pv_save(comparison_charts()$slope, path, quiet = TRUE))
  expect_gt(file.size(path), 20000)
  unlink(path)
})

test_that("dumbbell renders without JavaScript errors", {
  render_skip_if_no_chrome()
  path <- tempfile(fileext = ".png")
  expect_no_warning(pv_save(comparison_charts()$dumbbell, path,
                            quiet = TRUE))
  expect_gt(file.size(path), 20000)
  unlink(path)
})

test_that("waterfall renders without JavaScript errors", {
  render_skip_if_no_chrome()
  path <- tempfile(fileext = ".png")
  expect_no_warning(pv_save(comparison_charts()$waterfall, path,
                            quiet = TRUE))
  expect_gt(file.size(path), 20000)
  unlink(path)
})

test_that("bullet renders without JavaScript errors", {
  render_skip_if_no_chrome()
  path <- tempfile(fileext = ".png")
  expect_no_warning(pv_save(comparison_charts()$bullet, path,
                            quiet = TRUE))
  expect_gt(file.size(path), 20000)
  unlink(path)
})

test_that("comparison option variants render without JavaScript errors", {
  render_skip_if_no_chrome()
  wf <- waterfall_steps()
  rev <- bullet_sales()
  rev$lo <- round(0.6 * rev$revenue)
  rev$hi <- round(1.2 * rev$revenue)
  variants <- list(
    # A zero start, no total, forced labels: the other waterfall paths.
    pv_waterfall(wf$steps, x = "period", y = "change", total = FALSE,
                 value_labels = TRUE),
    # Textured waterfall bars: the greyscale-safe hatch per role.
    pv_waterfall(wf$steps, x = "period", y = "change",
                 start = wf$start) |> pv_textures(),
    # Per-row scales, per-row bands: every row draws its own axis.
    pv_bullet(rev, "region", "revenue", "target",
              bands = c("lo", "hi"), shared = FALSE),
    # A slope with no highlight and suppressed-by-default titles.
    pv_slope(slope_pop(), x = "year", y = "population", group = "city"),
    # An unsorted dumbbell with custom dot names.
    pv_dumbbell(dumbbell_fiscal(8), y = "municipality",
                x1 = "resource_index_2020", x2 = "resource_index_2027",
                labels = c("2020", "2027"), sort = FALSE)
  )
  for (w in variants) {
    path <- tempfile(fileext = ".png")
    expect_no_warning(pv_save(w, path, quiet = TRUE))
    expect_gt(file.size(path), 20000)
    unlink(path)
  }
})

test_that("each comparison chart exports a standalone SVG", {
  render_skip_if_no_chrome()
  charts <- comparison_charts()
  dir <- withr::local_tempdir()
  for (id in names(charts)) {
    f <- file.path(dir, paste0(id, ".svg"))
    expect_no_warning(pv_save(charts[[id]], f, quiet = TRUE))
    svg <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"),
                 collapse = "\n")
    expect_match(svg, "^<\\?xml")
    expect_match(svg, "<svg", fixed = TRUE)
    expect_match(svg, charts[[id]]$x$title, fixed = TRUE)
  }
})

# Stages a widget the way pv_save() does (light mode, no entrance
# animation, filling a page opened at a fixed size) and hands back the
# live Chrome session, for the hover checks below. The caller closes
# the session.
comparison_page_session <- function(w, width = 700, height = 460) {
  w$x$mode <- "light"
  w$x$duration <- 0
  w$width <- NULL
  w$height <- NULL
  w$sizingPolicy$browser$fill <- TRUE
  w$sizingPolicy$browser$padding <- 0
  stage <- tempfile("pv-comparison-page-")
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

test_that("hovering a slope line shows both values and the change", {
  render_skip_if_no_chrome()
  s <- comparison_page_session(comparison_charts()$slope)
  withr::defer(try(s$b$close(), silent = TRUE))
  expect_identical(s$errors$msgs, character())
  res <- s$b$Runtime$evaluate("
    (function () {
      /* Each slope row's last line is its invisible fat hover twin;
         poke the pointer at its midpoint and read the tooltip. */
      var rows = document.querySelectorAll('g.sl');
      if (!rows.length) return 'no rows';
      var lines = rows[0].querySelectorAll('line');
      var over = lines[lines.length - 1];
      var r = over.getBoundingClientRect();
      var cx = r.left + r.width / 2, cy = r.top + r.height / 2;
      over.dispatchEvent(new PointerEvent('pointerenter',
        { clientX: cx, clientY: cy, bubbles: true }));
      over.dispatchEvent(new PointerEvent('pointermove',
        { clientX: cx, clientY: cy, bubbles: true }));
      var tip = document.querySelector('.pv-tooltip');
      return JSON.stringify({ opacity: tip.style.opacity,
        html: tip.innerHTML, rows: rows.length });
    })()", returnByValue = TRUE)$result$value
  got <- jsonlite::fromJSON(res)
  expect_equal(got$rows, 4)
  expect_equal(got$opacity, "1")
  expect_match(got$html, "1930: <b>")
  expect_match(got$html, "2024: <b>")
  expect_match(got$html, "change: <b>[+\u2212-]")
})

test_that("hovering a waterfall bar reports its running total", {
  render_skip_if_no_chrome()
  s <- comparison_page_session(comparison_charts()$waterfall)
  withr::defer(try(s$b$close(), silent = TRUE))
  expect_identical(s$errors$msgs, character())
  res <- s$b$Runtime$evaluate("
    (function () {
      var bars = document.querySelectorAll('rect.bar');
      if (!bars.length) return 'no bars';
      /* The second bar is the first real contribution (the first is
         the start amount). */
      var bar = bars[1];
      var r = bar.getBoundingClientRect();
      var cx = r.left + r.width / 2, cy = r.top + r.height / 2;
      bar.dispatchEvent(new PointerEvent('pointerenter',
        { clientX: cx, clientY: cy, bubbles: true }));
      bar.dispatchEvent(new PointerEvent('pointermove',
        { clientX: cx, clientY: cy, bubbles: true }));
      var tip = document.querySelector('.pv-tooltip');
      return JSON.stringify({ opacity: tip.style.opacity,
        html: tip.innerHTML, bars: bars.length });
    })()", returnByValue = TRUE)$result$value
  got <- jsonlite::fromJSON(res)
  # Six periods plus the start and total bars.
  expect_equal(got$bars, 8)
  expect_equal(got$opacity, "1")
  expect_match(got$html, "contribution")
  expect_match(got$html, "running total: <b>")
})
