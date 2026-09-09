expect_pvchart <- function(w, type) {
  expect_s3_class(w, "htmlwidget")
  expect_equal(attr(w, "package"), "polyviz")
  expect_equal(w$x$type, type)
  invisible(w)
}

test_that("bar widget builds and validates", {
  agg <- aggregate(revenue ~ region, pv_sales, sum)
  w <- expect_pvchart(pv_bar(agg, "region", "revenue"), "bar")
  expect_equal(nrow(w$x$data), 4)
  expect_error(pv_bar(agg, "nope", "revenue"), "not in `data`")
})

test_that("bar sorts single series when asked", {
  agg <- aggregate(revenue ~ region, pv_sales, sum)
  w <- pv_bar(agg, "region", "revenue", sort = TRUE)
  expect_true(!is.unsorted(rev(w$x$data$y)))
})

test_that("bar sends its auto flags through and validates them", {
  agg <- aggregate(revenue ~ region, pv_sales, sum)
  w <- pv_bar(agg, "region", "revenue")
  expect_equal(w$x$horizontal, "auto")
  expect_equal(w$x$valueLabels, "auto")
  w2 <- pv_bar(agg, "region", "revenue",
               horizontal = TRUE, value_labels = FALSE)
  expect_true(w2$x$horizontal)
  expect_false(w2$x$valueLabels)
  expect_error(pv_bar(agg, "region", "revenue", horizontal = "sideways"),
               'TRUE, FALSE, or "auto"')
  expect_error(pv_bar(agg, "region", "revenue", value_labels = 1),
               'TRUE, FALSE, or "auto"')
})

test_that("stacked bars carry cumulative offsets computed in R", {
  agg <- aggregate(revenue ~ region + product, pv_sales, sum)
  w <- pv_bar(agg, "region", "revenue", series = "product", stack = "stack")
  expect_equal(w$x$stack, "stack")
  d <- w$x$data
  expect_true(all(c("y0", "y1", "total") %in% names(d)))
  # Within every category the segments tile [0, total] without overlap.
  for (cat in unique(d$x)) {
    seg <- d[d$x == cat, ]
    expect_equal(seg$y0, c(0, head(seg$y1, -1)))
    expect_equal(max(seg$y1), seg$total[[1]])
    expect_equal(sum(seg$y), seg$total[[1]])
  }
  # Rows arrive in stacking order: categories then series, both by
  # first appearance, so palette slot one is always the bottom segment.
  expect_equal(unique(d$series), unique(as.character(agg$product)))
  expect_equal(unique(d$x), unique(as.character(agg$region)))
})

test_that("percent stacking normalises each category to 1", {
  agg <- aggregate(revenue ~ region + product, pv_sales, sum)
  w <- pv_bar(agg, "region", "revenue", series = "product",
              stack = "percent")
  d <- w$x$data
  expect_true("share" %in% names(d))
  shares <- tapply(d$share, d$x, sum)
  expect_true(all(abs(shares - 1) < 1e-9))
  expect_true(all(abs(tapply(d$y1, d$x, max) - 1) < 1e-9))
  # The exact values still travel for the tooltips.
  expect_equal(sort(d$y), sort(agg$revenue))
  # A percent axis explains itself: the default y title falls away,
  # an explicit one stays.
  expect_equal(w$x$ylab, "")
  w2 <- pv_bar(agg, "region", "revenue", series = "product",
               stack = "percent", ylab = "Share")
  expect_equal(w2$x$ylab, "Share")
})

test_that("stack validates its value and its requirements", {
  agg <- aggregate(revenue ~ region + product, pv_sales, sum)
  single <- aggregate(revenue ~ region, pv_sales, sum)
  # The default payload says "none" and carries no stack columns.
  w <- pv_bar(single, "region", "revenue")
  expect_equal(w$x$stack, "none")
  expect_false(any(c("y0", "y1") %in% names(w$x$data)))
  expect_error(pv_bar(agg, "region", "revenue", series = "product",
                      stack = "sideways"), "should be one of")
  expect_error(pv_bar(single, "region", "revenue", stack = "stack"),
               "needs a `series` mapping")
  neg <- agg
  neg$revenue[[1]] <- -1
  expect_error(pv_bar(neg, "region", "revenue", series = "product",
                      stack = "stack"),
               "negative values, which stacked bars cannot draw")
  # Grouped bars still draw negatives as before.
  expect_pvchart(pv_bar(neg, "region", "revenue", series = "product"),
                 "bar")
  # One row per series/category pair, stacked or not.
  dup <- rbind(agg, agg[1, ])
  expect_error(pv_bar(dup, "region", "revenue", series = "product",
                      stack = "stack"),
               "more than one row per series/category")
})

test_that("stacked bars may go horizontal, forced or by auto", {
  agg <- aggregate(revenue ~ region + product, pv_sales, sum)
  w <- pv_bar(agg, "region", "revenue", series = "product",
              stack = "stack", horizontal = TRUE)
  expect_true(w$x$horizontal)
  w2 <- pv_bar(agg, "region", "revenue", series = "product",
               stack = "percent")
  expect_equal(w2$x$horizontal, "auto")
})

test_that("stacked and percent bars render without JavaScript errors", {
  render_skip_if_no_chrome()
  agg <- aggregate(revenue ~ region + product, pv_sales, sum)
  for (mode in c("stack", "percent")) {
    for (horiz in c(FALSE, TRUE)) {
      w <- pv_bar(agg, "region", "revenue", series = "product",
                  stack = mode, horizontal = horiz, value_labels = TRUE)
      path <- tempfile(fileext = ".png")
      expect_no_warning(pv_save(w, path, quiet = TRUE))
      expect_gt(file.size(path), 20000)
      unlink(path)
    }
  }
})

test_that("horizontal = TRUE still refuses a series, but auto allows one", {
  agg <- aggregate(revenue ~ region + product, pv_sales, sum)
  expect_error(pv_bar(agg, "region", "revenue", series = "product",
                      horizontal = TRUE), "single series")
  w <- pv_bar(agg, "region", "revenue", series = "product")
  expect_equal(w$x$horizontal, "auto")
})

test_that("axis titles can be kept, replaced, or suppressed", {
  agg <- aggregate(revenue ~ region, pv_sales, sum)
  w <- pv_bar(agg, "region", "revenue")
  expect_equal(w$x$xlab, "region")
  expect_equal(w$x$ylab, "revenue")
  w2 <- pv_bar(agg, "region", "revenue", xlab = "Region", ylab = NA)
  expect_equal(w2$x$xlab, "Region")
  expect_equal(w2$x$ylab, "")
  monthly <- aggregate(revenue ~ month, pv_sales, sum)
  w3 <- pv_line(monthly, "month", "revenue", ylab = "")
  expect_equal(w3$x$xlab, "month")
  expect_equal(w3$x$ylab, "")
  w4 <- pv_scatter(mtcars, "wt", "mpg", xlab = "Weight (1000 lbs)")
  expect_equal(w4$x$xlab, "Weight (1000 lbs)")
  expect_equal(w4$x$ylab, "mpg")
})

test_that("line widget detects x type and keeps category order", {
  monthly <- aggregate(revenue ~ month + region, pv_sales, sum)
  w <- expect_pvchart(pv_line(monthly, "month", "revenue", series = "region"),
                      "line")
  expect_equal(w$x$xtype, "category")
  daily <- aggregate(revenue ~ date, pv_sales, sum)
  expect_equal(pv_line(daily, "date", "revenue")$x$xtype, "date")
})

test_that("line sends its point and curve options through and validates them", {
  monthly <- aggregate(revenue ~ month + region, pv_sales, sum)
  # The defaults reproduce the old rendering: no markers, straight lines.
  w <- pv_line(monthly, "month", "revenue", series = "region")
  expect_false(w$x$showPoints)
  expect_equal(w$x$curve, "linear")
  expect_false(w$x$zoom)
  # "auto" travels unresolved: the JavaScript side decides at render time.
  w2 <- pv_line(monthly, "month", "revenue", series = "region",
                show_points = "auto", curve = "monotone")
  expect_equal(w2$x$showPoints, "auto")
  expect_equal(w2$x$curve, "monotone")
  single <- aggregate(revenue ~ month, pv_sales, sum)
  w3 <- pv_line(single, "month", "revenue", show_points = TRUE,
                curve = "step")
  expect_true(w3$x$showPoints)
  expect_equal(w3$x$curve, "step")
  expect_error(pv_line(monthly, "month", "revenue", show_points = "yes"),
               'TRUE, FALSE, or "auto"')
  expect_error(pv_line(monthly, "month", "revenue", curve = "wavy"),
               "should be one of")
})

test_that("line zoom is a plain switch and needs a continuous x axis", {
  pop <- subset(pv_city_population, city %in% c("Luzern", "Zug"))
  w <- pv_line(pop, "year", "population", series = "city", zoom = TRUE)
  expect_true(w$x$zoom)
  daily <- aggregate(revenue ~ date, pv_sales, sum)
  expect_true(pv_line(daily, "date", "revenue", zoom = TRUE)$x$zoom)
  # No "auto" here - there is no data-driven decision to defer.
  expect_error(pv_line(pop, "year", "population", zoom = "auto"),
               "TRUE or FALSE")
  monthly <- aggregate(revenue ~ month, pv_sales, sum)
  expect_error(pv_line(monthly, "month", "revenue", zoom = TRUE),
               "categorical")
})

test_that("line markers, curves, and zoom render without JavaScript errors", {
  render_skip_if_no_chrome()
  monthly <- aggregate(revenue ~ month + region, pv_sales, sum)
  pop <- subset(pv_city_population,
                city %in% c("Luzern", "Emmen", "Kriens", "Zug"))
  charts <- list(
    pv_line(monthly, "month", "revenue", series = "region",
            show_points = "auto", curve = "monotone"),
    pv_line(monthly, "month", "revenue", series = "region",
            show_points = TRUE, curve = "step"),
    pv_line(pop, "year", "population", series = "city", zoom = TRUE),
    # Zoom composes with faceting by dropping the strip quietly.
    pv_line(pop, "year", "population", series = "city", zoom = TRUE) |>
      pv_facet(pop$city, ncol = 2)
  )
  for (w in charts) {
    path <- tempfile(fileext = ".png")
    expect_no_warning(pv_save(w, path, quiet = TRUE))
    expect_gt(file.size(path), 20000)
    unlink(path)
  }
})

test_that("the zoom strip stays out of the standalone SVG export", {
  render_skip_if_no_chrome()
  pop <- subset(pv_city_population,
                city %in% c("Luzern", "Emmen", "Kriens", "Zug"))
  w <- pv_line(pop, "year", "population", series = "city", zoom = TRUE)
  path <- tempfile(fileext = ".svg")
  expect_no_warning(pv_save(w, path, quiet = TRUE))
  svg <- paste(readLines(path, warn = FALSE), collapse = "\n")
  # The root document plus the one embedded plot - no strip svg, and no
  # trace of the brush chrome.
  expect_equal(lengths(regmatches(svg, gregexpr("<svg", svg))), 2L)
  expect_false(grepl("overlay", svg, fixed = TRUE))
  unlink(path)
})

test_that("scatter enforces the 3-colour cap", {
  expect_pvchart(pv_scatter(mtcars, "wt", "mpg", size = "hp"), "scatter")
  mt <- mtcars
  mt$g <- rep(letters[1:4], 8)
  expect_error(pv_scatter(mt, "wt", "mpg", color = "g"), "3")
})

test_that("line and scatter send the legend flag through and validate it", {
  monthly <- aggregate(revenue ~ month + region, pv_sales, sum)
  w <- pv_line(monthly, "month", "revenue", series = "region")
  expect_equal(w$x$legend, "auto")
  expect_true(w$x$showLegend)
  w2 <- pv_line(monthly, "month", "revenue", series = "region",
                legend = FALSE)
  expect_false(w2$x$legend)
  expect_error(pv_line(monthly, "month", "revenue", legend = "nope"),
               'TRUE, FALSE, or "auto"')
  s <- pv_scatter(mtcars, "wt", "mpg", legend = TRUE)
  expect_true(s$x$legend)
  expect_false(s$x$showLegend)
  expect_error(pv_scatter(mtcars, "wt", "mpg", legend = NA),
               'TRUE, FALSE, or "auto"')
})

test_that("scatter density and canvas flags travel and validate", {
  # The defaults reproduce the old rendering: point marks, SVG under
  # the canvas threshold.
  w <- pv_scatter(mtcars, "wt", "mpg")
  expect_false(w$x$density)
  expect_equal(w$x$canvas, "auto")
  w2 <- pv_scatter(mtcars, "wt", "mpg", density = TRUE)
  expect_true(w2$x$density)
  w3 <- pv_scatter(mtcars, "wt", "mpg", canvas = TRUE)
  expect_true(w3$x$canvas)
  w4 <- pv_scatter(mtcars, "wt", "mpg", canvas = FALSE)
  expect_false(w4$x$canvas)
  # Density is a plain switch - no "auto" middle ground to defer.
  expect_error(pv_scatter(mtcars, "wt", "mpg", density = "auto"),
               "TRUE or FALSE")
  expect_error(pv_scatter(mtcars, "wt", "mpg", canvas = "yes"),
               'TRUE, FALSE, or "auto"')
})

test_that("density contours refuse per-point aesthetics by name", {
  mt <- mtcars
  mt$g <- rep(letters[1:2], 16)
  expect_error(pv_scatter(mt, "wt", "mpg", color = "g", density = TRUE),
               "no per-point aesthetics.*`color`")
  expect_error(pv_scatter(mt, "wt", "mpg", size = "hp", label = "g",
                          density = TRUE),
               "`size`, `label` mappings")
  # Density and canvas together: density wins, canvas rides along
  # ignored, and nothing errors.
  w <- pv_scatter(mt, "wt", "mpg", density = TRUE, canvas = TRUE)
  expect_true(w$x$density)
  expect_true(w$x$canvas)
})

test_that("scatter density accepts the contour and hex spellings", {
  # "hex" travels as itself; "contours" is the readable spelling of the
  # original TRUE and travels as TRUE, so old and new payloads for the
  # contour treatment stay byte-identical.
  w <- pv_scatter(mtcars, "wt", "mpg", density = "hex")
  expect_identical(w$x$density, "hex")
  w2 <- pv_scatter(mtcars, "wt", "mpg", density = "contours")
  expect_identical(w2$x$density, TRUE)
  # Anything else is refused with the full menu in the message.
  expect_error(pv_scatter(mtcars, "wt", "mpg", density = "hexes"),
               '"contours", or "hex"')
  expect_error(pv_scatter(mtcars, "wt", "mpg", density = c(TRUE, FALSE)),
               "TRUE or FALSE")
  expect_error(pv_scatter(mtcars, "wt", "mpg", density = NA),
               '"contours", or "hex"')
})

test_that("hex binning refuses per-point aesthetics like the contours do", {
  mt <- mtcars
  mt$g <- rep(letters[1:2], 16)
  expect_error(pv_scatter(mt, "wt", "mpg", color = "g", density = "hex"),
               "no per-point aesthetics.*`color`")
  # The message names the hex treatment, not the contours.
  expect_error(pv_scatter(mt, "wt", "mpg", size = "hp", density = "hex"),
               "count-filled hexagons")
})

test_that("density contours and canvas points render without JavaScript errors", {
  render_skip_if_no_chrome()
  set.seed(7)
  big <- data.frame(x = stats::rnorm(9000), y = stats::rnorm(9000))
  charts <- list(
    # 9000 points: past the ~8000 "auto" threshold, so this exercises
    # the canvas layer without asking for it by name.
    pv_scatter(big, "x", "y"),
    pv_scatter(big, "x", "y", density = TRUE),
    # A forced canvas under the threshold, with a trend drawn over it.
    pv_scatter(head(big, 400), "x", "y", canvas = TRUE) |> pv_trend("lm"),
    pv_scatter(big, "x", "y", density = TRUE) |> pv_trend("lm"),
    # A linked density chart has no marks to select; the renderer must
    # carry the keys without complaint and simply not build a brush.
    pv_scatter(head(big, 400), "x", "y", density = TRUE) |>
      pv_link(crosstalk::SharedData$new(head(big, 400)))
  )
  for (w in charts) {
    path <- tempfile(fileext = ".png")
    expect_no_warning(pv_save(w, path, quiet = TRUE))
    expect_gt(file.size(path), 20000)
    unlink(path)
  }
})

# Stages a widget the way pv_save() does (light mode, no entrance
# animation, filling a page opened at a fixed size) and hands back the
# live Chrome session, for tests that drive the rendered chart. The
# caller closes the session.
widget_page_session <- function(w, width = 700, height = 460) {
  w$x$mode <- "light"
  w$x$duration <- 0
  w$width <- NULL
  w$height <- NULL
  w$sizingPolicy$browser$fill <- TRUE
  w$sizingPolicy$browser$padding <- 0
  stage <- tempfile("pv-widget-page-")
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

test_that("the canvas scatter's quadtree drives the hover tooltip", {
  render_skip_if_no_chrome()
  # Points on a [0, 10] square, which nice() keeps as the domain - so
  # the point at (5, 5) sits exactly at the canvas centre, wherever the
  # margins land the plot.
  pts <- data.frame(x = c(0, 10, 5, 2, 8), y = c(0, 10, 5, 8, 2))
  s <- widget_page_session(pv_scatter(pts, x = "x", y = "y", canvas = TRUE))
  withr::defer(try(s$b$close(), silent = TRUE))
  expect_identical(s$errors$msgs, character())
  res <- s$b$Runtime$evaluate("
    (function () {
      var cv = document.querySelector('canvas.pv-canvas');
      if (!cv) return 'no canvas';
      var r = cv.getBoundingClientRect();
      var cx = r.left + r.width / 2, cy = r.top + r.height / 2;
      var target = document.elementFromPoint(cx, cy);
      target.dispatchEvent(new PointerEvent('pointermove',
        { clientX: cx, clientY: cy, bubbles: true }));
      var tip = document.querySelector('.pv-tooltip');
      return JSON.stringify({
        opacity: tip.style.opacity,
        html: tip.innerHTML,
        circles: document.querySelectorAll('circle.pt').length
      });
    })()", returnByValue = TRUE)$result$value
  got <- jsonlite::fromJSON(res)
  # The marks are on the canvas - no SVG circle per point...
  expect_equal(got$circles, 0)
  # ...yet hovering the centre finds (5, 5) through the quadtree.
  expect_equal(got$opacity, "1")
  expect_match(got$html, "x: <b>5</b>", fixed = TRUE)
  expect_match(got$html, "y: <b>5</b>", fixed = TRUE)
})

test_that("the canvas scatter's brush reads the quadtree into crosstalk", {
  render_skip_if_no_chrome()
  pts <- data.frame(x = c(0, 10, 5, 2, 8), y = c(0, 10, 5, 8, 2))
  sd <- crosstalk::SharedData$new(pts, key = c("a", "b", "c", "d", "e"))
  w <- pv_scatter(pts, x = "x", y = "y", canvas = TRUE) |> pv_link(sd)
  group <- w$x$ctGroup
  s <- widget_page_session(w)
  withr::defer(try(s$b$close(), silent = TRUE))
  expect_identical(s$errors$msgs, character())
  rect <- s$b$Runtime$evaluate(paste0(
    "JSON.stringify(document.querySelector('canvas.pv-canvas')",
    ".getBoundingClientRect())"), returnByValue = TRUE)$result$value
  r <- jsonlite::fromJSON(rect)
  # Drag a real brush over the middle half of the plot: only the centre
  # point (key "c") lies inside it.
  x0 <- r$left + r$width * 0.25
  x1 <- r$left + r$width * 0.75
  y0 <- r$top + r$height * 0.25
  y1 <- r$top + r$height * 0.75
  s$b$Input$dispatchMouseEvent(type = "mouseMoved", x = x0, y = y0)
  s$b$Input$dispatchMouseEvent(type = "mousePressed", x = x0, y = y0,
                               button = "left", clickCount = 1)
  for (i in 1:8) {
    s$b$Input$dispatchMouseEvent(type = "mouseMoved",
                                 x = x0 + (x1 - x0) * i / 8,
                                 y = y0 + (y1 - y0) * i / 8,
                                 button = "left")
  }
  s$b$Input$dispatchMouseEvent(type = "mouseReleased", x = x1, y = y1,
                               button = "left", clickCount = 1)
  Sys.sleep(0.5)
  sel <- s$b$Runtime$evaluate(sprintf(
    "JSON.stringify(window.crosstalk.group('%s').var('selection').get())",
    group), returnByValue = TRUE)$result$value
  expect_identical(jsonlite::fromJSON(sel), "c")
})

test_that("the density scatter reports the band under the cursor", {
  render_skip_if_no_chrome()
  # One tight gaussian blob: the plot centre sits in the densest band.
  set.seed(3)
  blob <- data.frame(x = stats::rnorm(2000), y = stats::rnorm(2000))
  s <- widget_page_session(
    pv_scatter(blob, x = "x", y = "y", density = TRUE))
  withr::defer(try(s$b$close(), silent = TRUE))
  expect_identical(s$errors$msgs, character())
  res <- s$b$Runtime$evaluate("
    (function () {
      /* The first svg in the widget is the download control's icon;
         the plot svg is the big one. */
      var svg = null;
      document.querySelectorAll('.pvchart svg').forEach(function (el) {
        if (!svg || el.clientWidth > svg.clientWidth) { svg = el; }
      });
      var r = svg.getBoundingClientRect();
      var cx = r.left + r.width / 2, cy = r.top + r.height / 2;
      var target = document.elementFromPoint(cx, cy);
      target.dispatchEvent(new PointerEvent('pointermove',
        { clientX: cx, clientY: cy, bubbles: true }));
      return document.querySelector('.pv-tooltip').innerHTML;
    })()", returnByValue = TRUE)$result$value
  expect_match(res, "density band <b>")
  expect_match(res, "of the peak level", fixed = TRUE)
})

test_that("hex-binned scatters render and export without JavaScript errors", {
  render_skip_if_no_chrome()
  set.seed(7)
  big <- data.frame(x = stats::rnorm(6000), y = stats::rnorm(6000))
  charts <- list(
    pv_scatter(big, "x", "y", density = "hex"),
    # Trend fits draw over the hexes, exactly as they draw over contours.
    pv_scatter(big, "x", "y", density = "hex") |> pv_trend("lm"),
    # Canvas rides along ignored - the hexes are already the aggregate.
    pv_scatter(head(big, 500), "x", "y", density = "hex", canvas = TRUE)
  )
  for (w in charts) {
    path <- tempfile(fileext = ".png")
    expect_no_warning(pv_save(w, path, quiet = TRUE))
    expect_gt(file.size(path), 20000)
    unlink(path)
  }
  # The hexes are plain SVG paths, so the standalone SVG export carries
  # the whole mosaic: one root document, one embedded plot, and far more
  # paths than the axes alone could account for.
  svg_path <- tempfile(fileext = ".svg")
  expect_no_warning(pv_save(pv_scatter(big, "x", "y", density = "hex"),
                            svg_path, quiet = TRUE))
  svg <- paste(readLines(svg_path, warn = FALSE), collapse = "\n")
  expect_equal(lengths(regmatches(svg, gregexpr("<svg", svg))), 2L)
  expect_gt(lengths(regmatches(svg, gregexpr("<path", svg))), 100L)
  unlink(svg_path)
})

test_that("the hex scatter reports the bin's count and centre on hover", {
  render_skip_if_no_chrome()
  # One tight gaussian blob: the plot centre lands in a well-filled bin.
  set.seed(3)
  blob <- data.frame(x = stats::rnorm(2000), y = stats::rnorm(2000))
  s <- widget_page_session(
    pv_scatter(blob, x = "x", y = "y", density = "hex"))
  withr::defer(try(s$b$close(), silent = TRUE))
  expect_identical(s$errors$msgs, character())
  res <- s$b$Runtime$evaluate("
    (function () {
      /* The first svg in the widget is the download control's icon;
         the plot svg is the big one. */
      var svg = null;
      document.querySelectorAll('.pvchart svg').forEach(function (el) {
        if (!svg || el.clientWidth > svg.clientWidth) { svg = el; }
      });
      var r = svg.getBoundingClientRect();
      var cx = r.left + r.width / 2, cy = r.top + r.height / 2;
      var target = document.elementFromPoint(cx, cy);
      target.dispatchEvent(new PointerEvent('pointermove',
        { clientX: cx, clientY: cy, bubbles: true }));
      return document.querySelector('.pv-tooltip').innerHTML;
    })()", returnByValue = TRUE)$result$value
  # The tooltip leads with the exact count and follows with the cell's
  # centre in data units, one row per axis.
  expect_match(res, "points</b>", fixed = TRUE)
  expect_match(res, "x \u2248 <b>", fixed = TRUE)
  expect_match(res, "y \u2248 <b>", fixed = TRUE)
})

test_that("empty space near a sparse scatter's point hovers the nearest mark", {
  render_skip_if_no_chrome()
  # Five labelled points on a [0, 10] square, which nice() keeps as the
  # domain - the centre point sits exactly at the plot's midpoint.
  pts <- data.frame(x = c(0, 10, 5, 2, 8), y = c(0, 10, 5, 8, 2),
                    who = c("sw", "ne", "centre", "nw", "se"))
  charts <- list(
    plain = pv_scatter(pts, x = "x", y = "y", label = "who"),
    # The linked variant routes the same hover through the plot group,
    # underneath the brush that drag-to-select needs to keep.
    linked = pv_scatter(pts, x = "x", y = "y", label = "who") |>
      pv_link(crosstalk::SharedData$new(pts))
  )
  for (w in charts) {
    s <- widget_page_session(w)
    withr::defer(try(s$b$close(), silent = TRUE))
    expect_identical(s$errors$msgs, character())
    res <- s$b$Runtime$evaluate("
      (function () {
        var svg = null;
        document.querySelectorAll('.pvchart svg').forEach(function (el) {
          if (!svg || el.clientWidth > svg.clientWidth) { svg = el; }
        });
        var r = svg.getBoundingClientRect();
        /* 6% right of and 5% above the plot centre: tens of pixels of
           empty plot away from every mark, but nearest to the centre
           point - a spot the old per-mark listeners answered with
           nothing at all. */
        var cx = r.left + r.width * 0.56, cy = r.top + r.height * 0.45;
        var target = document.elementFromPoint(cx, cy);
        target.dispatchEvent(new PointerEvent('pointermove',
          { clientX: cx, clientY: cy, bubbles: true }));
        var tip = document.querySelector('.pv-tooltip');
        return JSON.stringify({
          onMark: target.tagName.toLowerCase() === 'circle',
          opacity: tip.style.opacity,
          html: tip.innerHTML
        });
      })()", returnByValue = TRUE)$result$value
    got <- jsonlite::fromJSON(res)
    # The pointer really was on empty plot, not on a mark...
    expect_false(got$onMark)
    # ...yet the nearest mark - the centre point - answers the hover.
    expect_equal(got$opacity, "1")
    expect_match(got$html, "<b>centre</b>", fixed = TRUE)
    expect_match(got$html, "x: <b>5</b>", fixed = TRUE)
    expect_match(got$html, "y: <b>5</b>", fixed = TRUE)
  }
})

test_that("the SVG scatter's brush still owns drags under the hover upgrade", {
  render_skip_if_no_chrome()
  pts <- data.frame(x = c(0, 10, 5, 2, 8), y = c(0, 10, 5, 8, 2))
  sd <- crosstalk::SharedData$new(pts, key = c("a", "b", "c", "d", "e"))
  w <- pv_scatter(pts, x = "x", y = "y") |> pv_link(sd)
  group <- w$x$ctGroup
  s <- widget_page_session(w)
  withr::defer(try(s$b$close(), silent = TRUE))
  expect_identical(s$errors$msgs, character())
  rect <- s$b$Runtime$evaluate("
    (function () {
      var svg = null;
      document.querySelectorAll('.pvchart svg').forEach(function (el) {
        if (!svg || el.clientWidth > svg.clientWidth) { svg = el; }
      });
      return JSON.stringify(svg.getBoundingClientRect());
    })()", returnByValue = TRUE)$result$value
  r <- jsonlite::fromJSON(rect)
  # Drag a real brush over the middle half of the plot: only the centre
  # point (key "c") lies inside it - exactly the canvas brush test, but
  # on the SVG marks whose hover now listens on the plot group.
  x0 <- r$left + r$width * 0.3
  x1 <- r$left + r$width * 0.7
  y0 <- r$top + r$height * 0.3
  y1 <- r$top + r$height * 0.7
  s$b$Input$dispatchMouseEvent(type = "mouseMoved", x = x0, y = y0)
  s$b$Input$dispatchMouseEvent(type = "mousePressed", x = x0, y = y0,
                               button = "left", clickCount = 1)
  for (i in 1:8) {
    s$b$Input$dispatchMouseEvent(type = "mouseMoved",
                                 x = x0 + (x1 - x0) * i / 8,
                                 y = y0 + (y1 - y0) * i / 8,
                                 button = "left")
  }
  s$b$Input$dispatchMouseEvent(type = "mouseReleased", x = x1, y = y1,
                               button = "left", clickCount = 1)
  Sys.sleep(0.5)
  sel <- s$b$Runtime$evaluate(sprintf(
    "JSON.stringify(window.crosstalk.group('%s').var('selection').get())",
    group), returnByValue = TRUE)$result$value
  expect_identical(jsonlite::fromJSON(sel), "c")
})

test_that("the spaghetti crosshair still singles out the nearest line", {
  render_skip_if_no_chrome()
  # Ten flat series - past the palette's 8 hues, so the spaghetti
  # treatment applies and hover picks one line at a time. Hovering
  # between the columns at the height of s03 must light up s03.
  many <- expand.grid(x = 1:5, series = sprintf("s%02d", 1:10))
  many$y <- as.numeric(sub("s", "", many$series)) * 10
  w <- pv_line(many, x = "x", y = "y", series = "series")
  s <- widget_page_session(w)
  withr::defer(try(s$b$close(), silent = TRUE))
  expect_identical(s$errors$msgs, character())
  res <- s$b$Runtime$evaluate("
    (function () {
      /* The crosshair's transparent surface spans exactly the panel,
         so panel-relative aim needs no margin arithmetic. */
      var surf = document.querySelector(
        '.pvchart svg rect[fill=\"transparent\"]');
      var r = surf.getBoundingClientRect();
      /* y runs 0..100 bottom to top; s03 sits at y = 30, which is 70%
         of the way down the panel. 45% across lands between the x = 2
         and x = 3 columns, nearer the x = 3 observations. */
      var cx = r.left + r.width * 0.45;
      var cy = r.top + r.height * 0.70;
      surf.dispatchEvent(new PointerEvent('pointermove',
        { clientX: cx, clientY: cy, bubbles: true }));
      return document.querySelector('.pv-tooltip').innerHTML;
    })()", returnByValue = TRUE)$result$value
  expect_match(res, "s03", fixed = TRUE)
  expect_match(res, "<b>3</b>", fixed = TRUE)
})

test_that("force widget validates link ids", {
  w <- expect_pvchart(
    pv_force(pv_network$nodes, pv_network$links, group = "group"), "force")
  expect_true(all(c("id", "label", "group") %in% names(w$x$nodes)))
  bad <- data.frame(source = "Ada", target = "Nobody")
  expect_error(pv_force(pv_network$nodes, bad), "unknown node ids")
})

test_that("chord widget wants a square matrix", {
  expect_pvchart(pv_chord(pv_flows), "chord")
  expect_error(pv_chord(matrix(1:6, 2)), "square")
})

test_that("sunburst builds a nested hierarchy", {
  w <- expect_pvchart(
    pv_sunburst(pv_sales, levels = c("region", "product"),
                value = "revenue"), "sunburst")
  expect_equal(length(w$x$root$children), 4)
  kid <- w$x$root$children[[1]]
  expect_equal(length(kid$children), 5)
  total <- sum(vapply(w$x$root$children, function(r) {
    sum(vapply(r$children, `[[`, numeric(1), "value"))
  }, numeric(1)))
  expect_equal(total, sum(pv_sales$revenue))
})

test_that("the series cap reads the active theme's palette", {
  on.exit(pv_reset_theme())
  # The packaged theme fills all 8 slots, so nothing changes by default.
  expect_equal(theme_palette_slots(), 8)
  expect_silent(check_theme_palette_fit(letters[1:8], "grp"))
  expect_error(check_theme_palette_fit(letters[1:9], "grp"),
               "9 levels but the active theme's palette has 8 colours")
  # A smaller theme lowers the cap - the paper theme brings 5 slots.
  pv_set_theme(pv_theme_paper())
  expect_equal(theme_palette_slots(), 5)
  expect_error(check_theme_palette_fit(letters[1:6], "grp"),
               "6 levels but the active theme's palette has 5 colours")
  expect_silent(check_theme_palette_fit(letters[1:5], "grp"))
  # Resetting the theme restores the packaged 8.
  pv_reset_theme()
  expect_equal(theme_palette_slots(), 8)
  expect_silent(check_theme_palette_fit(letters[1:8], "grp"))
})
