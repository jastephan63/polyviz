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
