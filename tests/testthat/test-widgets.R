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
