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
