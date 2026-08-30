expect_pvchart <- function(w, type) {
  expect_s3_class(w, "htmlwidget")
  expect_equal(attr(w, "package"), "polyviz")
  expect_equal(w$x$type, type)
  invisible(w)
}

test_that("donut widget builds and carries its options", {
  seats <- aggregate(elected ~ party,
                     pv_elections[pv_elections$year == 2024, ], sum)
  w <- expect_pvchart(pv_donut(seats, "party", "elected"), "donut")
  expect_equal(nrow(w$x$data), 8)
  expect_equal(w$x$innerRadius, 0.62)
  expect_equal(w$x$vlab, "elected")
  expect_equal(pv_donut(seats, "party", "elected",
                        inner_radius = 0)$x$innerRadius, 0)
  expect_error(pv_donut(seats, "nope", "elected"), "not in `data`")
})

test_that("donut validates slices, values, and inner_radius", {
  many <- data.frame(cat = letters[1:9], val = 9:1)
  expect_error(pv_donut(many, "cat", "val"), "Other")
  neg <- data.frame(cat = c("a", "b"), val = c(5, -1))
  expect_error(pv_donut(neg, "cat", "val"), "non-negative")
  ok <- data.frame(cat = c("a", "b"), val = c(1, 2))
  expect_error(pv_donut(ok, "cat", "val", inner_radius = 0.9), "0.85")
  expect_error(pv_donut(ok, "cat", "val", inner_radius = -0.1), "0.85")
})

test_that("donut sums rows that share a category, keeping order", {
  dup <- data.frame(cat = c("b", "a", "b"), val = c(1, 2, 3))
  w <- pv_donut(dup, "cat", "val")
  expect_equal(w$x$data$category, c("b", "a"))
  expect_equal(w$x$data$value, c(4, 2))
})

test_that("treemap builds the nested hierarchy on real data", {
  lu <- pv_city_landuse[pv_city_landuse$city == "Luzern", ]
  w <- expect_pvchart(
    pv_treemap(lu, levels = c("group", "category"), value = "hectares"),
    "treemap")
  expect_equal(length(w$x$root$children), 3)
  leafsum <- sum(vapply(w$x$root$children, function(gr) {
    sum(vapply(gr$children, `[[`, numeric(1), "value"))
  }, numeric(1)))
  expect_equal(leafsum, sum(lu$hectares))
  expect_error(pv_treemap(lu, levels = "nope", value = "hectares"),
               "not in `data`")
  expect_error(pv_treemap(lu, levels = character(0), value = "hectares"),
               "at least one")
})

test_that("treemap caps top-level groups at the palette size", {
  many <- data.frame(g = letters[1:9], v = 1:9)
  expect_error(pv_treemap(many, levels = "g", value = "v"), "Other")
  expect_pvchart(
    pv_treemap(data.frame(g = letters[1:8], v = 1:8),
               levels = "g", value = "v"), "treemap")
})

test_that("lollipop sorts by default and preserves order when asked", {
  f25 <- pv_fiscal[pv_fiscal$year == 2025, ]
  top <- head(f25[order(-f25$equalization_chf), ], 20)
  w <- expect_pvchart(
    pv_lollipop(top, "municipality", "equalization_chf"), "lollipop")
  expect_equal(nrow(w$x$data), 20)
  expect_true(!is.unsorted(rev(w$x$data$y)))
  reversed <- top[rev(seq_len(nrow(top))), ]
  w2 <- pv_lollipop(reversed, "municipality", "equalization_chf",
                    sort = FALSE)
  expect_equal(w2$x$data$x, rev(top$municipality))
})

test_that("lollipop enforces the 40-category cap", {
  f25 <- pv_fiscal[pv_fiscal$year == 2025, ]
  expect_error(pv_lollipop(f25, "municipality", "equalization_chf"), "40")
})
