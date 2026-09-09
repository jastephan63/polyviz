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

test_that("donut labels flag validates and rides along in the payload", {
  ok <- data.frame(cat = c("a", "b"), val = c(1, 2))
  expect_equal(pv_donut(ok, "cat", "val")$x$labels, "auto")
  expect_true(pv_donut(ok, "cat", "val", labels = TRUE)$x$labels)
  expect_false(pv_donut(ok, "cat", "val", labels = FALSE)$x$labels)
  expect_error(pv_donut(ok, "cat", "val", labels = "yes"),
               'TRUE, FALSE, or "auto"')
  expect_error(pv_donut(ok, "cat", "val", labels = NA),
               'TRUE, FALSE, or "auto"')
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

test_that("treemap labels flag validates and rides along in the payload", {
  df <- data.frame(g = c("a", "b"), v = 1:2)
  expect_equal(pv_treemap(df, levels = "g", value = "v")$x$labels, "auto")
  expect_true(pv_treemap(df, levels = "g", value = "v",
                         labels = TRUE)$x$labels)
  expect_false(pv_treemap(df, levels = "g", value = "v",
                          labels = FALSE)$x$labels)
  expect_error(pv_treemap(df, levels = "g", value = "v", labels = 1),
               'TRUE, FALSE, or "auto"')
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

test_that("lollipop value_labels flag validates and rides in the payload", {
  df <- data.frame(m = c("a", "b"), v = c(2, 1))
  expect_equal(pv_lollipop(df, "m", "v")$x$valueLabels, "auto")
  expect_true(pv_lollipop(df, "m", "v", value_labels = TRUE)$x$valueLabels)
  expect_false(pv_lollipop(df, "m", "v", value_labels = FALSE)$x$valueLabels)
  expect_error(pv_lollipop(df, "m", "v", value_labels = "no"),
               'TRUE, FALSE, or "auto"')
})

test_that("waffle widget builds and carries its options", {
  seats <- aggregate(elected ~ party,
                     pv_elections[pv_elections$year == 2024, ], sum)
  w <- expect_pvchart(pv_waffle(seats, "party", "elected"), "waffle")
  expect_equal(nrow(w$x$data), 8)
  expect_equal(w$x$rows, 10L)
  expect_equal(w$x$vlab, "elected")
  # The grid is always exactly full: the rounded counts sum to rows^2.
  expect_equal(sum(w$x$data$units), 100L)
  expect_equal(pv_waffle(seats, "party", "elected", rows = 5)$x$rows, 5L)
  expect_equal(sum(pv_waffle(seats, "party", "elected",
                             rows = 5)$x$data$units), 25L)
  expect_error(pv_waffle(seats, "nope", "elected"), "not in `data`")
})

test_that("waffle validates rows, values, and the category cap", {
  ok <- data.frame(cat = c("a", "b"), val = c(1, 2))
  expect_error(pv_waffle(ok, "cat", "val", rows = 1), "2 to 20")
  expect_error(pv_waffle(ok, "cat", "val", rows = 21), "2 to 20")
  expect_error(pv_waffle(ok, "cat", "val", rows = 2.5), "2 to 20")
  expect_error(pv_waffle(ok, "cat", "val", rows = NA), "2 to 20")
  neg <- data.frame(cat = c("a", "b"), val = c(5, -1))
  expect_error(pv_waffle(neg, "cat", "val"), "non-negative")
  nas <- data.frame(cat = c("a", "b"), val = c(5, NA))
  expect_error(pv_waffle(nas, "cat", "val"), "non-negative")
  zero <- data.frame(cat = c("a", "b"), val = c(0, 0))
  expect_error(pv_waffle(zero, "cat", "val"), "sums to zero")
  many <- data.frame(cat = letters[1:9], val = 9:1)
  expect_error(pv_waffle(many, "cat", "val"), "Other")
})

test_that("waffle sums rows that share a category, keeping order", {
  dup <- data.frame(cat = c("b", "a", "b"), val = c(1, 2, 3))
  w <- pv_waffle(dup, "cat", "val")
  expect_equal(w$x$data$category, c("b", "a"))
  expect_equal(w$x$data$value, c(4, 2))
})

test_that("waffle rounds to whole squares by largest remainder", {
  # Three equal thirds of 100 squares: 33 each plus one leftover, which
  # goes to the first category because ties break on first appearance.
  thirds <- data.frame(cat = c("x", "y", "z"), val = c(1, 1, 1))
  expect_equal(pv_waffle(thirds, "cat", "val")$x$data$units,
               c(34L, 33L, 33L))
  # A category is never more than one square from its exact share.
  seats <- aggregate(elected ~ party,
                     pv_elections[pv_elections$year == 2024, ], sum)
  w <- pv_waffle(seats, "party", "elected")
  exact <- w$x$data$value / sum(w$x$data$value) * 100
  expect_true(all(abs(w$x$data$units - exact) < 1))
  # A very small category can honestly round down to no squares at all.
  tiny <- data.frame(cat = c("big", "small"), val = c(1000, 1))
  expect_equal(pv_waffle(tiny, "cat", "val")$x$data$units, c(100L, 0L))
})

test_that("waffle generates alt text without a dedicated describer", {
  seats <- aggregate(elected ~ party,
                     pv_elections[pv_elections$year == 2024, ], sum)
  alt <- pv_alt_text(pv_waffle(seats, "party", "elected",
                               title = "Council seats by party"))
  expect_true(is.character(alt) && length(alt) == 1 && nzchar(alt))
})

test_that("waffle renders without JavaScript errors", {
  render_skip_if_no_chrome()
  seats <- aggregate(elected ~ party,
                     pv_elections[pv_elections$year == 2024, ], sum)
  w <- pv_waffle(seats, category = "party", value = "elected",
                 title = "Council seats by party")
  path <- tempfile("waffle-", fileext = ".png")
  on.exit(unlink(path), add = TRUE)
  expect_no_warning(pv_save(w, path, quiet = TRUE))
  expect_true(file.exists(path))
  expect_gt(file.size(path), 20000)
})

test_that("textured waffle renders without JavaScript errors", {
  render_skip_if_no_chrome()
  seats <- aggregate(elected ~ party,
                     pv_elections[pv_elections$year == 2024, ], sum)
  w <- pv_waffle(seats, category = "party", value = "elected",
                 title = "Council seats by party") |>
    pv_textures()
  path <- tempfile("waffle-tex-", fileext = ".png")
  on.exit(unlink(path), add = TRUE)
  expect_no_warning(pv_save(w, path, quiet = TRUE))
  expect_true(file.exists(path))
  expect_gt(file.size(path), 20000)
})
