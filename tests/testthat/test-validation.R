# The shared input-validation contract: every wrong or degenerate input
# fails loudly and helpfully, uniformly across the chart constructors.

expect_pvchart <- function(w, type) {
  expect_s3_class(w, "htmlwidget")
  expect_equal(attr(w, "package"), "polyviz")
  expect_equal(w$x$type, type)
  invisible(w)
}

region_sales <- function() {
  aggregate(revenue ~ region, pv_sales, sum)
}

test_that("non-data-frame data and misspelled columns fail helpfully", {
  agg <- region_sales()
  # A matrix or NULL is named for what it is, not blamed on its columns.
  expect_error(pv_bar(as.matrix(mtcars[, c("wt", "mpg")]), "wt", "mpg"),
               "must be a data frame \\(got a matrix\\)")
  expect_error(pv_bar(NULL, "wt", "mpg"),
               "must be a data frame \\(got NULL\\)")
  expect_error(pv_scatter(list(a = 1), "a", "a"), "must be a data frame")
  # A missing column lists what is available ...
  expect_error(pv_bar(agg, "zzz", "revenue"),
               "not in `data`: zzz. Available: region, revenue")
  # ... and a near-miss gets a did-you-mean hint.
  expect_error(pv_bar(agg, "regoin", "revenue"), "Did you mean `region`\\?")
  expect_error(pv_line(agg, "region", "Revenue"), "Did you mean `revenue`\\?")
  # No hint when nothing is close.
  err <- tryCatch(pv_bar(agg, "zzz", "revenue"), error = conditionMessage)
  expect_false(grepl("Did you mean", err))
})

test_that("factor or character columns in numeric roles are refused by name", {
  agg <- region_sales()
  fct <- agg
  fct$revenue <- factor(fct$revenue)
  chr <- agg
  chr$revenue <- as.character(chr$revenue)
  expect_error(pv_bar(fct, "region", "revenue"),
               "Column `revenue` is a factor, not numeric")
  expect_error(pv_bar(chr, "region", "revenue"),
               "Column `revenue` is a character column, not numeric")
  expect_error(pv_line(fct, "region", "revenue"), "a factor, not numeric")
  expect_error(pv_scatter(fct, "revenue", "revenue"), "not numeric")
  expect_error(pv_scatter(agg, "revenue", "revenue", size = "region"),
               "Column `region` is a character column")
  expect_error(pv_donut(chr, "region", "revenue"), "not numeric")
  expect_error(pv_treemap(fct, levels = "region", value = "revenue"),
               "not numeric")
  expect_error(pv_sunburst(fct, levels = "region", value = "revenue"),
               "not numeric")
  expect_error(pv_pack(fct, levels = "region", value = "revenue"),
               "not numeric")
  expect_error(pv_lollipop(fct, "region", "revenue"), "not numeric")
  expect_error(pv_area(fct, "region", "revenue"), "not numeric")
  expect_error(pv_heatmap(fct, "region", "region", "revenue"), "not numeric")
  cal <- data.frame(day = as.Date("2024-01-01") + 0:1, v = c("1", "2"))
  expect_error(pv_calendar(cal, "day", "v"), "not numeric")
  lk <- data.frame(source = "A", target = "B", value = "3")
  expect_error(pv_sankey(lk), "Column `value` is a character column")
  f25 <- subset(pv_fiscal, year == 2025)
  f25$resource_index <- factor(f25$resource_index)
  expect_error(pv_choropleth(f25, id = "municipality_id",
                             value = "resource_index"), "a factor")
  mot <- data.frame(yr = rep(2000:2001, 2), who = rep(c("A", "B"), each = 2),
                    val = factor(1:4))
  expect_error(pv_race(mot, "yr", "who", "val"), "a factor")
  expect_error(pv_bump(mot, "yr", "who", "val"), "a factor")
  # A factor in a category role stays fine - only numeric roles refuse.
  cat_fct <- agg
  cat_fct$region <- factor(cat_fct$region)
  expect_pvchart(pv_bar(cat_fct, "region", "revenue"), "bar")
})

test_that("mode and duration are validated once for every chart", {
  agg <- region_sales()
  expect_error(pv_bar(agg, "region", "revenue", mode = "blue"),
               '`mode` must be "auto", "light", or "dark"')
  expect_error(pv_line(agg, "region", "revenue", mode = NA), "`mode`")
  expect_error(pv_donut(agg, "region", "revenue", mode = c("light", "dark")),
               "`mode`")
  expect_error(pv_bar(agg, "region", "revenue", duration = "fast"),
               "`duration` must be a single non-negative number")
  expect_error(pv_bar(agg, "region", "revenue", duration = -5), "`duration`")
  expect_error(pv_bar(agg, "region", "revenue", duration = Inf), "`duration`")
  expect_error(pv_bar(agg, "region", "revenue", duration = NA_real_),
               "`duration`")
  expect_error(pv_scatter(mtcars, "wt", "mpg", duration = c(1, 2)),
               "`duration`")
  # Valid values still travel, and an integer duration becomes numeric.
  w <- pv_bar(agg, "region", "revenue", mode = "dark", duration = 0L)
  expect_equal(w$x$mode, "dark")
  expect_identical(w$x$duration, 0)
})

test_that("zero-row data refuses to draw, everywhere", {
  agg <- region_sales()
  empty <- agg[0, ]
  msg <- "has no rows; nothing to draw"
  expect_error(pv_bar(empty, "region", "revenue"), msg)
  expect_error(pv_line(empty, "region", "revenue"), msg)
  expect_error(pv_scatter(mtcars[0, ], "wt", "mpg"), msg)
  expect_error(pv_area(empty, "region", "revenue"), msg)
  expect_error(pv_donut(empty, "region", "revenue"), msg)
  expect_error(pv_treemap(empty, levels = "region", value = "revenue"), msg)
  expect_error(pv_sunburst(empty, levels = "region", value = "revenue"), msg)
  expect_error(pv_pack(empty, levels = "region", value = "revenue"), msg)
  expect_error(pv_lollipop(empty, "region", "revenue"), msg)
  expect_error(pv_heatmap(empty, "region", "region", "revenue"), msg)
  cal0 <- data.frame(day = as.Date(character(0)), v = numeric(0))
  expect_error(pv_calendar(cal0, "day", "v"), msg)
  expect_error(pv_histogram(mtcars[0, ], "mpg"), msg)
  expect_error(pv_boxplot(mtcars[0, ], "mpg"), msg)
  expect_error(pv_violin(mtcars[0, ], "mpg", group = "cyl"), msg)
  expect_error(pv_ridgeline(mtcars[0, ], "mpg", group = "cyl"), msg)
  expect_error(pv_beeswarm(mtcars[0, ], "mpg"), msg)
  expect_error(pv_parallel(mtcars[0, ], c("wt", "mpg")), msg)
  lk0 <- data.frame(source = character(0), target = character(0),
                    value = numeric(0))
  expect_error(pv_sankey(lk0), "`links` has no rows")
  expect_error(pv_force(pv_network$nodes[0, ], pv_network$links),
               "`nodes` has no rows")
  expect_error(pv_chord(matrix(numeric(0), 0, 0)), "`matrix` has no rows")
  f0 <- subset(pv_fiscal, year == 3000)
  expect_error(pv_choropleth(f0, id = "municipality_id",
                             value = "resource_index"), msg)
  expect_error(pv_race(f0, "year", "municipality", "resource_index"), msg)
  expect_error(pv_bump(f0, "year", "municipality", "resource_index"), msg)
})

test_that("rows with missing plotted values are dropped with a warning", {
  agg <- region_sales()
  nay <- agg
  nay$revenue[2] <- NA
  expect_warning(w <- pv_bar(nay, "region", "revenue"),
                 "Dropped 1 row\\(s\\) with missing `revenue` values")
  # The NA row is gone from the payload - no phantom zero-label bar.
  expect_equal(nrow(w$x$data), nrow(agg) - 1)
  expect_false(anyNA(w$x$data$y))
  nax <- agg
  nax$region[1] <- NA
  expect_warning(wb <- pv_bar(nax, "region", "revenue"),
                 "missing `region` values")
  expect_equal(nrow(wb$x$data), nrow(agg) - 1)
  expect_warning(wl <- pv_line(nay, "region", "revenue"),
                 "missing `revenue` values")
  expect_false(anyNA(wl$x$data$y))
  mt <- mtcars
  mt$wt[3] <- NA
  expect_warning(ws <- pv_scatter(mt, "wt", "mpg"), "missing `wt` values")
  expect_equal(nrow(ws$x$data), nrow(mtcars) - 1)
  mt2 <- mtcars
  mt2$hp[5] <- NA
  expect_warning(pv_scatter(mt2, "wt", "mpg", size = "hp"),
                 "missing `hp` values")
  # Without the size mapping the same NA is harmless - no warning.
  expect_silent(pv_scatter(mt2, "wt", "mpg"))
  ar <- data.frame(t = 1:3, v = c(1, NA, 3))
  expect_warning(wa <- pv_area(ar, "t", "v"), "missing `v` values")
  expect_equal(nrow(wa$x$data), 2)
  lol <- data.frame(m = c("a", "b", "c"), v = c(2, NA, 1))
  expect_warning(pv_lollipop(lol, "m", "v"), "missing `v` values")
  hist_na <- data.frame(v = c(1, 2, 3, NA))
  expect_warning(pv_histogram(hist_na, "v"), "missing `v` values")
  bx <- data.frame(v = c(1, 2, NA, 4), g = c("a", "a", "b", NA))
  expect_warning(expect_warning(
    pv_boxplot(bx, "v", group = "g"),
    "missing `v` values"), "missing `g` values")
  vio <- data.frame(v = c(1, 2, NA, 4, 5), g = c("a", "a", "a", "b", "b"))
  expect_warning(pv_violin(vio, "v", group = "g"), "missing `v` values")
  expect_warning(pv_ridgeline(vio, "v", group = "g"), "missing `v` values")
  # Losing every row is an error, not a warning and a blank chart.
  all_na <- agg
  all_na$revenue <- NA_real_
  expect_error(pv_bar(all_na, "region", "revenue"),
               "`revenue` has no non-missing values")
  # The aborting charts keep aborting - a donut or sankey with a missing
  # value is refused outright.
  expect_error(pv_donut(nay, "region", "revenue"), "non-negative")
  lk <- data.frame(source = "A", target = "B", value = NA_real_)
  expect_error(pv_sankey(lk), "no missing values")
})

test_that("negative values that corrupt area layouts are refused", {
  neg <- data.frame(g = c("a", "b"), v = c(5, -1))
  expect_error(pv_treemap(neg, levels = "g", value = "v"),
               "`v` has negative values; treemap cell areas must be non-negative")
  expect_error(pv_pack(neg, levels = "g", value = "v"),
               "circle areas must be non-negative")
  expect_error(pv_sunburst(neg, levels = "g", value = "v"),
               "segment sizes must be non-negative")
  # Stacked bands cannot fold below their baseline in any offset - the
  # stream's centred stack piles bands on each other all the same.
  ar <- data.frame(t = rep(1:2, 2), v = c(1, 2, -3, 4),
                   s = rep(c("a", "b"), each = 2))
  expect_error(pv_area(ar, "t", "v", series = "s"),
               "negative values, which stacked bands cannot draw")
  expect_error(pv_area(ar, "t", "v", series = "s", offset = "percent"),
               "cannot draw")
  expect_error(pv_area(ar, "t", "v", series = "s", offset = "stream"),
               "cannot draw")
  # Non-negative data still builds in every offset.
  ok <- data.frame(t = rep(1:2, 2), v = c(1, 2, 3, 4),
                   s = rep(c("a", "b"), each = 2))
  expect_pvchart(pv_area(ok, "t", "v", series = "s", offset = "stream"),
                 "area")
})

test_that("duplicate keys are refused like the heatmap already does", {
  agg <- region_sales()
  dup <- rbind(agg, agg[1, ])
  expect_error(pv_bar(dup, "region", "revenue"),
               "more than one row per category; aggregate it first")
  # With a series, the pair is the key: same category twice is fine
  # across series, an error within one.
  grouped <- data.frame(cat = c("a", "a", "b"), v = 1:3,
                        s = c("s1", "s2", "s1"))
  expect_pvchart(pv_bar(grouped, "cat", "v", series = "s"), "bar")
  clash <- data.frame(cat = c("a", "a"), v = 1:2, s = c("s1", "s1"))
  expect_error(pv_bar(clash, "cat", "v", series = "s"),
               "series/category combination; aggregate it first")
  # Lines key per (x, series), not per x alone.
  ln <- data.frame(t = c(1, 1, 2), v = 1:3, s = c("a", "b", "a"))
  expect_pvchart(pv_line(ln, "t", "v", series = "s"), "line")
  lndup <- data.frame(t = c(1, 1, 2), v = 1:3, s = c("a", "a", "a"))
  expect_error(pv_line(lndup, "t", "v", series = "s"),
               "series/x combination; aggregate it first")
  expect_error(pv_line(data.frame(t = c(1, 1), v = 1:2), "t", "v"),
               "aggregate it first")
  # The same sankey flow twice is one flow that needs aggregating.
  lk <- data.frame(source = c("A", "A"), target = c("B", "B"), value = 1:2)
  expect_error(pv_sankey(lk),
               "same source/target pair; aggregate it first")
  # Distinct flows between distinct pairs stay legal.
  ok <- data.frame(source = c("A", "A"), target = c("B", "C"), value = 1:2)
  expect_pvchart(pv_sankey(ok), "sankey")
})
