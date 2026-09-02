# pv_pairs packs everything statistical - histogram bins, correlation
# coefficients, pairwise counts - in R; these tests pin the payload down
# and end with a real headless render through pv_save().

expect_pvchart <- function(w, type) {
  expect_s3_class(w, "htmlwidget")
  expect_equal(attr(w, "package"), "polyviz")
  expect_equal(w$x$type, type)
  invisible(w)
}

pairs25 <- function() {
  f25 <- subset(pv_fiscal, year == 2025)
  f25$side <- ifelse(f25$equalization_chf > 0,
                     "receives equalization", "contributes")
  f25
}

fiscal_cols <- c("resource_per_capita", "resource_index", "equalization_chf")

test_that("pairs packs variables, bins, and the correlation matrix", {
  f25 <- pairs25()
  w <- expect_pvchart(pv_pairs(f25, columns = fiscal_cols), "pairs")
  expect_equal(vapply(w$x$variables, function(v) v$name, character(1)),
               fiscal_cols)
  # points travel under positional names, so data columns can never
  # collide with the series/label mappings
  expect_equal(names(w$x$data), c("p1", "p2", "p3"))
  expect_equal(w$x$data$p1, f25$resource_per_capita)
  # bins are R's own hist() binning, and the limits are the bin edges
  for (k in seq_along(fiscal_cols)) {
    v <- w$x$variables[[k]]
    ref <- pv_histbins(f25[[fiscal_cols[k]]])
    expect_equal(v$bins, ref)
    expect_equal(v$lim, c(min(ref$x0), max(ref$x1)))
  }
  # the correlation matrix matches stats::cor row for row
  cm <- stats::cor(f25[fiscal_cols], use = "pairwise.complete.obs")
  for (i in 1:3) expect_equal(w$x$cor[[i]], unname(cm[i, ]))
  expect_equal(w$x$cor[[1]][1], 1)
  # complete data: every pair rests on every row
  expect_true(all(unlist(w$x$n) == nrow(f25)))
  expect_equal(w$x$method, "pearson")
  expect_equal(w$x$methodLabel, "Pearson correlation")
})

test_that("pairs defaults to the first 6 numeric columns", {
  w <- pv_pairs(pv_fiscal)
  expect_equal(vapply(w$x$variables, function(v) v$name, character(1)),
               c("year", "municipality_id", "resource_per_capita",
                 "resource_index", "equalization_chf"))
  # columns mapped to color or label are not paired with themselves
  fs <- pv_fiscal
  fs$grp <- as.numeric(fs$equalization_chf > 0)
  w2 <- pv_pairs(fs, color = "grp")
  expect_false("grp" %in%
                 vapply(w2$x$variables, function(v) v$name, character(1)))
  # more than 6 numeric columns: only the first 6 are taken
  wide <- as.data.frame(matrix(rnorm(140), ncol = 7))
  w3 <- pv_pairs(wide)
  expect_length(w3$x$variables, 6)
})

test_that("pairs validates its columns", {
  f25 <- pairs25()
  expect_error(pv_pairs(f25, columns = c("resource_index", "nope")),
               "not in `data`")
  expect_error(pv_pairs(f25, columns = c("resource_index", "municipality")),
               "numeric")
  expect_error(pv_pairs(f25, columns = "resource_index"), "at least 2")
  expect_error(
    pv_pairs(f25, columns = c("resource_index", "resource_index")),
    "more than once")
  wide <- as.data.frame(matrix(rnorm(180), ncol = 9))
  expect_error(pv_pairs(wide, columns = names(wide)), "up to 8")
  # too few numeric columns to fall back on
  expect_error(pv_pairs(data.frame(a = letters[1:3], b = 1:3)),
               "at least 2")
  expect_error(pv_pairs(data.frame(a = c(NA, NA, 1), b = 1:3),
                        columns = c("a", "b")),
               "at least 2 non-missing")
})

test_that("pairs maps color and label, with the 8-level palette cap", {
  f25 <- pairs25()
  w <- pv_pairs(f25, columns = fiscal_cols, color = "side",
                label = "municipality")
  expect_equal(names(w$x$data), c("p1", "p2", "p3", "series", "label"))
  expect_equal(w$x$data$series, f25$side)
  expect_equal(w$x$data$label, f25$municipality)
  expect_error(pv_pairs(pv_fiscal, columns = fiscal_cols,
                        color = "municipality"), "Other")
  # 8 levels (the years) squeak through
  pv_fiscal$year_chr <- as.character(pv_fiscal$year)
  w2 <- pv_pairs(pv_fiscal, columns = fiscal_cols, color = "year_chr")
  expect_length(unique(w2$x$data$series), 8)
})

test_that("pairs colour cap follows the active theme's palette", {
  on.exit(pv_reset_theme())
  six_years <- sort(unique(pv_fiscal$year))[1:6]
  f <- subset(pv_fiscal, year %in% six_years)
  f$year_chr <- as.character(f$year)
  # Six colour levels fit the packaged theme's 8 slots...
  expect_pvchart(pv_pairs(f, columns = fiscal_cols, color = "year_chr"),
                 "pairs")
  # ...but not the paper theme's 5: refuse rather than recycle colours.
  pv_set_theme(pv_theme_paper())
  expect_error(pv_pairs(f, columns = fiscal_cols, color = "year_chr"),
               "6 levels but the active theme's palette has 5 colours")
  pv_reset_theme()
  expect_pvchart(pv_pairs(f, columns = fiscal_cols, color = "year_chr"),
                 "pairs")
})

test_that("pairs names constant columns instead of base R's sd warning", {
  # year is constant in the 2025 subset, and the default column pick
  # includes it - the natural way to hit a zero-variance column.
  f25 <- subset(pv_fiscal, year == 2025)
  expect_warning(
    w <- pv_pairs(f25),
    "`year` is constant in this data; its correlations are undefined.",
    fixed = TRUE)
  # Every off-diagonal coefficient touching year travels as NA - the
  # matrix cells already draw an em dash there.
  vars <- vapply(w$x$variables, function(v) v$name, character(1))
  expect_equal(vars[1], "year")
  expect_true(all(is.na(w$x$cor[[1]][-1])))
  # Coefficients between the varying columns are untouched.
  cm <- suppressWarnings(
    stats::cor(f25[vars], use = "pairwise.complete.obs"))
  expect_equal(w$x$cor[[3]], unname(cm[3, ]))
  # One house-worded warning; the base one never leaks.
  warns <- character()
  withCallingHandlers(pv_pairs(f25), warning = function(cnd) {
    warns <<- c(warns, conditionMessage(cnd))
    invokeRestart("muffleWarning")
  })
  expect_length(warns, 1)
  expect_false(any(grepl("standard deviation", warns)))
  # Two constant columns are named together, verbs matching.
  f25$flat <- 7
  expect_warning(
    pv_pairs(f25, columns = c("year", "flat", "resource_index")),
    "`year` and `flat` are constant in this data; their correlations are undefined.",
    fixed = TRUE)
})

test_that("pairs computes spearman and kendall when asked", {
  f25 <- utils::head(pairs25(), 40)
  for (m in c("spearman", "kendall")) {
    w <- pv_pairs(f25, columns = fiscal_cols, method = m)
    cm <- stats::cor(f25[fiscal_cols], use = "pairwise.complete.obs",
                     method = m)
    expect_equal(w$x$cor[[1]], unname(cm[1, ]))
    expect_equal(w$x$method, m)
  }
  expect_equal(
    pv_pairs(f25, columns = fiscal_cols,
             method = "spearman")$x$methodLabel,
    "Spearman rank correlation")
  expect_equal(
    pv_pairs(f25, columns = fiscal_cols,
             method = "kendall")$x$methodLabel,
    "Kendall rank correlation")
  expect_error(pv_pairs(f25, columns = fiscal_cols, method = "cosine"),
               "should be one of")
})

test_that("pairs warns once about incomplete rows and keeps them pairwise", {
  f25 <- pairs25()
  f25$resource_index[1:3] <- NA
  f25$equalization_chf[3:5] <- NA
  expect_warning(w <- pv_pairs(f25, columns = fiscal_cols),
                 "5 row\\(s\\) have missing values")
  # nothing is dropped: every row stays, gaps travel as nulls
  expect_equal(nrow(w$x$data), nrow(f25))
  # correlations are pairwise-complete, matching stats::cor exactly
  cm <- stats::cor(f25[fiscal_cols], use = "pairwise.complete.obs")
  for (i in 1:3) expect_equal(w$x$cor[[i]], unname(cm[i, ]))
  # the pairwise counts state what each coefficient rests on
  expect_equal(w$x$n[[2]][3], nrow(f25) - 5)
  expect_equal(w$x$n[[1]][2], nrow(f25) - 3)
  # bins are computed on each column's own non-missing values
  expect_equal(w$x$variables[[2]]$bins,
               pv_histbins(f25$resource_index))
})

test_that("pairs drops rows with a missing colour group, with a warning", {
  f25 <- pairs25()
  f25$side[2] <- NA
  expect_warning(w <- pv_pairs(f25, columns = fiscal_cols, color = "side"),
                 "Dropped 1 row\\(s\\) with missing `side` values")
  expect_equal(nrow(w$x$data), nrow(f25) - 1)
  # the statistics run on what is drawn: the remaining rows
  kept <- f25[!is.na(f25$side), ]
  cm <- stats::cor(kept[fiscal_cols], use = "pairwise.complete.obs")
  expect_equal(w$x$cor[[1]], unname(cm[1, ]))
})

test_that("pairs shares the standard chart options and validation", {
  f25 <- pairs25()
  expect_error(pv_pairs(f25, columns = fiscal_cols, mode = "sepia"),
               "`mode` must be")
  expect_error(pv_pairs(f25, columns = fiscal_cols, duration = -1),
               "non-negative")
  expect_error(pv_pairs(f25[0, ], columns = fiscal_cols), "no rows")
  expect_error(pv_pairs("nope"), "data frame")
  w <- pv_pairs(f25, columns = fiscal_cols, title = "T", subtitle = "S",
                source = "Src", mode = "dark")
  expect_equal(w$x$title, "T")
  expect_equal(w$x$subtitle, "S")
  expect_equal(w$x$source, "Src")
  expect_equal(w$x$mode, "dark")
})

test_that("pairs alt text names the variables and the strongest pair", {
  w <- pv_pairs(pairs25(), columns = fiscal_cols, title = "Fiscal pairs")
  expect_match(pv_alt_text(w), "^A scatterplot matrix titled")
  expect_match(pv_alt_text(w), "Fiscal pairs")
  expect_match(pv_alt_text(w), fiscal_cols[1], fixed = TRUE)
  expect_match(pv_alt_text(w), "strongest Pearson correlation")
  expect_equal(w$x$alt, pv_alt_text(w))
})

# The same render contract as test-render.R: pv_save() through real
# headless Chrome must finish with no warning (a warning means the
# renderer threw), and the PNG must be too big to be a blank capture.

test_that("pairs renders without JavaScript errors", {
  render_skip_if_no_chrome()
  w <- pv_pairs(pv_fiscal, title = "How the fiscal measures move together")
  path <- file.path(render_out_dir(), "pairs.png")
  expect_no_warning(pv_save(w, path, quiet = TRUE))
  expect_true(file.exists(path))
  expect_gt(file.size(path), 20000)
  render_publish(path)
})

test_that("coloured spearman pairs renders without JavaScript errors", {
  render_skip_if_no_chrome()
  f25 <- pairs25()
  w <- pv_pairs(f25, columns = fiscal_cols, color = "side",
                label = "municipality", method = "spearman",
                title = "Receivers and contributors, pair by pair")
  path <- file.path(render_out_dir(), "pairs_color.png")
  expect_no_warning(pv_save(w, path, quiet = TRUE))
  expect_true(file.exists(path))
  expect_gt(file.size(path), 20000)
  render_publish(path)
})
