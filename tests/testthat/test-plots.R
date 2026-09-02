# pv_plot_corr builds its matrix with stats::cor, so the method tests
# work on a fixture where values and ranks tell different stories:
# v = 10^u is perfectly monotone (rank correlation exactly 1) but so
# convex that its Pearson r with u is only about 0.53, and w is a
# scrambled ranking that separates Spearman from Kendall as well.
corr_fixture <- function() {
  data.frame(u = 1:12, v = 10^(1:12),
             w = c(6, 1, 8, 3, 10, 5, 12, 7, 2, 9, 4, 11))
}

test_that("the correlation method is validated", {
  expect_error(pv_plot_corr(mtcars, method = "cosine"), "pearson")
  expect_error(pv_plot_corr(mtcars, method = NA), "character")
})

test_that("the default plot is still the plain Pearson one", {
  p_default <- pv_plot_corr(mtcars)
  p_pearson <- pv_plot_corr(mtcars, method = "pearson")
  expect_null(p_default$labels$subtitle)
  expect_identical(p_default$data, p_pearson$data)
  # same numbers stats::cor gives with no method argument at all
  m <- stats::cor(mtcars, use = "pairwise.complete.obs")
  expect_equal(p_default$data$r,
               m[cbind(as.character(p_default$data$y),
                       as.character(p_default$data$x))],
               ignore_attr = TRUE)
})

test_that("spearman and kendall produce their own matrices", {
  fix <- corr_fixture()
  # every cell of the plot must match stats::cor under the same method;
  # the plot data carries each cell's column names, so the whole matrix
  # can be looked up directly
  for (method in c("spearman", "kendall")) {
    p <- pv_plot_corr(fix, method = method)
    m <- stats::cor(fix, use = "pairwise.complete.obs", method = method)
    expect_equal(p$data$r,
                 m[cbind(as.character(p$data$y), as.character(p$data$x))],
                 ignore_attr = TRUE)
  }
  # the fixture really does split the methods: ranks agree perfectly on
  # u and v while the raw values do not come close
  sp <- stats::cor(fix$u, fix$v, method = "spearman")
  pe <- stats::cor(fix$u, fix$v)
  expect_equal(sp, 1)
  expect_gt(sp - pe, 0.4)
  # and the spearman plot carries the spearman number, not the pearson one
  p <- pv_plot_corr(fix, method = "spearman")
  uv <- p$data$r[p$data$x == "u" & p$data$y == "v"]
  expect_equal(uv, sp, ignore_attr = TRUE)
  expect_gt(abs(uv - pe), 0.4)
})

test_that("rank-based methods are named in the subtitle", {
  expect_match(pv_plot_corr(mtcars, method = "spearman")$labels$subtitle,
               "Spearman")
  expect_match(pv_plot_corr(mtcars, method = "kendall")$labels$subtitle,
               "Kendall")
})

test_that("fewer than two numeric columns is refused", {
  expect_error(pv_plot_corr(data.frame(a = 1:5, b = letters[1:5])),
               "two numeric columns")
})
