#' Missingness plot
#'
#' Horizontal bars of percent-missing per column, worst first. Columns with
#' no missing values are kept (at zero) so absence of a problem is visible
#' too.
#'
#' @param data A data frame.
#' @param mode `"light"` or `"dark"`.
#' @return A ggplot object.
#' @examples
#' pv_plot_missing(airquality)
#' @export
pv_plot_missing <- function(data, mode = "light") {
  pct <- vapply(data, function(x) 100 * mean(is.na(x)), numeric(1))
  df <- data.frame(variable = names(pct), pct = unname(pct))
  df$variable <- stats::reorder(df$variable, df$pct)
  ink <- pv_colors$ink[[mode]]

  ggplot2::ggplot(df, ggplot2::aes(x = .data$pct, y = .data$variable)) +
    ggplot2::geom_col(fill = pv_palette(1, mode), width = 0.62) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.1f%%", .data$pct)),
      hjust = -0.15, size = 3.2, colour = ink$secondary) +
    ggplot2::scale_x_continuous(
      limits = c(0, max(5, max(df$pct) * 1.18)),
      expand = ggplot2::expansion(mult = c(0, 0.02))) +
    ggplot2::labs(title = "Missing values by column",
                  x = "% missing", y = NULL) +
    theme_polyviz(mode = mode) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                   axis.line.x = ggplot2::element_blank())
}

#' Correlation heatmap
#'
#' Pairwise correlations of the numeric columns, on a diverging blue–red
#' scale with a neutral midpoint at zero. Pearson by default; the
#' rank-based alternatives are available through `method` and are named
#' in the plot's subtitle.
#'
#' @param data A data frame (only numeric columns are used).
#' @param mode `"light"` or `"dark"`.
#' @param method Correlation coefficient, as in [stats::cor()]:
#'   `"pearson"` (default), `"spearman"`, or `"kendall"`. The default
#'   Pearson plot keeps its usual look; the rank-based methods add a
#'   subtitle naming the method, so the plot says which coefficient the
#'   cells hold.
#' @return A ggplot object.
#' @examples
#' pv_plot_corr(mtcars)
#' pv_plot_corr(mtcars, method = "spearman")
#' @export
pv_plot_corr <- function(data, mode = "light",
                         method = c("pearson", "spearman", "kendall")) {
  method <- match.arg(method)
  num <- data[vapply(data, is.numeric, logical(1))]
  if (ncol(num) < 2) {
    rlang::abort("Need at least two numeric columns for a correlation plot.")
  }
  cm <- stats::cor(num, use = "pairwise.complete.obs", method = method)
  # ggplot wants one row per cell, so unroll the matrix into long form.
  # Reversing the y side puts the diagonal top-left to bottom-right, the
  # way people expect to read a correlation matrix.
  df <- expand.grid(x = colnames(cm), y = rev(colnames(cm)),
                    KEEP.OUT.ATTRS = FALSE)
  df$r <- cm[cbind(as.character(df$y), as.character(df$x))]
  ink <- pv_colors$ink[[mode]]
  div <- pv_colors$diverging[[mode]]
  midpoint <- div$mid

  # Pearson is the coefficient every correlation matrix is assumed to
  # show, so the default plot carries no extra label; the rank-based
  # methods announce themselves in a subtitle so nobody mistakes a rho
  # or tau for a plain r. (switch() returns NULL for "pearson".)
  subtitle <- switch(method,
                     spearman = "Spearman rank correlation",
                     kendall = "Kendall rank correlation")

  p <- ggplot2::ggplot(df, ggplot2::aes(.data$x, .data$y, fill = .data$r)) +
    ggplot2::geom_tile(colour = ink$surface, linewidth = 1.5) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.2f", .data$r)),
      size = 2.9, colour = ink$primary) +
    ggplot2::scale_fill_gradient2(
      low = div$low, mid = midpoint, high = div$high,
      limits = c(-1, 1), name = "r") +
    ggplot2::coord_fixed() +
    ggplot2::labs(title = "Correlation matrix", x = NULL, y = NULL) +
    theme_polyviz(mode = mode) +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   axis.line.x = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  if (!is.null(subtitle)) {
    p <- p + ggplot2::labs(subtitle = subtitle)
  }
  p
}
