#' polyviz colour tokens
#'
#' The palette polyviz uses everywhere — ggplot2 themes and D3 widgets alike.
#' The categorical slots are ordered so that adjacent pairs stay separable
#' under the common colour-vision deficiencies; the order is part of the
#' design and series are always assigned in slot order, never cycled.
#'
#' @format A list with elements `categorical` (light/dark, 8 hex colours
#'   each), `sequential` (11-step single-hue blue ramp), `diverging`
#'   (blue/red poles with a neutral grey midpoint), and `ink` (chart chrome:
#'   surface, text, grid, baseline for both modes).
#' @export
pv_colors <- list(
  categorical = list(
    light = c("#2a78d6", "#eb6834", "#1baf7a", "#eda100",
              "#e87ba4", "#008300", "#4a3aa7", "#e34948"),
    dark  = c("#3987e5", "#d95926", "#199e70", "#c98500",
              "#d55181", "#008300", "#9085e9", "#e66767")
  ),
  sequential = c("#cde2fb", "#b7d3f6", "#9ec5f4", "#86b6ef", "#6da7ec",
                 "#5598e7", "#3987e5", "#2a78d6", "#256abf", "#1c5cab",
                 "#184f95", "#104281", "#0d366b"),
  diverging = list(
    low = "#2a78d6", mid_light = "#f0efec", mid_dark = "#383835",
    high = "#e34948"
  ),
  ink = list(
    light = list(surface = "#fcfcfb", primary = "#0b0b0b",
                 secondary = "#52514e", muted = "#898781",
                 grid = "#e1e0d9", baseline = "#c3c2b7"),
    dark  = list(surface = "#1a1a19", primary = "#ffffff",
                 secondary = "#c3c2b7", muted = "#898781",
                 grid = "#2c2c2a", baseline = "#383835")
  )
)

#' Categorical palette
#'
#' Returns the first `n` categorical colours in their fixed, CVD-safe order.
#'
#' @param n Number of colours (1–8).
#' @param mode `"light"` or `"dark"` — the surface the chart renders on.
#' @return Character vector of `n` hex colours.
#' @examples
#' pv_palette(3)
#' @export
pv_palette <- function(n = 8, mode = c("light", "dark")) {
  mode <- match.arg(mode)
  pal <- pv_colors$categorical[[mode]]
  if (n < 1 || n > length(pal)) {
    rlang::abort(sprintf(
      "`n` must be between 1 and %d. With more than %d series, fold the extras into an \"Other\" category or facet the chart instead of adding colours.",
      length(pal), length(pal)))
  }
  pal[seq_len(n)]
}

#' ggplot2 scales using the polyviz palette
#'
#' Discrete colour/fill scales that assign the categorical slots in fixed
#' order.
#'
#' @param mode `"light"` or `"dark"`.
#' @param ... Passed on to [ggplot2::discrete_scale()].
#' @return A ggplot2 scale object.
#' @export
scale_colour_pv <- function(mode = "light", ...) {
  ggplot2::discrete_scale(
    "colour",
    palette = function(n) pv_palette(n, mode = mode), ...)
}

#' @rdname scale_colour_pv
#' @export
scale_color_pv <- scale_colour_pv

#' @rdname scale_colour_pv
#' @export
scale_fill_pv <- function(mode = "light", ...) {
  ggplot2::discrete_scale(
    "fill",
    palette = function(n) pv_palette(n, mode = mode), ...)
}
