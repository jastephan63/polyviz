#' polyviz colour tokens
#'
#' The palette every polyviz chart draws from — ggplot2 themes and D3
#' widgets alike. The hues are anchored on The Economist's published web
#' chart palette, then re-stepped in OKLCH so that every colour sits in a
#' legal lightness band for its surface, and the slot ORDER was chosen by
#' exhaustive search so that adjacent pairs stay distinguishable under the
#' common colour-vision deficiencies in both light and dark mode (worst
#' adjacent pair: Delta E 11.6 light / 10.2 dark, target 8). The first
#' three slots additionally survive the stricter every-pair-adjacent test,
#' which is why scatter plots cap colour groups at three.
#'
#' Series always take colours in slot order — the order is part of the
#' accessibility guarantee, never cosmetic.
#'
#' @format A list with elements `categorical` (light/dark, 8 hex colours
#'   each, same hue per slot across modes), `sequential` (light/dark
#'   11-step single-hue blue ramps, low to high, with the low end receding
#'   toward the surface), `diverging` (blue/red poles with a warm neutral
#'   midpoint, per mode), and `ink` (chart chrome: warm paper surface,
#'   near-black text, hairline grid, per mode).
#' @export
pv_colors <- list(
  categorical = list(
    light = c("#006ba2", "#db444b", "#3ebcd2", "#b2b837",
              "#a25a81", "#dca61c", "#1b9c8c", "#d4a75d"),
    dark  = c("#006ba2", "#db444b", "#11a2b8", "#939807",
              "#a25a81", "#b78807", "#1b9c8c", "#b4883c")
  ),
  sequential = list(
    light = c("#cbe2f4", "#b3cfe5", "#9cbdd7", "#86acc9", "#6f9abb",
              "#5989ac", "#42789e", "#2a6790", "#095783", "#00466c",
              "#013655"),
    dark  = c("#142634", "#1f3546", "#2a4559", "#35566d", "#426782",
              "#4e7897", "#5b8aad", "#689dc4", "#76afda", "#83c3f2",
              "#9fd5fe")
  ),
  diverging = list(
    light = list(low = "#026fa8", mid = "#efece4", high = "#b94548"),
    dark  = list(low = "#5aa6dd", mid = "#383734", high = "#cd5f5f")
  ),
  ink = list(
    light = list(surface = "#fbf9f5", primary = "#161511",
                 secondary = "#57544b", muted = "#8b8779",
                 grid = "#eae6dd", baseline = "#c9c4b6",
                 tooltipBg = "#ffffff", tooltipText = "#161511",
                 tooltipBorder = "#ddd8cc"),
    dark  = list(surface = "#1b1a18", primary = "#f6f4ef",
                 secondary = "#c7c3b8", muted = "#8b8779",
                 grid = "#2e2d29", baseline = "#45433d",
                 tooltipBg = "#262522", tooltipText = "#f6f4ef",
                 tooltipBorder = "#45433d")
  )
)

# The CSS font stack every widget renders with. Inter is bundled with the
# package (SIL Open Font License); the rest of the stack is the fallback
# when the font file can't load. Kept in one place so the R side and the
# JavaScript side always agree.
pv_font_stack <- function() {
  paste0('"InterVariable", "Inter", system-ui, -apple-system, ',
         '"Segoe UI", sans-serif')
}

#' Categorical palette
#'
#' Returns the first `n` categorical colours in their fixed,
#' colourblind-checked order.
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
