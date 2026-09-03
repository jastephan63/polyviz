# Built-in themes. Each one is a complete, pv_colors-shaped token bundle
# (plus a font stack) that pv_set_theme() applies wholesale - so a theme
# here goes through exactly the same accessibility gate as a user's own
# palette. The first and so far only resident is the paper theme.

#' A print-first theme for the printed page
#'
#' A complete theme tuned for figures that end up on paper: a pure white
#' surface, near-black neutral inks, and a five-colour categorical
#' palette whose slots climb a greyscale luminance ladder — so series
#' can still be told apart even when the journal prints your figure in
#' greyscale. Apply it with `pv_set_theme(pv_theme_paper())`.
#'
#' The categorical slots run dark to light (royal blue, crimson,
#' cornflower, rose, gold) with adjacent slots about 10 L* apart in CIE
#' lightness — the widest even ladder that fits inside
#' [pv_check_palette()]'s lightness band — while still passing the full
#' colour-vision-deficiency gauntlet in colour, in both modes. Five slots
#' is deliberate: a greyscale ladder wide enough to survive print cannot
#' honestly carry more. With more than five series, fold the extras into
#' an "Other" category or facet the chart.
#'
#' The sequential and diverging ramps are re-anchored to the same royal
#' blue and crimson so a mixed figure panel reads as one family, and the
#' chart chrome swaps polyviz's warm paper tint for neutral greys on pure
#' white — what a printing press actually reproduces.
#'
#' This is a light-first, print-first choice. A dark twin is still
#' derived (so on-screen dark mode keeps working), but the design target
#' is the printed page; leave charts in light mode when exporting for
#' print.
#'
#' For belt-and-braces greyscale safety, pair the luminance ladder with
#' the [pv_textures()] modifier, which adds hatching as a second,
#' colour-free encoding on filled marks.
#'
#' @param serif Set the widget font stack to a serif face
#'   (`"Source Serif 4", Georgia, "Times New Roman", serif`) for journals
#'   that set figures in serif. `FALSE` (default) keeps the packaged
#'   Inter stack.
#' @return An object of class `"pv_theme"`: a complete token bundle
#'   (categorical palette, sequential and diverging ramps, ink chrome,
#'   font) ready to hand to [pv_set_theme()].
#' @examples
#' pv_set_theme(pv_theme_paper())
#' sales <- aggregate(revenue ~ region, pv_sales, sum)
#' pv_bar(sales, x = "region", y = "revenue", title = "Ready for print")
#' pv_reset_theme()
#'
#' \dontrun{
#' # Full greyscale safety: the luminance ladder plus hatched fills
#' pv_set_theme(pv_theme_paper(serif = TRUE))
#' w <- pv_bar(sales, x = "region", y = "revenue")
#' pv_textures(w, TRUE)
#' }
#' @export
pv_theme_paper <- function(serif = FALSE) {
  if (!is.logical(serif) || length(serif) != 1 || is.na(serif)) {
    rlang::abort("`serif` must be TRUE or FALSE.")
  }
  structure(list(
    categorical = list(
      # Dark to light in even ~10 L* greyscale steps (CIE lightness
      # 33.1, 42.9, 52.8, 62.8, 72.7), hues alternating cool/warm so
      # adjacent pairs also keep a blue-yellow difference that survives
      # protanopia and deuteranopia. The dark twins are derived by
      # pv_set_theme(), like any user palette's.
      light = c("#2243b2", "#b03d4d", "#517ec9", "#de7a8b", "#d5ae34")
    ),
    sequential = list(
      # The packaged ramp's lightness/chroma trajectory, re-anchored on
      # the categorical royal blue, with the low end receding toward the
      # pure white surface instead of warm paper.
      light = c("#dce9fd", "#c4d5f1", "#adc1e3", "#98aed3", "#829bc5",
                "#6d89b5", "#5976a7", "#456498", "#315289", "#254271",
                "#1b3259"),
      dark  = c("#172233", "#243145", "#314059", "#3e506e", "#4c6183",
                "#5b7299", "#6984b0", "#7896c7", "#88a9de", "#97bcf7",
                "#b3cffc")
    ),
    diverging = list(
      # Same poles as the packaged scale in depth and weight, on the
      # theme's own blue and crimson hues; the midpoint drops the warm
      # tint for a neutral grey that vanishes into the white page.
      light = list(low = "#3d67ae", mid = "#e8e8e8", high = "#b84453"),
      dark  = list(low = "#759fe3", mid = "#373737", high = "#cc5e69")
    ),
    ink = list(
      # Pure white and neutral near-black: on press there is no warm
      # paper tint to match, so every grey is a plain grey.
      light = list(surface = "#ffffff", primary = "#111111",
                   secondary = "#474747", muted = "#767676",
                   grid = "#ececec", baseline = "#c6c6c6",
                   tooltipBg = "#ffffff", tooltipText = "#111111",
                   tooltipBorder = "#d9d9d9"),
      dark  = list(surface = "#161616", primary = "#f2f2f2",
                   secondary = "#c4c4c4", muted = "#8a8a8a",
                   grid = "#2a2a2a", baseline = "#454545",
                   tooltipBg = "#222222", tooltipText = "#f2f2f2",
                   tooltipBorder = "#454545")
    ),
    font = if (serif) {
      '"Source Serif 4", Georgia, "Times New Roman", serif'
    } else {
      pv_font_stack()
    }
  ), class = "pv_theme")
}
