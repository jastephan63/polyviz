#' polyviz ggplot2 theme
#'
#' A quiet chart chrome: recessive hairline gridlines, a single baseline
#' axis, muted axis text, and no visual furniture competing with the data.
#'
#' @param base_size Base font size in points.
#' @param mode `"light"` or `"dark"` — picks the matching surface and ink.
#' @return A ggplot2 theme object.
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg)) + geom_point() + theme_polyviz()
#' @export
theme_polyviz <- function(base_size = 12, mode = c("light", "dark")) {
  mode <- match.arg(mode)
  ink <- pv_colors$ink[[mode]]
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.background   = ggplot2::element_rect(fill = ink$surface, colour = NA),
      panel.background  = ggplot2::element_rect(fill = ink$surface, colour = NA),
      panel.grid.major  = ggplot2::element_line(colour = ink$grid, linewidth = 0.3),
      panel.grid.minor  = ggplot2::element_blank(),
      axis.line.x       = ggplot2::element_line(colour = ink$baseline, linewidth = 0.4),
      axis.ticks        = ggplot2::element_blank(),
      axis.text         = ggplot2::element_text(colour = ink$muted),
      axis.title        = ggplot2::element_text(colour = ink$secondary),
      plot.title        = ggplot2::element_text(colour = ink$primary, face = "bold"),
      plot.subtitle     = ggplot2::element_text(colour = ink$secondary),
      plot.caption      = ggplot2::element_text(colour = ink$muted, size = base_size * 0.75),
      legend.text       = ggplot2::element_text(colour = ink$secondary),
      legend.title      = ggplot2::element_text(colour = ink$secondary),
      strip.text        = ggplot2::element_text(colour = ink$secondary, face = "bold"),
      plot.title.position = "plot"
    )
}
