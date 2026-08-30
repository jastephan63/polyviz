# Crosstalk linking. pv_link() attaches a crosstalk selection group to a
# chart's payload; the JavaScript side (pvchart.js) opens a
# SelectionHandle on that group, re-renders with ctx.selected holding the
# selected keys, and renderers dim everything else via pv.keyOpacity.
# No Shiny server needed - crosstalk links widgets to each other right
# in a static HTML page or R Markdown document.

#' Link a chart into a crosstalk selection group
#'
#' Connects a polyviz chart to a [crosstalk::SharedData] object so that
#' selections made in one linked widget - another polyviz chart, a
#' `DT::datatable()`, a `crosstalk::filter_select()` - highlight the
#' matching marks here, and vice versa. The link works in static HTML
#' (R Markdown, Quarto, `htmltools::browsable()`), no Shiny required.
#'
#' The correspondence is by row: mark *i* of the chart is matched with
#' row *i* of the shared data, so `sd` must wrap the very table (or one
#' row-aligned with it) that built the chart. Charts that respond to a
#' linked selection by dimming unselected marks: scatter, beeswarm, bar,
#' choropleth, force, and treemap. Every other chart type carries the
#' link without complaint and simply ignores it.
#'
#' @param w A polyviz chart.
#' @param sd A [crosstalk::SharedData] object wrapping a data frame with
#'   exactly as many rows as the data the chart was built from.
#' @return The chart, carrying the selection keys, the group name, and
#'   the crosstalk JavaScript dependencies - ready for more pipe steps.
#' @examples
#' f25 <- subset(pv_fiscal, year == 2025)
#' sd <- crosstalk::SharedData$new(f25)
#' pv_scatter(f25, x = "resource_index", y = "equalization_chf",
#'            label = "municipality") |>
#'   pv_link(sd)
#' @export
pv_link <- function(w, sd) {
  check_pv_widget(w, "pv_link")
  if (!inherits(sd, "SharedData")) {
    rlang::abort(
      "`sd` must be a crosstalk::SharedData object (crosstalk::SharedData$new(data)).")
  }
  # Find the chart's row-level table - the thing whose rows the keys must
  # line up with. Most charts ship it as $data; the force network's rows
  # are its nodes; tree-shaped charts aggregate rows away entirely.
  rows <- if (identical(w$x$type, "force")) w$x$nodes else w$x$data
  if (!is.data.frame(rows)) {
    rlang::abort(sprintf(paste(
      "A %s chart's payload has no row-level table to line selection",
      "keys up with, so pv_link() cannot connect it."), w$x$type))
  }
  keys <- sd$key()
  if (length(keys) != nrow(rows)) {
    rlang::abort(sprintf(paste(
      "pv_link() matches mark i of the chart with row i of the shared",
      "data, so the two must have the same number of rows - the chart",
      "was built from %d rows, but `sd` wraps %d. Pass the same data",
      "frame (or one row-aligned with it) to both."),
      nrow(rows), length(keys)))
  }
  # I() keeps a single key an array in JSON - the JavaScript side always
  # expects a list.
  w$x$ctKeys <- I(as.character(keys))
  w$x$ctGroup <- sd$groupName()
  w$dependencies <- c(w$dependencies, crosstalk::crosstalkLibs())
  w
}
