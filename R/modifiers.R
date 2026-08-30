# Pipe-able chart modifiers. Every chart function returns an htmlwidget,
# and the functions in this file take one, add something to its payload,
# and hand it back - so layers read as a sentence:
#
#   pv_scatter(df, "x", "y") |> pv_trend("loess") |> pv_annotate(...)
#
# The drawing itself happens in JavaScript (pv.drawAnnotations and
# pv.drawTrends in inst/htmlwidgets/lib/pv-common/pv-common.js); this
# side only shapes and validates the payload those helpers read.

# Every modifier starts here: only a polyviz chart can be modified. The
# message names the modifier so a failure deep in a pipe still reads.
check_pv_widget <- function(w, fn) {
  if (!inherits(w, "htmlwidget") ||
      !identical(attr(w, "package"), "polyviz")) {
    rlang::abort(sprintf(
      "`w` must be a polyviz chart - pipe one into %s().", fn))
  }
  invisible(w)
}

# One scalar annotation value, coerced to what the chart's axes actually
# speak: Dates become the ISO strings ("2025-01-01") the charts ship and
# parse, numbers stay numbers, and anything else becomes a category
# string. The JavaScript side feeds these straight into the axis scale.
ann_value <- function(v, name) {
  if (length(v) != 1 || is.na(v)) {
    rlang::abort(sprintf("`%s` must be a single non-missing value.", name))
  }
  if (inherits(v, "Date")) return(format(v, "%Y-%m-%d"))
  if (is.numeric(v)) return(as.numeric(v))
  as.character(v)
}

# An optional single string (a label or a colour): NULL passes through,
# anything else must be one non-missing string.
ann_string <- function(v, name) {
  if (is.null(v)) return(NULL)
  if (!is.character(v) || length(v) != 1 || is.na(v)) {
    rlang::abort(sprintf("`%s` must be a single string or NULL.", name))
  }
  v
}

# One numeric scalar (a pixel offset), defaulting cleanly.
ann_number <- function(v, name) {
  if (!is.numeric(v) || length(v) != 1 || is.na(v)) {
    rlang::abort(sprintf("`%s` must be a single number.", name))
  }
  as.numeric(v)
}

# Builds the plain list that travels to JavaScript, with NULL fields
# dropped so the JSON stays exactly the shape pv.drawAnnotations reads.
ann_object <- function(...) {
  fields <- list(...)
  fields <- fields[!vapply(fields, is.null, logical(1))]
  structure(fields, class = "pv_annotation")
}

#' Annotation layers for polyviz charts
#'
#' Small builder functions for the objects [pv_annotate()] accepts:
#' `pv_hline()` and `pv_vline()` draw dashed reference lines across the
#' plot, `pv_band()` shades a region behind the data, and `pv_note()`
#' places a short text label at a data position, with an optional leader
#' line when it is nudged away from its anchor.
#'
#' Positions are given in data units. `Date` values are converted to the
#' ISO strings the charts use internally, and category values (say a bar
#' chart's category name) pass through the axis scale unchanged, so
#' `pv_vline("Luzern")` works as well as `pv_vline(100)`.
#'
#' @param at Where the reference line crosses its axis: a y value for
#'   `pv_hline()`, an x value for `pv_vline()`.
#' @param label Optional short text drawn next to the line or inside the
#'   band.
#' @param color Optional line/label colour. `NULL` (default) uses the
#'   theme's secondary ink, which is the right choice almost always -
#'   reference lines should recede, not compete with the data.
#' @param x0,x1 Both ends of a vertical band on the x axis.
#' @param y0,y1 Both ends of a horizontal band on the y axis. Give
#'   exactly one pair - `x0`+`x1` or `y0`+`y1`, never both.
#' @param x,y Data position the note points at.
#' @param text The note's text. Keep it short - it is drawn on the chart,
#'   not in a tooltip.
#' @param dx,dy Pixel offset of the text from its anchor point. When
#'   either is non-zero, a thin leader line connects text and anchor.
#' @return An annotation object to pass to [pv_annotate()].
#' @examples
#' f25 <- subset(pv_fiscal, year == 2025)
#' pv_scatter(f25, x = "resource_index", y = "equalization_chf") |>
#'   pv_annotate(pv_vline(100, label = "cantonal average"),
#'               pv_band(x0 = 36, x1 = 100))
#' @name annotations
NULL

#' @rdname annotations
#' @export
pv_hline <- function(at, label = NULL, color = NULL) {
  ann_object(type = "hline", at = ann_value(at, "at"),
             label = ann_string(label, "label"),
             color = ann_string(color, "color"))
}

#' @rdname annotations
#' @export
pv_vline <- function(at, label = NULL, color = NULL) {
  ann_object(type = "vline", at = ann_value(at, "at"),
             label = ann_string(label, "label"),
             color = ann_string(color, "color"))
}

#' @rdname annotations
#' @export
pv_band <- function(x0 = NULL, x1 = NULL, y0 = NULL, y1 = NULL,
                    label = NULL) {
  has_x <- !is.null(x0) || !is.null(x1)
  has_y <- !is.null(y0) || !is.null(y1)
  # A band shades between two positions on ONE axis; the JavaScript side
  # decides horizontal vs vertical by which pair arrived.
  if (has_x == has_y) {
    rlang::abort(paste(
      "A band needs exactly one axis pair: `x0` and `x1` for a vertical",
      "band, or `y0` and `y1` for a horizontal one."))
  }
  if (has_x && (is.null(x0) || is.null(x1))) {
    rlang::abort("A vertical band needs both `x0` and `x1`.")
  }
  if (has_y && (is.null(y0) || is.null(y1))) {
    rlang::abort("A horizontal band needs both `y0` and `y1`.")
  }
  if (has_x) {
    ann_object(type = "band", x0 = ann_value(x0, "x0"),
               x1 = ann_value(x1, "x1"), label = ann_string(label, "label"))
  } else {
    ann_object(type = "band", y0 = ann_value(y0, "y0"),
               y1 = ann_value(y1, "y1"), label = ann_string(label, "label"))
  }
}

#' @rdname annotations
#' @export
pv_note <- function(x, y, text, dx = 0, dy = 0) {
  if (!is.character(text) || length(text) != 1 || is.na(text) ||
      !nzchar(text)) {
    rlang::abort("`text` must be a single non-empty string.")
  }
  ann_object(type = "label", x = ann_value(x, "x"), y = ann_value(y, "y"),
             text = text, dx = ann_number(dx, "dx"),
             dy = ann_number(dy, "dy"))
}

#' Annotate a cartesian polyviz chart
#'
#' Adds reference lines, shaded bands, and text notes to a chart that has
#' x/y axes. Annotations are drawn in data coordinates, so they stay put
#' when the chart resizes: bands go underneath the data, lines and notes
#' on top of it. Repeated calls append, so layers can be built up one
#' pipe step at a time.
#'
#' Works on the cartesian charts: bar, line, scatter, histogram, area,
#' and beeswarm. Charts without x/y axes (chord, sunburst, treemap, ...)
#' have nowhere to anchor a reference line, and asking for one errors.
#'
#' @param w A polyviz chart (from [pv_bar()], [pv_line()],
#'   [pv_scatter()], [pv_histogram()], [pv_area()], or [pv_beeswarm()]).
#' @param ... Annotation objects built with [pv_hline()], [pv_vline()],
#'   [pv_band()], or [pv_note()].
#' @return The chart, with the annotations attached - ready for more
#'   pipe steps.
#' @examples
#' f25 <- subset(pv_fiscal, year == 2025)
#' pv_scatter(f25, x = "resource_index", y = "equalization_chf",
#'            label = "municipality") |>
#'   pv_annotate(pv_vline(100, label = "cantonal average"),
#'               pv_note(65.9, 23278722, "Emmen", dx = 14, dy = 10))
#' @export
pv_annotate <- function(w, ...) {
  check_pv_widget(w, "pv_annotate")
  cartesian <- c("bar", "line", "scatter", "histogram", "area", "beeswarm")
  if (!(w$x$type %in% cartesian)) {
    rlang::abort(sprintf(paste(
      "pv_annotate() places marks on x/y axes, which a %s chart does not",
      "have. It works on: %s."),
      w$x$type, paste(cartesian, collapse = ", ")))
  }
  anns <- list(...)
  if (!length(anns)) {
    rlang::abort(paste(
      "Nothing to annotate - pass objects from pv_hline(), pv_vline(),",
      "pv_band(), or pv_note()."))
  }
  ok <- vapply(anns, inherits, logical(1), "pv_annotation")
  if (!all(ok)) {
    rlang::abort(paste(
      "Every argument after the chart must be an annotation built with",
      "pv_hline(), pv_vline(), pv_band(), or pv_note()."))
  }
  # Strip the S3 class so the payload serialises as plain JSON objects,
  # and append - a second pv_annotate() call adds, never replaces.
  w$x$annotations <- c(w$x$annotations, lapply(anns, unclass))
  w
}
