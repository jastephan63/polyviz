# Point-cloud charts: beeswarm. As everywhere in polyviz, this file only
# shapes the payload - the drawing (including the collision layout) lives
# in inst/htmlwidgets/lib/pv-renderers/points.js.

#' Interactive D3 beeswarm chart
#'
#' Every observation drawn as its own dot: position along the horizontal
#' axis is the value, and a collision layout (a d3 force simulation run to
#' completion before anything is drawn) nudges overlapping dots apart so
#' each one stays visible. Where a boxplot summarises and a histogram
#' bins, a beeswarm keeps the individuals — the right choice when single
#' cases have names worth hovering, up to a few hundred of them.
#'
#' With `group`, dots settle into one horizontal lane per level (max 8),
#' labelled on the left; lane labels may claim at most 45% of the chart
#' width — longer names are shortened with an ellipsis and stay complete
#' in the tooltip. Dot size adapts to the crowd: 5px up to 150 dots,
#' shrinking to 3px by 500. Past 800 dots the swarm stops reading as
#' individuals, so anything larger is refused with advice to aggregate.
#' Hovering a dot grows it and shows its label, group, and exact value.
#'
#' @param data A data frame.
#' @param value Name of the numeric column giving each dot's position.
#'   Rows with a missing value or group are dropped with a warning.
#' @param group Optional name of a grouping column (one lane per level,
#'   max 8, coloured by lane). Omit it for a single central swarm.
#' @param label Optional name of a column shown first in tooltips —
#'   the whole point of a beeswarm is that each dot is somebody.
#' @param xlab Value-axis title. `NULL` (the default) uses the `value`
#'   column name; `NA` or `""` suppresses it; any other string replaces
#'   it.
#' @inheritParams pv_bar
#' @return An htmlwidget.
#' @examples
#' f25 <- subset(pv_fiscal, year == 2025)
#' f25$side <- ifelse(f25$equalization_chf > 0,
#'                    "receives equalization", "contributes")
#' pv_beeswarm(f25, value = "resource_index", group = "side",
#'             label = "municipality",
#'             title = "Most municipalities sit below the average")
#' @export
pv_beeswarm <- function(data, value, group = NULL, label = NULL,
                        xlab = NULL,
                        title = NULL, subtitle = NULL, mode = "auto",
                        duration = 500, source = NULL, width = NULL,
                        height = NULL, elementId = NULL) {
  check_columns(data, list(value, group, label))
  check_nonempty(data)
  check_numeric_col(data, value)
  vals <- as.numeric(data[[value]])
  grp <- if (is.null(group)) NULL else as.character(data[[group]])
  lab <- if (is.null(label)) NULL else as.character(data[[label]])
  keep <- !is.na(vals)
  if (!is.null(grp)) keep <- keep & !is.na(grp)
  if (!any(keep)) {
    rlang::abort("`value` needs at least 1 non-missing value.")
  }
  warn_dropped(sum(is.na(vals)), value)
  if (!is.null(grp)) {
    warn_dropped(sum(!is.na(vals) & is.na(grp)), group)
  }
  # A swarm's promise is one visible dot per case. Past 800 the dots can
  # only shrink or pile up, so refuse and point at the summary charts.
  if (sum(keep) > 800) {
    rlang::abort(sprintf(
      paste("%d dots is too many for a beeswarm (max 800); it stops",
            "reading as individuals. Aggregate first, or use",
            "pv_histogram()/pv_boxplot()."),
      sum(keep)))
  }
  df <- data.frame(value = vals[keep])
  if (!is.null(grp)) {
    df$group <- grp[keep]
    check_palette_fit(unique(df$group), group)
  }
  if (!is.null(lab)) df$label <- lab[keep]
  pv_widget("beeswarm", c(list(
    data = df, xlab = resolve_lab(xlab, value)
  ), chart_opts(title, subtitle, mode, duration, source)),
  width, height, elementId)
}
