# The scatterplot matrix: every pairwise view of a set of numeric
# columns in one grid. As everywhere in polyviz, R computes the
# statistics - histogram bins, correlation coefficients, pairwise
# counts - and inst/htmlwidgets/lib/pv-renderers/pairs.js only draws
# the geometry.

#' Interactive D3 scatterplot matrix
#'
#' All pairwise relationships in a set of numeric columns, drawn as one
#' matrix: scatter cells below the diagonal, each variable's own
#' histogram on it, and the correlation coefficient for every pair above
#' it, printed large and inked by sign and strength on the diverging
#' ramp - a strong positive and a strong negative pair announce
#' themselves before any single cell is read. The correlation method's
#' name appears once, under the chart's heading, never per cell.
#'
#' Variable names label the diagonal cells, and axis ticks appear only
#' on the matrix's outer edge - the left column and the bottom row - so
#' the inner cells stay clean. Hovering a scatter point names the pair
#' of variables (and the row, when `label` is mapped); hovering a
#' correlation cell restates the coefficient to three decimals with the
#' number of pairs behind it.
#'
#' Missing values follow the pairwise rule: correlations use
#' pairwise-complete observations, and each scatter cell drops only the
#' rows incomplete for its own pair, so no cell loses data to a gap in
#' some other column. Rows with any missing value are counted in a
#' single warning.
#'
#' @param data A data frame.
#' @param columns Character vector of numeric column names to pair, in
#'   matrix order. `NULL` (the default) takes the first 6 numeric
#'   columns (excluding any mapped to `color` or `label`). At most 8 -
#'   past that the cells shrink below anything readable.
#' @param color Optional name of a grouping column (max 8 levels,
#'   coloured from the categorical palette). Rows with a missing group
#'   are dropped with a warning.
#' @param label Optional name of a column naming each row, shown first
#'   in scatter-cell tooltips.
#' @param method Which correlation the upper triangle shows:
#'   `"pearson"` (default), `"spearman"`, or `"kendall"`, computed by
#'   [stats::cor()] on pairwise-complete observations.
#' @inheritParams pv_bar
#' @return An htmlwidget.
#' @examples
#' pv_pairs(subset(pv_fiscal, year == 2025),
#'          columns = c("resource_per_capita", "resource_index",
#'                      "equalization_chf"),
#'          title = "How Lucerne's fiscal measures move together")
#' @export
pv_pairs <- function(data, columns = NULL, color = NULL, label = NULL,
                     method = c("pearson", "spearman", "kendall"),
                     title = NULL, subtitle = NULL, mode = "auto",
                     duration = 400, source = NULL, width = NULL,
                     height = NULL, elementId = NULL) {
  check_columns(data, list(columns, color, label))
  check_nonempty(data)
  method <- match.arg(method)
  if (is.null(columns)) {
    numeric_cols <- names(data)[vapply(data, is.numeric, logical(1))]
    numeric_cols <- setdiff(numeric_cols, c(color, label))
    if (length(numeric_cols) < 2) {
      rlang::abort(sprintf(
        "`data` has %d numeric column(s); a pairs matrix needs at least 2. Name them with `columns`.",
        length(numeric_cols)))
    }
    columns <- utils::head(numeric_cols, 6)
  }
  columns <- as.character(columns)
  if (length(columns) < 2) {
    rlang::abort("`columns` needs at least 2 column names to pair.")
  }
  if (anyDuplicated(columns)) {
    rlang::abort("`columns` lists the same column more than once.")
  }
  # The matrix grows quadratically: past 8 variables every cell is a
  # postage stamp and nothing in it can be read. Refuse rather than
  # shrink.
  if (length(columns) > 8) {
    rlang::abort(sprintf(
      "`columns` lists %d variables; a pairs matrix stays readable up to 8. Pick the ones that matter most.",
      length(columns)))
  }
  for (col in columns) check_numeric_col(data, col)

  # Point rows travel under positional names (p1, p2, ...) so a data
  # column that happens to be called "series" or "label" can never
  # collide with the colour and label mappings; the variables list
  # below carries the real names for the JavaScript side's labels.
  vcols <- paste0("p", seq_along(columns))
  pts <- stats::setNames(
    as.data.frame(lapply(columns, function(col) as.numeric(data[[col]]))),
    vcols)
  if (!is.null(color)) {
    pts$series <- as.character(data[[color]])
    pts <- drop_missing(pts, is.na(pts$series), color)
    check_palette_fit(unique(pts$series), color)
  }
  if (!is.null(label)) pts$label <- as.character(data[[label]])
  for (k in seq_along(columns)) {
    if (sum(!is.na(pts[[vcols[k]]])) < 2) {
      rlang::abort(sprintf(
        "`%s` needs at least 2 non-missing values.", columns[k]))
    }
  }
  # One warning covers the whole NA policy: nothing else is dropped
  # here, because each cell keeps every row complete for its own pair.
  incomplete <- sum(!stats::complete.cases(pts[vcols]))
  if (incomplete > 0) {
    rlang::warn(sprintf(
      paste("%d row(s) have missing values in the paired columns;",
            "correlations use pairwise-complete observations and each",
            "scatter cell drops its incomplete pairs."), incomplete))
  }

  # Each variable's histogram bins (R's own Sturges binning, matching
  # pv_histogram) and its axis limits. hist() breaks always cover the
  # data, so the bin edges double as the shared limits every cell in
  # that variable's row and column draws with - which is what keeps the
  # diagonal histogram and the outer-edge ticks aligned with the
  # scatters.
  variables <- lapply(seq_along(columns), function(k) {
    bins <- pv_histbins(pts[[vcols[k]]])
    list(name = columns[k], bins = bins,
         lim = c(min(bins$x0), max(bins$x1)))
  })

  cm <- stats::cor(pts[vcols], use = "pairwise.complete.obs",
                   method = method)
  # How many complete pairs each coefficient rests on - stated in the
  # correlation cell's tooltip, so a coefficient from thin pairwise
  # coverage can't masquerade as one from the full data.
  npairs <- crossprod(!is.na(as.matrix(pts[vcols])))
  method_label <- switch(method,
    pearson = "Pearson correlation",
    spearman = "Spearman rank correlation",
    kendall = "Kendall rank correlation")

  pv_widget("pairs", c(list(
    data = pts,
    variables = variables,
    cor = unname(apply(cm, 1, as.numeric, simplify = FALSE)),
    n = unname(apply(npairs, 1, as.numeric, simplify = FALSE)),
    method = method, methodLabel = method_label
  ), chart_opts(title, subtitle, mode, duration, source)),
  width, height, elementId)
}
