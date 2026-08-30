#' Summarise a data frame
#'
#' A one-look data-quality summary: dimensions, completeness, and a
#' per-column table of type, missingness, distinct counts, and a compact
#' value sketch (range for numerics, top level for categoricals). SAS
#' variable labels, when present, are carried into the table.
#'
#' @param data A data frame.
#' @return A `pv_summary` object; print it, or take `$columns` for the
#'   per-column data frame.
#' @examples
#' pv_summary(airquality)
#' @export
pv_summary <- function(data) {
  stopifnot(is.data.frame(data))
  labels <- pv_labels(data)

  # Build one summary row per column. The "sketch" is a short human-readable
  # description: a range for numbers, the most common value for categories.
  columns <- do.call(rbind, lapply(names(data), function(nm) {
    x <- data[[nm]]
    n_missing <- sum(is.na(x))
    sketch <- if (is.numeric(x)) {
      clean <- x[!is.na(x)]
      if (length(clean)) {
        sprintf("%s to %s (median %s)",
                format(min(clean), digits = 3), format(max(clean), digits = 3),
                format(stats::median(clean), digits = 3))
      } else "all missing"
    } else {
      tab <- sort(table(x), decreasing = TRUE)
      if (length(tab)) {
        sprintf("top: %s (%d)", names(tab)[1], tab[[1]])
      } else "all missing"
    }
    data.frame(
      variable = nm,
      label = labels[[nm]],
      class = paste(class(x), collapse = "/"),
      n_missing = n_missing,
      pct_missing = round(100 * n_missing / length(x), 1),
      n_distinct = length(unique(x[!is.na(x)])),
      sketch = sketch
    )
  }))
  rownames(columns) <- NULL

  structure(
    list(
      n_rows = nrow(data),
      n_cols = ncol(data),
      complete_rate = mean(stats::complete.cases(data)),
      columns = columns
    ),
    class = "pv_summary"
  )
}

#' @export
print.pv_summary <- function(x, ...) {
  cat(sprintf("<pv_summary> %s rows x %s cols | %.1f%% complete cases\n\n",
              format(x$n_rows, big.mark = ","), x$n_cols,
              100 * x$complete_rate))
  # Hide the label column entirely when no column has a SAS-style label,
  # so plain data frames print without an empty column of NAs.
  cols <- x$columns
  if (all(is.na(cols$label))) {
    cols$label <- NULL
  }
  print(cols, row.names = FALSE, right = FALSE)
  invisible(x)
}
