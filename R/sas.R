#' Read a SAS dataset
#'
#' Reads `.sas7bdat` or `.xpt` (SAS transport) files, preserving variable
#' labels as column attributes. No SAS installation is required.
#'
#' @param path Path to a `.sas7bdat` or `.xpt` file.
#' @return A data frame (tibble) with variable labels attached; inspect
#'   them with [pv_labels()].
#' @examples
#' demo <- system.file("extdata", "demo_sales.xpt", package = "polyviz")
#' df <- pv_read_sas(demo)
#' pv_labels(df)
#' @export
pv_read_sas <- function(path) {
  ext <- tolower(tools::file_ext(path))
  switch(ext,
    "sas7bdat" = haven::read_sas(path),
    "xpt"      = haven::read_xpt(path),
    rlang::abort(sprintf(
      "Unsupported SAS extension '.%s' (expected .sas7bdat or .xpt).", ext))
  )
}

#' Write a SAS dataset
#'
#' Writes `.xpt` (SAS transport, version 8 — the durable interchange
#' format) or `.sas7bdat` files. Variable labels attached to columns are
#' written into the file.
#'
#' @param data A data frame.
#' @param path Output path ending in `.xpt` or `.sas7bdat`.
#' @return `path`, invisibly.
#' @export
pv_write_sas <- function(data, path) {
  ext <- tolower(tools::file_ext(path))
  switch(ext,
    "xpt"      = haven::write_xpt(data, path, version = 8),
    "sas7bdat" = haven::write_sas(data, path),
    rlang::abort(sprintf(
      "Unsupported SAS extension '.%s' (expected .xpt or .sas7bdat).", ext))
  )
  invisible(path)
}

#' Get variable labels
#'
#' SAS-style variable labels for each column, as carried by `haven` (the
#' `label` attribute).
#'
#' @param data A data frame.
#' @return Named character vector: one element per column, `NA` where a
#'   column has no label.
#' @export
pv_labels <- function(data) {
  # haven stores each SAS variable label as a "label" attribute on the
  # column itself, so we just walk the columns and collect them.
  vapply(data, function(col) {
    lbl <- attr(col, "label", exact = TRUE)
    if (is.null(lbl)) NA_character_ else as.character(lbl)
  }, character(1))
}

#' Set variable labels
#'
#' @param data A data frame.
#' @param labels Named character vector mapping column names to labels.
#'   Columns not named are left unchanged.
#' @return `data` with labels attached, ready for [pv_write_sas()].
#' @examples
#' df <- pv_set_labels(mtcars, c(mpg = "Miles per US gallon"))
#' pv_labels(df)[["mpg"]]
#' @export
pv_set_labels <- function(data, labels) {
  unknown <- setdiff(names(labels), names(data))
  if (length(unknown)) {
    rlang::abort(sprintf("Unknown columns: %s", paste(unknown, collapse = ", ")))
  }
  for (nm in names(labels)) {
    attr(data[[nm]], "label") <- unname(labels[[nm]])
  }
  data
}
