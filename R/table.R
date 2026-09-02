# pv_table() is the one polyviz chart that is not drawn as SVG: the
# renderer (inst/htmlwidgets/lib/pv-renderers/table.js) builds a real
# HTML <table>, which is what screen readers, keyboard users, and
# copy-paste get the most out of. The division of labour stays the
# package's usual one - R validates and computes (column roles, digit
# counts, bar and shade domains), JavaScript only lays out and formats.

# How many decimal places a column needs when the user didn't say: whole
# numbers print none, and fractional values get the fewest decimals (up
# to two) that reproduce them at display precision.
table_auto_digits <- function(v) {
  v <- v[is.finite(v)]
  if (!length(v)) {
    return(0L)
  }
  for (d in 0:2) {
    if (all(abs(v - round(v, d)) < 1e-9)) {
      return(as.integer(d))
    }
  }
  2L
}

# In-cell encodings (and per-column digits) only make sense on columns
# the table actually shows.
table_check_shown <- function(cols, shown, arg) {
  hidden <- setdiff(cols, shown)
  if (length(hidden)) {
    rlang::abort(sprintf(
      "`%s` names column(s) %s, which `columns` leaves out of the table.",
      arg, paste0("`", hidden, "`", collapse = ", ")))
  }
}

# Bars and shading read a single number per cell, so their columns must
# be plainly numeric - not text, not a factor, and not a list-column.
table_check_numeric <- function(data, col, arg) {
  v <- data[[col]]
  if (!is.numeric(v)) {
    what <- if (is.factor(v)) {
      "a factor"
    } else if (is.list(v)) {
      "a list-column"
    } else {
      sprintf("a %s column", class(v)[[1]])
    }
    rlang::abort(sprintf("`%s` column `%s` is %s, not numeric.",
                         arg, col, what))
  }
}

# A spark column holds one numeric vector per row - a list-column, e.g.
# built with I(lapply(...)) or split(). Anything else in a cell would
# leave the sparkline nothing honest to draw, so it is refused by row.
table_check_spark <- function(data, col) {
  v <- data[[col]]
  if (!is.list(v) || is.data.frame(v)) {
    rlang::abort(sprintf(paste(
      "`spark` column `%s` must be a list-column of numeric vectors -",
      "build one with `I(lapply(...))` or `split()`."), col))
  }
  for (i in seq_along(v)) {
    cell <- v[[i]]
    if (is.null(cell) || is.numeric(cell)) {
      next
    }
    rlang::abort(sprintf(paste(
      "`spark` column `%s` must hold numeric vectors; row %d holds a",
      "%s."), col, i, class(cell)[[1]]))
  }
}

# Resolves `digits` into a named per-column vector over the displayed
# numeric columns, or NULL when formatting stays automatic.
table_resolve_digits <- function(digits, num_cols) {
  if (is.null(digits)) {
    return(NULL)
  }
  if (!is.numeric(digits) || !length(digits) || anyNA(digits) ||
      any(!is.finite(digits)) || any(digits < 0) || any(digits > 12) ||
      any(digits != round(digits))) {
    rlang::abort(
      "`digits` must be whole decimal counts between 0 and 12.")
  }
  nm <- names(digits)
  if (is.null(nm)) {
    if (length(digits) != 1) {
      rlang::abort(paste(
        "`digits` must be one count applied to every numeric column,",
        "or a named vector of per-column counts."))
    }
    if (!length(num_cols)) {
      rlang::abort("`digits` was given, but the table has no numeric columns.")
    }
    return(stats::setNames(rep(as.integer(digits), length(num_cols)),
                           num_cols))
  }
  if (any(is.na(nm)) || any(!nzchar(nm))) {
    rlang::abort("Every element of a named `digits` vector needs a name.")
  }
  unknown <- setdiff(nm, num_cols)
  if (length(unknown)) {
    rlang::abort(sprintf(
      "`digits` names column(s) %s, which are not numeric columns of the table.",
      paste0("`", unknown, "`", collapse = ", ")))
  }
  stats::setNames(as.integer(digits), nm)
}

#' Interactive design-system table
#'
#' A first-class table widget: the accessibility fallback the design
#' guidelines call for, and the natural companion to every chart. The
#' renderer builds a real HTML `<table>` in the package's chart chrome -
#' title block, source line, light/dark theming, hover row highlight -
#' with numbers right-aligned in tabular numerals and formatted through
#' the same locale machinery the charts use ([pv_locale()]): Swiss
#' grouping marks where a locale is active, and whole numbers under
#' 10'000 always ungrouped, so a year column reads `2020`, never
#' `2,020`.
#'
#' Three in-cell encodings turn columns into small visualisations:
#' `bars` draws a thin series-1-coloured bar (rounded at the data end)
#' scaled to the column's maximum, with the value printed beside it;
#' `shade` washes each cell's background with the sequential ramp,
#' flipping the label ink per cell so the text keeps its contrast (the
#' same rule the heatmap applies); and `spark` renders list-column cells
#' - one numeric vector per row - as inline sparklines with an end dot
#' and no axes.
#'
#' Because the table is HTML rather than SVG, two things differ from the
#' other charts: [pv_save()] refuses `.svg` and `.gif` for tables (save
#' as `.png`, `.pdf`, or `.html` instead), and the in-page download
#' control is left off, since it too would produce an SVG that cannot
#' hold the table.
#'
#' @param data A data frame with one row per table row. At most 5,000
#'   rows - a longer table stops being a summary; aggregate it or page
#'   through a smaller slice.
#' @param columns Character vector selecting (and ordering) the columns
#'   to show. `NULL` (default) shows every column in its original
#'   order. Headers come from the column names. Date columns print as
#'   ISO dates; list-columns must be named in `spark`.
#' @param digits Decimal places for numeric columns. `NULL` (default)
#'   chooses per column: none for whole numbers, up to two otherwise,
#'   with trailing zeros trimmed. A single count applies to every
#'   numeric column; a named vector (e.g. `c(revenue = 0, share = 1)`)
#'   sets individual columns, printed with exactly that many decimals.
#' @param bars Character vector of numeric columns that get a thin
#'   in-cell bar scaled to the column maximum, the value printed beside
#'   it. Values at or below zero draw no bar (the number still prints).
#' @param shade Character vector of numeric columns whose cell
#'   backgrounds encode the value on the sequential colour ramp, low to
#'   high across the column's range.
#' @param spark Character vector of list-columns - one numeric vector
#'   per cell - drawn as inline sparklines: a tiny line with a dot on
#'   the final value, no axes. Hovering a sparkline reads out its first
#'   and last values. Spark columns are not sortable.
#' @param sortable Click a column header to sort by it? `TRUE`
#'   (default) or `FALSE`. Numeric columns open largest-first, text
#'   columns A-to-Z, and a second click reverses; the active column
#'   wears an arrow, missing values always sink to the bottom, and
#'   re-sorting keeps every in-cell encoding intact.
#' @param page_size `NULL` (default) shows every row (the table scrolls
#'   inside the widget when it is taller); a whole number shows that
#'   many rows at a time, with a minimal previous/next pager in the
#'   footer next to the source line.
#' @inheritParams pv_bar
#' @return An htmlwidget.
#' @examples
#' regions <- aggregate(revenue ~ region, pv_sales, sum)
#' pv_table(regions, bars = "revenue", digits = 0,
#'          title = "Revenue by region")
#'
#' # A sparkline column: one population trajectory per city, built as a
#' # list-column from the long data.
#' pop <- pv_city_population[order(pv_city_population$city,
#'                                 pv_city_population$year), ]
#' trend <- split(pop$population, pop$city)
#' latest <- pop[pop$year == 2024, c("city", "population")]
#' latest$trend <- I(trend[latest$city])
#' pv_table(head(latest[order(-latest$population), ], 10),
#'          bars = "population", spark = "trend",
#'          title = "The ten largest Swiss cities")
#' @export
pv_table <- function(data, columns = NULL, digits = NULL, bars = NULL,
                     shade = NULL, spark = NULL, sortable = TRUE,
                     page_size = NULL, title = NULL, subtitle = NULL,
                     mode = "auto", source = NULL, width = NULL,
                     height = NULL, elementId = NULL) {
  check_columns(data, list(columns, bars, shade, spark))
  check_nonempty(data)
  if (nrow(data) > 5000) {
    rlang::abort(sprintf(paste(
      "`data` has %s rows - more than a table can usefully hold (5,000).",
      "Aggregate it, or page through a smaller slice."),
      format(nrow(data), big.mark = ",")))
  }
  if (!isTRUE(sortable) && !isFALSE(sortable)) {
    rlang::abort("`sortable` must be TRUE or FALSE.")
  }
  if (!is.null(page_size) &&
      (!is.numeric(page_size) || length(page_size) != 1 ||
       !is.finite(page_size) || page_size < 1 ||
       page_size != round(page_size))) {
    rlang::abort(
      "`page_size` must be a single whole number of rows, or NULL to show everything.")
  }
  columns <- columns %||% names(data)
  if (anyDuplicated(columns)) {
    rlang::abort(sprintf("`columns` repeats %s.",
      paste0("`", unique(columns[duplicated(columns)]), "`",
             collapse = ", ")))
  }
  table_check_shown(bars, columns, "bars")
  table_check_shown(shade, columns, "shade")
  table_check_shown(spark, columns, "spark")
  both <- intersect(bars, shade)
  if (length(both)) {
    rlang::abort(sprintf(paste(
      "Column(s) %s appear in both `bars` and `shade`; a cell carries",
      "one encoding, so pick one per column."),
      paste0("`", both, "`", collapse = ", ")))
  }
  overloaded <- intersect(spark, c(bars, shade))
  if (length(overloaded)) {
    rlang::abort(sprintf(
      "`spark` column(s) %s cannot also carry bars or shading.",
      paste0("`", overloaded, "`", collapse = ", ")))
  }
  for (col in bars) table_check_numeric(data, col, "bars")
  for (col in shade) table_check_numeric(data, col, "shade")
  for (col in spark) table_check_spark(data, col)

  num_cols <- columns[vapply(columns, function(col) {
    is.numeric(data[[col]]) && !col %in% spark
  }, logical(1))]
  user_digits <- table_resolve_digits(digits, num_cols)

  # One spec per displayed column: its role, its number formatting, and
  # the domains the in-cell encodings are scaled over - all computed
  # here, so the JavaScript side only draws.
  specs <- lapply(columns, function(col) {
    v <- data[[col]]
    if (col %in% spark) {
      return(list(key = col, label = col, type = "spark"))
    }
    if (is.list(v)) {
      rlang::abort(sprintf(paste(
        "Column `%s` is a list-column; name it in `spark` to draw its",
        "vectors as sparklines, or leave it out of `columns`."), col))
    }
    if (!is.numeric(v)) {
      return(list(key = col, label = col, type = "text"))
    }
    fin <- v[is.finite(v)]
    fixed <- !is.null(user_digits) && col %in% names(user_digits)
    spec <- list(
      key = col, label = col, type = "number",
      digits = if (fixed) user_digits[[col]] else table_auto_digits(v),
      fixed = fixed,
      # The package-wide tick rule travels into cells: a column of whole
      # numbers under 10'000 prints ungrouped, so years stay "2020".
      small = length(fin) > 0 && all(abs(fin) < 10000) &&
        all(abs(fin - round(fin)) < 1e-9)
    )
    if (col %in% bars) {
      spec$bar <- TRUE
      spec$barMax <- if (length(fin)) max(fin) else 0
    }
    if (col %in% shade) {
      spec$shade <- TRUE
      spec$shadeMin <- if (length(fin)) min(fin) else 0
      spec$shadeMax <- if (length(fin)) max(fin) else 0
    }
    spec
  })

  # The payload rows keep their original column names as keys. Dates
  # print as ISO strings, factors and everything else non-numeric as
  # text; spark list-columns ride along as arrays.
  df <- data[columns]
  rownames(df) <- NULL
  for (col in columns) {
    v <- df[[col]]
    if (is.list(v)) {
      next
    }
    if (inherits(v, "Date")) {
      df[[col]] <- format(v, "%Y-%m-%d")
    } else if (!is.numeric(v)) {
      df[[col]] <- as.character(v)
    }
  }

  payload <- list(
    data = df, columns = specs, sortable = sortable,
    # The in-page download menu saves charts as SVG, which cannot hold
    # an HTML table, so tables never grow the control.
    downloads = FALSE
  )
  if (!is.null(page_size)) {
    payload$pageSize <- as.integer(page_size)
  }
  pv_widget("table", c(payload, chart_opts(title, subtitle, mode, 0, source)),
            width, height, elementId)
}
