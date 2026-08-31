# The evolution-and-matrix chart family: pv_area (stacked, percent, and
# stream areas), pv_heatmap, and pv_calendar. Rendered by
# inst/htmlwidgets/lib/pv-renderers/evolution.js.

# Local copy of the axis logic in widgets.R so this family stands alone:
# dates travel as ISO strings for d3 to re-parse, numbers stay numbers,
# anything else becomes a category.
evolution_axis_values <- function(x) {
  if (inherits(x, "Date")) {
    list(values = format(x, "%Y-%m-%d"), xtype = "date")
  } else if (is.numeric(x)) {
    list(values = as.numeric(x), xtype = "number")
  } else {
    list(values = as.character(x), xtype = "category")
  }
}

# TRUE / FALSE / "auto" chart flags travel to JavaScript exactly as given:
# the JavaScript side resolves "auto" at render time, when it can see the
# real data and pixel sizes. Here we only check the shape.
evolution_flag <- function(value, name) {
  ok <- isTRUE(value) || isFALSE(value) ||
    (is.character(value) && length(value) == 1 && !is.na(value) &&
     value == "auto")
  if (!ok) {
    rlang::abort(sprintf("`%s` must be TRUE, FALSE, or \"auto\".", name))
  }
  value
}

# Axis-title overrides: NULL means "use the fallback" (a column name, or
# nothing), NA or "" suppresses the title. The payload always carries a
# string, because a NULL element would come out of the JSON as an empty
# object rather than disappearing.
evolution_axis_title <- function(value, fallback, name) {
  if (is.null(value)) return(fallback)
  if (length(value) != 1) {
    rlang::abort(sprintf("`%s` must be a single string, NA, or NULL.", name))
  }
  if (is.na(value) || !nzchar(value)) return("")
  as.character(value)
}

#' Interactive D3 area chart
#'
#' Stacked areas showing how a total and its parts move together over
#' time. `offset` picks the variant: `"stacked"` piles the raw values so
#' the outer edge is the total; `"percent"` normalises every x position to
#' 100%, turning the chart into composition over time; `"stream"` draws a
#' streamgraph — the stack is centred and smoothed, and the y axis
#' disappears because only the band widths mean anything. A crosshair
#' reads every series out of one tooltip at the hovered x position (with
#' each band's share in percent mode), and hovering a band lifts it out of
#' the stack. Series/x combinations without a row count as zero.
#'
#' @param data A data frame in long form: one row per series per x.
#'   Rows with a missing x or value are dropped with a warning.
#' @param x Name of the x column — `Date`, numeric, or categorical.
#' @param y Name of the numeric value column. Values must be
#'   non-negative — stacked bands cannot represent a negative height.
#' @param series Optional name of the series column, one band per level.
#'   At most 8 levels — beyond that, fold the small ones into an
#'   `"Other"` level. Omit it for a single-series area.
#' @param offset `"stacked"` (default), `"percent"`, or `"stream"`.
#' @param legend `TRUE`, `FALSE`, or `"auto"` (default). `TRUE` always
#'   draws the legend row (even for a single series), `FALSE` never draws
#'   it, and `"auto"` draws it exactly when the chart has more than one
#'   series.
#' @param zoom Add a brush-to-zoom strip below the chart? `FALSE`
#'   (default) or `TRUE`; needs a date or numeric x axis. The strip is a
#'   muted silhouette of the full stack: dragging across it narrows the
#'   main panel to that x window, and double-clicking the strip (or
#'   clicking it outside the brushed window) restores the full range.
#'   The chart always opens at the full range, so any export shows the
#'   complete series; SVG and PDF exports leave the strip out entirely,
#'   while a PNG page capture keeps the muted strip in view.
#'   [pv_facet()] drops the strip quietly: its panels share axes, which
#'   a per-panel brush would break.
#' @param xlab,ylab Axis titles. `NULL` (default) uses the column names;
#'   `NA` or `""` suppresses a title; any other string replaces it. The
#'   default y title only appears on `offset = "stacked"` (a percent axis
#'   explains itself), but an explicit `ylab` shows there too. Streams
#'   have no y axis, so they never draw a y title.
#' @inheritParams pv_bar
#' @return An htmlwidget.
#' @examples
#' agglo <- subset(pv_city_population,
#'                 city %in% c("Luzern", "Emmen", "Kriens", "Horw", "Ebikon"))
#' pv_area(agglo, x = "year", y = "population", series = "city",
#'         title = "How the Lucerne agglomeration grew",
#'         source = "Source: Bundesamt für Statistik")
#' # The same data as a streamgraph:
#' pv_area(agglo, x = "year", y = "population", series = "city",
#'         offset = "stream")
#' @export
pv_area <- function(data, x, y, series = NULL,
                    offset = c("stacked", "percent", "stream"),
                    legend = "auto", zoom = FALSE, xlab = NULL, ylab = NULL,
                    title = NULL, subtitle = NULL, mode = "auto",
                    duration = 600, source = NULL, width = NULL,
                    height = NULL, elementId = NULL) {
  check_columns(data, list(x, y, series))
  check_nonempty(data)
  check_value_column(data, y)
  offset <- match.arg(offset)
  legend <- evolution_flag(legend, "legend")
  # Zoom is a plain on/off switch: there is no data-driven decision for
  # the JavaScript side to make, so "auto" has no meaning here.
  if (!isTRUE(zoom) && !isFALSE(zoom)) {
    rlang::abort("`zoom` must be TRUE or FALSE.")
  }
  ax <- evolution_axis_values(data[[x]])
  # Brushing narrows a continuous window; a category axis has none.
  if (isTRUE(zoom) && ax$xtype == "category") {
    rlang::abort(sprintf(
      "`zoom` needs a date or numeric x axis; `%s` is categorical, so there is no continuous window to brush.",
      x))
  }
  df <- data.frame(x = ax$values, y = as.numeric(data[[y]]))
  df$series <- if (is.null(series)) "value" else as.character(data[[series]])
  df <- drop_missing(df, is.na(df$x), x)
  df <- drop_missing(df, is.na(df$y), y)
  if (!is.null(series)) df <- drop_missing(df, is.na(df$series), series)
  # Every offset piles the bands on top of each other, so a negative
  # value would fold a band back over its neighbours - the stream's
  # centred stack included. Refuse it rather than draw it wrong.
  if (any(df$y < 0)) {
    rlang::abort(sprintf(paste(
      "`%s` has negative values, which stacked bands cannot draw.",
      "Fix the data, or plot the series with pv_line()."), y))
  }
  # Two rows for the same series at the same x would silently overwrite
  # each other in the stack pivot on the JavaScript side, so refuse them
  # here where the message can say what to do about it.
  if (anyDuplicated(paste(df$x, df$series, sep = "\r"))) {
    rlang::abort(
      "`data` has more than one row per series/x combination; aggregate it first.")
  }
  series_names <- unique(df$series)
  if (length(series_names) > 8) {
    rlang::abort(sprintf(
      "`series` has %d levels but the palette has 8 slots. Fold the smaller series into an \"Other\" level instead of adding colours.",
      length(series_names)))
  }
  # Colours follow first-appearance order. Within a series the rows keep
  # their arrival order on a category axis (sorting "Jan, Feb, ..."
  # alphabetically would scramble it); dates and numbers sort naturally.
  ord <- if (ax$xtype == "category") {
    order(match(df$series, series_names))
  } else {
    order(match(df$series, series_names), df$x)
  }
  df <- df[ord, ]
  pv_widget("area", c(list(
    data = df, xtype = ax$xtype,
    xlab = evolution_axis_title(xlab, x, "xlab"),
    # The default y title only makes sense against a raw-value axis, so it
    # falls away for percent and stream offsets unless set explicitly.
    ylab = evolution_axis_title(
      ylab, if (offset == "stacked") y else "", "ylab"),
    series = series_names, offset = offset, legend = legend, zoom = zoom
  ), chart_opts(title, subtitle, mode, duration, source)),
  width, height, elementId)
}

#' Interactive D3 heatmap
#'
#' A matrix of two categorical axes with each cell coloured by a numeric
#' value — the chart for spotting patterns across a whole grid at once.
#' `palette = "sequential"` maps magnitude onto the theme's single-hue
#' ramp; `"diverging"` is for values that span zero and pins the neutral
#' midpoint there (the colour domain is made symmetric so the two poles
#' carry equal weight). Cells print their value when there is room,
#' hovering rings a cell and shows the exact number, and a compact
#' colour-scale legend sits in the header.
#'
#' @param data A data frame with at most one row per x/y cell. Rows with
#'   a missing value are dropped with a warning — their cells stay blank.
#' @param x Name of the column mapped to the matrix columns.
#' @param y Name of the column mapped to the matrix rows.
#' @param value Name of the numeric column mapped to colour.
#' @param palette `"sequential"` (default, for magnitudes) or
#'   `"diverging"` (for values spanning zero, centred there).
#' @param cell_values `TRUE`, `FALSE`, or `"auto"` (default). Whether the
#'   cells print their value. `"auto"` prints only when both cell
#'   dimensions exceed 40px; `TRUE` always prints, shrinking the font to
#'   9px when the smaller dimension is 24–40px (below 24px nothing fits at
#'   any honest size, so nothing prints); `FALSE` never prints. The exact
#'   value is always in the cell tooltip.
#' @param truncate_labels Maximum length of an axis tick label, in
#'   characters, before it is shortened with an ellipsis (default 24).
#'   Narrow charts may shorten row labels further so the label margin
#'   never eats more than 40% of the width; the tooltip always carries the
#'   full x and y names.
#' @param xlab,ylab Optional axis titles. The heatmap draws none by
#'   default (`NULL`) — its axes are self-evident category lists — but a
#'   string here adds one; `NA` or `""` is the same as `NULL`.
#' @inheritParams pv_bar
#' @return An htmlwidget.
#' @examples
#' pop24 <- subset(pv_city_population, year == 2024)
#' top12 <- head(pop24[order(-pop24$population), "city"], 12)
#' emp <- subset(pv_city_sectors, city %in% top12)
#' pv_heatmap(emp, x = "city", y = "sector", value = "share",
#'            title = "Where Swiss city jobs are",
#'            source = "Source: Bundesamt für Statistik")
#' @export
pv_heatmap <- function(data, x, y, value,
                       palette = c("sequential", "diverging"),
                       cell_values = "auto", truncate_labels = 24,
                       xlab = NULL, ylab = NULL,
                       title = NULL, subtitle = NULL, mode = "auto",
                       duration = 500, source = NULL, width = NULL,
                       height = NULL, elementId = NULL) {
  check_columns(data, list(x, y, value))
  check_nonempty(data)
  check_value_column(data, value)
  palette <- match.arg(palette)
  cell_values <- evolution_flag(cell_values, "cell_values")
  if (!is.numeric(truncate_labels) || length(truncate_labels) != 1 ||
      is.na(truncate_labels) || truncate_labels < 1) {
    rlang::abort(
      "`truncate_labels` must be a single positive number of characters.")
  }
  truncate_labels <- as.integer(truncate_labels)
  df <- data.frame(x = as.character(data[[x]]),
                   y = as.character(data[[y]]),
                   value = as.numeric(data[[value]]))
  df <- drop_missing(df, is.na(df$x), x)
  df <- drop_missing(df, is.na(df$y), y)
  df <- drop_missing(df, is.na(df$value), value)
  if (anyDuplicated(paste(df$x, df$y, sep = "\r"))) {
    rlang::abort(
      "`data` has more than one row per x/y cell; aggregate it first.")
  }
  # The colour domain is a statistic, so it is decided here: the data
  # range for sequential, symmetric around zero for diverging so that the
  # midpoint colour always means zero.
  domain <- if (palette == "diverging") {
    m <- max(abs(range(df$value)))
    c(-m, m)
  } else {
    range(df$value)
  }
  # All-equal values would collapse the scale; give it a token width.
  if (domain[1] >= domain[2]) domain <- domain[1] + c(-1, 1)
  pv_widget("heatmap", c(list(
    data = df, xlab = x, ylab = y, vlab = value,
    palette = palette, domain = domain,
    cellValues = cell_values, truncateLabels = truncate_labels,
    # Unlike the area chart, xlab/ylab here stay the column names (the
    # tooltip contract); the drawn titles are their own fields and default
    # to nothing.
    xtitle = evolution_axis_title(xlab, "", "xlab"),
    ytitle = evolution_axis_title(ylab, "", "ylab")
  ), chart_opts(title, subtitle, mode, duration, source)),
  width, height, elementId)
}

#' Interactive D3 calendar heatmap
#'
#' The GitHub-style calendar: one horizontal block per year (its label on
#' the left), weeks as columns and weekdays as rows with Monday at the
#' top, each day a small rounded cell coloured by its value on the
#' theme's single-hue sequential ramp over the observed value range. The
#' form for daily data with a weekly rhythm — a whole year of weather,
#' commits, or sales reads at a glance, and weekday/weekend stripes jump
#' out. Month initials run along the top of each block, and a compact
#' gradient legend in the header shows the colour scale. Days that fall
#' inside a shown year but have no row in the data keep a faint
#' grid-coloured cell, so gaps are visibly absent rather than silently
#' painted as zero. Hovering a day rings it and shows the full formatted
#' date with the exact value ("no data" for the faint cells).
#'
#' Cells shrink with the chart — 53 week columns always fit, so on a
#' phone-width chart they get tiny but the year labels and legend stay
#' clear. A diverging variant (for values spanning zero, pinned to a
#' neutral midpoint) is future work; today the ramp is always sequential.
#'
#' @param data A data frame with at most one row per day.
#' @param date Name of the date column — `Date`, or anything
#'   `as.Date()` understands (e.g. `"2024-01-31"` strings).
#' @param value Name of the numeric column mapped to colour. Days whose
#'   value is missing are dropped with a warning and render as absent.
#' @param years `NULL` (default) shows every year present in the data;
#'   an integer vector (e.g. `years = 2023:2025`) picks specific years
#'   instead. Either way at most 6 year blocks are drawn — beyond that
#'   the blocks would shrink past legibility, so the function stops and
#'   asks for a window.
#' @inheritParams pv_bar
#' @return An htmlwidget.
#' @examples
#' pv_calendar(pv_weather, date = "date", value = "temp_max",
#'             years = 2023:2025,
#'             title = "Three summers, getting hotter",
#'             source = "Source: MeteoSwiss")
#' @export
pv_calendar <- function(data, date, value, years = NULL,
                        title = NULL, subtitle = NULL, mode = "auto",
                        duration = 500, source = NULL, width = NULL,
                        height = NULL, elementId = NULL) {
  check_columns(data, list(date, value))
  check_nonempty(data)
  check_value_column(data, value)
  d <- data[[date]]
  # The calendar needs real dates. Coerce anything as.Date() understands
  # (ISO strings, POSIXct); refuse with a plain message if that fails or
  # quietly turns values into NA. Bare numbers are refused too - modern R
  # would silently read them as days since 1970, which is never what a
  # column of plain numbers means.
  if (!inherits(d, "Date")) {
    coerced <- if (is.numeric(d)) NULL else {
      tryCatch(as.Date(d), error = function(e) NULL)
    }
    if (is.null(coerced) || any(is.na(coerced) & !is.na(d))) {
      rlang::abort(sprintf(
        "`%s` must be a Date column, or coercible with `as.Date()` (e.g. \"2024-01-31\" strings).",
        date))
    }
    d <- coerced
  }
  df <- data.frame(date = d, value = as.numeric(data[[value]]))
  # A day with a missing value is the same as a day with no row: both
  # render as the faint "absent" cell, so drop them here - with a
  # warning, so the absence never goes unnoticed on the R side.
  df <- drop_missing(df, is.na(df$date), date)
  df <- drop_missing(df, is.na(df$value), value)
  if (!is.null(years)) {
    ok <- is.numeric(years) && length(years) >= 1 && !anyNA(years) &&
      all(years == trunc(years))
    if (!ok) {
      rlang::abort(
        "`years` must be whole calendar years, e.g. `years = c(2023, 2024)`.")
    }
    years <- sort(unique(as.integer(years)))
    keep <- as.integer(format(df$date, "%Y")) %in% years
    if (!any(keep)) {
      rlang::abort(sprintf(
        "No data in the requested years; `%s` runs %s to %s.",
        date, format(min(df$date)), format(max(df$date))))
    }
    df <- df[keep, , drop = FALSE]
  }
  # The blocks to draw: the requested years as given (a requested year
  # with no data still renders, all faint - deliberate, visible absence),
  # or every year the data touches.
  show_years <- years %||% sort(unique(as.integer(format(df$date, "%Y"))))
  if (length(show_years) > 6) {
    rlang::abort(sprintf(
      "%d year blocks is more than one calendar can hold legibly (max 6). Pick a window with `years =`, e.g. `years = %d:%d` for the most recent six.",
      length(show_years), max(show_years) - 5L, max(show_years)))
  }
  # Two rows for the same day would silently overpaint each other's cell,
  # so refuse them here where the message can say what to do about it.
  if (anyDuplicated(df$date)) {
    rlang::abort("`data` has more than one row per day; aggregate it first.")
  }
  # The colour domain is a statistic, so it is decided here: the observed
  # range of what is actually shown. All-equal values would collapse the
  # scale; give it a token width.
  domain <- range(df$value)
  if (domain[1] >= domain[2]) domain <- domain[1] + c(-1, 1)
  pv_widget("calendar", c(list(
    data = data.frame(date = format(df$date, "%Y-%m-%d"), value = df$value),
    years = show_years, vlab = value, domain = domain
  ), chart_opts(title, subtitle, mode, duration, source)),
  width, height, elementId)
}
