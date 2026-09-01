# Every chart function below ends up here. This packs the data and options
# into one payload, attaches the colour palette, and hands it to the
# JavaScript side (inst/htmlwidgets/pvchart.js), where d3 draws it. The
# "type" field tells the JavaScript which renderer to use.
pv_widget <- function(type, payload, width = NULL, height = NULL,
                      elementId = NULL) {
  payload$type <- type
  # The active theme is the packaged one unless pv_set_theme() swapped in
  # a validated user theme for this session.
  tokens <- the$theme %||% pv_colors
  payload$theme <- list(
    categorical = tokens$categorical,
    sequential = tokens$sequential,
    diverging = tokens$diverging,
    ink = tokens$ink,
    font = the$font %||% pv_font_stack()
  )
  # The active locale (pv_locale) rides along beside the theme; with
  # none set the assignment is a no-op, the field stays absent, and the
  # JavaScript side keeps its stock US-style formatting.
  payload$locale <- pv_locale_payload()
  # Every chart carries a written description of itself (R/alt-text.R) -
  # the alt text screen readers and knitted figures use. pv_alt()
  # replaces it with the author's own words.
  payload$alt <- alt_describe(payload)
  # Send data frames as one object per row (easier to loop over in d3),
  # and missing values as null so JavaScript can spot them.
  attr(payload, "TOJSON_ARGS") <- list(dataframe = "rows", na = "null")
  htmlwidgets::createWidget(
    name = "pvchart",
    x = payload,
    width = width,
    height = height,
    package = "polyviz",
    elementId = elementId,
    sizingPolicy = htmlwidgets::sizingPolicy(
      defaultWidth = "100%", defaultHeight = 420,
      viewer.fill = TRUE, browser.fill = TRUE, knitr.figure = FALSE
    )
  )
}

# Fails early with a readable message if a chart is handed something that
# is not a data frame, or asked to use a column the data frame does not
# have. A missing column's message lists what is available and, when the
# name looks like a typo, offers the closest real column.
check_columns <- function(data, cols) {
  if (!is.data.frame(data)) {
    got <- if (is.null(data)) "NULL" else paste0("a ", class(data)[[1]])
    rlang::abort(sprintf("`data` must be a data frame (got %s).", got))
  }
  cols <- cols[!vapply(cols, is.null, logical(1))]
  missing <- setdiff(unlist(cols), names(data))
  if (length(missing)) {
    avail <- if (length(names(data))) {
      paste(names(data), collapse = ", ")
    } else {
      "(none)"
    }
    near <- vapply(missing, function(m) {
      d <- utils::adist(m, names(data), ignore.case = TRUE)
      best <- which.min(d)
      if (length(best) && d[best] <= 2 && d[best] < nchar(m)) {
        names(data)[best]
      } else {
        NA_character_
      }
    }, character(1))
    near <- unique(near[!is.na(near)])
    hint <- if (length(near)) {
      sprintf(" Did you mean %s?",
              paste0("`", near, "`", collapse = ", "))
    } else {
      ""
    }
    rlang::abort(sprintf("Column(s) not in `data`: %s. Available: %s.%s",
                         paste(missing, collapse = ", "), avail, hint))
  }
}

# Columns mapped to a numeric role must hold numbers. as.numeric() on a
# factor would silently chart its level codes, and on a character column
# it would chart NAs, so both are refused here by name.
check_value_column <- function(data, col) {
  v <- data[[col]]
  if (is.factor(v) || is.character(v)) {
    rlang::abort(sprintf(
      "Column `%s` is %s, not numeric.",
      col, if (is.factor(v)) "a factor" else "a character column"))
  }
}

# Zero rows means nothing to draw. Every constructor refuses that early
# with the same plain message, instead of crashing further down or
# rendering an empty chart.
check_nonempty <- function(data, arg = "data") {
  if (!nrow(data)) {
    rlang::abort(sprintf("`%s` has no rows; nothing to draw.", arg))
  }
}

# Charts that draw individual values have nowhere to put a missing one,
# so those rows are dropped with a warning that names the column.
warn_dropped <- function(n, col) {
  if (n > 0) {
    rlang::warn(sprintf("Dropped %d row(s) with missing `%s` values.",
                        n, col))
  }
}

# Drops the rows flagged in `bad` from a payload data frame, warning with
# the column's name. Losing every row that way is an error instead - a
# fully missing column deserves more than a warning and a blank chart.
drop_missing <- function(df, bad, col) {
  if (!any(bad)) {
    return(df)
  }
  if (all(bad)) {
    rlang::abort(sprintf(
      "`%s` has no non-missing values; nothing to draw.", col))
  }
  warn_dropped(sum(bad), col)
  df <- df[!bad, , drop = FALSE]
  rownames(df) <- NULL
  df
}

chart_opts <- function(title, subtitle, mode, duration, source = NULL) {
  if (!is.character(mode) || length(mode) != 1 || is.na(mode) ||
      !mode %in% c("auto", "light", "dark")) {
    rlang::abort('`mode` must be "auto", "light", or "dark".')
  }
  if (!is.numeric(duration) || length(duration) != 1 ||
      !is.finite(duration) || duration < 0) {
    rlang::abort(
      "`duration` must be a single non-negative number of milliseconds.")
  }
  list(title = title, subtitle = subtitle, mode = mode,
       duration = as.numeric(duration), source = source)
}

# Several chart options accept TRUE, FALSE, or "auto". "auto" is the
# interesting one: it travels to the JavaScript side as-is, which decides
# at render time from the actual data and pixel sizes - something R cannot
# know when the widget is built. Anything else is refused here.
check_flag <- function(value, name) {
  ok <- isTRUE(value) || isFALSE(value) ||
    (is.character(value) && length(value) == 1 && !is.na(value) &&
       value == "auto")
  if (!ok) {
    rlang::abort(sprintf('`%s` must be TRUE, FALSE, or "auto".', name))
  }
  value
}

# Resolves an axis-title override: NULL keeps the column name, NA or ""
# suppresses the title, and any other string replaces it. The empty
# string is what travels to JavaScript for "no title" - it is falsy
# there, so both the axis label and its margin space are skipped.
axis_title <- function(override, colname) {
  if (is.null(override)) return(colname)
  if (length(override) != 1) {
    rlang::abort("Axis title overrides must be a single string, NA, or NULL.")
  }
  if (is.na(override) || !nzchar(override)) return("")
  as.character(override)
}

# Works out what kind of x axis a column needs. Dates go across as ISO
# strings ("2025-01-01") and are parsed back into dates by d3; numbers stay
# numbers; anything else is treated as a list of categories.
as_axis_values <- function(x) {
  if (inherits(x, "Date")) {
    list(values = format(x, "%Y-%m-%d"), xtype = "date")
  } else if (is.numeric(x)) {
    list(values = as.numeric(x), xtype = "number")
  } else {
    list(values = as.character(x), xtype = "category")
  }
}

#' Interactive D3 bar chart
#'
#' An animated bar chart rendered by the bundled D3.js — bars grow from the
#' baseline, and hovering any bar shows a tooltip. Give `series` to get
#' grouped bars with a legend.
#'
#' @param data A data frame with one row per bar (per category, or per
#'   category/series combination) — more than one is an error; aggregate
#'   first. Rows with a missing category or value are dropped with a
#'   warning.
#' @param x Name of the category column.
#' @param y Name of the numeric value column.
#' @param series Optional name of a grouping column — side-by-side bars by
#'   default, one segment per level when `stack` piles them up.
#' @param stack How to lay out the `series` levels within a category.
#'   `"none"` (default) draws them side by side; `"stack"` piles them
#'   into one bar per category whose full height is the total;
#'   `"percent"` stacks them normalised to 100%, turning the chart into
#'   composition per category. Both stacked modes need a `series`
#'   mapping, and values must be non-negative — a stacked segment cannot
#'   point down.
#' @param sort Sort bars by value, largest first? (Single-series only.)
#' @param horizontal Draw bars horizontally — the readable choice for long
#'   category names, with labels upright and the exact value at each bar's
#'   end. `TRUE` forces it (single-series or stacked only), `FALSE`
#'   forces vertical bars; `"auto"` (default) flips to horizontal when
#'   the chart draws one bar per category (single-series or stacked) and
#'   the category labels are too long to sit side by side under vertical
#'   bars.
#' @param value_labels Print each bar's value on the chart. `TRUE` always,
#'   `FALSE` never; `"auto"` (default) shows values on vertical bars only
#'   when the chart is single-series with at most 12 bars wide enough to
#'   carry a number, and always on horizontal bars, which have the room
#'   at the bar ends. In the stacked modes labels sit inside the
#'   segments — the value in `"stack"`, the share in `"percent"` — and
#'   only where a segment is big enough to carry the text; `"stack"`
#'   also prints each category's total at the bar's end. The tooltip
#'   always has the exact values.
#' @param xlab,ylab Axis titles. `NULL` (default) uses the column names;
#'   `NA` or `""` suppresses the title entirely; any other string
#'   replaces it.
#' @param title,subtitle Optional chart heading text.
#' @param mode `"auto"` (default: follow the viewer's light/dark setting),
#'   `"light"`, or `"dark"`.
#' @param duration Entrance transition length in ms.
#' @param source Optional source/credit line, shown small and grey at the
#'   bottom left — e.g. `"Source: Bundesamt für Statistik"`.
#' @param width,height,elementId Standard htmlwidgets sizing arguments.
#' @return An htmlwidget.
#' @examples
#' sales <- aggregate(revenue ~ region, pv_sales, sum)
#' pv_bar(sales, x = "region", y = "revenue", title = "Revenue by region")
#' # One bar per region, a segment per product, full height = the total:
#' mix <- aggregate(revenue ~ region + product, pv_sales, sum)
#' pv_bar(mix, x = "region", y = "revenue", series = "product",
#'        stack = "stack")
#' @export
pv_bar <- function(data, x, y, series = NULL,
                   stack = c("none", "stack", "percent"), sort = FALSE,
                   horizontal = "auto", value_labels = "auto",
                   xlab = NULL, ylab = NULL,
                   title = NULL, subtitle = NULL, mode = "auto",
                   duration = 500, source = NULL, width = NULL, height = NULL,
                   elementId = NULL) {
  check_columns(data, list(x, y, series))
  check_nonempty(data)
  check_value_column(data, y)
  stack <- match.arg(stack)
  check_flag(horizontal, "horizontal")
  check_flag(value_labels, "value_labels")
  if (stack != "none" && is.null(series)) {
    rlang::abort(sprintf(
      '`stack = "%s"` needs a `series` mapping - the stacked segments are the series levels.',
      stack))
  }
  if (isTRUE(horizontal) && !is.null(series) && stack == "none") {
    rlang::abort("`horizontal` bars support a single series only.")
  }
  df <- data.frame(x = as.character(data[[x]]), y = as.numeric(data[[y]]))
  if (!is.null(series)) df$series <- as.character(data[[series]])
  df <- drop_missing(df, is.na(df$x), x)
  df <- drop_missing(df, is.na(df$y), y)
  if (!is.null(series)) df <- drop_missing(df, is.na(df$series), series)
  # Stacked segments pile on top of each other, so a negative value
  # would fold a segment back over its neighbours. Refuse it rather
  # than draw it wrong.
  if (stack != "none" && any(df$y < 0)) {
    rlang::abort(sprintf(paste(
      "`%s` has negative values, which stacked bars cannot draw.",
      "Fix the data, or keep the bars side by side (`stack = \"none\"`)."),
      y))
  }
  # Two rows for the same bar would draw one bar over the other, so
  # refuse them here where the message can say what to do about it.
  key <- paste(df$x, if (is.null(series)) "" else df$series, sep = "\r")
  if (anyDuplicated(key)) {
    rlang::abort(if (is.null(series)) {
      paste("`data` has more than one row per category; aggregate it first,",
            "or map the extra grouping with `series`.")
    } else {
      "`data` has more than one row per series/category combination; aggregate it first."
    })
  }
  if (is.null(series) && isTRUE(sort)) {
    df <- df[order(-df$y), ]
  }
  if (stack != "none") {
    # The cumulative offsets are statistics, so they are computed here:
    # each row gets the bounds of its segment (y0 to y1) plus the
    # category total, and the JavaScript side only places rectangles.
    # Stacking runs in first-appearance series order - the same order
    # the palette is assigned in - so the bottom (or left) segment is
    # always the first series.
    series_names <- unique(df$series)
    cat_names <- unique(df$x)
    df <- df[order(match(df$x, cat_names), match(df$series, series_names)), ]
    rownames(df) <- NULL
    cum <- stats::ave(df$y, df$x, FUN = cumsum)
    df$y0 <- cum - df$y
    df$y1 <- cum
    df$total <- stats::ave(df$y, df$x, FUN = sum)
    if (stack == "percent") {
      # Normalise each category to 1; the segment's share travels along
      # for labels and tooltips. A category whose total is zero has no
      # composition to show, so its segments stay flat at zero.
      scale <- ifelse(df$total > 0, df$total, 1)
      df$share <- ifelse(df$total > 0, df$y / scale, 0)
      df$y0 <- ifelse(df$total > 0, df$y0 / scale, 0)
      df$y1 <- ifelse(df$total > 0, df$y1 / scale, 0)
    }
  }
  pv_widget("bar", c(list(
    data = df, xlab = axis_title(xlab, x),
    # A percent axis explains itself, so the default y title falls away
    # there; an explicit ylab still shows.
    ylab = axis_title(ylab, if (stack == "percent") "" else y),
    stack = stack, horizontal = horizontal, valueLabels = value_labels
  ), chart_opts(title, subtitle, mode, duration, source)),
  width, height, elementId)
}

#' Interactive D3 line chart
#'
#' Multi-series line chart with a draw-in animation and a crosshair
#' tooltip that reads out every series at the hovered x position. Series
#' are direct-labelled at the line ends.
#'
#' @param data A data frame with one row per x position per series —
#'   more than one is an error; aggregate first. Rows with a missing x
#'   or value are dropped with a warning.
#' @param x Name of the x column — `Date`, numeric, or categorical.
#' @param y Name of the numeric value column.
#' @param series Optional name of a series column (one line per level).
#' @param legend Show the colour legend row above the chart? `TRUE` always
#'   shows it, `FALSE` hides it (the direct labels at the line ends
#'   remain); `"auto"` (default) shows it exactly when a `series` mapping
#'   with more than one level exists.
#' @param show_points Mark every observation with a dot on its line —
#'   the connected-scatter treatment. `TRUE` always, `FALSE` (default)
#'   never; `"auto"` shows the dots when the individual observations
#'   matter: no series longer than 30 points, with at least about 12
#'   horizontal pixels between neighbouring dots. Each dot takes its
#'   line's colour with the usual 2px surface ring, and the crosshair
#'   tooltip works exactly as before. Spaghetti charts (more series than
#'   the palette's 8 hues) never draw them — their lines share one muted
#'   ink, so per-point dots would only add noise.
#' @param curve How the line travels between observations. `"linear"`
#'   (default) connects them with straight segments; `"monotone"` draws
#'   a smoothed curve that still passes through every point without
#'   overshooting it; `"step"` holds each value flat until the next
#'   observation — the honest shape for rates and thresholds that change
#'   at discrete moments.
#' @param zoom Add a brush-to-zoom strip below the chart? `FALSE`
#'   (default) or `TRUE`; needs a date or numeric x axis. The strip is a
#'   muted miniature of the full series: dragging across it narrows the
#'   main panel to that x window, and double-clicking the strip (or
#'   clicking it outside the brushed window) restores the full range.
#'   The chart always opens at the full range, so any export shows the
#'   complete series; SVG and PDF exports leave the strip out entirely,
#'   while a PNG page capture keeps the muted strip in view.
#'   [pv_facet()] drops the strip quietly: its panels share axes, which
#'   a per-panel brush would break.
#' @inheritParams pv_bar
#' @return An htmlwidget.
#' @examples
#' monthly <- aggregate(revenue ~ month + region, pv_sales, sum)
#' pv_line(monthly, x = "month", y = "revenue", series = "region")
#' @export
pv_line <- function(data, x, y, series = NULL, legend = "auto",
                    show_points = FALSE,
                    curve = c("linear", "monotone", "step"), zoom = FALSE,
                    xlab = NULL, ylab = NULL,
                    title = NULL, subtitle = NULL, mode = "auto",
                    duration = 800, source = NULL, width = NULL, height = NULL,
                    elementId = NULL) {
  check_columns(data, list(x, y, series))
  check_nonempty(data)
  check_value_column(data, y)
  check_flag(legend, "legend")
  check_flag(show_points, "show_points")
  curve <- match.arg(curve)
  # Zoom is a plain on/off switch: there is no data-driven decision for
  # the JavaScript side to make, so "auto" has no meaning here.
  if (!isTRUE(zoom) && !isFALSE(zoom)) {
    rlang::abort("`zoom` must be TRUE or FALSE.")
  }
  ax <- as_axis_values(data[[x]])
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
  # Two rows for the same series at the same x have no defensible drawing
  # order, so refuse them here where the message can say what to do.
  if (anyDuplicated(paste(df$x, df$series, sep = "\r"))) {
    rlang::abort(paste0(
      "`data` has more than one row per series/x combination; aggregate it first.",
      if (is.null(series)) " Or map the extra grouping with `series`."))
  }
  # Points must be in drawing order within each line. For category axes we
  # keep the rows in the order they arrived (sorting "Jan, Feb, ..."
  # alphabetically would scramble them); dates and numbers sort naturally.
  ord <- if (ax$xtype == "category") order(df$series) else order(df$series, df$x)
  df <- df[ord, ]
  pv_widget("line", c(list(
    data = df, xtype = ax$xtype,
    xlab = axis_title(xlab, x), ylab = axis_title(ylab, y),
    showLegend = !is.null(series), legend = legend,
    showPoints = show_points, curve = curve, zoom = zoom
  ), chart_opts(title, subtitle, mode, duration, source)),
  width, height, elementId)
}

#' Interactive D3 scatter plot
#'
#' Scatter plot with per-point tooltips; optional colour (categorical) and
#' size (numeric) encodings.
#'
#' @param data A data frame. Rows missing an x, y, or (when mapped) size
#'   value are dropped with a warning.
#' @param x,y Names of numeric columns.
#' @param color Optional name of a categorical column (max 3 distinct
#'   values keeps every pair distinguishable; more will error).
#' @param size Optional name of a numeric column mapped to point area.
#' @param label Optional name of a column shown in tooltips.
#' @param legend Show the colour legend row above the chart? `TRUE` always
#'   shows it (when a `color` mapping exists), `FALSE` hides it; `"auto"`
#'   (default) shows it exactly when a `color` mapping exists.
#' @inheritParams pv_bar
#' @return An htmlwidget.
#' @examples
#' pv_scatter(mtcars, x = "wt", y = "mpg", size = "hp")
#' @export
pv_scatter <- function(data, x, y, color = NULL, size = NULL, label = NULL,
                       legend = "auto", xlab = NULL, ylab = NULL,
                       title = NULL, subtitle = NULL, mode = "auto",
                       duration = 400, source = NULL, width = NULL,
                       height = NULL,
                       elementId = NULL) {
  check_columns(data, list(x, y, color, size, label))
  check_nonempty(data)
  check_value_column(data, x)
  check_value_column(data, y)
  if (!is.null(size)) check_value_column(data, size)
  check_flag(legend, "legend")
  if (!is.null(color)) {
    n_levels <- length(unique(data[[color]]))
    if (n_levels > 3) {
      rlang::abort(paste(
        "`color` has more than 3 levels; with all point pairs adjacent,",
        "only 3 colours stay reliably distinguishable. Facet or group",
        "the variable instead."))
    }
  }
  df <- data.frame(x = as.numeric(data[[x]]), y = as.numeric(data[[y]]))
  if (!is.null(color)) df$series <- as.character(data[[color]])
  if (!is.null(size)) df$size <- as.numeric(data[[size]])
  if (!is.null(label)) df$label <- as.character(data[[label]])
  df <- drop_missing(df, is.na(df$x), x)
  df <- drop_missing(df, is.na(df$y), y)
  if (!is.null(size)) df <- drop_missing(df, is.na(df$size), size)
  if (!is.null(color)) df <- drop_missing(df, is.na(df$series), color)
  pv_widget("scatter", c(list(
    data = df, xlab = axis_title(xlab, x), ylab = axis_title(ylab, y),
    sizelab = size,
    showLegend = !is.null(color), legend = legend
  ), chart_opts(title, subtitle, mode, duration, source)),
  width, height, elementId)
}

#' Interactive D3 force-directed network
#'
#' The classic D3 force layout: nodes repel, links act as springs, and you
#' can drag nodes around. Node colour encodes `group`; link width encodes
#' `value`.
#'
#' @param nodes Data frame of nodes.
#' @param links Data frame of links with `source`/`target` columns holding
#'   node ids, and optionally `value` for link strength/width.
#' @param id Name of the node id column (default `"id"`).
#' @param label Name of the node label column (defaults to the id column).
#' @param group Optional name of a grouping column mapped to colour.
#' @inheritParams pv_bar
#' @return An htmlwidget.
#' @examples
#' pv_force(pv_network$nodes, pv_network$links, group = "group")
#' @export
pv_force <- function(nodes, links, id = "id", label = id, group = NULL,
                     title = NULL, subtitle = NULL, mode = "auto",
                     duration = 0, source = NULL, width = NULL, height = NULL,
                     elementId = NULL) {
  check_columns(nodes, list(id, label, group))
  check_nonempty(nodes, "nodes")
  check_columns(links, list("source", "target"))
  if ("value" %in% names(links)) check_value_column(links, "value")
  nd <- data.frame(id = as.character(nodes[[id]]),
                   label = as.character(nodes[[label]]))
  if (!is.null(group)) nd$group <- as.character(nodes[[group]])
  lk <- data.frame(source = as.character(links$source),
                   target = as.character(links$target))
  lk$value <- if ("value" %in% names(links)) as.numeric(links$value) else 1
  # A link pointing at a node that doesn't exist would make d3's force
  # simulation throw a cryptic error, so catch it here with a clear one.
  unknown <- setdiff(c(lk$source, lk$target), nd$id)
  if (length(unknown)) {
    rlang::abort(sprintf("Links reference unknown node ids: %s",
                         paste(unique(unknown), collapse = ", ")))
  }
  pv_widget("force", c(list(
    nodes = nd, links = lk
  ), chart_opts(title, subtitle, mode, duration, source)),
  width, height, elementId)
}

#' Interactive D3 chord diagram
#'
#' Flows between entities as a chord diagram — hovering a group fades all
#' unrelated ribbons.
#'
#' @param matrix A square numeric matrix; `matrix[i, j]` is the flow from
#'   entity `i` to entity `j`. Row names (or `labels`) name the entities.
#' @param labels Optional character vector of entity names.
#' @inheritParams pv_bar
#' @return An htmlwidget.
#' @examples
#' pv_chord(pv_flows, title = "Inter-region shipments")
#' @export
pv_chord <- function(matrix, labels = NULL,
                     title = NULL, subtitle = NULL, mode = "auto",
                     duration = 600, source = NULL, width = NULL, height = NULL,
                     elementId = NULL) {
  m <- as.matrix(matrix)
  if (!is.numeric(m)) {
    rlang::abort("`matrix` must be numeric - each cell a flow size.")
  }
  if (nrow(m) != ncol(m)) {
    rlang::abort("`matrix` must be square.")
  }
  if (!nrow(m)) {
    rlang::abort("`matrix` has no rows; nothing to draw.")
  }
  labels <- labels %||% rownames(m) %||% paste0("G", seq_len(nrow(m)))
  pv_widget("chord", c(list(
    matrix = unname(apply(m, 1, as.numeric, simplify = FALSE)),
    labels = as.character(labels)
  ), chart_opts(title, subtitle, mode, duration, source)),
  width, height, elementId)
}

#' Zoomable D3 sunburst
#'
#' A hierarchy as concentric rings. Click a segment to zoom into it;
#' click the centre to zoom back out.
#'
#' @param data A data frame in long form: one row per leaf.
#' @param levels Character vector of column names, outermost grouping
#'   first, defining the hierarchy.
#' @param value Name of the numeric column summed within each segment.
#'   Values must be non-negative — a segment's size is an angle.
#' @inheritParams pv_bar
#' @return An htmlwidget.
#' @examples
#' pv_sunburst(pv_sales, levels = c("region", "product"), value = "revenue")
#' @export
pv_sunburst <- function(data, levels, value,
                        title = NULL, subtitle = NULL, mode = "auto",
                        duration = 650, source = NULL, width = NULL,
                        height = NULL,
                        elementId = NULL) {
  check_columns(data, list(levels, value))
  check_nonempty(data)
  check_value_column(data, value)
  if (length(levels) < 1) {
    rlang::abort("`levels` needs at least one column name.")
  }
  # A negative value has no angle to sweep; the layout would silently
  # come out wrong, so refuse it here.
  if (any(data[[value]] < 0, na.rm = TRUE)) {
    rlang::abort(sprintf(
      "`%s` has negative values; sunburst segment sizes must be non-negative.",
      value))
  }
  # Turn the flat table into the nested {name, children/value} tree that
  # d3.hierarchy expects: split the data by the first level column, then
  # recurse into each piece with the remaining levels. At the last level,
  # sum up the value column instead of recursing further.
  build <- function(df, lvls) {
    key <- as.character(df[[lvls[[1]]]])
    parts <- split(df, key)
    lapply(names(parts), function(nm) {
      part <- parts[[nm]]
      if (length(lvls) == 1) {
        list(name = nm, value = sum(as.numeric(part[[value]]), na.rm = TRUE))
      } else {
        list(name = nm, children = build(part, lvls[-1]))
      }
    })
  }
  root <- list(name = "root", children = build(data, levels))
  pv_widget("sunburst", c(list(
    root = root
  ), chart_opts(title, subtitle, mode, duration, source)),
  width, height, elementId)
}

#' Shiny bindings for polyviz charts
#'
#' Output and render functions for using polyviz D3 widgets within Shiny
#' apps.
#'
#' @param outputId Output variable to read from.
#' @param width,height CSS sizes for the widget.
#' @param expr An expression returning a polyviz widget.
#' @param env The environment in which to evaluate `expr`.
#' @param quoted Is `expr` a quoted expression?
#' @return `pvchartOutput` returns a Shiny output element;
#'   `renderPvchart` returns a Shiny render function.
#' @name pvchart-shiny
#' @export
pvchartOutput <- function(outputId, width = "100%", height = "420px") {
  htmlwidgets::shinyWidgetOutput(outputId, "pvchart", width, height,
                                 package = "polyviz")
}

#' @rdname pvchart-shiny
#' @export
renderPvchart <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) {
    expr <- substitute(expr)
  }
  htmlwidgets::shinyRenderWidget(expr, pvchartOutput, env, quoted = TRUE)
}
