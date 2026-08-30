# Every chart function below ends up here. This packs the data and options
# into one payload, attaches the colour palette, and hands it to the
# JavaScript side (inst/htmlwidgets/pvchart.js), where d3 draws it. The
# "type" field tells the JavaScript which renderer to use.
pv_widget <- function(type, payload, width = NULL, height = NULL,
                      elementId = NULL) {
  payload$type <- type
  payload$theme <- list(
    categorical = pv_colors$categorical,
    ink = pv_colors$ink
  )
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

# Fails early with a readable message if a chart is asked to use a column
# that the data frame does not have.
check_columns <- function(data, cols) {
  cols <- cols[!vapply(cols, is.null, logical(1))]
  missing <- setdiff(unlist(cols), names(data))
  if (length(missing)) {
    rlang::abort(sprintf("Column(s) not in `data`: %s",
                         paste(missing, collapse = ", ")))
  }
}

chart_opts <- function(title, subtitle, mode, duration) {
  list(title = title, subtitle = subtitle, mode = mode, duration = duration)
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
#' @param data A data frame.
#' @param x Name of the category column.
#' @param y Name of the numeric value column.
#' @param series Optional name of a grouping column (grouped bars).
#' @param sort Sort bars by value, largest first? (Single-series only.)
#' @param title,subtitle Optional chart heading text.
#' @param mode `"auto"` (default: follow the viewer's light/dark setting),
#'   `"light"`, or `"dark"`.
#' @param duration Entrance transition length in ms.
#' @param width,height,elementId Standard htmlwidgets sizing arguments.
#' @return An htmlwidget.
#' @examples
#' sales <- aggregate(revenue ~ region, pv_sales, sum)
#' pv_bar(sales, x = "region", y = "revenue", title = "Revenue by region")
#' @export
pv_bar <- function(data, x, y, series = NULL, sort = FALSE,
                   title = NULL, subtitle = NULL, mode = "auto",
                   duration = 700, width = NULL, height = NULL,
                   elementId = NULL) {
  check_columns(data, list(x, y, series))
  df <- data.frame(x = as.character(data[[x]]), y = as.numeric(data[[y]]))
  if (!is.null(series)) {
    df$series <- as.character(data[[series]])
  } else if (isTRUE(sort)) {
    df <- df[order(-df$y), ]
  }
  pv_widget("bar", c(list(
    data = df, xlab = x, ylab = y
  ), chart_opts(title, subtitle, mode, duration)),
  width, height, elementId)
}

#' Interactive D3 line chart
#'
#' Multi-series line chart with a draw-in animation and a crosshair
#' tooltip that reads out every series at the hovered x position. Series
#' are direct-labelled at the line ends.
#'
#' @param data A data frame.
#' @param x Name of the x column — `Date`, numeric, or categorical.
#' @param y Name of the numeric value column.
#' @param series Optional name of a series column (one line per level).
#' @inheritParams pv_bar
#' @return An htmlwidget.
#' @examples
#' monthly <- aggregate(revenue ~ month + region, pv_sales, sum)
#' pv_line(monthly, x = "month", y = "revenue", series = "region")
#' @export
pv_line <- function(data, x, y, series = NULL,
                    title = NULL, subtitle = NULL, mode = "auto",
                    duration = 900, width = NULL, height = NULL,
                    elementId = NULL) {
  check_columns(data, list(x, y, series))
  ax <- as_axis_values(data[[x]])
  df <- data.frame(x = ax$values, y = as.numeric(data[[y]]))
  df$series <- if (is.null(series)) "value" else as.character(data[[series]])
  # Points must be in drawing order within each line. For category axes we
  # keep the rows in the order they arrived (sorting "Jan, Feb, ..."
  # alphabetically would scramble them); dates and numbers sort naturally.
  ord <- if (ax$xtype == "category") order(df$series) else order(df$series, df$x)
  df <- df[ord, ]
  pv_widget("line", c(list(
    data = df, xtype = ax$xtype, xlab = x, ylab = y,
    showLegend = !is.null(series)
  ), chart_opts(title, subtitle, mode, duration)),
  width, height, elementId)
}

#' Interactive D3 scatter plot
#'
#' Scatter plot with per-point tooltips; optional colour (categorical) and
#' size (numeric) encodings.
#'
#' @param data A data frame.
#' @param x,y Names of numeric columns.
#' @param color Optional name of a categorical column (max 3 distinct
#'   values keeps every pair distinguishable; more will error).
#' @param size Optional name of a numeric column mapped to point area.
#' @param label Optional name of a column shown in tooltips.
#' @inheritParams pv_bar
#' @return An htmlwidget.
#' @examples
#' pv_scatter(mtcars, x = "wt", y = "mpg", size = "hp")
#' @export
pv_scatter <- function(data, x, y, color = NULL, size = NULL, label = NULL,
                       title = NULL, subtitle = NULL, mode = "auto",
                       duration = 500, width = NULL, height = NULL,
                       elementId = NULL) {
  check_columns(data, list(x, y, color, size, label))
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
  pv_widget("scatter", c(list(
    data = df, xlab = x, ylab = y, sizelab = size,
    showLegend = !is.null(color)
  ), chart_opts(title, subtitle, mode, duration)),
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
                     duration = 0, width = NULL, height = NULL,
                     elementId = NULL) {
  check_columns(nodes, list(id, label, group))
  check_columns(links, list("source", "target"))
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
  ), chart_opts(title, subtitle, mode, duration)),
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
                     duration = 800, width = NULL, height = NULL,
                     elementId = NULL) {
  m <- as.matrix(matrix)
  if (nrow(m) != ncol(m)) {
    rlang::abort("`matrix` must be square.")
  }
  labels <- labels %||% rownames(m) %||% paste0("G", seq_len(nrow(m)))
  pv_widget("chord", c(list(
    matrix = unname(apply(m, 1, as.numeric, simplify = FALSE)),
    labels = as.character(labels)
  ), chart_opts(title, subtitle, mode, duration)),
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
#' @inheritParams pv_bar
#' @return An htmlwidget.
#' @examples
#' pv_sunburst(pv_sales, levels = c("region", "product"), value = "revenue")
#' @export
pv_sunburst <- function(data, levels, value,
                        title = NULL, subtitle = NULL, mode = "auto",
                        duration = 750, width = NULL, height = NULL,
                        elementId = NULL) {
  check_columns(data, list(levels, value))
  if (length(levels) < 1) {
    rlang::abort("`levels` needs at least one column name.")
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
  ), chart_opts(title, subtitle, mode, duration)),
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
