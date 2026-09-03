# Flow and multivariate charts: sankey diagram, parallel coordinates.
# Like every chart file, this one only shapes the payload - the drawing
# lives in inst/htmlwidgets/lib/pv-renderers/flow.js.

#' Interactive D3 sankey diagram
#'
#' Directed flows between stages, drawn with the bundled d3-sankey plugin.
#' Node height is the total volume passing through; ribbon width is the
#' size of each individual flow. Hovering a ribbon reads out its exact
#' value; hovering a node fades every flow that doesn't touch it and
#' totals its traffic in and out.
#'
#' The node list is built for you from the values in the `source` and
#' `target` columns, in first-appearance order (which is also the colour
#' order). Flows must be acyclic — a loop is caught here with a clear
#' message before d3-sankey can fail cryptically. If the same name
#' genuinely appears on both sides (say, a region people commute both to
#' and from), disambiguate one occurrence, e.g. with a trailing space.
#'
#' @param links A data frame of flows: one row per link. Repeating the
#'   same source/target pair is an error — aggregate those rows first.
#' @param source Name of the column holding each link's origin node.
#' @param target Name of the column holding each link's destination node.
#' @param value Name of the numeric column holding the flow size
#'   (non-negative, no missing values).
#' @param align How nodes line up horizontally: `"justify"` (default —
#'   endpoints hug both edges), `"left"`, `"right"`, or `"center"`.
#' @param note Optional source/credit line, shown small and grey at the
#'   bottom left. (It plays the role `source` plays in the other charts;
#'   here `source` already names the link column.)
#' @inheritParams pv_bar
#' @return An htmlwidget.
#' @examples
#' latest <- pv_commuters[pv_commuters$period == max(pv_commuters$period), ]
#' # Inbound rows flow region -> Zug, outbound rows Zug -> region. The
#' # trailing space keeps each outbound destination distinct from its
#' # inbound namesake, so every region shows up on both sides.
#' links <- data.frame(
#'   source = ifelse(latest$direction == "to Zug", latest$region, "Zug"),
#'   target = ifelse(latest$direction == "to Zug", "Zug",
#'                   paste0(latest$region, " ")),
#'   value = latest$commuters
#' )
#' pv_sankey(links, title = "Commuting to and from Canton Zug")
#' @export
pv_sankey <- function(links, source = "source", target = "target",
                      value = "value",
                      align = c("justify", "left", "right", "center"),
                      title = NULL, subtitle = NULL, mode = "auto",
                      duration = 600, note = NULL, width = NULL,
                      height = NULL, elementId = NULL) {
  check_columns(links, list(source, target, value))
  check_nonempty(links, "links")
  check_value_column(links, value)
  align <- match.arg(align)
  src <- as.character(links[[source]])
  tgt <- as.character(links[[target]])
  val <- as.numeric(links[[value]])
  if (anyNA(val) || any(val < 0)) {
    rlang::abort("`value` must be non-negative numbers with no missing values.")
  }
  # The same flow twice would draw as two overlapping ribbons that read
  # as one thinner-than-real flow, so refuse it here.
  if (anyDuplicated(paste(src, tgt, sep = "\r"))) {
    rlang::abort(
      "`links` has more than one row for the same source/target pair; aggregate it first.")
  }

  # Nodes in first-appearance order, reading each link left to right.
  # That order decides the colours, so it is part of the chart's look.
  node_names <- unique(c(rbind(src, tgt)))
  si <- match(src, node_names)
  ti <- match(tgt, node_names)

  # d3-sankey places nodes by walking source -> target and never
  # terminates on a cycle, dying with a cryptic error. Catch cycles here
  # with a depth-first search and name one so it can be fixed.
  n <- length(node_names)
  adj <- split(ti, factor(si, levels = seq_len(n)))
  state <- integer(n) # 0 = untouched, 1 = on the current path, 2 = done
  cycle <- NULL
  visit <- function(i, path) {
    state[i] <<- 1L
    for (j in adj[[i]]) {
      if (!is.null(cycle)) return(invisible(NULL))
      if (state[j] == 1L) {
        cycle <<- c(path[match(j, path):length(path)], j)
        return(invisible(NULL))
      }
      if (state[j] == 0L) visit(j, c(path, j))
    }
    state[i] <<- 2L
    invisible(NULL)
  }
  for (i in seq_len(n)) {
    if (state[i] == 0L && is.null(cycle)) visit(i, i)
  }
  if (!is.null(cycle)) {
    rlang::abort(sprintf(
      "`links` contain a cycle (%s). A sankey flows strictly one way; break the loop, or rename one occurrence (e.g. add a trailing space).",
      paste(node_names[cycle], collapse = " -> ")))
  }

  pv_widget("sankey", c(list(
    nodes = data.frame(name = node_names),
    links = data.frame(source = si - 1L, target = ti - 1L, value = val),
    align = align
  ), chart_opts(title, subtitle, mode, duration, note)),
  width, height, elementId)
}

#' Interactive D3 parallel coordinates
#'
#' Each row of the data becomes one line threading across a set of
#' vertical axes, one per numeric column, each on its own independent
#' scale. The shape of a line is the row's profile; bundles of similar
#' shapes are the clusters.
#'
#' The axes are interactive filters: drag along any axis to brush a value
#' range — rows outside it fade out, and brushes on several axes combine
#' so only rows passing all of them stay lit. Double-click an axis to
#' clear its brush. Hovering a line raises it and reads out the full row.
#' In Shiny, every brush change reports the rows passing all the active
#' brushes (each row's `label`, or its row number when no `label` is
#' mapped) as `input$<outputId>_brush`.
#'
#' Rows with a missing value on any axis are dropped with a warning — a
#' broken polyline would be unreadable.
#'
#' @param data A data frame.
#' @param columns Character vector of 2–8 numeric column names — one
#'   vertical axis each, in this order.
#' @param color Optional name of a categorical column (max 3 distinct
#'   values keeps overlapping lines distinguishable; more will error).
#' @param label Optional name of a column shown first in tooltips.
#' @inheritParams pv_bar
#' @return An htmlwidget.
#' @examples
#' # Land-use mix per city: share of each city's total area, in percent.
#' lu <- pv_city_landuse
#' lu$share <- 100 * lu$hectares / ave(lu$hectares, lu$city, FUN = sum)
#' wide <- reshape(
#'   lu[lu$category %in% c("Buildings", "Agriculture", "Forest"),
#'      c("city", "category", "share")],
#'   direction = "wide", idvar = "city", timevar = "category")
#' names(wide) <- sub("^share\\.", "", names(wide))
#' pv_parallel(wide, columns = c("Buildings", "Agriculture", "Forest"),
#'             label = "city", title = "How Swiss cities use their land")
#' @export
pv_parallel <- function(data, columns, color = NULL, label = NULL,
                        title = NULL, subtitle = NULL, mode = "auto",
                        duration = 500, source = NULL, width = NULL,
                        height = NULL, elementId = NULL) {
  check_columns(data, list(columns, color, label))
  check_nonempty(data)
  if (length(columns) < 2 || length(columns) > 8) {
    rlang::abort("`columns` needs between 2 and 8 column names - one vertical axis each.")
  }
  not_num <- columns[!vapply(columns, function(cl) is.numeric(data[[cl]]),
                             logical(1))]
  if (length(not_num)) {
    rlang::abort(sprintf(
      "Column(s) not numeric: %s. Every parallel axis needs a numeric column.",
      paste(not_num, collapse = ", ")))
  }
  if (!is.null(color)) {
    n_levels <- length(unique(data[[color]]))
    if (n_levels > 3) {
      rlang::abort(paste(
        "`color` has more than 3 levels; with overlapping lines, only 3",
        "colours stay reliably distinguishable. Facet or group the",
        "variable instead."))
    }
  }
  # Axis columns travel as v1..vk so real column names (which could be
  # anything, including "series" or "label") can't collide with the
  # payload's own fields. The display names ride along in `columns`.
  df <- as.data.frame(lapply(columns, function(cl) as.numeric(data[[cl]])))
  names(df) <- paste0("v", seq_along(columns))
  if (!is.null(color)) df$series <- as.character(data[[color]])
  if (!is.null(label)) df$label <- as.character(data[[label]])
  complete <- stats::complete.cases(df[paste0("v", seq_along(columns))])
  if (!any(complete)) {
    rlang::abort(
      "Every row is missing a value on at least one axis; nothing to draw.")
  }
  if (any(!complete)) {
    rlang::warn(sprintf(
      "Dropped %d row(s) with missing values on the parallel axes.",
      sum(!complete)))
  }
  df <- df[complete, , drop = FALSE]
  rownames(df) <- NULL
  pv_widget("parallel", c(list(
    data = df, columns = as.character(columns),
    showLegend = !is.null(color)
  ), chart_opts(title, subtitle, mode, duration, source)),
  width, height, elementId)
}
