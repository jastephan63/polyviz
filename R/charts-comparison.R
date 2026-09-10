# Comparison charts: slope, dumbbell, pyramid, waterfall, bullet. The
# forms whose whole job is putting two or more values side by side so the
# gap is the message. Each function validates here in R, computes whatever
# is statistics (running totals, sort orders, the two slope ends), and
# ships a tidy payload to inst/htmlwidgets/lib/pv-renderers/comparison.js.

#' Interactive D3 slope chart
#'
#' Two moments in time, one line per group between them. The slope of
#' each line IS the message: what rose, what fell, and by how much.
#' Group names sit at the left ends with their starting values, the
#' final values sit at the right ends, and labels that would overlap
#' are nudged apart the way the line chart spaces its end labels.
#' Rising lines carry the full text ink; falling lines step back into
#' the muted grey, so decline reads as receding ink rather than as a
#' dashed guess (dashes are reserved for projections). Hovering a line
#' shows both values and the change, absolute and in percent.
#'
#' @param data A data frame with one row per group per x position —
#'   more than one is an error; aggregate it first. Rows with a missing
#'   x, value, or group are dropped with a warning, and so are groups
#'   present at only one of the two positions (a slope needs both ends).
#' @param x Name of the column holding the two positions — two years,
#'   two dates, two labels. It must hold exactly two distinct values;
#'   numbers and dates order themselves, anything else keeps the order
#'   it first appears in.
#' @param y Name of the numeric value column.
#' @param group Name of the column naming each line.
#' @param highlight Optional group name, or a vector of them, drawn in
#'   the accent colour and full weight regardless of direction — the
#'   lines the chart is about. Names not present in the data are an
#'   error.
#' @param xlab Optional title under the chart. `NULL` (default) draws
#'   none — the two position labels under the verticals already say
#'   what the axis is; any string draws it, `NA` or `""` is the same
#'   as `NULL`.
#' @param ylab Optional rotated title on the value dimension. `NULL`
#'   (default) draws none — the values are written at both line ends,
#'   so the chart usually explains itself; any string draws it.
#' @inheritParams pv_bar
#' @return An htmlwidget.
#' @examples
#' cities <- c("Luzern", "Emmen", "Kriens", "Horw", "Zug", "Baar")
#' pop <- pv_city_population[pv_city_population$city %in% cities &
#'                             pv_city_population$year %in% c(1930, 2024), ]
#' pv_slope(pop, x = "year", y = "population", group = "city",
#'          highlight = "Zug", title = "A century of urban growth")
#' @export
pv_slope <- function(data, x, y, group, highlight = NULL,
                     xlab = NULL, ylab = NULL,
                     title = NULL, subtitle = NULL, mode = "auto",
                     duration = 800, source = NULL, width = NULL,
                     height = NULL, elementId = NULL) {
  check_columns(data, list(x, y, group))
  check_nonempty(data)
  check_value_column(data, y)
  xv <- data[[x]]
  df <- data.frame(pos = if (inherits(xv, "Date")) {
    format(xv, "%Y-%m-%d")
  } else {
    as.character(xv)
  }, y = as.numeric(data[[y]]), group = as.character(data[[group]]),
  stringsAsFactors = FALSE)
  df <- drop_missing(df, is.na(df$pos), x)
  df <- drop_missing(df, is.na(df$y), y)
  df <- drop_missing(df, is.na(df$group), group)
  pos <- unique(df$pos)
  if (length(pos) != 2) {
    shown <- if (length(pos) > 5) {
      sprintf("%s, and %d more", paste(pos[1:4], collapse = ", "),
              length(pos) - 4)
    } else {
      paste(pos, collapse = ", ")
    }
    rlang::abort(sprintf(paste(
      "A slope chart needs exactly two x positions;",
      "`%s` has %d (%s). Filter `data` down to the two moments to compare."),
      x, length(pos), shown))
  }
  # Numbers and dates take their natural left-to-right order; any other
  # column keeps the order it first appears in, like a category axis.
  if (is.numeric(xv)) {
    # Order as numbers ("9" before "10"), then back to the exact
    # strings the rows carry.
    pos <- as.character(sort(as.numeric(pos)))
  } else if (inherits(xv, "Date")) {
    pos <- sort(pos)  # ISO strings sort as dates do
  }
  if (anyDuplicated(paste(df$group, df$pos, sep = "\r"))) {
    rlang::abort(
      "`data` has more than one row per group/x combination; aggregate it first.")
  }
  # Fold the long rows into one row per group with both ends. Groups
  # that only show up at one position have no slope to draw; they are
  # dropped with a warning that names them.
  lv <- unique(df$group)
  left <- df[df$pos == pos[[1]], ]
  right <- df[df$pos == pos[[2]], ]
  wide <- data.frame(group = lv,
                     y1 = left$y[match(lv, left$group)],
                     y2 = right$y[match(lv, right$group)],
                     stringsAsFactors = FALSE)
  half <- is.na(wide$y1) | is.na(wide$y2)
  if (all(half)) {
    rlang::abort(sprintf(
      "No group in `%s` has a value at both x positions; nothing to draw.",
      group))
  }
  if (any(half)) {
    rlang::warn(sprintf(
      "Dropped %d group(s) present at only one of the two x positions: %s.",
      sum(half), paste(wide$group[half], collapse = ", ")))
    wide <- wide[!half, , drop = FALSE]
    rownames(wide) <- NULL
  }
  if (!is.null(highlight)) {
    if (!is.character(highlight) || !length(highlight) ||
        anyNA(highlight)) {
      rlang::abort("`highlight` must be a character vector of group names.")
    }
    unknown <- setdiff(highlight, wide$group)
    if (length(unknown)) {
      rlang::abort(sprintf("`highlight` names unknown group(s): %s.",
                           paste(unknown, collapse = ", ")))
    }
  }
  pv_widget("slope", c(list(
    data = wide, xlevels = as.character(pos),
    highlight = as.list(highlight),
    xlab = axis_title(xlab, ""), ylab = axis_title(ylab, "")
  ), chart_opts(title, subtitle, mode, duration, source)),
  width, height, elementId)
}

#' Interactive D3 dumbbell chart
#'
#' Two values per category joined by a connector — before and after,
#' here and there — with the categories running down the left like a
#' horizontal bar chart, so long names stay upright and readable. The
#' first value is a quiet grey dot, the second wears the accent colour,
#' and a two-entry legend above the chart names them. Where the
#' connector is long enough to carry it, the gap between the two values
#' is written right at its midpoint. Hovering a row shows both exact
#' values and the change, absolute and in percent.
#'
#' @param data A data frame with one row per category — more than one
#'   is an error; aggregate it first. At most 40 rows (beyond that no
#'   ranking stays readable). Rows with a missing category or value are
#'   dropped with a warning.
#' @param y Name of the category column (one row of the chart per level).
#' @param x1,x2 Names of the two numeric value columns — the dot pair
#'   each connector joins.
#' @param labels Names for the two value dots, in `x1`, `x2` order —
#'   e.g. `c("2010", "2024")`. They feed the legend and the tooltip.
#'   `NULL` (default) uses the two column names.
#' @param sort Row order, top to bottom. `"gap"` (default) sorts by
#'   `x2 - x1`, largest gain first; `"x1"` and `"x2"` sort by that end,
#'   largest first; `FALSE` keeps the rows as they arrive.
#' @param xlab Optional value-axis title under the chart. `NULL`
#'   (default) draws none — with the legend naming both dot sets a
#'   title is usually redundant; any string draws it.
#' @param ylab Ignored (kept for the family signature): the category
#'   names down the left are their own axis title.
#' @inheritParams pv_bar
#' @return An htmlwidget.
#' @examples
#' f20 <- pv_fiscal[pv_fiscal$year == 2020,
#'                  c("municipality", "resource_index")]
#' f27 <- pv_fiscal[pv_fiscal$year == 2027,
#'                  c("municipality", "resource_index")]
#' both <- merge(f20, f27, by = "municipality",
#'               suffixes = c("_2020", "_2027"))
#' top <- head(both[order(-both$resource_index_2027), ], 12)
#' pv_dumbbell(top, y = "municipality", x1 = "resource_index_2020",
#'             x2 = "resource_index_2027", labels = c("2020", "2027"),
#'             title = "Tax strength, first vs latest year")
#' @export
pv_dumbbell <- function(data, y, x1, x2, labels = NULL, sort = "gap",
                        xlab = NULL, ylab = NULL,
                        title = NULL, subtitle = NULL, mode = "auto",
                        duration = 600, source = NULL, width = NULL,
                        height = NULL, elementId = NULL) {
  check_columns(data, list(y, x1, x2))
  check_nonempty(data)
  check_value_column(data, x1)
  check_value_column(data, x2)
  sort_ok <- isFALSE(sort) ||
    (is.character(sort) && length(sort) == 1 && !is.na(sort) &&
       sort %in% c("gap", "x1", "x2"))
  if (!sort_ok) {
    rlang::abort('`sort` must be "gap", "x1", "x2", or FALSE.')
  }
  if (is.null(labels)) {
    labels <- c(x1, x2)
  }
  if (!is.character(labels) || length(labels) != 2 || anyNA(labels) ||
      !all(nzchar(labels))) {
    rlang::abort(
      "`labels` must be two non-empty strings, one per value column.")
  }
  df <- data.frame(y = as.character(data[[y]]),
                   x1 = as.numeric(data[[x1]]),
                   x2 = as.numeric(data[[x2]]),
                   stringsAsFactors = FALSE)
  df <- drop_missing(df, is.na(df$y), y)
  df <- drop_missing(df, is.na(df$x1), x1)
  df <- drop_missing(df, is.na(df$x2), x2)
  if (anyDuplicated(df$y)) {
    rlang::abort(sprintf(
      "`data` has more than one row per `%s` category; aggregate it first.",
      y))
  }
  if (nrow(df) > 40) {
    rlang::abort(sprintf(paste(
      "%d categories won't fit a readable dumbbell (the limit is 40).",
      "Pre-filter to the categories you care about, e.g. the top 25."),
      nrow(df)))
  }
  # The sort order is a statistic, so it is settled here; the renderer
  # draws the rows exactly as they arrive.
  if (!isFALSE(sort)) {
    df <- df[order(-switch(sort, gap = df$x2 - df$x1,
                           x1 = df$x1, x2 = df$x2)), ]
    rownames(df) <- NULL
  }
  pv_widget("dumbbell", c(list(
    data = df, labels = labels, xlab = axis_title(xlab, "")
  ), chart_opts(title, subtitle, mode, duration, source)),
  width, height, elementId)
}

#' Interactive D3 pyramid chart
#'
#' The population-pyramid form: one row per category, two non-negative
#' values per row drawn as bars mirrored from a shared centre spine —
#' the first side extends leftward, the second rightward. The classic
#' shape for opposing flows: age bands split male/female, commuters in
#' against commuters out, imports against exports. Both sides share one
#' symmetric scale sized to the larger side, and the tick labels read as
#' absolute values on both — a leftward bar of 12,000 reads 12,000,
#' never -12,000. The left side wears the palette's first colour, the
#' right its second, and a two-entry legend above the chart names them.
#' Hovering a row shows both exact values.
#'
#' @param data A data frame with one row per category — more than one
#'   is an error; aggregate it first. At most 40 rows (beyond that no
#'   row list stays readable). Rows with a missing category or value
#'   are dropped with a warning.
#' @param y Name of the category column (one row of the chart per level).
#' @param left,right Names of the two numeric value columns — `left`
#'   extends leftward from the centre, `right` rightward. Both must be
#'   non-negative: the mirroring supplies the direction, so a signed
#'   value has no honest place on either side.
#' @param labels Names for the two sides, in `left`, `right` order —
#'   e.g. `c("to Zug", "from Zug")`. They feed the legend and the
#'   tooltip. `NULL` (default) uses the two column names.
#' @param sort Row order, top to bottom. `FALSE` (default) keeps the
#'   rows as they arrive — the right choice for ordered categories like
#'   age bands; `"total"` sorts by `left + right`, largest first;
#'   `"left"` and `"right"` sort by that side, largest first.
#' @param xlab Optional value-axis title under the chart. `NULL`
#'   (default) draws none — with the legend naming both sides a title
#'   is usually redundant; any string draws it.
#' @param ylab Ignored (kept for the family signature): the category
#'   names down the left are their own axis title.
#' @inheritParams pv_bar
#' @return An htmlwidget.
#' @examples
#' latest <- pv_commuters[pv_commuters$period ==
#'                          max(pv_commuters$period), ]
#' inbound <- latest[latest$direction == "to Zug",
#'                   c("region", "commuters")]
#' outbound <- latest[latest$direction == "from Zug",
#'                    c("region", "commuters")]
#' both <- merge(inbound, outbound, by = "region",
#'               suffixes = c("_in", "_out"))
#' pv_pyramid(both, y = "region", left = "commuters_in",
#'            right = "commuters_out", labels = c("to Zug", "from Zug"),
#'            sort = "total", title = "Commuters in and out of Zug")
#' @export
pv_pyramid <- function(data, y, left, right, labels = NULL, sort = FALSE,
                       xlab = NULL, ylab = NULL,
                       title = NULL, subtitle = NULL, mode = "auto",
                       duration = 600, source = NULL, width = NULL,
                       height = NULL, elementId = NULL) {
  check_columns(data, list(y, left, right))
  check_nonempty(data)
  check_value_column(data, left)
  check_value_column(data, right)
  sort_ok <- isFALSE(sort) ||
    (is.character(sort) && length(sort) == 1 && !is.na(sort) &&
       sort %in% c("total", "left", "right"))
  if (!sort_ok) {
    rlang::abort('`sort` must be "total", "left", "right", or FALSE.')
  }
  if (is.null(labels)) {
    labels <- c(left, right)
  }
  if (!is.character(labels) || length(labels) != 2 || anyNA(labels) ||
      !all(nzchar(labels))) {
    rlang::abort(
      "`labels` must be two non-empty strings, one per side.")
  }
  df <- data.frame(y = as.character(data[[y]]),
                   left = as.numeric(data[[left]]),
                   right = as.numeric(data[[right]]),
                   stringsAsFactors = FALSE)
  df <- drop_missing(df, is.na(df$y), y)
  df <- drop_missing(df, is.na(df$left), left)
  df <- drop_missing(df, is.na(df$right), right)
  # The mirroring is the direction channel: leftward means the first
  # side, rightward the second. A negative value would fold a bar back
  # across the spine and read as the other side, so both must be
  # non-negative.
  for (side in c("left", "right")) {
    if (any(df[[side]] < 0)) {
      rlang::abort(sprintf(paste(
        "`%s` has negative values; pyramid bars grow outward from the",
        "centre, so both sides must be non-negative."),
        if (side == "left") left else right))
    }
  }
  if (anyDuplicated(df$y)) {
    rlang::abort(sprintf(
      "`data` has more than one row per `%s` category; aggregate it first.",
      y))
  }
  if (nrow(df) > 40) {
    rlang::abort(sprintf(paste(
      "%d categories won't fit a readable pyramid (the limit is 40).",
      "Pre-filter to the categories you care about, e.g. the top 25."),
      nrow(df)))
  }
  # The sort order is a statistic, so it is settled here; the renderer
  # draws the rows exactly as they arrive.
  if (!isFALSE(sort)) {
    df <- df[order(-switch(sort, total = df$left + df$right,
                           left = df$left, right = df$right)), ]
    rownames(df) <- NULL
  }
  pv_widget("pyramid", c(list(
    data = df, labels = labels, xlab = axis_title(xlab, "")
  ), chart_opts(title, subtitle, mode, duration, source)),
  width, height, elementId)
}

#' Interactive D3 waterfall chart
#'
#' Signed contributions building left to right from a start value:
#' each bar floats where the running total left off, gains in the
#' palette's first blue, losses in the theme's diverging red pole, and
#' thin connectors carry the level across the gaps. A distinct
#' grey-inked total bar closes the sequence (and, when `start` is not
#' zero, an equally distinct start bar opens it). Value labels ride the
#' bars where they fit; the tooltip always has the exact contribution
#' and the running total after it.
#'
#' @param data A data frame with one row per contribution, in the order
#'   the bars should build — more than one row per category is an
#'   error; aggregate it first. Rows with a missing category or value
#'   are dropped with a warning; infinite values are an error, because
#'   a bar cannot float to infinity.
#' @param x Name of the category column naming each contribution.
#' @param y Name of the signed numeric contribution column.
#' @param start The value the running total starts from (default 0).
#'   A non-zero start is drawn as its own "Start" bar in the total
#'   ink, so the floating bars never hang from an invisible ledge.
#' @param total Close the sequence with a bar for the final running
#'   total? `TRUE` (default) labels it "Total"; a single string uses
#'   that as the label instead; `FALSE` ends on the last contribution.
#' @param value_labels Print each bar's value on the chart — signed for
#'   the contributions, plain for the start and total bars. `TRUE`
#'   always, `FALSE` never; `"auto"` (default) shows them when at most
#'   12 bars leave each one wide enough to carry a number.
#' @inheritParams pv_bar
#' @return An htmlwidget.
#' @examples
#' lu <- pv_city_population[pv_city_population$city == "Luzern", ]
#' lu <- lu[order(lu$year), ]
#' steps <- data.frame(
#'   period = paste(head(lu$year, -1), lu$year[-1], sep = "\u2013"),
#'   change = diff(lu$population))
#' pv_waterfall(steps, x = "period", y = "change",
#'              start = lu$population[1],
#'              title = "How Lucerne's population moved")
#' @export
pv_waterfall <- function(data, x, y, start = 0, total = TRUE,
                         value_labels = "auto",
                         xlab = NULL, ylab = NULL,
                         title = NULL, subtitle = NULL, mode = "auto",
                         duration = 500, source = NULL, width = NULL,
                         height = NULL, elementId = NULL) {
  check_columns(data, list(x, y))
  check_nonempty(data)
  check_value_column(data, y)
  check_flag(value_labels, "value_labels")
  if (!is.numeric(start) || length(start) != 1 || !is.finite(start)) {
    rlang::abort("`start` must be a single finite number.")
  }
  total_label <- if (isTRUE(total)) {
    "Total"
  } else if (isFALSE(total)) {
    NULL
  } else if (is.character(total) && length(total) == 1 && !is.na(total) &&
             nzchar(total)) {
    total
  } else {
    rlang::abort(
      "`total` must be TRUE, FALSE, or a single string to label the total bar.")
  }
  df <- data.frame(x = as.character(data[[x]]), y = as.numeric(data[[y]]),
                   stringsAsFactors = FALSE)
  df <- drop_missing(df, is.na(df$x), x)
  df <- drop_missing(df, is.na(df$y), y)
  if (any(!is.finite(df$y))) {
    rlang::abort(sprintf(
      "`%s` has infinite values; every contribution must be finite.", y))
  }
  if (anyDuplicated(df$x)) {
    rlang::abort(
      "`data` has more than one row per category; aggregate it first.")
  }
  # The running totals are statistics, so they are computed here: each
  # bar gets the level it floats from (y0) and reaches (y1), and the
  # JavaScript side only places rectangles. The start and total bars
  # anchor at zero - they are amounts, not changes.
  df$y0 <- start + cumsum(df$y) - df$y
  df$y1 <- start + cumsum(df$y)
  df$kind <- "delta"
  if (start != 0) {
    df <- rbind(data.frame(x = "Start", y = start, y0 = 0, y1 = start,
                           kind = "start", stringsAsFactors = FALSE), df)
  }
  if (!is.null(total_label)) {
    final <- start + sum(df$y[df$kind == "delta"])
    df <- rbind(df, data.frame(x = total_label, y = final, y0 = 0,
                               y1 = final, kind = "total",
                               stringsAsFactors = FALSE))
  }
  rownames(df) <- NULL
  pv_widget("waterfall", c(list(
    data = df, xlab = axis_title(xlab, x), ylab = axis_title(ylab, y),
    valueLabels = value_labels, start = start
  ), chart_opts(title, subtitle, mode, duration, source)),
  width, height, elementId)
}

#' Interactive D3 bullet chart
#'
#' Stephen Few's bullet graph: one compact row per measure, a thin
#' accent-coloured bar for the value over muted grey qualitative bands,
#' and a near-black tick marking the target. Rows stack tightly and by
#' default share one scale, so the eye can run straight down the
#' column; per-row scales put every measure on its own footing instead.
#' The greys come from the theme's ink ramp — darkest for the lowest
#' band, per Few — so the qualitative context never competes with the
#' value for attention. [pv_textures()] leaves this chart alone on
#' purpose: its bands are already colour-free.
#'
#' @param data A data frame with one row per measure — more than one is
#'   an error; aggregate it first. At most 15 rows (a bullet list is a
#'   compact form). Rows with a missing label, value, or target are
#'   dropped with a warning.
#' @param label Name of the column naming each measure.
#' @param value Name of the numeric column drawn as the bar. Values
#'   must be non-negative — the bar grows from zero.
#' @param target Name of the numeric column drawn as the target tick.
#'   Must be non-negative.
#' @param bands Qualitative range thresholds behind the bar, up to 3.
#'   A numeric vector shares the same thresholds across every row; a
#'   character vector of column names reads them per row. Thresholds
#'   must be non-negative and are sorted ascending; each band shades
#'   the ground from zero up to its threshold, darkest first.
#'   `NULL` (default) draws no bands.
#' @param shared Share one value scale across all rows? `TRUE`
#'   (default) draws a single axis under the stack — the honest choice
#'   when the measures share a unit; `FALSE` gives every row its own
#'   scale and its own small axis.
#' @param xlab Optional value-axis title under the chart. `NULL`
#'   (default) draws none; any string draws it.
#' @param ylab Ignored (kept for the family signature): the measure
#'   names down the left are their own axis title.
#' @inheritParams pv_bar
#' @return An htmlwidget.
#' @examples
#' rev <- aggregate(revenue ~ region, pv_sales, sum)
#' rev$target <- round(1.08 * mean(rev$revenue), -4)
#' pv_bullet(rev, label = "region", value = "revenue", target = "target",
#'           bands = round(max(rev$revenue) * c(0.5, 0.8, 1.1), -4),
#'           title = "Revenue against target")
#' @export
pv_bullet <- function(data, label, value, target, bands = NULL,
                      shared = TRUE,
                      xlab = NULL, ylab = NULL,
                      title = NULL, subtitle = NULL, mode = "auto",
                      duration = 500, source = NULL, width = NULL,
                      height = NULL, elementId = NULL) {
  check_columns(data, list(label, value, target))
  check_nonempty(data)
  check_value_column(data, value)
  check_value_column(data, target)
  if (!isTRUE(shared) && !isFALSE(shared)) {
    rlang::abort("`shared` must be TRUE or FALSE.")
  }
  band_cols <- NULL
  band_shared <- NULL
  if (!is.null(bands)) {
    if (is.character(bands)) {
      if (!length(bands) || length(bands) > 3 || anyNA(bands)) {
        rlang::abort(
          "`bands` takes at most 3 column names (or numeric thresholds).")
      }
      check_columns(data, as.list(bands))
      for (b in bands) check_value_column(data, b)
      band_cols <- bands
    } else if (is.numeric(bands)) {
      if (!length(bands) || length(bands) > 3 || anyNA(bands) ||
          any(!is.finite(bands))) {
        rlang::abort(
          "`bands` takes at most 3 finite numeric thresholds (or column names).")
      }
      if (any(bands < 0)) {
        rlang::abort("`bands` thresholds must be non-negative.")
      }
      band_shared <- sort(as.numeric(bands))
    } else {
      rlang::abort(paste(
        "`bands` must be numeric thresholds shared by every row,",
        "or column names holding them per row."))
    }
  }
  df <- data.frame(label = as.character(data[[label]]),
                   value = as.numeric(data[[value]]),
                   target = as.numeric(data[[target]]),
                   stringsAsFactors = FALSE)
  if (!is.null(band_cols)) {
    for (i in seq_along(band_cols)) {
      df[[paste0("b", i)]] <- as.numeric(data[[band_cols[[i]]]])
    }
  }
  df <- drop_missing(df, is.na(df$label), label)
  df <- drop_missing(df, is.na(df$value), value)
  df <- drop_missing(df, is.na(df$target), target)
  if (!is.null(band_cols)) {
    for (i in seq_along(band_cols)) {
      df <- drop_missing(df, is.na(df[[paste0("b", i)]]), band_cols[[i]])
    }
    # Per-row thresholds shade nested ranges, so they must come in
    # ascending order within each row; sorting here keeps the renderer
    # a pure painter.
    k <- length(band_cols)
    if (k > 1) {
      sorted <- t(apply(as.matrix(df[paste0("b", seq_len(k))]), 1, sort))
      for (i in seq_len(k)) df[[paste0("b", i)]] <- sorted[, i]
    }
    for (i in seq_len(k)) {
      if (any(df[[paste0("b", i)]] < 0)) {
        rlang::abort(sprintf(
          "`%s` has negative values; band thresholds must be non-negative.",
          band_cols[[i]]))
      }
    }
  }
  # The bar grows from zero and the bands shade up from zero, so a
  # negative measure has no place in this form.
  if (any(df$value < 0)) {
    rlang::abort(sprintf(
      "`%s` has negative values; a bullet bar grows from zero.", value))
  }
  if (any(df$target < 0)) {
    rlang::abort(sprintf(
      "`%s` has negative values; a bullet target sits on a zero-based scale.",
      target))
  }
  if (anyDuplicated(df$label)) {
    rlang::abort(sprintf(
      "`data` has more than one row per `%s` measure; aggregate it first.",
      label))
  }
  if (nrow(df) > 15) {
    rlang::abort(sprintf(paste(
      "%d rows won't fit a readable bullet list (the limit is 15).",
      "Pre-filter to the measures you care about."), nrow(df)))
  }
  pv_widget("bullet", c(list(
    data = df, bands = band_shared,
    bandCount = if (is.null(band_cols)) length(band_shared)
                else length(band_cols),
    shared = shared, vlab = value, tlab = target,
    xlab = axis_title(xlab, "")
  ), chart_opts(title, subtitle, mode, duration, source)),
  width, height, elementId)
}
