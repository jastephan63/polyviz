# The trend modifier. The statistics happen here in R - stats::loess or
# stats::lm, exactly what a statistician would run by hand - and only the
# fitted points travel to JavaScript, where pv.drawTrends (in
# inst/htmlwidgets/lib/pv-common/pv-common.js) draws the ribbon and line.
# Shipping computed points instead of porting the model keeps R the
# single source of statistical truth.

#' Add a fitted trend to a scatter or line chart
#'
#' Fits a smooth curve (`"loess"`, the default) or a straight regression
#' line (`"lm"`) through the chart's points and overlays it, with a
#' shaded confidence ribbon. The model runs in R via [stats::loess()] or
#' [stats::lm()]; the chart only receives ~80 fitted points, evenly
#' spaced across the x range.
#'
#' Works on scatter charts and on line charts with a numeric or date x
#' axis. The fit pools every point into one curve, so multi-series
#' charts are refused - facet instead: build one chart per series level
#' and trend each. Repeated calls append, so a chart can carry a loess
#' curve and a straight line at once.
#'
#' @param w A polyviz scatter or line chart.
#' @param method `"loess"` (default) for a local smoother, `"lm"` for a
#'   straight least-squares line.
#' @param level Confidence level for the ribbon (default `0.95`). `NA`
#'   drops the ribbon and draws the fitted line alone.
#' @param slot Which palette slot colours the trend (default `2`, so it
#'   contrasts with data drawn in slot 1).
#' @param span The loess smoothing span (default `0.75`); larger is
#'   smoother. Ignored by `"lm"`.
#' @return The chart, with the fitted trend attached - ready for more
#'   pipe steps.
#' @examples
#' f25 <- subset(pv_fiscal, year == 2025)
#' pv_scatter(f25, x = "resource_index", y = "equalization_chf") |>
#'   pv_trend("loess")
#' daily <- aggregate(revenue ~ date, pv_sales, sum)
#' pv_line(daily, x = "date", y = "revenue") |>
#'   pv_trend("lm", level = NA)
#' @export
pv_trend <- function(w, method = c("loess", "lm"), level = 0.95,
                     slot = 2, span = 0.75) {
  check_pv_widget(w, "pv_trend")
  method <- match.arg(method)
  if (!(w$x$type %in% c("scatter", "line"))) {
    rlang::abort(sprintf(paste(
      "pv_trend() fits y against x, which needs a scatter or line",
      "chart - not a %s chart."), w$x$type))
  }
  df <- w$x$data
  if (!is.null(df$series) && length(unique(df$series)) > 1) {
    rlang::abort(sprintf(paste(
      "pv_trend() pools every point into one fit, but this chart has %d",
      "series - one curve through all of them would mislead. Facet",
      "instead: build one chart per series level and trend each."),
      length(unique(df$series))))
  }
  is_date <- identical(w$x$xtype, "date")
  if (w$x$type == "line" && !is_date && !identical(w$x$xtype, "number")) {
    rlang::abort(paste(
      "pv_trend() needs a numeric or date x axis; this line chart has a",
      "category axis, where 'in between' positions have no meaning."))
  }
  if (!(length(level) == 1 &&
        (is.na(level) ||
         (is.numeric(level) && level > 0 && level < 1)))) {
    rlang::abort("`level` must be a number in (0, 1), or NA for no ribbon.")
  }
  n_slots <- length(w$x$theme$categorical$light)
  if (!is.numeric(slot) || length(slot) != 1 || is.na(slot) ||
      slot != round(slot) || slot < 1 || slot > n_slots) {
    rlang::abort(sprintf("`slot` must be a whole number from 1 to %d.",
                         n_slots))
  }
  if (!is.numeric(span) || length(span) != 1 || is.na(span) || span <= 0) {
    rlang::abort("`span` must be a single positive number.")
  }

  # Pool the chart's points. Date charts store x as ISO strings, so the
  # model runs on numeric time (days since 1970) and the fitted grid is
  # converted back to the same ISO strings before shipping.
  xn <- if (is_date) as.numeric(as.Date(df$x)) else as.numeric(df$x)
  yn <- as.numeric(df$y)
  keep <- is.finite(xn) & is.finite(yn)
  xn <- xn[keep]
  yn <- yn[keep]
  if (length(xn) < 4) {
    rlang::abort("pv_trend() needs at least 4 complete (x, y) points.")
  }
  if (min(xn) == max(xn)) {
    rlang::abort("All x values are identical - there is no trend to fit.")
  }

  # Predict on ~80 evenly spaced x positions: enough that loess curvature
  # renders smoothly at any chart width, few enough that the payload
  # stays small.
  grid <- seq(min(xn), max(xn), length.out = 80)
  pool <- data.frame(x = xn, y = yn)
  if (method == "loess") {
    fit <- stats::loess(y ~ x, data = pool, span = span)
    pr <- stats::predict(fit, newdata = data.frame(x = grid), se = TRUE)
    yhat <- as.numeric(pr$fit)
    if (!is.na(level)) {
      q <- stats::qt(1 - (1 - level) / 2, pr$df)
      lo <- yhat - q * as.numeric(pr$se.fit)
      hi <- yhat + q * as.numeric(pr$se.fit)
    }
  } else {
    fit <- stats::lm(y ~ x, data = pool)
    if (is.na(level)) {
      yhat <- as.numeric(stats::predict(fit,
                                        newdata = data.frame(x = grid)))
    } else {
      pr <- stats::predict(fit, newdata = data.frame(x = grid),
                           interval = "confidence", level = level)
      yhat <- as.numeric(pr[, "fit"])
      lo <- as.numeric(pr[, "lwr"])
      hi <- as.numeric(pr[, "upr"])
    }
  }

  pts <- data.frame(
    x = if (is_date) {
      format(as.Date(grid, origin = "1970-01-01"), "%Y-%m-%d")
    } else {
      grid
    },
    y = yhat
  )
  if (!is.na(level)) {
    pts$lo <- lo
    pts$hi <- hi
  }
  # A short date range can land several grid points on the same day once
  # rounded to whole dates; keep the first of each so x stays strictly
  # increasing, which the d3 line generator relies on.
  if (is_date) pts <- pts[!duplicated(pts$x), , drop = FALSE]
  pts <- pts[is.finite(pts$y), , drop = FALSE]

  w$x$trends <- c(w$x$trends, list(list(
    slot = as.integer(slot), dash = FALSE, points = pts
  )))
  w
}
