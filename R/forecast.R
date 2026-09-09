# The forecast layer. Exactly like the trend layer (R/trends.R), the
# statistics happen here in R - stats::HoltWinters, stats::arima, or the
# naive random walk, nothing beyond base and stats - and only computed
# points travel to JavaScript, where pv.drawForecast (in
# inst/htmlwidgets/lib/pv-common/pv-common.js) draws the fan. Shipping
# computed points instead of porting the models keeps R the single
# source of statistical truth.

# Classifies the spacing of an x series. Date steps that all fall inside
# one calendar unit count as regular even though the day counts wobble
# (months are 28 to 31 days long), and the unit decides two things: how
# the future dates are laid out (seq.Date by that unit, so month ends
# and leap years come out right) and whether the spacing implies a
# seasonal period - 12 for monthly data, 4 for quarterly, 7 for daily
# (the day-of-week cycle). Weekly and yearly steps are regular but imply
# no period a short series could support. Anything unclassified is
# regular only when no step strays more than 5% from the median.
forecast_spacing <- function(xn, is_date) {
  steps <- diff(xn)
  med <- stats::median(steps)
  if (is_date) {
    lo <- min(steps)
    hi <- max(steps)
    if (lo == 1 && hi == 1) {
      return(list(step = 1, by = "day", freq = 7, regular = TRUE))
    }
    if (lo == 7 && hi == 7) {
      return(list(step = 7, by = "week", freq = NA, regular = TRUE))
    }
    if (lo >= 28 && hi <= 31) {
      return(list(step = med, by = "month", freq = 12, regular = TRUE))
    }
    if (lo >= 89 && hi <= 92) {
      return(list(step = med, by = "quarter", freq = 4, regular = TRUE))
    }
    if (lo >= 365 && hi <= 366) {
      return(list(step = med, by = "year", freq = NA, regular = TRUE))
    }
  }
  list(step = med, by = NULL, freq = NA,
       regular = max(abs(steps - med)) <= 0.05 * abs(med))
}

#' Project a line chart forward with a forecast fan
#'
#' Fits a time-series model to a single-series line chart and extends it
#' `horizon` steps past the data: nested prediction bands (one per
#' confidence level, widest outermost - the classic fan chart), the
#' point forecast as a dashed continuation of the line (dashes carry
#' their reserved projection meaning throughout polyviz), and a thin
#' vertical rule where the observed data ends. The crosshair works over
#' the projected region too, reading the point estimate and every
#' interval. The model runs entirely in R via the stats package; the
#' chart receives only the computed points.
#'
#' @section How each method forecasts, honestly:
#' `"ets"` is exponential smoothing via [stats::HoltWinters()] with
#' additive components. When the x spacing implies a seasonal period
#' (monthly data gets a 12-step season, quarterly 4, daily 7) and the
#' series covers at least two full cycles, the seasonal fit is tried
#' first - with the function's defaults, then once more with
#' `start.periods = 3` if the optimiser fails - before falling back,
#' with a warning, to a non-seasonal fit; any other spacing (yearly and
#' weekly included) is fitted non-seasonally from the start. This is
#' classical Holt-Winters smoothing, not the full ETS state-space
#' family the name is shorthand for.
#'
#' `"arima"` is [stats::arima()] with a deliberately tiny order search,
#' nothing like `auto.arima`: the differencing order d is 1 when first
#' differences have smaller variance than the series itself and 0
#' otherwise, then every (p, q) pair with p and q from 0 to 2 is fitted
#' at that d and the lowest AIC wins (candidates that fail to converge
#' are dropped). The search never considers seasonal terms - for
#' clearly seasonal data prefer `"ets"`. The chosen order is stored in
#' the payload (`w$x$forecast$order`), so nothing about the fit is
#' hidden.
#'
#' `"naive"` carries the last observed value forward - the honest
#' baseline every fancier forecast has to beat. Its intervals come from
#' the random-walk model: the half-width at step h is
#' `qnorm((1 + level/100) / 2) * sd(diff(y)) * sqrt(h)`.
#'
#' All intervals are model-based and symmetric, and they say only what
#' the model says: roughly, "if the future behaves like the fitted
#' past, this band should contain it about level% of the time". They
#' know nothing about structural breaks ahead.
#'
#' @section What the data must look like:
#' A single-series line chart on a numeric or date x axis, with at
#' least 4 observations. Multi-series charts are refused - one model
#' pooling several series would mislead; facet and forecast each panel
#' instead. The models assume evenly spaced observations, so irregular
#' spacing draws a warning and the fit proceeds as if every step were
#' the median one - fine for a census every decade or so, misleading
#' for wildly uneven gaps. Future dates follow the calendar (a monthly
#' series continues month by month, leap years and month ends intact);
#' anything unclassified extends by the median step.
#'
#' @param w A polyviz line chart with one series.
#' @param horizon How many steps past the data to forecast - a single
#'   positive whole number, in the series' own spacing (12 on a monthly
#'   chart is one year).
#' @param method `"ets"` (default), `"arima"`, or `"naive"` - see the
#'   methods section for what each honestly does.
#' @param levels Confidence levels for the nested bands, in percent -
#'   each strictly between 0 and 100, no duplicates (default
#'   `c(50, 80, 95)`). They are sorted ascending, so the widest band is
#'   always the highest level.
#' @return The chart, with the forecast attached - ready for more pipe
#'   steps.
#' @seealso [pv_trend()] for fitted trends over the observed data.
#' @examples
#' monthly <- aggregate(gwh ~ date, pv_electricity, sum)
#' pv_line(monthly, x = "date", y = "gwh") |>
#'   pv_forecast(horizon = 12)
#' # The honest baseline, with one 90% band:
#' pv_line(monthly, x = "date", y = "gwh") |>
#'   pv_forecast(horizon = 12, method = "naive", levels = 90)
#' @export
pv_forecast <- function(w, horizon, method = c("ets", "arima", "naive"),
                        levels = c(50, 80, 95)) {
  check_pv_widget(w, "pv_forecast")
  method <- match.arg(method)
  if (!identical(w$x$type, "line")) {
    rlang::abort(sprintf(paste(
      "pv_forecast() extends a series through time, which needs a line",
      "chart - not a %s chart."), w$x$type))
  }
  df <- w$x$data
  if (length(unique(df$series)) > 1) {
    rlang::abort(sprintf(paste(
      "pv_forecast() fits one model to one series, but this chart has",
      "%d series. Facet instead: build one chart per series level and",
      "forecast each."), length(unique(df$series))))
  }
  is_date <- identical(w$x$xtype, "date")
  if (!is_date && !identical(w$x$xtype, "number")) {
    rlang::abort(paste(
      "pv_forecast() needs a numeric or date x axis; this line chart",
      "has a category axis, where 'one step ahead' has no meaning."))
  }
  if (!is.null(w$x$forecast)) {
    rlang::abort(paste(
      "This chart already carries a forecast - one fan per chart keeps",
      "it readable. Build a second chart for a second method."))
  }
  if (!is.numeric(horizon) || length(horizon) != 1 || is.na(horizon) ||
      horizon != round(horizon) || horizon < 1) {
    rlang::abort("`horizon` must be a single positive whole number.")
  }
  horizon <- as.integer(horizon)
  if (!is.numeric(levels) || !length(levels) || anyNA(levels) ||
      any(levels <= 0 | levels >= 100)) {
    rlang::abort(paste(
      "`levels` must be confidence percentages strictly between 0 and",
      "100, e.g. c(50, 80, 95)."))
  }
  if (anyDuplicated(levels)) {
    rlang::abort("`levels` has duplicate values.")
  }
  levels <- sort(as.numeric(levels))

  # pv_line() already sorted the rows by x and refused duplicate x
  # positions, so the series arrives ordered and strictly increasing.
  xn <- if (is_date) as.numeric(as.Date(df$x)) else as.numeric(df$x)
  yn <- as.numeric(df$y)
  n <- length(yn)
  if (n < 4) {
    rlang::abort("pv_forecast() needs at least 4 observations.")
  }
  if (any(!is.finite(yn)) || any(!is.finite(xn))) {
    rlang::abort("pv_forecast() needs finite x and y values throughout.")
  }

  sp <- forecast_spacing(xn, is_date)
  if (!sp$regular) {
    steps <- diff(xn)
    rlang::warn(sprintf(paste(
      "The x values are irregularly spaced (steps from %s to %s%s).",
      "The model treats the series as evenly spaced, and the forecast",
      "extends x by the median step of %s - read the fan accordingly."),
      format(min(steps)), format(max(steps)),
      if (is_date) " days" else "", format(sp$step)))
  }

  # The three fits. Each hands back the point forecasts plus a
  # horizon-by-levels matrix of interval half-widths, all computed by
  # stats:: exactly as documented above.
  z <- stats::qnorm(1 / 2 + levels / 200)
  if (method == "ets") {
    hw_try <- function(...) {
      tryCatch(stats::HoltWinters(...), error = function(e) NULL)
    }
    fit <- NULL
    seasonal <- !is.na(sp$freq) && n >= 2 * sp$freq
    if (seasonal) {
      fit <- hw_try(stats::ts(yn, frequency = sp$freq))
      if (is.null(fit)) {
        fit <- hw_try(stats::ts(yn, frequency = sp$freq),
                      start.periods = 3)
      }
      if (is.null(fit)) {
        rlang::warn(sprintf(paste(
          "The seasonal Holt-Winters fit (period %d) failed to",
          "converge; falling back to the non-seasonal fit."), sp$freq))
        seasonal <- FALSE
      }
    }
    if (is.null(fit)) {
      fit <- hw_try(stats::ts(yn), gamma = FALSE)
    }
    if (is.null(fit)) {
      rlang::abort(paste(
        "The Holt-Winters optimiser failed on this series - try",
        'method = "naive" for the plain baseline.'))
    }
    # predict() builds the fit/upr/lwr matrix per level; the interval
    # is symmetric, so the half-width is upr minus fit.
    half <- vapply(levels, function(l) {
      pr <- stats::predict(fit, n.ahead = horizon,
                           prediction.interval = TRUE, level = l / 100)
      as.numeric(pr[, "upr"] - pr[, "fit"])
    }, numeric(horizon))
    half <- matrix(half, nrow = horizon)
    yhat <- as.numeric(stats::predict(fit, n.ahead = horizon))
    spec <- if (seasonal) {
      sprintf("Holt-Winters additive, period %d", sp$freq)
    } else {
      "Holt-Winters, non-seasonal"
    }
    ord <- NULL
  } else if (method == "arima") {
    d <- if (stats::var(diff(yn)) < stats::var(yn)) 1L else 0L
    best <- NULL
    best_aic <- Inf
    ord <- NULL
    for (p in 0:2) {
      for (q in 0:2) {
        f <- tryCatch(
          suppressWarnings(stats::arima(yn, order = c(p, d, q))),
          error = function(e) NULL)
        if (!is.null(f) && is.finite(f$aic) && f$aic < best_aic) {
          best <- f
          best_aic <- f$aic
          ord <- c(p, d, q)
        }
      }
    }
    if (is.null(best)) {
      rlang::abort(paste(
        "No ARIMA candidate in the small default grid converged on",
        'this series - try method = "naive" for the plain baseline.'))
    }
    pr <- stats::predict(best, n.ahead = horizon)
    yhat <- as.numeric(pr$pred)
    half <- outer(as.numeric(pr$se), z)
    spec <- sprintf("ARIMA(%d,%d,%d)", ord[1], ord[2], ord[3])
  } else {
    # The naive baseline: last value carried forward, with the
    # random-walk interval documented above - half-width z * sigma *
    # sqrt(h), sigma the sample sd of the first differences.
    yhat <- rep(yn[n], horizon)
    half <- outer(sqrt(seq_len(horizon)), stats::sd(diff(yn)) * z)
    spec <- "last value carried forward, random-walk intervals"
    ord <- NULL
  }

  # Future x positions in the series' own spacing. Dates follow the
  # calendar when the spacing named a unit (month ends and leap years
  # come out right); otherwise the median step carries on.
  if (is_date) {
    last_day <- as.Date(df$x[n])
    fx <- if (!is.null(sp$by)) {
      seq(last_day, by = sp$by, length.out = horizon + 1)[-1]
    } else {
      last_day + round(sp$step * seq_len(horizon))
    }
    fx <- format(fx, "%Y-%m-%d")
  } else {
    fx <- xn[n] + sp$step * seq_len(horizon)
  }

  pts <- data.frame(x = fx, y = yhat)
  for (i in seq_along(levels)) {
    pts[[paste0("lo", levels[i])]] <- yhat - half[, i]
    pts[[paste0("hi", levels[i])]] <- yhat + half[, i]
  }

  # I() keeps a single level an array in the JSON, the shape the fan
  # drawer loops over. `spec` (and the ARIMA order) record exactly what
  # was fitted, so the payload never hides the model.
  fc <- list(method = method, spec = spec, levels = I(levels),
             points = pts)
  if (!is.null(ord)) fc$order <- I(as.integer(ord))
  w$x$forecast <- fc
  w
}
