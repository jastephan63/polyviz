# Time-series statistics. Both tools in this file follow the package's
# oldest rule - R computes, JavaScript draws - and push it one step
# further: neither one adds any JavaScript at all. pv_decompose() builds
# an ordinary long data frame and hands it to pv_line() + pv_facet();
# pv_changepoints() finds level shifts in R and marks them through
# pv_annotate(). Everything on screen is machinery that already existed.

# The x column of a time series: dates or numbers, complete, sorted, and
# without repeats. Returns the sorted x and y plus whether x is a date.
# Aborts are plain and name the column, matching the constructors.
ts_xy <- function(data, x, y, fn) {
  xv <- data[[x]]
  if (!inherits(xv, "Date") && !is.numeric(xv)) {
    rlang::abort(sprintf(paste(
      "%s() needs a time order, so `%s` must hold dates or numbers -",
      "a category column has no 'before' and 'after'."), fn, x))
  }
  yv <- as.numeric(data[[y]])
  if (anyNA(xv) || !all(is.finite(yv))) {
    rlang::abort(sprintf(paste(
      "%s() needs a complete series, but `%s`/`%s` contain missing or",
      "non-finite values. Fill or drop them first - a gap has no",
      "decomposition."), fn, x, y))
  }
  ord <- order(xv)
  xv <- xv[ord]
  yv <- yv[ord]
  if (anyDuplicated(xv)) {
    rlang::abort(sprintf(paste(
      "`%s` has repeated values, so this is not a single regular series.",
      "Aggregate to one row per time point first."), x))
  }
  list(x = xv, y = yv, is_date = inherits(xv, "Date"))
}

# Consecutive-month counter for a Date vector: 2020-01 is one step below
# 2020-02 whatever day of the month each row carries, which is how
# monthly and quarterly spacing stay "regular" even though their day
# counts wobble between 28 and 31.
ts_month_index <- function(dates) {
  lt <- as.POSIXlt(dates)
  (lt$year + 1900L) * 12L + lt$mon
}

# Works out the seasonal frequency, or checks the one the caller gave.
# Inference only trusts what the dates state outright: equal one-month
# steps mean monthly data (frequency 12), three-month steps quarterly
# (4), and one-day steps daily data, whose natural short cycle is the
# week (7). Anything else - irregular spacing, a numeric axis with no
# calendar - is refused plainly rather than guessed at.
ts_frequency <- function(xv, is_date, frequency, x) {
  dx <- diff(as.numeric(xv))
  dmo <- if (is_date) diff(ts_month_index(xv)) else integer(0)
  regular <- all(dx == dx[1]) ||
    (length(dmo) > 0 && all(dmo == dmo[1]) && dmo[1] >= 1)
  if (!is.null(frequency)) {
    ok <- is.numeric(frequency) && length(frequency) == 1 &&
      is.finite(frequency) && frequency >= 2 &&
      frequency == floor(frequency)
    if (!ok) {
      rlang::abort(paste(
        "`frequency` must be a single whole number of at least 2 - the",
        "number of observations in one seasonal cycle (12 for monthly",
        "data, 4 for quarterly, 7 for daily data with a weekly rhythm)."))
    }
    if (!regular) {
      rlang::abort(sprintf(paste(
        "`%s` is not evenly spaced, and the decomposition assumes one",
        "observation per regular step. Make the series regular first."),
        x))
    }
    return(as.integer(frequency))
  }
  if (!is_date) {
    rlang::abort(sprintf(paste(
      "`%s` is numeric, which carries no calendar to read a season",
      "from. Pass `frequency` yourself - the number of observations",
      "in one cycle."), x))
  }
  if (length(dmo) > 0 && all(dmo == 1L)) {
    return(12L)
  }
  if (length(dmo) > 0 && all(dmo == 3L)) {
    return(4L)
  }
  if (all(dx == 1)) {
    return(7L)
  }
  rlang::abort(sprintf(paste(
    "Could not infer a seasonal frequency from `%s`: the dates are not",
    "evenly spaced monthly, quarterly, or daily. Pass `frequency`",
    "yourself (12 for monthly, 4 for quarterly, 7 for daily data with",
    "a weekly rhythm)."), x))
}

#' Decompose a time series into trend, seasonal, and remainder
#'
#' Splits one regular time series into the three additive parts a
#' seasonal series is made of - the slow trend, the repeating seasonal
#' shape, and the remainder left over - and returns them as four line
#' panels stacked in one column: observed, trend, seasonal, remainder.
#' The panels share the x axis, so a feature lines up vertically across
#' all four; a dashed zero line rides along in each panel, marking the
#' level the seasonal and remainder components swing around (in the
#' observed and trend panels it coincides with the zero baseline).
#'
#' The statistics run in R, on the exact functions a statistician would
#' call by hand. `method = "stl"` (the default) is [stats::stl()] -
#' seasonal-trend decomposition by loess, with `s.window = "periodic"`
#' unless `s_window` says otherwise - and estimates the trend all the
#' way to both ends of the series. `method = "classical"` is
#' [stats::decompose()]: a centred moving average for the trend and
#' per-cycle averages for the seasonal shape. The moving average has no
#' value in the first and last half cycle, so those points are simply
#' absent from the classical trend and remainder panels - the method's
#' honest edge, not missing data. Both methods are additive:
#' observed = trend + seasonal + remainder at every point where the
#' parts are defined (for a multiplicative series, decompose its log
#' upstream).
#'
#' The panels deliberately do **not** share a y scale (`pv_facet()`'s
#' `share_y = FALSE`): the seasonal swing and the remainder are usually
#' a small fraction of the observed level, and forcing all four panels
#' onto the observed scale would flatten them into near-straight lines.
#' Each panel scales to its own data instead - the convention
#' `plot(stl(...))` uses - so read amplitudes off each panel's own
#' axis, and compare timing, not height, across panels.
#'
#' The seasonal `frequency` is read from the dates when they speak
#' plainly: equal one-month steps mean monthly data (12), three-month
#' steps quarterly (4), and one-day steps daily data, taken as a weekly
#' rhythm (7). Irregular dates, and numeric x values (which carry no
#' calendar), need `frequency` passed explicitly.
#'
#' @param data A data frame holding one regular time series - one row
#'   per time point, no gaps, no missing values.
#' @param x Name of the time column: `Date` or numeric, evenly spaced.
#' @param y Name of the numeric value column.
#' @param frequency Observations per seasonal cycle (12 for monthly, 4
#'   for quarterly, 7 for daily data with a weekly rhythm). `NULL`
#'   (default) infers it from the dates as described above.
#' @param method `"stl"` (default) for [stats::stl()], `"classical"`
#'   for [stats::decompose()]. The series must cover at least two full
#'   cycles (stl needs one point more than two cycles).
#' @param s_window The stl seasonal window: `"periodic"` (default) fits
#'   one fixed seasonal shape for the whole series; an odd whole number
#'   of at least 7 lets the seasonal shape drift, with larger numbers
#'   changing more slowly. Ignored by `method = "classical"`, which has
#'   no such window.
#' @param ... Further arguments for [pv_line()] - `title`, `subtitle`,
#'   `source`, `curve`, `height`, and friends. `data`, `x`, `y`,
#'   `series`, and `legend` are set here and cannot be overridden. Left
#'   alone, `ylab` falls back to the `y` column's name, the default
#'   height rises to 720 so four stacked panels keep their room, and on
#'   a date axis the x title is dropped (every panel would repeat it,
#'   and the date ticks already say what the axis is) - pass `xlab` to
#'   bring one back.
#' @return A faceted polyviz widget (as from [pv_facet()]): four line
#'   panels in one column, sharing the x axis. The payload carries the
#'   method and frequency under `$x$decompose`.
#' @examples
#' monthly <- aggregate(gwh ~ date, pv_electricity, sum)
#' pv_decompose(monthly, x = "date", y = "gwh",
#'              title = "Swiss electricity production, decomposed")
#' pv_decompose(monthly, x = "date", y = "gwh", method = "classical")
#' @export
pv_decompose <- function(data, x, y, frequency = NULL,
                         method = c("stl", "classical"),
                         s_window = "periodic", ...) {
  check_columns(data, list(x, y))
  check_nonempty(data)
  check_value_column(data, y)
  method <- match.arg(method)
  s_ok <- identical(s_window, "periodic") ||
    (is.numeric(s_window) && length(s_window) == 1 &&
       is.finite(s_window) && s_window >= 7 && s_window %% 2 == 1)
  if (!s_ok) {
    rlang::abort(paste(
      '`s_window` must be "periodic" or an odd whole number of at',
      "least 7 (the span of the stl seasonal loess window)."))
  }
  series <- ts_xy(data, x, y, "pv_decompose")
  f <- ts_frequency(series$x, series$is_date, frequency, x)
  n <- length(series$y)
  # Each method's own floor: classical needs two full cycles for its
  # per-cycle seasonal averages, stl one point more than that.
  need <- if (method == "stl") 2L * f + 1L else 2L * f
  if (n < need) {
    rlang::abort(sprintf(paste(
      "The series has %d points, but a %s decomposition at frequency",
      "%d needs at least %d - two full seasonal cycles%s. Use more",
      "data or a smaller `frequency`."),
      n, method, f, need,
      if (method == "stl") " plus one point" else ""))
  }

  tsy <- stats::ts(series$y, frequency = f)
  if (method == "stl") {
    fit <- stats::stl(tsy, s.window = s_window)
    comp <- fit$time.series
    trend <- as.numeric(comp[, "trend"])
    seasonal <- as.numeric(comp[, "seasonal"])
    remainder <- as.numeric(comp[, "remainder"])
  } else {
    fit <- stats::decompose(tsy)
    trend <- as.numeric(fit$trend)
    seasonal <- as.numeric(fit$seasonal)
    remainder <- as.numeric(fit$random)
  }

  # The long frame the line chart reads: one row per point per
  # component. The classical trend's undefined edges (and the remainder
  # rows that depend on them) are dropped here - their absence in those
  # panels is the documented behaviour, not something to warn about.
  parts <- list(Observed = series$y, Trend = trend,
                Seasonal = seasonal, Remainder = remainder)
  long <- do.call(rbind, lapply(names(parts), function(nm) {
    data.frame(x = series$x, value = parts[[nm]], component = nm)
  }))
  long <- long[is.finite(long$value), , drop = FALSE]
  rownames(long) <- NULL

  args <- list(...)
  if (length(args) && (is.null(names(args)) || any(!nzchar(names(args))))) {
    rlang::abort(
      "Arguments in `...` must be named options for pv_line().")
  }
  reserved <- intersect(names(args),
                        c("data", "x", "y", "series", "legend"))
  if (length(reserved)) {
    rlang::abort(sprintf(paste(
      "pv_decompose() builds the pv_line() call itself, so `%s` cannot",
      "be passed through `...`."), paste(reserved, collapse = "`, `")))
  }
  # The panels label themselves, so no legend; the y title falls back
  # to the caller's column name; and four stacked panels need more
  # height than one chart, so the default rises unless the caller chose
  # one. On a date axis the x title is dropped by default - each panel
  # would repeat it, and the year ticks already say what the axis is; a
  # numeric axis keeps its column name, which an index genuinely needs.
  if (!"xlab" %in% names(args)) {
    args$xlab <- if (series$is_date) NA else x
  }
  if (!"ylab" %in% names(args)) args$ylab <- y
  if (!"height" %in% names(args)) args$height <- 720

  w <- do.call(pv_line, c(
    list(data = long, x = "x", y = "value", series = "component",
         legend = FALSE), args))
  # The zero reference, before faceting (pv_annotate speaks line, not
  # facet). The facet renderer clones top-level fields into every
  # panel, so the one hline lands in all four: at the axis baseline in
  # the observed and trend panels, and as the honest centre line of the
  # seasonal and remainder panels.
  w <- pv_annotate(w, pv_hline(0))
  # share_y = FALSE is the decompose convention (see above); the shared
  # x limits are what keep a feature vertically aligned across panels.
  fw <- pv_facet(w, by = w$x$data$series, ncol = 1, share_y = FALSE)

  # pv_facet ordered the panels the way the line constructor sorted its
  # series - alphabetically. A decomposition reads top to bottom in one
  # fixed order, so put the panels back into it.
  names_now <- vapply(fw$x$panels, `[[`, character(1), "name")
  fw$x$panels <- fw$x$panels[match(names(parts), names_now)]
  fw$x$decompose <- list(method = method, frequency = f)

  # The generated alt text described a four-line chart; say what this
  # actually is. Facts only, straight from the computed components.
  clause <- sprintf(
    "splitting %s into trend, seasonal, and remainder parts (%s, frequency %d)",
    alt_lab(fw$x$ylab), if (method == "stl") "stl" else "classical", f)
  fin_tr <- trend[is.finite(trend)]
  fw$x$alt <- paste(
    alt_lead(fw$x, "A seasonal decomposition", clause),
    sprintf(paste(
      "Four aligned line panels share the %s axis, each on its own",
      "value scale: observed, trend, seasonal, remainder."),
      alt_lab(fw$x$xlab, if (series$is_date) "date" else "x")),
    sprintf(paste(
      "The trend runs from %s to %s; the seasonal part swings between",
      "%s and %s around zero."),
      alt_num(fin_tr[1]), alt_num(fin_tr[length(fin_tr)]),
      alt_num(min(seasonal)), alt_num(max(seasonal))))
  fw
}

# ---- changepoints -----------------------------------------------------------

# Residual sum of squares of y[s..e] around its own mean, from the
# cumulative sums - O(1) per segment, clamped at zero where floating
# point dips below it.
cp_rss <- function(cs, cs2, s, e) {
  m <- e - s + 1
  max(0, (cs2[e + 1] - cs2[s]) - (cs[e + 1] - cs[s])^2 / m)
}

# The best single split of y[s..e]: the cut point that removes the most
# residual sum of squares, leaving at least min_seg points on each
# side. Ties go to the earliest cut (which.max), so the result never
# depends on evaluation order. NULL when the segment is too short to
# split at all.
cp_best_split <- function(cs, cs2, s, e, min_seg) {
  lo <- s + min_seg - 1
  hi <- e - min_seg
  if (hi < lo) {
    return(NULL)
  }
  t <- lo:hi
  m1 <- t - s + 1
  r1 <- pmax(0, (cs2[t + 1] - cs2[s]) - (cs[t + 1] - cs[s])^2 / m1)
  m2 <- e - t
  r2 <- pmax(0, (cs2[e + 1] - cs2[t + 1]) - (cs[e + 1] - cs[t + 1])^2 / m2)
  gain <- cp_rss(cs, cs2, s, e) - (r1 + r2)
  i <- which.max(gain)
  list(at = t[i], gain = gain[i])
}

# Binary segmentation for shifts in mean, with a BIC stopping rule.
# Greedy and classical: repeatedly take the single cut anywhere in the
# series that most reduces the residual sum of squares, and keep it
# only while BIC judges the larger model worth its extra parameters.
# Under a common-variance normal model, adding a changepoint adds two
# parameters (a mean and a cut position), so a cut is accepted while
#   n * log(RSS_after / RSS_before) + 2 * log(n) < 0.
# Deterministic throughout: the same y always yields the same cuts.
# Returns the cut positions as the last index of each left-hand
# segment, in increasing order.
cp_binseg <- function(y, max_changes, min_seg) {
  n <- length(y)
  cs <- c(0, cumsum(y))
  cs2 <- c(0, cumsum(y * y))
  segs <- list(c(1L, n))
  cuts <- integer(0)
  total <- cp_rss(cs, cs2, 1L, n)
  while (length(cuts) < max_changes) {
    cands <- lapply(segs, function(se) {
      cp_best_split(cs, cs2, se[1], se[2], min_seg)
    })
    gains <- vapply(cands, function(cand) {
      if (is.null(cand)) -Inf else cand$gain
    }, numeric(1))
    i <- which.max(gains)
    if (!is.finite(gains[i]) || gains[i] <= 0) {
      break
    }
    new_total <- total - gains[i]
    # A perfect fit has no likelihood left to compare; stop rather than
    # take log(0).
    if (new_total <= 0) {
      break
    }
    if (n * log(new_total / total) + 2 * log(n) >= 0) {
      break
    }
    at <- cands[[i]]$at
    se <- segs[[i]]
    segs[[i]] <- c(se[1], at)
    segs[[length(segs) + 1]] <- c(at + 1L, se[2])
    cuts <- c(cuts, at)
    total <- new_total
  }
  sort(cuts)
}

# One axis position for annotations and labels. The d3 time scale reads
# numbers as milliseconds since epoch but not the ISO strings the data
# rows carry (those go through the renderer's own parser), so date
# positions ship as epoch milliseconds and keep the readable ISO date
# for the label.
cp_at <- function(v, is_date) {
  if (is_date) as.numeric(as.Date(v)) * 86400000 else as.numeric(v)
}

cp_label <- function(v, is_date) {
  if (is_date) as.character(v) else alt_pos(as.numeric(v))
}

#' Mark sustained level shifts on a line chart
#'
#' Scans a single-series line chart for points where the series' mean
#' level shifts and stays shifted, and marks each one with a dashed
#' vertical line through the existing annotation machinery - the same
#' marks [pv_vline()] draws by hand, labelled with the date (or x
#' value) of the first observation at the new level. `levels = TRUE`
#' additionally draws a dashed horizontal line at each stretch's mean;
#' those lines span the whole chart width, so read each one against its
#' own stretch between the vertical marks.
#'
#' The statistics are computed in R, from first principles: binary
#' segmentation for shifts in mean with a BIC stopping rule. The series
#' is repeatedly cut at the point that most reduces the residual sum of
#' squares around segment means, and each cut is kept only while the
#' Bayesian information criterion judges the extra mean-and-position
#' parameters worth their cost - at most `max_changes` cuts, each
#' leaving at least 5 points on both sides. The result is
#' deterministic: the same chart always yields the same marks.
#'
#' Be honest about what this finds: **sustained shifts in the average
#' level**, not every wiggle. A trend that climbs smoothly, a seasonal
#' cycle, or a change in spread rather than level can all be carved
#' into spurious "shifts" or masked entirely - detrend or deseasonalise
#' first (see [pv_decompose()]) when the series has those. And BIC is a
#' screening rule, not proof: on noisy series it can add a point too
#' many or miss a small true shift. When no cut survives the rule, the
#' chart comes back unchanged, with a message saying so.
#'
#' @param w A polyviz line chart with a single series and a date or
#'   numeric x axis, carrying at least 20 points.
#' @param levels Also draw each stretch's mean level as a dashed
#'   horizontal line, labelled with the mean? Default `FALSE`.
#' @param max_changes The most shifts the search may keep (default 5,
#'   the cap; a chart with more vertical marks than that stops being
#'   readable). Must be a whole number from 1 to 5.
#' @return The chart with the marks attached - ready for more pipe
#'   steps. The payload records the analysis under `$x$changepoints`.
#' @examples
#' shifted <- data.frame(day = 1:60,
#'                       value = rep(c(10, 14, 11), each = 20) +
#'                         sin(1:60))
#' pv_line(shifted, x = "day", y = "value",
#'         title = "Two shifts, found and marked") |>
#'   pv_changepoints(levels = TRUE)
#' @export
pv_changepoints <- function(w, levels = FALSE, max_changes = 5) {
  check_pv_widget(w, "pv_changepoints")
  if (!identical(w$x$type, "line")) {
    rlang::abort(sprintf(paste(
      "pv_changepoints() reads a series in time order, which needs a",
      "line chart - not a %s chart."), w$x$type))
  }
  df <- w$x$data
  if (length(unique(df$series)) > 1) {
    rlang::abort(sprintf(paste(
      "pv_changepoints() works on one series, but this chart has %d.",
      "Build one chart per series and scan each."),
      length(unique(df$series))))
  }
  is_date <- identical(w$x$xtype, "date")
  if (!is_date && !identical(w$x$xtype, "number")) {
    rlang::abort(paste(
      "pv_changepoints() needs a date or numeric x axis; this line",
      "chart has a category axis, whose order the data does not fix."))
  }
  if (!isTRUE(levels) && !isFALSE(levels)) {
    rlang::abort("`levels` must be TRUE or FALSE.")
  }
  if (!is.numeric(max_changes) || length(max_changes) != 1 ||
      is.na(max_changes) || max_changes != round(max_changes) ||
      max_changes < 1 || max_changes > 5) {
    rlang::abort("`max_changes` must be a whole number from 1 to 5.")
  }

  # The constructor sorted the rows by x and refused duplicates, so the
  # y column already is the series in time order; the sort here only
  # defends against a payload someone rearranged by hand.
  xv <- if (is_date) as.Date(df$x) else as.numeric(df$x)
  ord <- order(xv)
  xv <- xv[ord]
  yv <- as.numeric(df$y)[ord]
  # The constructor already dropped missing values; infinities would
  # poison every cumulative sum below, so refuse them outright.
  if (!all(is.finite(yv))) {
    rlang::abort(
      "pv_changepoints() needs finite values; this chart carries Inf.")
  }
  n <- length(yv)
  if (n < 20) {
    rlang::abort(sprintf(paste(
      "pv_changepoints() needs at least 20 points to tell a sustained",
      "shift from noise; this chart has %d."), n))
  }

  cuts <- cp_binseg(yv, as.integer(max_changes), min_seg = 5L)
  if (!length(cuts)) {
    rlang::inform(paste(
      "pv_changepoints() found no sustained level shifts (binary",
      "segmentation, BIC); the chart is returned unchanged."))
    return(w)
  }

  # Each cut is the last point of the old level; the mark stands on the
  # first point of the new one, wearing its date (or x value).
  starts <- xv[cuts + 1L]
  bounds <- c(0L, cuts, n)
  seg_means <- vapply(seq_len(length(bounds) - 1L), function(i) {
    mean(yv[(bounds[i] + 1L):bounds[i + 1L]])
  }, numeric(1))

  anns <- lapply(starts, function(v) {
    pv_vline(cp_at(v, is_date), label = cp_label(v, is_date))
  })
  if (levels) {
    anns <- c(anns, lapply(seg_means, function(m) {
      pv_hline(m, label = paste("mean", alt_num(m)))
    }))
  }
  w <- do.call(pv_annotate, c(list(w), anns))

  labels <- vapply(starts, cp_label, character(1), is_date = is_date)
  w$x$changepoints <- list(
    index = as.integer(cuts + 1L), x = labels, mean = seg_means)
  note <- sprintf(paste(
    "Changepoint analysis (binary segmentation with a BIC stopping",
    "rule) marks %s in the mean level, starting at %s."),
    alt_count(length(cuts), "sustained shift", "sustained shifts"),
    alt_join(labels))
  if (levels) {
    note <- paste(note,
                  "Dashed horizontal lines mark each stretch's mean.")
  }
  w$x$alt <- if (alt_str(w$x$alt)) paste(w$x$alt, note) else note
  w
}
