# Internal statistics helpers shared by the distribution charts.
# The design rule for polyviz: statistics are computed here in R, and the
# JavaScript side only draws geometry. That keeps the JS simple and means
# results match what R users expect from stats::density() and friends.

# Kernel density estimate as a plain two-column data frame, trimmed to the
# observed range so ridgelines and density plots don't trail off into
# regions with no data.
pv_kde <- function(x, n = 200, adjust = 1) {
  x <- x[!is.na(x)]
  if (length(x) < 2) {
    return(data.frame(x = numeric(0), y = numeric(0)))
  }
  d <- stats::density(x, n = n, adjust = adjust, from = min(x), to = max(x))
  data.frame(x = d$x, y = d$y)
}

# The five numbers a boxplot needs, plus the outliers beyond the whiskers.
# Whiskers use the classic Tukey rule: the most extreme data points still
# within 1.5 IQR of the box.
pv_boxstats <- function(x) {
  x <- x[!is.na(x)]
  q <- stats::quantile(x, c(0.25, 0.5, 0.75), names = FALSE)
  iqr <- q[3] - q[1]
  lo_fence <- q[1] - 1.5 * iqr
  hi_fence <- q[3] + 1.5 * iqr
  inside <- x[x >= lo_fence & x <= hi_fence]
  list(
    q1 = q[1], median = q[2], q3 = q[3],
    lo = min(inside), hi = max(inside),
    outliers = x[x < lo_fence | x > hi_fence],
    n = length(x)
  )
}

# Histogram bins as a data frame of bar positions. Uses R's own binning
# (Sturges by default) so the chart matches what hist() would show.
pv_histbins <- function(x, bins = NULL) {
  x <- x[!is.na(x)]
  breaks <- if (is.null(bins)) "Sturges" else
    seq(min(x), max(x), length.out = bins + 1)
  h <- graphics::hist(x, breaks = breaks, plot = FALSE)
  data.frame(x0 = utils::head(h$breaks, -1), x1 = h$breaks[-1],
             count = h$counts)
}
