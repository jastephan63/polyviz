# Distribution charts: histogram, boxplot, ridgeline. All the statistics
# come from the helpers in stats-helpers.R — R does the maths, and the
# JavaScript side (lib/pv-renderers/distribution.js) only draws geometry.

check_numeric_col <- function(data, col) {
  if (!is.numeric(data[[col]])) {
    rlang::abort(sprintf("`%s` must be a numeric column.", col))
  }
}

# The categorical palette holds 8 colours, assigned in order and never
# cycled. More groups than that is a data problem, not a colour problem.
check_palette_fit <- function(groups, col) {
  if (length(groups) > 8) {
    rlang::abort(sprintf(
      paste("`%s` has %d levels but the palette has 8 colours.",
            "Fold the rare levels into an \"Other\" group first."),
      col, length(groups)))
  }
}

# A reproducible sample that leaves the caller's random stream untouched:
# stash .Random.seed, draw with a fixed seed, put everything back.
sample_fixed <- function(x, size) {
  seed <- if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
    get(".Random.seed", envir = globalenv(), inherits = FALSE)
  }
  on.exit({
    if (is.null(seed)) {
      rm(".Random.seed", envir = globalenv())
    } else {
      assign(".Random.seed", seed, envir = globalenv())
    }
  })
  set.seed(4242)
  sample(x, size)
}

#' Interactive D3 histogram
#'
#' The distribution of one numeric column as bars over equal-width bins,
#' with a tooltip giving each bin's exact range and count. Binning follows
#' R's own [graphics::hist()] rules (Sturges by default), so the shape
#' matches what base R would show. With `density = TRUE` a kernel density
#' curve is overlaid, rescaled to count space (density × n × bin width) so
#' the curve and the bars share one honest y axis.
#'
#' @param data A data frame.
#' @param x Name of the numeric column to bin.
#' @param bins Number of equal-width bins, or `NULL` for R's Sturges
#'   default.
#' @param density Overlay a kernel density curve?
#' @inheritParams pv_bar
#' @return An htmlwidget.
#' @examples
#' lucerne25 <- subset(pv_fiscal, year == 2025)
#' pv_histogram(lucerne25, x = "resource_per_capita", bins = 20,
#'              density = TRUE,
#'              title = "Tax resources per resident, 2025")
#' @export
pv_histogram <- function(data, x, bins = NULL, density = FALSE,
                         title = NULL, subtitle = NULL, mode = "auto",
                         duration = 500, source = NULL, width = NULL,
                         height = NULL, elementId = NULL) {
  check_columns(data, list(x))
  check_numeric_col(data, x)
  vals <- data[[x]][!is.na(data[[x]])]
  if (length(vals) < 2) {
    rlang::abort("`x` needs at least 2 non-missing values.")
  }
  bars <- pv_histbins(vals, bins)
  curve <- NULL
  if (isTRUE(density)) {
    # density() integrates to 1; multiplying by n and the bin width turns
    # it into expected counts per bin, which is the scale the bars use.
    kde <- pv_kde(vals)
    binwidth <- bars$x1[1] - bars$x0[1]
    curve <- data.frame(x = kde$x, y = kde$y * length(vals) * binwidth)
  }
  pv_widget("histogram", c(list(
    data = bars, density = curve, xlab = x, ylab = "count"
  ), chart_opts(title, subtitle, mode, duration, source)),
  width, height, elementId)
}

#' Interactive D3 boxplot
#'
#' Tukey boxplots of a numeric column, one box per group: the box spans
#' the quartiles, the 2px line is the median, whiskers reach the most
#' extreme values within 1.5 IQR of the box, and anything beyond is drawn
#' as an outlier dot. Hovering a box dims the others and reads out all
#' five numbers plus the group size.
#'
#' @param data A data frame.
#' @param value Name of the numeric column to summarise.
#' @param group Optional name of a grouping column (one box per level,
#'   max 8). Omit it for a single box.
#' @param points Also show the raw values as jittered points behind each
#'   box? Groups with more than 400 values are thinned to a sample of 400,
#'   drawn with a fixed seed so the same data always shows the same
#'   points. The box statistics always use every value.
#' @inheritParams pv_bar
#' @return An htmlwidget.
#' @examples
#' pv_boxplot(pv_fiscal, value = "resource_per_capita", group = "year",
#'            points = TRUE, title = "Municipal tax resources by year")
#' @export
pv_boxplot <- function(data, value, group = NULL, points = FALSE,
                       title = NULL, subtitle = NULL, mode = "auto",
                       duration = 600, source = NULL, width = NULL,
                       height = NULL, elementId = NULL) {
  check_columns(data, list(value, group))
  check_numeric_col(data, value)
  grp <- if (is.null(group)) {
    # No grouping: one box, labelled with the column it summarises.
    rep(value, nrow(data))
  } else {
    as.character(data[[group]])
  }
  keep <- !is.na(data[[value]]) & !is.na(grp)
  vals <- data[[value]][keep]
  grp <- grp[keep]
  if (!length(vals)) {
    rlang::abort("`value` needs at least 1 non-missing value.")
  }
  groups <- unique(grp)
  check_palette_fit(groups, if (is.null(group)) value else group)

  boxes <- lapply(groups, function(gname) {
    s <- pv_boxstats(vals[grp == gname])
    list(group = gname, q1 = s$q1, median = s$median, q3 = s$q3,
         lo = s$lo, hi = s$hi, n = s$n,
         # I() keeps a lone outlier serialising as an array, not a scalar.
         outliers = I(as.numeric(s$outliers)))
  })
  pts <- NULL
  if (isTRUE(points)) {
    pts <- do.call(rbind, lapply(groups, function(gname) {
      v <- vals[grp == gname]
      if (length(v) > 400) v <- sample_fixed(v, 400)
      data.frame(group = gname, value = v)
    }))
  }
  pv_widget("boxplot", c(list(
    boxes = boxes, points = pts, xlab = group, ylab = value
  ), chart_opts(title, subtitle, mode, duration, source)),
  width, height, elementId)
}

#' Interactive D3 ridgeline chart
#'
#' Overlapping kernel density curves, one ridge per group, ordered by
#' median from the top down — the readable way to compare the shape of a
#' distribution across up to 8 groups. Hovering a ridge lifts it to full
#' opacity, dims the rest, and shows the group's median and size.
#'
#' @param data A data frame.
#' @param value Name of the numeric column whose distribution is drawn.
#' @param group Name of the grouping column (one ridge per level, max 8;
#'   every level needs at least 2 non-missing values).
#' @inheritParams pv_bar
#' @return An htmlwidget.
#' @examples
#' pv_ridgeline(pv_fiscal, value = "resource_index", group = "year",
#'              title = "Resource index across municipalities")
#' @export
pv_ridgeline <- function(data, value, group,
                         title = NULL, subtitle = NULL, mode = "auto",
                         duration = 600, source = NULL, width = NULL,
                         height = NULL, elementId = NULL) {
  check_columns(data, list(value, group))
  check_numeric_col(data, value)
  grp <- as.character(data[[group]])
  keep <- !is.na(data[[value]]) & !is.na(grp)
  vals <- data[[value]][keep]
  grp <- grp[keep]
  # All-missing data would otherwise sail through as zero ridges and hand
  # the renderer an empty array it cannot build scales from.
  if (!length(vals)) {
    rlang::abort("`value` needs at least 2 non-missing values.")
  }
  # Ridges read best sorted by their centre, largest on top.
  meds <- sort(tapply(vals, grp, stats::median), decreasing = TRUE)
  groups <- names(meds)
  check_palette_fit(groups, group)
  counts <- table(grp)
  small <- groups[counts[groups] < 2]
  if (length(small)) {
    rlang::abort(sprintf(
      "Each `group` level needs at least 2 values for a density; too few in: %s",
      paste(small, collapse = ", ")))
  }

  ridges <- lapply(groups, function(gname) {
    v <- vals[grp == gname]
    list(group = gname, median = stats::median(v), n = length(v),
         points = pv_kde(v))
  })
  pv_widget("ridgeline", c(list(
    ridges = ridges, xlab = value
  ), chart_opts(title, subtitle, mode, duration, source)),
  width, height, elementId)
}
