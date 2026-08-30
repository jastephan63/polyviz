# Small multiples. pv_facet() takes an already-built bar, line, scatter,
# or area widget and rewrites its payload into a grid of panels, one per
# level of a grouping vector. The JavaScript side
# (inst/htmlwidgets/lib/pv-renderers/facet.js) lays out the grid and asks
# the original chart's renderer to draw each panel.

# The renderers apply an explicit xlim/ylim verbatim - no nice() - so
# the shared limits are rounded here to what d3's default nice() would
# have picked, keeping faceted axes looking like unfaceted ones.
facet_nice <- function(r) {
  range(pretty(r, n = 10))
}

# Works out what kind of x values a faceted payload carries, so the
# shared x domain can be computed the right way. Bar categories are
# always strings; scatter x is always numeric; line and area recorded
# their axis type when the widget was built.
facet_xtype <- function(x) {
  switch(x$subtype,
    bar = "category",
    scatter = "number",
    x$xtype
  )
}

#' Facet a chart into small multiples
#'
#' Splits a bar, line, scatter, or area chart into a grid of panels, one
#' per level of a grouping vector — the small-multiples layout. Panels
#' appear in the order their group first appears in `by`, each carries
#' its name at the top left, and the title, subtitle, and source line
#' are drawn once for the whole grid.
#'
#' The chart functions keep only the mapped columns, so `by` is passed as
#' the original column of your data frame, not a column name:
#' `pv_scatter(df, x = "wt", y = "mpg") |> pv_facet(df$region)`. It must
#' line up row-for-row with the data the chart function received. Line
#' and area charts group their rows by series internally (and sorted
#' bars re-order by value), so hand those charts data that is already
#' sorted by series — then the row order survives unchanged and `by`
#' stays aligned.
#'
#' By default all panels share both axis ranges, which is what makes
#' small multiples comparable: the global x and y domains are computed
#' here across every panel and injected into the payload as `xlim`/
#' `ylim` for the renderers to honour. Set `share_y = FALSE` (or
#' `share_x = FALSE`) to let each panel scale to its own data — useful
#' when the panels live on very different orders of magnitude, but then
#' the panels no longer compare directly. For stacked areas the shared y
#' range covers the tallest stack; percent and stream areas normalise
#' themselves, so `share_y` has nothing to do there.
#'
#' @param w A polyviz widget made by [pv_bar()], [pv_line()],
#'   [pv_scatter()], or [pv_area()].
#' @param by A vector with one entry per row of the chart's data,
#'   assigning each row to a panel. Between 2 and 16 distinct levels.
#' @param ncol Number of grid columns, or `NULL` (default) for a
#'   near-square grid. The renderer never uses more than 4 columns — 2
#'   when the container is narrower than 700 pixels, and a single
#'   scrolling column below 440 — so every panel keeps room for its own
#'   axes and tick labels.
#' @param share_y,share_x Share the y (x) axis range across all panels?
#'   Default `TRUE`.
#' @return The widget, rewritten into a faceted one.
#' @examples
#' pop <- subset(pv_city_population,
#'               city %in% c("Luzern", "Emmen", "Kriens", "Zug"))
#' pv_line(pop, x = "year", y = "population", series = "city",
#'         title = "Four cities, four panels") |>
#'   pv_facet(pop$city, ncol = 2)
#'
#' fiscal <- subset(pv_fiscal, year >= 2023)
#' pv_scatter(fiscal, x = "resource_index", y = "equalization_chf") |>
#'   pv_facet(fiscal$year)
#' @export
pv_facet <- function(w, by, ncol = NULL, share_y = TRUE, share_x = TRUE) {
  if (!inherits(w, "htmlwidget") ||
      !identical(attr(w, "package"), "polyviz")) {
    rlang::abort(
      "`w` must be a polyviz chart, e.g. pv_line(...) |> pv_facet(...).")
  }
  if (identical(w$x$type, "facet")) {
    rlang::abort("This chart is already faceted.")
  }
  supported <- c("bar", "line", "scatter", "area")
  if (!w$x$type %in% supported) {
    rlang::abort(sprintf(
      "pv_facet() works on %s charts; this is a %s chart, which has no small-multiples layout.",
      paste(supported, collapse = ", "), w$x$type))
  }
  if (!(isTRUE(share_y) || isFALSE(share_y))) {
    rlang::abort("`share_y` must be TRUE or FALSE.")
  }
  if (!(isTRUE(share_x) || isFALSE(share_x))) {
    rlang::abort("`share_x` must be TRUE or FALSE.")
  }
  if (!is.null(ncol)) {
    ok <- is.numeric(ncol) && length(ncol) == 1 && !is.na(ncol) &&
      ncol >= 1 && ncol == floor(ncol)
    if (!ok) {
      rlang::abort("`ncol` must be a single whole number of at least 1.")
    }
  }

  data <- w$x$data
  if (is.null(by) || !is.atomic(by)) {
    rlang::abort(paste(
      "`by` must be a vector with one entry per data row - pass the",
      "original column, e.g. pv_scatter(df, ...) |> pv_facet(df$region)."))
  }
  if (length(by) != nrow(data)) {
    rlang::abort(sprintf(paste(
      "`by` has %d values but the chart carries %d data rows. Pass the",
      "original column of the same data frame the chart was built from,",
      "e.g. pv_scatter(df, ...) |> pv_facet(df$region)."),
      length(by), nrow(data)))
  }
  if (anyNA(by)) {
    rlang::abort(
      "`by` contains missing values; every row needs a panel.")
  }

  # One panel per level, in the order each level first appears - the
  # same convention the charts use for series colours.
  keys <- as.character(by)
  levels <- unique(keys)
  if (length(levels) < 2) {
    rlang::abort(
      "`by` has a single level; faceting needs at least 2 panels.")
  }
  if (length(levels) > 16) {
    rlang::abort(sprintf(paste(
      "`by` has %d levels, but more than 16 panels stop being readable.",
      "Collapse the variable or filter the data first."),
      length(levels)))
  }
  panels <- lapply(levels, function(k) {
    rows <- data[keys == k, , drop = FALSE]
    rownames(rows) <- NULL
    list(name = k, data = rows)
  })

  # Rewrite the payload: the facet renderer reads the subtype and panel
  # list, every other field stays once at the top level and is cloned
  # into each panel's sub-payload on the JavaScript side.
  w$x$subtype <- w$x$type
  w$x$type <- "facet"
  w$x$panels <- panels
  w$x$data <- NULL
  if (!is.null(ncol)) w$x$ncol <- as.integer(ncol)

  # Shared axis ranges, computed across every panel. The cartesian
  # renderers honour xlim/ylim over their own data-driven domains, so
  # injecting the global extent here is all the sharing needs.
  if (share_x) {
    w$x$xlim <- switch(facet_xtype(w$x),
      number = facet_nice(range(as.numeric(data$x), na.rm = TRUE)),
      # Dates travel as ISO strings; convert properly to find the true
      # range, then send ISO strings back for d3 to re-parse.
      date = format(range(as.Date(data$x)), "%Y-%m-%d"),
      # Category domains have no numeric range: the shared domain is the
      # union of categories, again in first-appearance order. I() keeps
      # a single category an array in the JSON.
      category = I(unique(as.character(data$x)))
    )
  }
  if (share_y) {
    yv <- as.numeric(data$y)
    ylim <- if (w$x$subtype == "scatter") {
      # Scatter plots fit their y domain to the data on both ends.
      range(yv, na.rm = TRUE)
    } else if (w$x$subtype == "area") {
      if (identical(w$x$offset, "stacked")) {
        # A stacked area's ceiling is the tallest stack, not the largest
        # single value: sum each panel's values at each x.
        totals <- tapply(yv, paste(keys, data$x, sep = "\r"), sum,
                         na.rm = TRUE)
        c(min(0, min(yv, na.rm = TRUE)), max(totals))
      } else {
        # Percent and stream areas normalise every panel to the same
        # scale by construction; there is no range to share.
        NULL
      }
    } else {
      # Bars and lines anchor at zero (or below, for negative values).
      c(min(0, min(yv, na.rm = TRUE)), max(yv, na.rm = TRUE))
    }
    if (!is.null(ylim)) w$x$ylim <- facet_nice(ylim)
  }

  w
}
