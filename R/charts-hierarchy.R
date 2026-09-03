# Hierarchy charts: zoomable circle packing, dendrogram. Like every chart
# file, this one only shapes the payload - the drawing lives in
# inst/htmlwidgets/lib/pv-renderers/hierarchy.js.

#' Zoomable D3 circle packing
#'
#' A hierarchy as nested circles: each circle's area encodes its value,
#' its hue names its top-level branch, and deeper levels fade a step
#' toward the background. Click a circle to zoom into it (the classic
#' d3 zoomable pack fly-through); click the background, or the zoomed
#' circle itself, to zoom back out one level. Circles are labelled only
#' where the name honestly fits at the current zoom level; hovering any
#' circle shows its full path, value, and share of the total.
#'
#' @param data A data frame in long form: one row per leaf.
#' @param levels Character vector of column names, outermost grouping
#'   first, defining the hierarchy. The first level takes the palette
#'   colours, so it allows at most 8 distinct groups.
#' @param value Name of the numeric column summed within each circle.
#'   Values must be non-negative — a circle's size is an area.
#' @param labels Write names inside the circles? `"auto"` (the default)
#'   labels exactly the circles the text honestly fits into at the
#'   current zoom level; `TRUE` squeezes labels into circles with about
#'   20% less room than that (the text must still fit at all — tiny
#'   circles stay blank); `FALSE` draws no circle text, leaving the
#'   names to the tooltip alone.
#' @inheritParams pv_bar
#' @return An htmlwidget.
#' @examples
#' # Summed over all 180 cities: what Swiss urban ground is made of.
#' pv_pack(pv_city_landuse, levels = c("group", "category"),
#'         value = "hectares", title = "Urban land use in Switzerland")
#' @export
pv_pack <- function(data, levels, value, labels = "auto",
                    title = NULL, subtitle = NULL, mode = "auto",
                    duration = 600, source = NULL, width = NULL,
                    height = NULL, elementId = NULL) {
  check_columns(data, list(levels, value))
  check_nonempty(data)
  check_value_column(data, value)
  check_flag(labels, "labels")
  if (length(levels) < 1) {
    rlang::abort("`levels` needs at least one column name.")
  }
  # A negative value has no area to fill; the layout would silently
  # come out wrong, so refuse it here.
  if (any(data[[value]] < 0, na.rm = TRUE)) {
    rlang::abort(sprintf(
      "`%s` has negative values; circle areas must be non-negative.",
      value))
  }
  n_top <- length(unique(as.character(data[[levels[[1]]]])))
  if (n_top > 8) {
    rlang::abort(sprintf(paste(
      "`%s` has %d top-level groups but the palette carries 8 colours.",
      "Fold the smallest groups into an \"Other\" group first."),
      levels[[1]], n_top))
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
  pv_widget("pack", c(list(
    root = root, labels = labels, vlab = value
  ), chart_opts(title, subtitle, mode, duration, source)),
  width, height, elementId)
}

#' Interactive D3 dendrogram
#'
#' Draws the tree inside an [stats::hclust()] object: leaves on the
#' right, reading normally, and every merge drawn as a right-angle
#' elbow at its actual merge height on a horizontal scale (axis on
#' top). Give `k` to cut the tree into `k` clusters the way
#' [stats::cutree()] does — each cluster takes one palette colour and
#' the branches above the cut stay neutral, so the cut line is visible
#' at a glance. Hovering any junction highlights its full subtree and
#' reads out how many leaves it holds and at what height it merged;
#' hovering a leaf gives its full name (labels may be truncated to
#' protect the plot).
#'
#' @param hc An object of class `hclust`, e.g. from [stats::hclust()].
#' @param labels Optional character vector naming the leaves, one entry
#'   per observation clustered. Defaults to `hc$labels`, and falls back
#'   to the observation index when the tree carries no labels.
#' @param k Optional number of clusters (an integer from 2 to 8 — the
#'   palette carries 8 colours) to colour the tree by, via
#'   [stats::cutree()]. `NULL` (default) draws the whole tree in one
#'   neutral ink.
#' @inheritParams pv_bar
#' @return An htmlwidget.
#' @examples
#' # Which large cities have similar economies? Cluster their sector mix.
#' big <- c("Luzern", "Zug", "Bern", "Basel", "Lausanne", "Winterthur",
#'          "Biel/Bienne", "St. Gallen")
#' mix <- stats::xtabs(share ~ city + sector_code,
#'                     pv_city_sectors[pv_city_sectors$city %in% big, ])
#' hc <- stats::hclust(stats::dist(scale(mix)), method = "ward.D2")
#' pv_dendrogram(hc, k = 3, title = "Cities by economic structure")
#' @export
pv_dendrogram <- function(hc, labels = NULL, k = NULL,
                          title = NULL, subtitle = NULL, mode = "auto",
                          duration = 500, source = NULL, width = NULL,
                          height = NULL, elementId = NULL) {
  if (!inherits(hc, "hclust")) {
    rlang::abort(paste(
      "`hc` must be an object of class \"hclust\"",
      "(the result of stats::hclust())."))
  }
  n <- nrow(hc$merge) + 1L
  leaf_labels <- labels %||% hc$labels %||% as.character(seq_len(n))
  if (length(leaf_labels) != n) {
    rlang::abort(sprintf(
      "`labels` must have one entry per leaf (%d), not %d.",
      n, length(leaf_labels)))
  }
  leaf_labels <- as.character(leaf_labels)
  cluster <- NULL
  if (!is.null(k)) {
    ok <- is.numeric(k) && length(k) == 1 && !is.na(k) && k == round(k)
    if (!ok || k < 2) {
      rlang::abort("`k` must be a single integer of at least 2.")
    }
    if (k > min(n, 8)) {
      rlang::abort(sprintf(paste(
        "`k` must be at most %d: the palette carries 8 colours and the",
        "tree has %d leaves."), min(n, 8), n))
    }
    cluster <- unname(stats::cutree(hc, k = as.integer(k)))
  }
  # Rebuild the tree that hclust stores flat: row i of `merge` joins two
  # earlier pieces at height[i] - negative entries are single leaves
  # (observation numbers), positive entries earlier merge rows. Recursing
  # from the last row unfolds the whole thing into the nested
  # {name, height, children} shape d3.hierarchy reads.
  build <- function(i) {
    if (i < 0) {
      leaf <- list(name = leaf_labels[-i], height = 0)
      if (!is.null(cluster)) leaf$cluster <- cluster[-i]
      leaf
    } else {
      list(name = "", height = hc$height[i],
           children = list(build(hc$merge[i, 1]), build(hc$merge[i, 2])))
    }
  }
  pv_widget("dendrogram", c(list(
    tree = build(nrow(hc$merge)),
    k = if (is.null(k)) NULL else as.integer(k)
  ), chart_opts(title, subtitle, mode, duration, source)),
  width, height, elementId)
}
