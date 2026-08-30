# Geographic charts: the choropleth map. Like every chart file, this one
# only shapes the payload - the drawing lives in
# inst/htmlwidgets/lib/pv-renderers/geo.js.

#' Interactive D3 choropleth map
#'
#' Regions of a map coloured by a numeric value — the chart for showing
#' how a measure varies across space. The map arrives as a GeoJSON
#' FeatureCollection stored as a plain R list (the bundled
#' [pv_lucerne_map] is the template: each feature carries
#' `properties$id` and `properties$name`), and `data` joins onto it by
#' matching the `id` column against the features' `properties$id`.
#'
#' `palette = "sequential"` maps magnitude onto the theme's single-hue
#' ramp over the data range. `palette = "diverging"` is for values that
#' sit around a meaningful reference point — an index of 100, a change of
#' zero — and needs that point as `center`: the neutral midpoint colour is
#' pinned there and the colour domain is made symmetric around it, so
#' equal distances above and below carry equally strong colour.
#'
#' Regions with no data row keep a neutral fill and a dashed outline —
#' absence stays visible instead of masquerading as a low value. Hovering
#' a region outlines it and shows its name and exact value, and a compact
#' colour-scale legend sits in the header, with the centre marked for
#' diverging palettes.
#'
#' @param data A data frame with at most one row per region.
#' @param map A GeoJSON FeatureCollection stored as an R list, with each
#'   feature carrying `properties$id` (join key) and `properties$name`
#'   (shown in tooltips). Defaults to [pv_lucerne_map], the 79 Lucerne
#'   municipalities.
#' @param id Name of the column joining `data` to the map's feature ids.
#'   Ids that match no feature are dropped with a warning; if none match
#'   at all, that is an error.
#' @param value Name of the numeric column mapped to colour. Rows with a
#'   missing value are dropped — their regions read as "no data".
#' @param palette `"sequential"` (default, for magnitudes) or
#'   `"diverging"` (for values around a reference point, which needs
#'   `center`).
#' @param center The reference value the diverging palette's neutral
#'   midpoint stands for (e.g. `100` for an index, `0` for a change).
#'   Required — and only allowed — when `palette = "diverging"`.
#' @inheritParams pv_bar
#' @return An htmlwidget.
#' @examples
#' fiscal25 <- subset(pv_fiscal, year == 2025)
#' pv_choropleth(fiscal25, id = "municipality_id", value = "resource_index",
#'               palette = "diverging", center = 100,
#'               title = "Fiscal strength of Lucerne municipalities")
#' @export
pv_choropleth <- function(data, map = pv_lucerne_map, id, value,
                          palette = c("sequential", "diverging"),
                          center = NULL,
                          title = NULL, subtitle = NULL, mode = "auto",
                          duration = 500, source = NULL, width = NULL,
                          height = NULL, elementId = NULL) {
  check_columns(data, list(id, value))
  palette <- match.arg(palette)
  # The map must at least look like a FeatureCollection before it is sent
  # across - d3 would otherwise fail with something unreadable.
  if (!is.list(map) || !identical(map$type, "FeatureCollection") ||
      !length(map$features)) {
    rlang::abort(paste(
      "`map` must be a GeoJSON FeatureCollection stored as an R list -",
      "a `type` of \"FeatureCollection\" and a non-empty `features` -",
      "like `pv_lucerne_map`."))
  }
  # The join keys the map offers. Ids are compared as strings on both
  # sides (here and in JavaScript), so 1001 and "1001" are the same
  # region. Background features without an id (lakes, in the bundled
  # map) simply never take part in the join.
  map_ids <- unlist(lapply(map$features, function(f) {
    v <- f$properties$id
    if (is.null(v)) NULL else as.character(v)
  }))
  if (!length(map_ids)) {
    rlang::abort("`map` has no feature with a `properties$id` to join on.")
  }

  df <- data.frame(id = as.character(data[[id]]),
                   value = as.numeric(data[[value]]))
  df <- df[!is.na(df$value), , drop = FALSE]
  rownames(df) <- NULL
  if (nrow(df) == 0) {
    rlang::abort("`value` has no non-missing values; nothing to colour.")
  }
  if (anyDuplicated(df$id)) {
    rlang::abort(
      "`data` has more than one row per region id; aggregate it first.")
  }

  # A join that silently matches nothing draws an all-grey map, so ids
  # that miss the map are worth a warning - and missing with every single
  # id means the wrong column or the wrong map, which is an error.
  unmatched <- setdiff(df$id, map_ids)
  if (length(unmatched) == nrow(df)) {
    rlang::abort(sprintf(
      "No value in `%s` matches any feature id on the map (map ids look like: %s). Wrong column, or wrong map?",
      id, paste(utils::head(map_ids, 3), collapse = ", ")))
  }
  if (length(unmatched)) {
    shown <- utils::head(unmatched, 5)
    extra <- length(unmatched) - length(shown)
    rlang::warn(sprintf(
      "%d id(s) in `%s` match no map feature and will not be drawn: %s%s.",
      length(unmatched), id, paste(shown, collapse = ", "),
      if (extra > 0) sprintf(" and %d more", extra) else ""))
  }

  # The colour domain is a statistic, so it is decided here: the data
  # range for sequential; symmetric around `center` for diverging so the
  # midpoint colour always means exactly the reference value.
  if (palette == "diverging") {
    if (!is.numeric(center) || length(center) != 1 || is.na(center)) {
      rlang::abort(paste(
        '`palette = "diverging"` needs a numeric `center` - the value the',
        "neutral midpoint colour stands for (e.g. 100 for an index)."))
    }
    center <- as.numeric(center)
    m <- max(abs(range(df$value) - center))
    domain <- center + c(-m, m)
  } else {
    if (!is.null(center)) {
      rlang::abort('`center` only applies to `palette = "diverging"`.')
    }
    domain <- range(df$value)
  }
  # All-equal values would collapse the scale; give it a token width.
  if (domain[1] >= domain[2]) domain <- domain[1] + c(-1, 1)

  pv_widget("choropleth", c(list(
    data = df, map = map, palette = palette, domain = domain,
    obs = range(df$value),
    center = if (palette == "diverging") center,
    vlab = value
  ), chart_opts(title, subtitle, mode, duration, source)),
  width, height, elementId)
}
