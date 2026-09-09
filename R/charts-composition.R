# Composition charts: donut, treemap, lollipop, waffle. Part-of-whole and
# ranking forms. Each function validates here in R and ships a tidy payload
# to the renderers in inst/htmlwidgets/lib/pv-renderers/composition.js.

# The display flags below are tri-state: TRUE and FALSE force a look, and
# "auto" leaves the decision to the JavaScript side, which resolves it at
# render time from the real data and pixel sizes - only the browser knows
# how wide the chart actually is. The value ships to JS as-is.
check_tristate <- function(value, name) {
  if (isTRUE(value) || isFALSE(value) || identical(value, "auto")) {
    return(invisible(value))
  }
  rlang::abort(sprintf('`%s` must be TRUE, FALSE, or "auto".', name))
}

#' Interactive D3 donut chart
#'
#' Part-of-whole as a ring. Slices sweep in largest-first, slices worth 5%
#' or more of the total carry their name and share directly, smaller ones
#' move to a legend, and the grand total sits in the hole. Hovering a
#' slice pops it outward and shows its exact value and share.
#'
#' @param data A data frame.
#' @param category Name of the category column — one slice per level, at
#'   most 8. Beyond 8 a donut stops being readable; fold the smallest
#'   categories into an `"Other"` bin first. Rows sharing a category are
#'   summed into one slice.
#' @param value Name of the numeric column giving slice sizes. Values
#'   must be non-negative.
#' @param inner_radius Size of the centre hole as a fraction of the outer
#'   radius, from 0 to 0.85. `0` draws a classic pie.
#' @param labels Show the outside name-and-share labels? `TRUE` labels
#'   every slice worth 2% or more of the total, `FALSE` sends every
#'   slice to the legend instead, and `"auto"` (the default) labels
#'   slices of 5% or more — unless the rendered chart is narrower than
#'   about 480px, where outside labels would collide, in which case it
#'   falls back to legend-only.
#' @inheritParams pv_bar
#' @return An htmlwidget.
#' @examples
#' seats <- aggregate(elected ~ party,
#'                    pv_elections[pv_elections$year == 2024, ], sum)
#' pv_donut(seats, category = "party", value = "elected",
#'          title = "Council seats by party, 2024")
#' @export
pv_donut <- function(data, category, value, inner_radius = 0.62,
                     labels = "auto",
                     title = NULL, subtitle = NULL, mode = "auto",
                     duration = 650, source = NULL, width = NULL,
                     height = NULL, elementId = NULL) {
  check_columns(data, list(category, value))
  check_nonempty(data)
  check_value_column(data, value)
  check_tristate(labels, "labels")
  if (!is.numeric(inner_radius) || length(inner_radius) != 1 ||
      is.na(inner_radius) || inner_radius < 0 || inner_radius > 0.85) {
    rlang::abort("`inner_radius` must be a single number between 0 and 0.85.")
  }
  df <- data.frame(category = as.character(data[[category]]),
                   value = as.numeric(data[[value]]))
  if (anyNA(df$value) || any(df$value < 0)) {
    rlang::abort("Slice values must be non-negative and not missing.")
  }
  # One slice per category: rows that share a label are summed, keeping
  # first-appearance order so colours stay stable.
  if (anyDuplicated(df$category)) {
    lv <- unique(df$category)
    sums <- tapply(df$value, factor(df$category, levels = lv), sum)
    df <- data.frame(category = lv, value = as.numeric(sums))
  }
  if (nrow(df) > 8) {
    rlang::abort(sprintf(paste(
      "A donut with %d slices is unreadable (the limit is 8).",
      "Fold the smallest categories into an \"Other\" bin first."),
      nrow(df)))
  }
  pv_widget("donut", c(list(
    data = df, innerRadius = inner_radius, labels = labels, vlab = value
  ), chart_opts(title, subtitle, mode, duration, source)),
  width, height, elementId)
}

#' Interactive D3 treemap
#'
#' A hierarchy as nested rectangles: each cell's area encodes its value,
#' its hue names its top-level branch, and deeper levels fade a step
#' toward the background. Cells are labelled where the label honestly
#' fits; hovering a cell dims the other branches and shows the full path
#' and share of the total.
#'
#' @param data A data frame in long form: one row per leaf.
#' @param levels Character vector of column names, outermost grouping
#'   first, defining the hierarchy. The first level takes the palette
#'   colours, so it allows at most 8 distinct groups.
#' @param value Name of the numeric column summed within each cell.
#'   Values must be non-negative — a cell's size is an area.
#' @param labels Write names inside the cells? `"auto"` (the default)
#'   labels exactly the cells the text honestly fits into; `TRUE`
#'   squeezes labels into cells with about 20% less room than that (the
#'   text must still fit at all — tiny slivers stay blank); `FALSE`
#'   draws no cell text, leaving the names to the tooltip alone.
#' @inheritParams pv_bar
#' @return An htmlwidget.
#' @examples
#' luzern <- pv_city_landuse[pv_city_landuse$city == "Luzern", ]
#' pv_treemap(luzern, levels = c("group", "category"), value = "hectares",
#'            title = "Land use in the city of Lucerne")
#' @export
pv_treemap <- function(data, levels, value, labels = "auto",
                       title = NULL, subtitle = NULL, mode = "auto",
                       duration = 550, source = NULL, width = NULL,
                       height = NULL, elementId = NULL) {
  check_columns(data, list(levels, value))
  check_nonempty(data)
  check_value_column(data, value)
  check_tristate(labels, "labels")
  if (length(levels) < 1) {
    rlang::abort("`levels` needs at least one column name.")
  }
  # A negative value has no area to fill; the layout would silently
  # overflow its box, so refuse it here.
  if (any(data[[value]] < 0, na.rm = TRUE)) {
    rlang::abort(sprintf(
      "`%s` has negative values; treemap cell areas must be non-negative.",
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
  pv_widget("treemap", c(list(
    root = root, labels = labels, vlab = value
  ), chart_opts(title, subtitle, mode, duration, source)),
  width, height, elementId)
}

#' Interactive D3 waffle chart
#'
#' Parts of a whole as countable unit squares: a `rows`-by-`rows` grid of
#' small rounded squares, each square standing for one part in
#' `rows * rows` of the total (1% on the default 10-by-10 grid), with
#' the categories filling
#' the grid column by column from the bottom left in the order they first
#' appear in the data. Where a donut asks the eye to compare angles, a
#' waffle lets it count — and with [pv_textures()] each category's squares
#' also wear their own hatch, so the chart survives greyscale printing
#' better than any donut. Hovering a square lights up its whole category
#' and shows the exact value and share.
#'
#' Values rarely divide into whole squares, so each category's square
#' count is rounded honestly: every category first takes the whole
#' squares its exact share contains, then the leftover squares go to the
#' categories with the largest remainders (first appearance breaks ties)
#' until the grid is exactly full. A category is therefore never more
#' than one square away from its exact share — but a very small one may
#' draw no squares at all. The legend and the tooltip always carry the
#' exact value and percentage, never the rounded ones.
#'
#' @param data A data frame. Rows sharing a category are summed into one
#'   category, keeping first-appearance order — that order also assigns
#'   the palette colours and fills the grid.
#' @param category Name of the category column. The active theme's
#'   palette caps how many distinct categories fit (8 in the packaged
#'   theme); fold the smallest into an `"Other"` bin first.
#' @param value Name of the numeric column giving category sizes. Values
#'   must be non-negative, and must not all be zero — a square is a share
#'   of the total.
#' @param rows Number of rows (and columns) in the unit grid, a whole
#'   number from 2 to 20. The default 10 draws the classic 100-square
#'   waffle where every square is 1%. The grid keeps this shape at every
#'   chart size — only the squares themselves grow and shrink.
#' @inheritParams pv_bar
#' @return An htmlwidget.
#' @examples
#' seats <- aggregate(elected ~ party,
#'                    pv_elections[pv_elections$year == 2024, ], sum)
#' pv_waffle(seats, category = "party", value = "elected",
#'           title = "Council seats by party, 2024")
#' @export
pv_waffle <- function(data, category, value, rows = 10,
                      title = NULL, subtitle = NULL, mode = "auto",
                      duration = 650, source = NULL, width = NULL,
                      height = NULL, elementId = NULL) {
  check_columns(data, list(category, value))
  check_nonempty(data)
  check_value_column(data, value)
  if (!is.numeric(rows) || length(rows) != 1 || is.na(rows) ||
      rows != round(rows) || rows < 2 || rows > 20) {
    rlang::abort(paste(
      "`rows` must be a single whole number from 2 to 20 -",
      "the grid holds rows * rows squares, and beyond 400 they stop",
      "being countable."))
  }
  df <- data.frame(category = as.character(data[[category]]),
                   value = as.numeric(data[[value]]))
  if (anyNA(df$value) || any(df$value < 0)) {
    rlang::abort("Category values must be non-negative and not missing.")
  }
  # One category per label: rows that share one are summed, keeping
  # first-appearance order so colours (and the fill order) stay stable.
  if (anyDuplicated(df$category)) {
    lv <- unique(df$category)
    sums <- tapply(df$value, factor(df$category, levels = lv), sum)
    df <- data.frame(category = lv, value = as.numeric(sums))
  }
  check_theme_palette_fit(df$category, category)
  total <- sum(df$value)
  if (total <= 0) {
    rlang::abort(sprintf(
      "`%s` sums to zero; a waffle has no squares to fill.", value))
  }
  # Round the shares to whole squares by largest remainder: everyone
  # keeps the whole squares their exact share contains, then the
  # leftovers go to the biggest remainders (first appearance breaks
  # ties) until the grid is exactly full. The JavaScript side just
  # counts these out - the exact values ride along for the tooltip.
  n_units <- as.integer(rows)^2
  exact <- df$value / total * n_units
  units <- floor(exact)
  short <- n_units - sum(units)
  if (short > 0) {
    take <- order(-(exact - units), seq_along(units))[seq_len(short)]
    units[take] <- units[take] + 1
  }
  df$units <- as.integer(units)
  pv_widget("waffle", c(list(
    data = df, rows = as.integer(rows), vlab = value
  ), chart_opts(title, subtitle, mode, duration, source)),
  width, height, elementId)
}

#' Interactive D3 lollipop chart
#'
#' A ranking chart — the bar chart's lighter cousin: one hairline stem
#' and dot per category, drawn horizontally so category names stay
#' upright and readable, with the value at each head. Sorted
#' largest-first by default. Capped at 40 categories, beyond which no
#' ranking stays readable.
#'
#' @param data A data frame with at most 40 rows (one per category).
#'   Rows with a missing category or value are dropped with a warning.
#' @param x Name of the category column.
#' @param y Name of the numeric value column.
#' @param sort Sort categories by value, largest first?
#' @param value_labels Print the compact value at each head? `TRUE`
#'   always, `FALSE` never, and `"auto"` (the default) shows them only
#'   while the plot area stays at least 200px wide — on narrower charts
#'   they cannot fit, so they are dropped and the tooltip alone carries
#'   the exact figures.
#' @inheritParams pv_bar
#' @return An htmlwidget.
#' @examples
#' f25 <- pv_fiscal[pv_fiscal$year == 2025, ]
#' top <- head(f25[order(-f25$equalization_chf), ], 15)
#' pv_lollipop(top, x = "municipality", y = "equalization_chf",
#'             title = "Largest equalization payments, 2025")
#' @export
pv_lollipop <- function(data, x, y, sort = TRUE, value_labels = "auto",
                        title = NULL, subtitle = NULL, mode = "auto",
                        duration = 600, source = NULL, width = NULL,
                        height = NULL, elementId = NULL) {
  check_columns(data, list(x, y))
  check_nonempty(data)
  check_value_column(data, y)
  check_tristate(value_labels, "value_labels")
  df <- data.frame(x = as.character(data[[x]]), y = as.numeric(data[[y]]))
  df <- drop_missing(df, is.na(df$x), x)
  df <- drop_missing(df, is.na(df$y), y)
  if (nrow(df) > 40) {
    rlang::abort(sprintf(paste(
      "%d categories won't fit a readable lollipop (the limit is 40).",
      "Pre-filter to the categories you care about, e.g. the top 25."),
      nrow(df)))
  }
  if (isTRUE(sort)) {
    df <- df[order(-df$y), ]
  }
  pv_widget("lollipop", c(list(
    data = df, xlab = x, ylab = y, valueLabels = value_labels
  ), chart_opts(title, subtitle, mode, duration, source)),
  width, height, elementId)
}
