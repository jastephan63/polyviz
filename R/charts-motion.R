# Motion charts: the bar-chart race and the bump (rank-over-time) chart.
# Like every chart file, this one only shapes the payload - the drawing
# lives in inst/htmlwidgets/lib/pv-renderers/motion.js.

# A motion chart's time column must genuinely order itself: numeric (a
# plain year works) or Date. Dates travel as ISO strings for d3 to
# re-parse; anything else has no defensible animation order, so refuse it
# here rather than animate alphabetically.
motion_time_values <- function(x) {
  if (inherits(x, "Date")) {
    list(values = format(x, "%Y-%m-%d"), ttype = "date")
  } else if (is.numeric(x)) {
    list(values = as.numeric(x), ttype = "number")
  } else {
    rlang::abort("`time` must be a numeric or Date column.")
  }
}

motion_top_n <- function(top_n) {
  ok <- is.numeric(top_n) && length(top_n) == 1 && !is.na(top_n) &&
    top_n == round(top_n) && top_n >= 2
  if (!ok) {
    rlang::abort("`top_n` must be a single whole number of at least 2.")
  }
  as.integer(top_n)
}

# Shared shaping for both motion charts: one clean row per time/id pair
# with the value and its rank at that time point (1 = largest, among
# everything measured then, ties broken by row order so ranks stay
# unique). Rows with a missing value simply don't exist - the entity is
# absent at that time point.
motion_ranked <- function(data, time, id, value) {
  check_columns(data, list(time, id, value))
  tv <- motion_time_values(data[[time]])
  df <- data.frame(t = tv$values,
                   id = as.character(data[[id]]),
                   value = as.numeric(data[[value]]),
                   stringsAsFactors = FALSE)
  df <- df[!is.na(df$value), , drop = FALSE]
  if (any(df$value < 0)) {
    rlang::abort("`value` must be non-negative - these charts rank from zero.")
  }
  if (anyDuplicated(df[c("t", "id")])) {
    rlang::abort(
      "`data` needs one row per time/id pair - aggregate duplicates first.")
  }
  times <- sort(unique(df$t))
  if (length(times) < 2) {
    rlang::abort(
      "`time` needs at least 2 distinct time points to animate between.")
  }
  df$rank <- ave(-df$value, df$t,
                 FUN = function(v) rank(v, ties.method = "first"))
  df <- df[order(match(df$t, times), df$rank), , drop = FALSE]
  rownames(df) <- NULL
  list(df = df, times = times, ttype = tv$ttype)
}

#' Animated D3 bar-chart race
#'
#' The bar-chart race: at every time point the `top_n` entities appear as
#' horizontal bars ranked by value, and the animation interpolates both
#' values and ranks between consecutive time points, so bars grow and
#' overtake each other smoothly while a large muted time label ticks
#' along in the corner. Value labels ride the bar ends. Entities that
#' rise into the ranking slide in from below the last visible row;
#' entities that fall out slide down the same way. When the race
#' finishes, a small circled-arrow control in the top-right corner
#' replays it. Hovering a bar reads out the entity's exact value at the
#' moment shown.
#'
#' The entities in the race are everyone who makes the `top_n` at *any*
#' time point, so late risers and early leaders all get their moment.
#' Colours follow the final standings in palette-slot order; beyond the
#' palette's 8 slots the colours repeat, faded toward the background.
#'
#' @param data A data frame in long form: one row per entity per time
#'   point (missing combinations mean the entity is absent then).
#' @param time Name of the time column — numeric (e.g. a year) or `Date`.
#'   At least 2 distinct time points are required.
#' @param id Name of the column identifying each racing entity.
#' @param value Name of the numeric column the entities race on
#'   (non-negative; `NA` counts as absent).
#' @param top_n How many ranked bars are visible at once (a whole number,
#'   at least 2; capped at the number of entities that ever qualify).
#' @param duration Tempo control: each step between consecutive time
#'   points takes `duration / 500 * 900` ms, so the default `500` gives
#'   roughly 900 ms per step. `0` skips the animation entirely and shows
#'   the final time point — no autoplay, but the replay control stays,
#'   and pressing it runs the race at the default tempo.
#' @inheritParams pv_bar
#' @return An htmlwidget.
#' @examples
#' pv_race(pv_city_population, time = "year", id = "city",
#'         value = "population", top_n = 12,
#'         title = "Swiss cities racing through a century",
#'         source = "Source: Bundesamt für Statistik")
#' @export
pv_race <- function(data, time, id, value, top_n = 10,
                    title = NULL, subtitle = NULL, mode = "auto",
                    duration = 500, source = NULL, width = NULL,
                    height = NULL, elementId = NULL) {
  top_n <- motion_top_n(top_n)
  r <- motion_ranked(data, time, id, value)
  df <- r$df
  times <- r$times

  # The cast of the race: anyone who makes the top_n at ANY time point,
  # so risers enter and fallers exit instead of being cut for how they
  # placed at one arbitrary moment.
  keep <- unique(df$id[df$rank <= top_n])
  if (length(keep) < 2) {
    rlang::abort("`data` needs at least 2 entities to race.")
  }
  top_n <- min(top_n, length(keep))
  df <- df[df$id %in% keep, , drop = FALSE]

  # Colour slots follow the final standings, leader first - the order
  # viewers see when the race settles. Entities gone by the final time
  # point queue after, biggest ever first.
  fin <- df[df$t == times[length(times)], , drop = FALSE]
  fin <- fin$id[order(fin$rank)]
  gone <- setdiff(keep, fin)
  if (length(gone)) {
    peak <- vapply(gone, function(e) max(df$value[df$id == e]), numeric(1))
    gone <- gone[order(-peak)]
  }
  entities <- c(fin, gone)

  # A complete time x entity grid: the renderer interpolates between
  # consecutive keyframes, so every entity needs a value and a rank at
  # every time point. Absent combinations become zero-width bars parked
  # at rank top_n + 1 - just below the visible rows - which is exactly
  # where entering and exiting bars slide from and to.
  grid <- expand.grid(id = entities, t = times,
                      KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  hit <- match(paste(grid$t, grid$id, sep = "\r"),
               paste(df$t, df$id, sep = "\r"))
  grid$value <- ifelse(is.na(hit), 0, df$value[hit])
  grid$rank <- ifelse(is.na(hit), top_n + 1, df$rank[hit])
  grid <- grid[order(match(grid$t, times), grid$rank), c("t", "id", "value", "rank")]
  rownames(grid) <- NULL

  pv_widget("race", c(list(
    data = grid, times = times, ttype = r$ttype,
    entities = entities, topN = top_n, vlab = value
  ), chart_opts(title, subtitle, mode, duration, source)),
  width, height, elementId)
}

#' Interactive D3 bump chart
#'
#' Rank over time: the `top_n` entities at the final time point, drawn as
#' lines through their rank (1 at the top) at every time point, with a
#' dot at each measurement. Crossing lines are the story — they mark the
#' moment one entity overtook another. Entity names label both ends of
#' each line, at its starting and finishing rank, nudged apart where they
#' would collide. Hovering a line raises it, dims the rest, and reads out
#' the rank and value at the nearest time point.
#'
#' Ranks are computed among *all* entities in `data` at each time point,
#' but only positions 1 to `top_n` are drawn. An entity that sits outside
#' the top `top_n` at some time points has no position to show there, so
#' its line simply spans the times where it ranks — entering late, or
#' breaking where it dipped out. Colours follow the final standings in
#' palette-slot order; beyond the palette's 8 slots the colours repeat,
#' faded toward the background.
#'
#' @param top_n How many rank positions are shown, and how many entities
#'   are drawn — the `top_n` by value at the final time point (a whole
#'   number, at least 2; capped at the number of entities present then).
#' @inheritParams pv_race
#' @inheritParams pv_bar
#' @return An htmlwidget.
#' @examples
#' pv_bump(pv_city_population, time = "year", id = "city",
#'         value = "population", top_n = 10,
#'         title = "Switzerland's largest cities, reranked",
#'         source = "Source: Bundesamt für Statistik")
#' @export
pv_bump <- function(data, time, id, value, top_n = 10,
                    title = NULL, subtitle = NULL, mode = "auto",
                    duration = 800, source = NULL, width = NULL,
                    height = NULL, elementId = NULL) {
  top_n <- motion_top_n(top_n)
  r <- motion_ranked(data, time, id, value)
  df <- r$df
  times <- r$times

  # The chart follows the entities that matter where the story ends: the
  # top_n by value at the final time point, in final-rank order (which is
  # also the colour order, so the right-hand labels read top to bottom in
  # palette order).
  fin <- df[df$t == times[length(times)], , drop = FALSE]
  fin <- fin[fin$rank <= top_n, , drop = FALSE]
  entities <- fin$id[order(fin$rank)]
  if (length(entities) < 2) {
    rlang::abort(
      "`data` needs at least 2 entities ranked at the final time point.")
  }

  # Keep only the moments where a followed entity actually holds one of
  # the drawn positions - outside the top_n it has no y position, and its
  # line spans (or breaks around) exactly the times it ranks. The y axis
  # runs to the deepest rank actually drawn, so a short field doesn't
  # leave empty rows.
  kept <- df[df$id %in% entities & df$rank <= top_n, , drop = FALSE]
  kept <- kept[order(match(kept$t, times), kept$rank),
               c("t", "id", "rank", "value")]
  rownames(kept) <- NULL

  pv_widget("bump", c(list(
    data = kept, times = times, ttype = r$ttype,
    entities = entities, topN = max(kept$rank), vlab = value
  ), chart_opts(title, subtitle, mode, duration, source)),
  width, height, elementId)
}
