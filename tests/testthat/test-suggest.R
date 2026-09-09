# pv_suggest() promises two things: the right chart shapes surface for
# the right data (and the wrong ones stay away), and every printed call
# runs as-is. The shape tests check the first promise dataset by dataset;
# the property test at the bottom parses and evaluates every printed
# suggestion for every bundled data frame and demands a widget back.

suggest_charts <- function(s) {
  vapply(s$suggestions, function(r) r$chart, character(1))
}

suggest_code <- function(s, chart) {
  s$suggestions[[match(chart, suggest_charts(s))]]$code
}

suggest_reason <- function(s, chart) {
  s$suggestions[[match(chart, suggest_charts(s))]]$reason
}

# Runs one suggestion's printed code against the data it was made for,
# exactly as a user would after copying it from the console.
suggest_eval <- function(code, frames = list()) {
  env <- new.env(parent = globalenv())
  for (nm in names(frames)) {
    assign(nm, frames[[nm]], envir = env)
  }
  suppressMessages(suppressWarnings(eval(parse(text = code), env)))
}

test_that("pv_suggest refuses non-data-frames with the house message", {
  expect_error(pv_suggest(pv_flows), "must be a data frame")
  expect_error(pv_suggest(42), "must be a data frame")
  expect_error(pv_suggest(NULL), "must be a data frame")
  expect_error(pv_suggest(data.frame()), "no rows")
})

test_that("n is validated and caps the list without changing the order", {
  expect_error(pv_suggest(pv_weather, n = 0), "whole number")
  expect_error(pv_suggest(pv_weather, n = 1.5), "whole number")
  expect_error(pv_suggest(pv_weather, n = "four"), "whole number")
  expect_error(pv_suggest(pv_weather, n = NA), "whole number")
  short <- pv_suggest(pv_weather, n = 2)
  full <- pv_suggest(pv_weather, n = 10)
  expect_lte(length(short$suggestions), 2)
  expect_identical(suggest_charts(short),
                   utils::head(suggest_charts(full), 2))
})

test_that("the object holds one record per chart, each fully formed", {
  s <- pv_suggest(pv_weather, n = 10)
  expect_s3_class(s, "pv_suggestions")
  expect_identical(s$data_name, "pv_weather")
  expect_identical(s$n_rows, nrow(pv_weather))
  expect_identical(s$n_cols, ncol(pv_weather))
  expect_false(anyDuplicated(suggest_charts(s)) > 0)
  for (r in s$suggestions) {
    expect_true(is.character(r$chart) && nzchar(r$chart))
    expect_true(is.character(r$code) && nzchar(r$code))
    # One line of plain-English reasoning, no more.
    expect_true(is.character(r$reason) && nzchar(r$reason))
    expect_false(grepl("\n", r$reason, fixed = TRUE))
  }
})

test_that("the print method renders code and reasons, invisibly", {
  s <- pv_suggest(pv_weather, n = 3)
  # Called as print.pv_suggestions() rather than through dispatch: the
  # S3method() registration only lands once roxygen next regenerates
  # NAMESPACE, and the direct call stays valid after it does.
  out <- capture.output(res <- withVisible(print.pv_suggestions(s)))
  expect_false(res$visible)
  expect_identical(res$value, s)
  expect_true(any(grepl("<pv_suggestions> pv_weather", out, fixed = TRUE)))
  expect_true(any(grepl("1) pv_calendar", out, fixed = TRUE)))
  expect_true(any(grepl('pv_calendar(pv_weather, date = "date"', out,
                        fixed = TRUE)))
})

test_that("a data frame passed as an expression is shown as `data`", {
  s <- pv_suggest(head(pv_weather, 400), n = 3)
  expect_identical(s$data_name, "data")
  expect_true(s$placeholder)
  out <- capture.output(print.pv_suggestions(s))
  expect_true(any(grepl("shown as `data`", out, fixed = TRUE)))
  w <- suggest_eval(s$suggestions[[1]]$code,
                    list(data = head(pv_weather, 400)))
  expect_s3_class(w, "pvchart")
})

test_that("yearly panel data reads as lines, not calendars or maps", {
  s <- pv_suggest(pv_city_population, n = 10)
  charts <- suggest_charts(s)
  expect_true("pv_line" %in% charts)
  code <- suggest_code(s, "pv_line")
  expect_match(code, 'x = "year"', fixed = TRUE)
  expect_match(code, 'y = "population"', fixed = TRUE)
  expect_match(code, 'series = "city"', fixed = TRUE)
  expect_false("pv_calendar" %in% charts)
  expect_false("pv_choropleth" %in% charts)
  expect_false("pv_bubble_map" %in% charts)
})

test_that("daily dates read as a calendar and a line", {
  s <- pv_suggest(pv_weather, n = 10)
  charts <- suggest_charts(s)
  expect_true("pv_calendar" %in% charts)
  expect_true("pv_line" %in% charts)
  expect_match(suggest_code(s, "pv_calendar"), 'date = "date"', fixed = TRUE)
  expect_match(suggest_reason(s, "pv_calendar"), "daily")
  # Five numeric measures also earn a pairs matrix.
  expect_true("pv_pairs" %in% charts)
  # Nothing here is a category, a share, or a map.
  expect_false("pv_bar" %in% charts)
  expect_false("pv_donut" %in% charts)
  expect_false("pv_waffle" %in% charts)
  expect_false("pv_choropleth" %in% charts)
  expect_false("pv_bubble_map" %in% charts)
  # Five numeric columns are a pairs matrix, not a before/after pair.
  expect_false("pv_dumbbell" %in% charts)
})

test_that("a lon/lat pair reads as a bubble map on the Swiss base map", {
  pop24 <- subset(pv_city_population, year == 2024)
  cities <- merge(pv_city_coords, pop24, by = "city")
  s <- pv_suggest(cities, n = 10)
  charts <- suggest_charts(s)
  expect_true("pv_bubble_map" %in% charts)
  code <- suggest_code(s, "pv_bubble_map")
  expect_match(code, 'lon = "lon"', fixed = TRUE)
  expect_match(code, 'lat = "lat"', fixed = TRUE)
  expect_match(code, 'size = "population"', fixed = TRUE)
  expect_match(code, 'label = "city"', fixed = TRUE)
  # The city ids are country-wide BFS numbers, not a bundled layer's set.
  expect_false("pv_choropleth" %in% charts)
  expect_false("pv_calendar" %in% charts)
})

test_that("ids matching the Lucerne layer read as a choropleth", {
  fiscal25 <- subset(pv_fiscal, year == 2025)
  s <- pv_suggest(fiscal25, n = 10)
  charts <- suggest_charts(s)
  expect_true("pv_choropleth" %in% charts)
  code <- suggest_code(s, "pv_choropleth")
  expect_match(code, 'id = "municipality_id"', fixed = TRUE)
  # The Lucerne layer is pv_choropleth's default map; no map argument.
  expect_false(grepl("map =", code, fixed = TRUE))
  expect_match(suggest_reason(s, "pv_choropleth"), "Lucerne")
  # Two clean numerics also earn a scatter; one year is no time axis.
  expect_true("pv_scatter" %in% charts)
  expect_false("pv_line" %in% charts)
  expect_false("pv_calendar" %in% charts)
})

test_that("repeated region ids get an aggregate() step before the map", {
  s <- pv_suggest(pv_fiscal, n = 10)
  code <- suggest_code(s, "pv_choropleth")
  expect_match(code, "aggregate(", fixed = TRUE)
  expect_match(code, "municipality_id", fixed = TRUE)
  expect_match(suggest_reason(s, "pv_choropleth"), "one row per")
})

test_that("canton ids point the choropleth at the cantons layer", {
  s <- pv_suggest(pv_tourism, n = 10)
  code <- suggest_code(s, "pv_choropleth")
  expect_match(code, 'map = "cantons"', fixed = TRUE)
  expect_match(code, 'id = "canton_id"', fixed = TRUE)
})

test_that("a wide numeric frame reads as a pairs matrix", {
  wide <- data.frame(a = sin(1:120), b = cos(1:120), c = sqrt(1:120),
                     d = log(1:120), e = (1:120) / 7)
  s <- pv_suggest(wide, n = 10)
  charts <- suggest_charts(s)
  expect_true("pv_pairs" %in% charts)
  code <- suggest_code(s, "pv_pairs")
  expect_match(code, 'columns = c("a", "b", "c", "d", "e")', fixed = TRUE)
  expect_true("pv_parallel" %in% charts)
  expect_false("pv_choropleth" %in% charts)
  expect_false("pv_bubble_map" %in% charts)
  expect_false("pv_calendar" %in% charts)
  expect_false("pv_line" %in% charts)
})

test_that("big point clouds get the density treatment, small ones do not", {
  big <- data.frame(x = sin(1:10000), y = cos(1:10000))
  s <- pv_suggest(big, n = 10)
  expect_true("pv_scatter" %in% suggest_charts(s))
  code <- suggest_code(s, "pv_scatter")
  expect_match(code, "density = TRUE", fixed = TRUE)
  expect_match(suggest_reason(s, "pv_scatter"), "10,000", fixed = TRUE)
  # The reason names the hexagon binning as the countable alternative.
  expect_match(suggest_reason(s, "pv_scatter"), 'density = "hex"',
               fixed = TRUE)
  # density = TRUE refuses per-point mappings, so no label rides along.
  expect_false(grepl("label =", code, fixed = TRUE))
  small <- data.frame(x = sin(1:200), y = cos(1:200))
  s2 <- pv_suggest(small, n = 10)
  expect_false(grepl("density", suggest_code(s2, "pv_scatter"),
                     fixed = TRUE))
})

test_that("repeated keys get the aggregate() step spelled out", {
  s <- pv_suggest(pv_sales, n = 10)
  code <- suggest_code(s, "pv_line")
  expect_match(code, "aggregate(", fixed = TRUE)
  expect_match(code, 'series = "region"', fixed = TRUE)
  expect_match(suggest_reason(s, "pv_line"), "summed to one row per")
  # The bar needs the same treatment: many rows per category.
  expect_match(suggest_code(s, "pv_bar"), "aggregate(", fixed = TRUE)
  # And two categories with a value earn a heatmap.
  expect_true("pv_heatmap" %in% suggest_charts(s))
})

test_that("grouped distributions pick their form by group cardinality", {
  two_deep <- data.frame(g = rep(c("a", "b"), each = 40), v = sin(1:80))
  s <- pv_suggest(two_deep, n = 10)
  expect_true("pv_violin" %in% suggest_charts(s))
  expect_false(any(c("pv_ridgeline", "pv_boxplot") %in% suggest_charts(s)))

  six_wide <- data.frame(g = rep(letters[1:6], each = 20), v = cos(1:120))
  s <- pv_suggest(six_wide, n = 10)
  expect_true("pv_ridgeline" %in% suggest_charts(s))
  expect_false(any(c("pv_violin", "pv_boxplot") %in% suggest_charts(s)))

  two_thin <- data.frame(g = rep(c("a", "b"), each = 8), v = sin(1:16))
  s <- pv_suggest(two_thin, n = 10)
  expect_true("pv_boxplot" %in% suggest_charts(s))
  expect_false(any(c("pv_violin", "pv_ridgeline") %in% suggest_charts(s)))
})

test_that("exactly two time points per group read as a slope chart", {
  pop <- pv_city_population[pv_city_population$year %in% c(1930, 2024), ]
  s <- pv_suggest(pop, n = 10)
  charts <- suggest_charts(s)
  # No map, calendar, or daily-date shape competes here, so the slope's
  # strong signal puts it first.
  expect_identical(charts[[1]], "pv_slope")
  code <- suggest_code(s, "pv_slope")
  expect_match(code, 'x = "year"', fixed = TRUE)
  expect_match(code, 'y = "population"', fixed = TRUE)
  expect_match(code, 'group = "city"', fixed = TRUE)
  reason <- suggest_reason(s, "pv_slope")
  expect_match(reason, "1930", fixed = TRUE)
  expect_match(reason, "2024", fixed = TRUE)
  w <- suggest_eval(code, list(pop = pop))
  expect_s3_class(w, "pvchart")
  # Three or more moments stay a line; a slope never fires there.
  full <- pv_suggest(pv_city_population, n = 10)
  expect_false("pv_slope" %in% suggest_charts(full))
  expect_true("pv_line" %in% suggest_charts(full))
})

test_that("the slope outranks the line when both shapes are present", {
  # One frame with both signals: a daily date axis feeds the line, and a
  # separate two-value year column feeds the slope.
  mixed <- data.frame(
    city = rep(c("A", "B", "C", "D"), each = 2),
    year = rep(c(2010L, 2024L), 4),
    when = as.Date("2024-01-01") + 0:7,
    value = c(1, 3, 2, 2, 5, 4, 3, 6))
  s <- pv_suggest(mixed, n = 10)
  charts <- suggest_charts(s)
  expect_true(all(c("pv_slope", "pv_line") %in% charts))
  expect_lt(match("pv_slope", charts), match("pv_line", charts))
})

test_that("more series than the palette holds fold into a horizon chart", {
  s <- pv_suggest(pv_tourism, n = 10)
  charts <- suggest_charts(s)
  expect_true(all(c("pv_horizon", "pv_line") %in% charts))
  # The horizon is the rescue for exactly the frame where the line
  # drowns, so it edges ahead of the line.
  expect_lt(match("pv_horizon", charts), match("pv_line", charts))
  code <- suggest_code(s, "pv_horizon")
  # 26 cantons, several origins per canton/year: the aggregate() step
  # comes spelled out, keyed on the axis and the series.
  expect_match(code, "aggregate(arrivals ~ year + canton", fixed = TRUE)
  expect_match(code, 'x = "year"', fixed = TRUE)
  expect_match(code, 'series = "canton"', fixed = TRUE)
  reason <- suggest_reason(s, "pv_horizon")
  expect_match(reason, "26 `canton` series", fixed = TRUE)
  expect_match(reason, "spaghetti")
  w <- suggest_eval(code, list(pv_tourism = pv_tourism))
  expect_s3_class(w, "pvchart")
})

test_that("an already-aggregated spaghetti panel gets the direct horizon call", {
  nights <- aggregate(nights ~ canton + year, pv_tourism, sum)
  s <- pv_suggest(nights, n = 10)
  code <- suggest_code(s, "pv_horizon")
  expect_false(grepl("aggregate(", code, fixed = TRUE))
  expect_identical(
    code, 'pv_horizon(nights, x = "year", y = "nights", series = "canton")')
  w <- suggest_eval(code, list(nights = nights))
  expect_s3_class(w, "pvchart")
})

test_that("the horizon stays away below the spaghetti threshold and above the height cap", {
  # A handful of series still fits the palette: that is line territory.
  few <- data.frame(year = rep(2000:2010, 4),
                    g = rep(c("a", "b", "c", "d"), each = 11), v = 1:44)
  s <- pv_suggest(few, n = 10)
  expect_false("pv_horizon" %in% suggest_charts(s))
  expect_true("pv_line" %in% suggest_charts(s))
  # 180 cities need more rows than the default height holds, and a
  # printed suggestion must run exactly as printed - so none is made.
  expect_false("pv_horizon" %in%
                 suggest_charts(pv_suggest(pv_city_population, n = 10)))
  # No time axis, no horizon, however many levels the grouping has.
  no_time <- data.frame(g = rep(letters[1:12], each = 3),
                        step = rep(1:3, 12) / 10, v = 1:36)
  expect_false("pv_horizon" %in% suggest_charts(pv_suggest(no_time, n = 10)))
})

test_that("two columns of place names and a measure read as a flow map", {
  latest <- subset(pv_commuters,
                   period == "2022-2024" & region != "Restliche Schweiz")
  flows <- data.frame(
    from = ifelse(latest$direction == "to Zug", latest$region, "Zug"),
    to = ifelse(latest$direction == "to Zug", "Zug", latest$region),
    commuters = latest$commuters)
  s <- pv_suggest(flows, n = 10)
  charts <- suggest_charts(s)
  expect_true("pv_flow_map" %in% charts)
  # The origin-destination shape is the most specific read here.
  expect_identical(charts[[1]], "pv_flow_map")
  code <- suggest_code(s, "pv_flow_map")
  expect_match(code, 'from = "from"', fixed = TRUE)
  expect_match(code, 'to = "to"', fixed = TRUE)
  expect_match(code, 'value = "commuters"', fixed = TRUE)
  # The cantons are pv_flow_map's default layer; no map argument.
  expect_false(grepl("map =", code, fixed = TRUE))
  expect_match(suggest_reason(s, "pv_flow_map"), "canton names")
  w <- suggest_eval(code, list(flows = flows))
  expect_s3_class(w, "pvchart")
})

test_that("municipality names point the flow map at the Lucerne layer", {
  lu <- data.frame(a = c("Emmen", "Kriens", "Horw"),
                   b = c("Luzern", "Luzern", "Luzern"),
                   n = c(10, 20, 30))
  s <- pv_suggest(lu, n = 10)
  code <- suggest_code(s, "pv_flow_map")
  expect_match(code, 'map = "lucerne"', fixed = TRUE)
  expect_match(code, 'from = "a"', fixed = TRUE)
  expect_match(suggest_reason(s, "pv_flow_map"), "Lucerne municipality")
  w <- suggest_eval(code, list(lu = lu))
  expect_s3_class(w, "pvchart")
})

test_that("the flow map reads endpoint roles off the column names", {
  # Destination column first: the names, not the order, decide the ends.
  swapped <- data.frame(to = c("Zug", "Zug", "Luzern"),
                        from = c("Aargau", "Schwyz", "Zug"),
                        n = c(5, 3, 4))
  code <- suggest_code(pv_suggest(swapped, n = 10), "pv_flow_map")
  expect_match(code, 'from = "from", to = "to"', fixed = TRUE)
})

test_that("repeated flow pairs get the aggregate() step, loops and strangers get nothing", {
  base <- data.frame(from = c("Aargau", "Schwyz", "Zug"),
                     to = c("Zug", "Zug", "Luzern"), n = c(5, 3, 4))
  # The same directed pair twice folds down before the map draws.
  doubled <- rbind(base, base[1, ])
  s <- pv_suggest(doubled, n = 10)
  code <- suggest_code(s, "pv_flow_map")
  expect_match(code, "aggregate(n ~ from + to", fixed = TRUE)
  w <- suggest_eval(code, list(doubled = doubled))
  expect_s3_class(w, "pvchart")
  # A place flowing to itself cannot be drawn, so nothing is offered.
  loop <- rbind(base, data.frame(from = "Zug", to = "Zug", n = 2))
  expect_false("pv_flow_map" %in% suggest_charts(pv_suggest(loop, n = 10)))
  # One name off the layer breaks the match - full precision, as in the
  # constructor, where an unmatched endpoint is an error.
  stranger <- rbind(base,
                    data.frame(from = "Restliche Schweiz", to = "Zug", n = 9))
  expect_false("pv_flow_map" %in%
                 suggest_charts(pv_suggest(stranger, n = 10)))
  # Two places shuttling between themselves stay with the plainer forms.
  shuttle <- data.frame(from = c("Zug", "Luzern"), to = c("Luzern", "Zug"),
                        n = c(7, 5))
  expect_false("pv_flow_map" %in%
                 suggest_charts(pv_suggest(shuttle, n = 10)))
})

test_that("two same-scale numerics per category earn a dumbbell beside the scatter", {
  f20 <- pv_fiscal[pv_fiscal$year == 2020,
                   c("municipality", "resource_index")]
  f27 <- pv_fiscal[pv_fiscal$year == 2027,
                   c("municipality", "resource_index")]
  both <- merge(f20, f27, by = "municipality",
                suffixes = c("_2020", "_2027"))
  both <- head(both[order(-both$resource_index_2027), ], 12)
  s <- pv_suggest(both, n = 10)
  charts <- suggest_charts(s)
  expect_true(all(c("pv_dumbbell", "pv_scatter") %in% charts))
  # The dumbbell is the more specific read, so it edges ahead.
  expect_lt(match("pv_dumbbell", charts), match("pv_scatter", charts))
  code <- suggest_code(s, "pv_dumbbell")
  expect_match(code, 'y = "municipality"', fixed = TRUE)
  expect_match(code, 'x1 = "resource_index_2020"', fixed = TRUE)
  expect_match(code, 'x2 = "resource_index_2027"', fixed = TRUE)
  w <- suggest_eval(code, list(both = both))
  expect_s3_class(w, "pvchart")
})

test_that("the dumbbell stays away from mismatched scales and wider frames", {
  # Two numerics whose ranges never overlap share no axis.
  apart <- data.frame(name = letters[1:8], small = (1:8) / 10,
                      big = 1000 + 1:8)
  expect_false("pv_dumbbell" %in% suggest_charts(pv_suggest(apart, n = 10)))
  # Three numerics are a frame of measures, not a before/after pair.
  three <- data.frame(name = letters[1:8], a = 1:8, b = 8:1,
                      c = 2 * (1:8))
  expect_false("pv_dumbbell" %in% suggest_charts(pv_suggest(three, n = 10)))
  # Repeated categories have no one-row-per-category pairing to draw.
  rep_cat <- data.frame(name = rep(letters[1:4], 2), a = 1:8, b = 8:1)
  expect_false("pv_dumbbell" %in%
                 suggest_charts(pv_suggest(rep_cat, n = 10)))
})

test_that("a signed measure with a minority of losses earns a waterfall guess", {
  budget <- data.frame(
    item = c("Wages", "Goods", "Fees", "Transfers", "Interest", "Rents"),
    change_chf = c(420, -180, 260, -90, 35, 150))
  s <- pv_suggest(budget, n = 10)
  expect_true("pv_waterfall" %in% suggest_charts(s))
  code <- suggest_code(s, "pv_waterfall")
  expect_match(code, 'x = "item"', fixed = TRUE)
  expect_match(code, 'y = "change_chf"', fixed = TRUE)
  reason <- suggest_reason(s, "pv_waterfall")
  # The reason owns up to the guess instead of asserting a story.
  expect_match(reason, "guess")
  expect_match(reason, "gains and losses")
  w <- suggest_eval(code, list(budget = budget))
  expect_s3_class(w, "pvchart")
  # Repeated categories get the aggregate() step, like every builder.
  doubled <- rbind(budget, budget)
  s2 <- pv_suggest(doubled, n = 10)
  expect_match(suggest_code(s2, "pv_waterfall"), "aggregate(", fixed = TRUE)
})

test_that("the waterfall needs losses to be a real minority", {
  # All gains are plain bars, not contributions toward a total.
  gains <- data.frame(item = letters[1:6], v = c(4, 2, 6, 1, 3, 5))
  expect_false("pv_waterfall" %in% suggest_charts(pv_suggest(gains, n = 10)))
  # Half negatives are diverging data; the "total" is not the story.
  half <- data.frame(item = letters[1:6], v = c(5, -5, 4, -4, 3, -3))
  expect_false("pv_waterfall" %in% suggest_charts(pv_suggest(half, n = 10)))
})

test_that("wherever a donut is suggested a waffle follows right behind", {
  seats <- data.frame(party = c("A", "B", "C"), n = c(6, 3, 1))
  s <- pv_suggest(seats, n = 10)
  charts <- suggest_charts(s)
  expect_true(all(c("pv_donut", "pv_waffle") %in% charts))
  expect_identical(match("pv_waffle", charts), match("pv_donut", charts) + 1L)
  code <- suggest_code(s, "pv_waffle")
  expect_match(code, 'category = "party"', fixed = TRUE)
  expect_match(code, 'value = "n"', fixed = TRUE)
  expect_match(suggest_reason(s, "pv_waffle"), "donut")
  w <- suggest_eval(code, list(seats = seats))
  expect_s3_class(w, "pvchart")
  # With repeated keys the pair prints the same aggregate() step.
  s2 <- pv_suggest(pv_sales, n = 10)
  expect_true(all(c("pv_donut", "pv_waffle") %in% suggest_charts(s2)))
  expect_match(suggest_code(s2, "pv_donut"), "aggregate(", fixed = TRUE)
  expect_match(suggest_code(s2, "pv_waffle"), "aggregate(", fixed = TRUE)
})

test_that("nested categories earn a sunburst whose reason offers the icicle", {
  s <- pv_suggest(pv_city_landuse, n = 10)
  expect_true("pv_sunburst" %in% suggest_charts(s))
  code <- suggest_code(s, "pv_sunburst")
  # Outermost grouping first: the parent leads the levels vector.
  expect_match(code, 'levels = c("group", "category")', fixed = TRUE)
  expect_match(code, 'value = "hectares"', fixed = TRUE)
  expect_match(suggest_reason(s, "pv_sunburst"), "pv_icicle", fixed = TRUE)
  w <- suggest_eval(code, list(pv_city_landuse = pv_city_landuse))
  expect_s3_class(w, "pvchart")
  # A code column and its 1:1 name column is a relabeling, not a tree.
  s2 <- pv_suggest(pv_city_sectors, n = 10)
  expect_false("pv_sunburst" %in% suggest_charts(s2))
})

test_that("a numeric list-column earns a sparkline table", {
  sp <- data.frame(name = c("a", "b", "c"))
  sp$trend <- list(c(1, 2, 3), c(3, 2, 1), c(2, 2, 5))
  s <- pv_suggest(sp, n = 10)
  expect_true("pv_table" %in% suggest_charts(s))
  code <- suggest_code(s, "pv_table")
  expect_match(code, 'spark = c("trend")', fixed = TRUE)
  w <- suggest_eval(code, list(sp = sp))
  expect_s3_class(w, "pvchart")
})

test_that("a frame with no chartable shape still offers the table", {
  s <- pv_suggest(pv_city_coords, n = 10)
  expect_identical(suggest_charts(s), "pv_table")
  expect_match(suggest_code(s, "pv_table"), "pv_table(pv_city_coords)",
               fixed = TRUE)
})

# The same promise for the shapes the bundled frames don't reach on
# their own: frames built to trigger each of the new suggestion paths,
# every printed call evaluated like the bundled loop below does.
test_that("every printed suggestion for the new-shape frames builds a widget", {
  frames <- list(
    two_moments = pv_city_population[
      pv_city_population$year %in% c(1930, 2024), ],
    paired_wide = head(merge(
      pv_fiscal[pv_fiscal$year == 2020, c("municipality", "resource_index")],
      pv_fiscal[pv_fiscal$year == 2027, c("municipality", "resource_index")],
      by = "municipality", suffixes = c("_2020", "_2027")), 30),
    contributions = data.frame(
      item = c("Wages", "Goods", "Fees", "Transfers", "Interest", "Rents"),
      change_chf = c(420, -180, 260, -90, 35, 150)),
    shares = data.frame(party = c("A", "B", "C", "D"), n = c(6, 3, 2, 1)),
    nested = pv_city_landuse[pv_city_landuse$city %in%
                               c("Luzern", "Zug", "Emmen"), ],
    spaghetti = aggregate(arrivals ~ year + canton, pv_tourism, sum),
    od_flows = data.frame(
      from = c("Aargau", "Luzern", "Schwyz", "Z\u00fcrich"),
      to = c("Zug", "Zug", "Zug", "Zug"),
      commuters = c(4905, 11251, 4576, 8000)))
  triggers <- list(two_moments = "pv_slope", paired_wide = "pv_dumbbell",
                   contributions = "pv_waterfall", shares = "pv_waffle",
                   nested = "pv_sunburst", spaghetti = "pv_horizon",
                   od_flows = "pv_flow_map")
  for (nm in names(frames)) {
    assign(nm, frames[[nm]])
    s <- eval(call("pv_suggest", as.name(nm), n = 10L))
    expect_true(triggers[[nm]] %in% suggest_charts(s), label = nm)
    expect_false(anyDuplicated(suggest_charts(s)) > 0)
    for (r in s$suggestions) {
      err <- NULL
      w <- tryCatch(
        suggest_eval(r$code, stats::setNames(list(frames[[nm]]), nm)),
        error = function(e) {
          err <<- conditionMessage(e)
          NULL
        })
      expect_true(
        is.null(err),
        info = sprintf("%s / %s errored: %s", nm, r$chart,
                       if (is.null(err)) "" else err))
      expect_true(
        inherits(w, "pvchart"),
        info = sprintf("%s / %s did not return a chart", nm, r$chart))
    }
  }
})

test_that("every printed suggestion for every bundled frame builds a widget", {
  items <- data(package = "polyviz")$results[, "Item"]
  for (item in items) {
    obj <- get(item)
    if (!is.data.frame(obj)) next
    # Called through the dataset's own name, exactly as a user would.
    s <- eval(call("pv_suggest", as.name(item), n = 10L))
    expect_gt(length(s$suggestions), 0, label = item)
    expect_false(anyDuplicated(suggest_charts(s)) > 0)
    for (r in s$suggestions) {
      err <- NULL
      w <- tryCatch(
        suggest_eval(r$code, stats::setNames(list(obj), item)),
        error = function(e) {
          err <<- conditionMessage(e)
          NULL
        })
      expect_true(
        is.null(err),
        info = sprintf("%s / %s errored: %s", item, r$chart,
                       if (is.null(err)) "" else err))
      expect_true(
        inherits(w, "pvchart"),
        info = sprintf("%s / %s did not return a chart", item, r$chart))
    }
  }
})
