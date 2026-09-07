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
  expect_false("pv_choropleth" %in% charts)
  expect_false("pv_bubble_map" %in% charts)
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
