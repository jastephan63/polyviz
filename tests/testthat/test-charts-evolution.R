# Same helper as test-widgets.R — each test file stands alone.
expect_pvchart <- function(w, type) {
  expect_s3_class(w, "htmlwidget")
  expect_equal(attr(w, "package"), "polyviz")
  expect_equal(w$x$type, type)
  invisible(w)
}

test_that("area widget builds from real data and validates columns", {
  agglo <- subset(pv_city_population,
                  city %in% c("Luzern", "Emmen", "Kriens", "Horw", "Ebikon"))
  w <- expect_pvchart(pv_area(agglo, "year", "population", series = "city"),
                      "area")
  expect_equal(w$x$offset, "stacked")
  expect_equal(w$x$xtype, "number")
  expect_setequal(w$x$series, unique(agglo$city))
  expect_equal(nrow(w$x$data), nrow(agglo))
  expect_error(pv_area(agglo, "nope", "population"), "not in `data`")
})

test_that("area keeps series in first-appearance order", {
  df <- data.frame(t = rep(1:3, 2), v = 1:6,
                   s = rep(c("late", "early"), each = 3))
  w <- pv_area(df, "t", "v", series = "s")
  expect_equal(w$x$series, c("late", "early"))
})

test_that("area refuses duplicate series/x rows", {
  dup <- data.frame(t = c(1, 1, 2), v = c(10, 99, 5), s = "a")
  expect_error(pv_area(dup, "t", "v", series = "s"), "aggregate")
  # Single-series charts hit the same pivot, so the same rule applies.
  expect_error(pv_area(dup, "t", "v"), "aggregate")
})

test_that("area caps series at the active theme's palette", {
  # Under the packaged theme nothing changes: the cap is 8, and the
  # message says which theme set it.
  expect_error(pv_area(pv_city_population, "year", "population",
                       series = "city"), "Other")
  expect_error(pv_area(pv_city_population, "year", "population",
                       series = "city"),
               "active theme's palette has 8 slots")
  # A smaller theme lowers the cap: the paper theme's 5 slots refuse a
  # sixth band instead of silently recycling a colour.
  on.exit(pv_reset_theme())
  six <- subset(pv_city_population,
                city %in% c("Luzern", "Emmen", "Kriens", "Horw", "Ebikon",
                            "Zug"))
  five <- subset(six, city != "Zug")
  pv_set_theme(pv_theme_paper())
  expect_error(pv_area(six, "year", "population", series = "city"),
               "6 levels but the active theme's palette has 5 slots")
  expect_pvchart(pv_area(five, "year", "population", series = "city"),
                 "area")
  # Resetting the theme restores the packaged 8-slot cap.
  pv_reset_theme()
  expect_pvchart(pv_area(six, "year", "population", series = "city"),
                 "area")
})

test_that("area understands offsets and x types", {
  agglo <- subset(pv_city_population, city %in% c("Luzern", "Emmen"))
  expect_equal(pv_area(agglo, "year", "population", series = "city",
                       offset = "percent")$x$offset, "percent")
  expect_equal(pv_area(agglo, "year", "population", series = "city",
                       offset = "stream")$x$offset, "stream")
  expect_error(pv_area(agglo, "year", "population", offset = "wavy"))
  dated <- data.frame(d = as.Date(c("2024-01-01", "2024-02-01")), v = 1:2)
  w <- pv_area(dated, "d", "v")
  expect_equal(w$x$xtype, "date")
  expect_equal(w$x$data$x, c("2024-01-01", "2024-02-01"))
  expect_equal(w$x$legend, "auto")
  expect_equal(w$x$series, "value")
})

test_that("area sends the legend flag as-is and validates its shape", {
  agglo <- subset(pv_city_population, city %in% c("Luzern", "Emmen"))
  # "auto" travels unresolved: the JavaScript side decides at render time.
  expect_equal(pv_area(agglo, "year", "population",
                       series = "city")$x$legend, "auto")
  expect_true(pv_area(agglo, "year", "population", series = "city",
                      legend = TRUE)$x$legend)
  expect_false(pv_area(agglo, "year", "population", series = "city",
                       legend = FALSE)$x$legend)
  expect_error(pv_area(agglo, "year", "population", legend = "yes"),
               "TRUE, FALSE")
  expect_error(pv_area(agglo, "year", "population", legend = NA),
               "TRUE, FALSE")
})

test_that("area resolves axis-title overrides in the payload", {
  agglo <- subset(pv_city_population, city %in% c("Luzern", "Emmen"))
  w <- pv_area(agglo, "year", "population", series = "city")
  expect_equal(w$x$xlab, "year")
  expect_equal(w$x$ylab, "population")
  # Percent and stream offsets drop the default y title (their y axes
  # don't show raw values), but an explicit one survives.
  expect_equal(pv_area(agglo, "year", "population", series = "city",
                       offset = "percent")$x$ylab, "")
  expect_equal(pv_area(agglo, "year", "population", series = "city",
                       offset = "percent", ylab = "Share")$x$ylab, "Share")
  # NA and "" both suppress; a string replaces the column name.
  w2 <- pv_area(agglo, "year", "population", series = "city",
                xlab = NA, ylab = "")
  expect_equal(w2$x$xlab, "")
  expect_equal(w2$x$ylab, "")
  expect_equal(pv_area(agglo, "year", "population", series = "city",
                       xlab = "Census year")$x$xlab, "Census year")
  expect_error(pv_area(agglo, "year", "population", series = "city",
                       xlab = c("a", "b")), "single string")
})

test_that("area zoom is a plain switch and needs a continuous x axis", {
  agglo <- subset(pv_city_population, city %in% c("Luzern", "Emmen"))
  # The default keeps the old rendering: no strip.
  expect_false(pv_area(agglo, "year", "population", series = "city")$x$zoom)
  expect_true(pv_area(agglo, "year", "population", series = "city",
                      zoom = TRUE)$x$zoom)
  # No "auto" here - there is no data-driven decision to defer.
  expect_error(pv_area(agglo, "year", "population", zoom = "auto"),
               "TRUE or FALSE")
  cats <- data.frame(m = month.abb[1:4], v = 1:4)
  expect_error(pv_area(cats, "m", "v", zoom = TRUE), "categorical")
})

test_that("area zoom renders and keeps the strip out of the SVG export", {
  render_skip_if_no_chrome()
  agglo <- subset(pv_city_population,
                  city %in% c("Luzern", "Emmen", "Kriens", "Horw", "Ebikon"))
  w <- pv_area(agglo, "year", "population", series = "city", zoom = TRUE)
  png <- tempfile(fileext = ".png")
  expect_no_warning(pv_save(w, png, quiet = TRUE))
  expect_gt(file.size(png), 20000)
  unlink(png)
  svg_path <- tempfile(fileext = ".svg")
  expect_no_warning(pv_save(w, svg_path, quiet = TRUE))
  svg <- paste(readLines(svg_path, warn = FALSE), collapse = "\n")
  # The root document plus the one embedded plot - no strip svg, and no
  # trace of the brush chrome.
  expect_equal(lengths(regmatches(svg, gregexpr("<svg", svg))), 2L)
  expect_false(grepl("overlay", svg, fixed = TRUE))
  unlink(svg_path)
})

test_that("heatmap builds and picks its colour domain in R", {
  emp <- subset(pv_city_sectors,
                city %in% c("Luzern", "Winterthur", "Bern"))
  w <- expect_pvchart(pv_heatmap(emp, "city", "sector", "share"), "heatmap")
  expect_equal(w$x$palette, "sequential")
  expect_equal(w$x$domain, range(emp$share))
  div <- data.frame(a = rep(c("p", "q"), 2), b = rep(c("r", "s"), each = 2),
                    v = c(-2, 5, 1, -4))
  wd <- pv_heatmap(div, "a", "b", "v", palette = "diverging")
  expect_equal(wd$x$domain, c(-5, 5))
})

test_that("heatmap validates cells and drops missing values", {
  dup <- data.frame(a = c("p", "p"), b = c("q", "q"), v = 1:2)
  expect_error(pv_heatmap(dup, "a", "b", "v"), "aggregate")
  bad <- data.frame(a = "p", b = "q", v = 1)
  expect_error(pv_heatmap(bad, "a", "b", "nope"), "not in `data`")
  nas <- data.frame(a = c("p", "q"), b = c("r", "r"), v = c(1, NA))
  expect_warning(wn <- pv_heatmap(nas, "a", "b", "v"),
                 "Dropped 1 row\\(s\\) with missing `v` values")
  expect_equal(nrow(wn$x$data), 1)
  all_na <- data.frame(a = c("p", "q"), b = "r", v = NA_real_)
  expect_error(pv_heatmap(all_na, "a", "b", "v"), "non-missing")
})

test_that("heatmap widens an all-equal colour domain", {
  flat <- data.frame(a = c("p", "q"), b = c("r", "r"), v = c(3, 3))
  expect_equal(pv_heatmap(flat, "a", "b", "v")$x$domain, c(2, 4))
})

test_that("heatmap sends the cell-value flag as-is and validates its shape", {
  emp <- subset(pv_city_sectors, city %in% c("Luzern", "Bern"))
  expect_equal(pv_heatmap(emp, "city", "sector", "share")$x$cellValues,
               "auto")
  expect_true(pv_heatmap(emp, "city", "sector", "share",
                         cell_values = TRUE)$x$cellValues)
  expect_false(pv_heatmap(emp, "city", "sector", "share",
                          cell_values = FALSE)$x$cellValues)
  expect_error(pv_heatmap(emp, "city", "sector", "share",
                          cell_values = "sometimes"), "TRUE, FALSE")
})

test_that("heatmap validates the label character budget", {
  emp <- subset(pv_city_sectors, city %in% c("Luzern", "Bern"))
  expect_equal(pv_heatmap(emp, "city", "sector", "share")$x$truncateLabels,
               24L)
  expect_equal(pv_heatmap(emp, "city", "sector", "share",
                          truncate_labels = 12)$x$truncateLabels, 12L)
  expect_error(pv_heatmap(emp, "city", "sector", "share",
                          truncate_labels = 0), "positive")
  expect_error(pv_heatmap(emp, "city", "sector", "share",
                          truncate_labels = "lots"), "positive")
})

test_that("heatmap draws axis titles only when set explicitly", {
  emp <- subset(pv_city_sectors, city %in% c("Luzern", "Bern"))
  w <- pv_heatmap(emp, "city", "sector", "share")
  expect_equal(w$x$xtitle, "")
  expect_equal(w$x$ytitle, "")
  # xlab/ylab keep carrying the column names — the tooltip contract.
  expect_equal(w$x$xlab, "city")
  expect_equal(w$x$ylab, "sector")
  w2 <- pv_heatmap(emp, "city", "sector", "share",
                   xlab = "City", ylab = "Economic sector")
  expect_equal(w2$x$xtitle, "City")
  expect_equal(w2$x$ytitle, "Economic sector")
  expect_equal(pv_heatmap(emp, "city", "sector", "share",
                          xlab = NA)$x$xtitle, "")
})

test_that("calendar builds from real data with ISO dates and an R-side domain", {
  w <- expect_pvchart(
    pv_calendar(pv_weather, "date", "temp_max", years = 2023:2025),
    "calendar")
  expect_equal(w$x$years, 2023:2025)
  expect_equal(w$x$vlab, "temp_max")
  # Dates travel as ISO strings for d3 to re-parse.
  expect_true(all(grepl("^\\d{4}-\\d{2}-\\d{2}$", w$x$data$date)))
  # Three full years of daily rows, none dropped.
  expect_equal(nrow(w$x$data), 365 + 366 + 365)
  # The colour domain is the observed range of what is shown.
  shown <- pv_weather[format(pv_weather$date, "%Y") %in% 2023:2025, ]
  expect_equal(w$x$domain, range(shown$temp_max))
  expect_error(pv_calendar(pv_weather, "nope", "temp_max"), "not in `data`")
})

test_that("calendar coerces coercible dates and refuses the rest", {
  chr <- data.frame(day = c("2024-01-01", "2024-01-02"), v = 1:2)
  w <- pv_calendar(chr, "day", "v")
  expect_equal(w$x$data$date, chr$day)
  expect_equal(w$x$years, 2024L)
  bad <- data.frame(day = c("first of March", "2024-01-02"), v = 1:2)
  expect_error(pv_calendar(bad, "day", "v"), "as.Date")
  expect_error(pv_calendar(data.frame(day = 1:2, v = 1:2), "day", "v"),
               "as.Date")
})

test_that("calendar filters by years and validates them", {
  w <- pv_calendar(pv_weather, "date", "temp_max", years = c(2024, 2022))
  # The years vector is sorted, and only their rows survive.
  expect_equal(w$x$years, c(2022L, 2024L))
  expect_equal(nrow(w$x$data), 365 + 366)
  expect_true(all(substr(w$x$data$date, 1, 4) %in% c("2022", "2024")))
  expect_error(pv_calendar(pv_weather, "date", "temp_max", years = 2023.5),
               "whole calendar years")
  expect_error(pv_calendar(pv_weather, "date", "temp_max", years = "2023"),
               "whole calendar years")
  expect_error(pv_calendar(pv_weather, "date", "temp_max", years = 1999),
               "runs 2020-01-01 to 2025-12-31")
})

test_that("calendar caps the span at 6 year blocks with advice", {
  # The bundled six years pass exactly at the cap.
  expect_pvchart(pv_calendar(pv_weather, "date", "temp_max"), "calendar")
  seven <- data.frame(day = seq(as.Date("2019-06-01"), by = "1 year",
                                length.out = 7), v = 1:7)
  expect_error(pv_calendar(seven, "day", "v"), "years = 2020:2025")
  expect_error(pv_calendar(seven, "day", "v", years = 2019:2025), "max 6")
})

test_that("calendar drops missing values with a warning and refuses duplicate days", {
  df <- data.frame(day = as.Date("2024-01-01") + 0:2, v = c(1, NA, 3))
  expect_warning(w <- pv_calendar(df, "day", "v"),
                 "Dropped 1 row\\(s\\) with missing `v` values")
  expect_equal(nrow(w$x$data), 2)
  expect_false("2024-01-02" %in% w$x$data$date)
  all_na <- data.frame(day = as.Date("2024-01-01") + 0:1, v = NA_real_)
  expect_error(pv_calendar(all_na, "day", "v"), "non-missing")
  dup <- data.frame(day = as.Date(c("2024-01-01", "2024-01-01")), v = 1:2)
  expect_error(pv_calendar(dup, "day", "v"), "aggregate")
  # Duplicates outside the requested years don't matter - they never draw.
  spread <- data.frame(day = as.Date(c("2023-05-01", "2023-05-01",
                                       "2024-05-01")), v = 1:3)
  expect_pvchart(pv_calendar(spread, "day", "v", years = 2024), "calendar")
})

test_that("calendar widens an all-equal colour domain", {
  flat <- data.frame(day = as.Date("2024-01-01") + 0:1, v = c(7, 7))
  expect_equal(pv_calendar(flat, "day", "v")$x$domain, c(6, 8))
})
