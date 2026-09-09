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

test_that("horizon builds from real data with a shared band scale", {
  nights <- aggregate(nights ~ canton + year, pv_tourism, sum)
  w <- expect_pvchart(pv_horizon(nights, "year", "nights",
                                 series = "canton"), "horizon")
  expect_equal(w$x$xtype, "number")
  expect_setequal(w$x$series, unique(nights$canton))
  expect_equal(w$x$bands, 3L)
  # The shared band scale is the uniform construction: the largest
  # absolute value anywhere in the data, split into `bands` slices.
  expect_equal(w$x$bandWidth, max(abs(nights$nights)) / 3)
  expect_true(w$x$mirror)
  expect_equal(w$x$vlab, "nights")
  expect_equal(w$x$xlab, "year")
  expect_equal(nrow(w$x$data), nrow(nights))
  expect_error(pv_horizon(nights, "nope", "nights", series = "canton"),
               "not in `data`")
})

test_that("horizon validates bands and mirror", {
  df <- data.frame(t = rep(1:3, 2), v = 1:6, s = rep(c("a", "b"), each = 3))
  expect_equal(pv_horizon(df, "t", "v", series = "s", bands = 2)$x$bands, 2L)
  expect_equal(pv_horizon(df, "t", "v", series = "s", bands = 5)$x$bands, 5L)
  for (bad in list(1, 6, 2.5, "three", NA)) {
    expect_error(pv_horizon(df, "t", "v", series = "s", bands = bad),
                 "between 2 and 5")
  }
  expect_false(pv_horizon(df, "t", "v", series = "s",
                          mirror = FALSE)$x$mirror)
  # No "auto" here: whether negatives exist is a fact of the data, not a
  # render-time decision.
  expect_error(pv_horizon(df, "t", "v", series = "s", mirror = "auto"),
               "TRUE or FALSE")
})

test_that("horizon folds negatives only when mirrored", {
  df <- data.frame(t = rep(1:3, 2), v = c(1, -2, 3, -4, 5, -6),
                   s = rep(c("a", "b"), each = 3))
  # The scale is symmetric in construction: the largest |value| sets it.
  expect_equal(pv_horizon(df, "t", "v", series = "s")$x$bandWidth, 2)
  expect_error(pv_horizon(df, "t", "v", series = "s", mirror = FALSE),
               "nowhere to fold")
})

test_that("horizon orders rows by appearance, name, or peak", {
  df <- data.frame(t = rep(1:2, 3), v = c(5, 5, 9, -9, 1, 1),
                   s = rep(c("beta", "zed", "ant"), each = 2))
  expect_equal(pv_horizon(df, "t", "v", series = "s")$x$series,
               c("beta", "zed", "ant"))
  expect_equal(pv_horizon(df, "t", "v", series = "s",
                          order = "alpha")$x$series,
               c("ant", "beta", "zed"))
  # "max" ranks by each series' largest absolute value, so the mirrored
  # -9 puts zed first.
  expect_equal(pv_horizon(df, "t", "v", series = "s",
                          order = "max")$x$series,
               c("zed", "beta", "ant"))
  expect_error(pv_horizon(df, "t", "v", series = "s", order = "sideways"))
})

test_that("horizon refuses duplicates and drops missing values", {
  dup <- data.frame(t = c(1, 1, 2), v = 1:3, s = "a")
  expect_error(pv_horizon(dup, "t", "v", series = "s"), "aggregate")
  nas <- data.frame(t = 1:3, v = c(1, NA, 3), s = "a")
  expect_warning(w <- pv_horizon(nas, "t", "v", series = "s"),
                 "Dropped 1 row\\(s\\) with missing `v` values")
  expect_equal(nrow(w$x$data), 2)
  all_na <- data.frame(t = 1:2, v = NA_real_, s = "a")
  expect_error(pv_horizon(all_na, "t", "v", series = "s"), "non-missing")
})

test_that("horizon gives an all-zero band scale a token width", {
  flat <- data.frame(t = 1:3, v = 0, s = "a")
  expect_equal(pv_horizon(flat, "t", "v", series = "s")$x$bandWidth, 1)
})

test_that("horizon refuses more series than the height can hold", {
  many <- expand.grid(t = 1:3, s = sprintf("s%02d", 1:30))
  many$v <- as.numeric(seq_len(nrow(many)))
  # 30 rows at the 12px floor need 460px; the default height is 420.
  expect_error(pv_horizon(many, "t", "v", series = "s"),
               "Too many series for the height")
  expect_error(pv_horizon(many, "t", "v", series = "s"), "height = 460")
  expect_pvchart(pv_horizon(many, "t", "v", series = "s", height = 460),
                 "horizon")
  # A non-numeric height (a CSS string) falls back to the 420 default.
  expect_error(pv_horizon(many, "t", "v", series = "s", height = "100%"),
               "Too many series")
})

test_that("horizon sorts rows by x within series and understands x types", {
  df <- data.frame(t = c(3, 1, 2, 2, 3, 1), v = 1:6,
                   s = rep(c("a", "b"), each = 3))
  w <- pv_horizon(df, "t", "v", series = "s")
  expect_equal(w$x$data$x, rep(1:3, 2))
  expect_equal(w$x$data$y[1:3], c(2, 3, 1))
  dated <- data.frame(d = as.Date(c("2024-02-01", "2024-01-01")), v = 1:2,
                      s = "a")
  wd <- pv_horizon(dated, "d", "v", series = "s")
  expect_equal(wd$x$xtype, "date")
  # Dates travel as ISO strings for d3 to re-parse, sorted within series.
  expect_equal(wd$x$data$x, c("2024-01-01", "2024-02-01"))
  # A category axis keeps the rows' arrival order.
  cats <- data.frame(m = rep(month.abb[1:3], 2), v = 1:6,
                     s = rep(c("a", "b"), each = 3))
  wc <- pv_horizon(cats, "m", "v", series = "s")
  expect_equal(wc$x$xtype, "category")
  expect_equal(wc$x$data$x, rep(month.abb[1:3], 2))
})

test_that("horizon resolves the x-axis title override", {
  df <- data.frame(t = rep(1:3, 2), v = 1:6, s = rep(c("a", "b"), each = 3))
  expect_equal(pv_horizon(df, "t", "v", series = "s")$x$xlab, "t")
  expect_equal(pv_horizon(df, "t", "v", series = "s", xlab = NA)$x$xlab, "")
  expect_equal(pv_horizon(df, "t", "v", series = "s", xlab = "")$x$xlab, "")
  expect_equal(pv_horizon(df, "t", "v", series = "s",
                          xlab = "Year")$x$xlab, "Year")
  expect_error(pv_horizon(df, "t", "v", series = "s",
                          xlab = c("a", "b")), "single string")
})

test_that("horizon carries generated alt text", {
  df <- data.frame(t = rep(1:3, 2), v = 1:6, s = rep(c("a", "b"), each = 3))
  w <- pv_horizon(df, "t", "v", series = "s", title = "Two ribbons")
  txt <- pv_alt_text(w)
  expect_true(is.character(txt) && length(txt) == 1 && nzchar(txt))
  expect_identical(w$x$alt, txt)
})

test_that("horizon renders and exports a standalone SVG", {
  render_skip_if_no_chrome()
  nights <- aggregate(nights ~ canton + year, pv_tourism, sum)
  w <- pv_horizon(nights, "year", "nights", series = "canton",
                  title = "Where Switzerland's guests sleep")
  png <- tempfile(fileext = ".png")
  expect_no_warning(pv_save(w, png, quiet = TRUE))
  expect_gt(file.size(png), 20000)
  unlink(png)
  svg_path <- tempfile(fileext = ".svg")
  expect_no_warning(pv_save(w, svg_path, quiet = TRUE))
  svg <- paste(readLines(svg_path, warn = FALSE, encoding = "UTF-8"),
               collapse = "\n")
  expect_match(svg, "^<\\?xml")
  # The root document plus the one embedded plot.
  expect_equal(lengths(regmatches(svg, gregexpr("<svg", svg))), 2L)
  expect_match(svg, "Where Switzerland's guests sleep", fixed = TRUE)
  # The band-scale legend rides along into the export.
  expect_match(svg, "each shade = one band of 2.31M nights", fixed = TRUE)
  unlink(svg_path)
})

test_that("a mirrored horizon renders without JavaScript errors", {
  render_skip_if_no_chrome()
  w <- pv_weather
  w$doy <- as.integer(format(w$date, "%j"))
  w$anomaly <- w$temp_mean - ave(w$temp_mean, format(w$date, "%m-%d"))
  w$year <- format(w$date, "%Y")
  wh <- pv_horizon(w, "doy", "anomaly", series = "year",
                   title = "Warm and cold spells")
  png <- tempfile(fileext = ".png")
  expect_no_warning(pv_save(wh, png, quiet = TRUE))
  expect_gt(file.size(png), 20000)
  unlink(png)
})

# Stages a widget the way pv_save() does (light mode, no entrance
# animation, filling a page opened at a fixed size) and hands back the
# live Chrome session, for the crosshair check below. Same machinery as
# test-charts-comparison.R - each test file stands alone.
evolution_page_session <- function(w, width = 700, height = 460) {
  w$x$mode <- "light"
  w$x$duration <- 0
  w$width <- NULL
  w$height <- NULL
  w$sizingPolicy$browser$fill <- TRUE
  w$sizingPolicy$browser$padding <- 0
  stage <- tempfile("pv-evolution-page-")
  dir.create(stage)
  page <- file.path(stage, "chart.html")
  htmlwidgets::saveWidget(w, page, selfcontained = FALSE, libdir = "lib")
  b <- chromote::ChromoteSession$new(width = width, height = height)
  errors <- polyviz:::export_watch_errors(b)
  loaded <- b$Page$loadEventFired(wait_ = FALSE)
  b$Page$navigate(utils::URLencode(paste0("file://", normalizePath(page))),
                  wait_ = FALSE)
  b$wait_for(loaded)
  polyviz:::export_wait_settled(b, errors, 0,
                                polyviz:::export_settle_count_js(w))
  list(b = b, errors = errors)
}

test_that("the horizon crosshair reads every series out of one tooltip", {
  render_skip_if_no_chrome()
  hz <- data.frame(t = rep(1:5, 2),
                   v = c(1, 2, 3, 4, 5, -1, -2, -3, -4, -5),
                   s = rep(c("north", "south"), each = 5))
  s <- evolution_page_session(pv_horizon(hz, "t", "v", series = "s",
                                         title = "Crosshair check"))
  withr::defer(try(s$b$close(), silent = TRUE))
  expect_identical(s$errors$msgs, character())
  res <- s$b$Runtime$evaluate("
    (function () {
      /* Poke the pointer at the middle of the hover overlay - with five
         x positions the nearest one is exactly t = 3 - and read the
         tooltip. The pointer's y falls in the second ribbon, so 'south'
         is the emphasised row. */
      var over = document.querySelector('rect.pv-hover');
      if (!over) return 'no overlay';
      var r = over.getBoundingClientRect();
      var cx = r.left + r.width / 2, cy = r.top + r.height * 0.75;
      over.dispatchEvent(new PointerEvent('pointermove',
        { clientX: cx, clientY: cy, bubbles: true }));
      var tip = document.querySelector('.pv-tooltip');
      var cross = document.querySelector('svg line[stroke-dasharray]');
      return JSON.stringify({ opacity: tip.style.opacity,
        html: tip.innerHTML,
        cross: cross ? cross.getAttribute('opacity') : 'none' });
    })()", returnByValue = TRUE)$result$value
  got <- jsonlite::fromJSON(res)
  expect_equal(got$opacity, "1")
  # The crosshair line is up across the rows.
  expect_equal(got$cross, "1")
  # The header names the x position, and every series reads out its true
  # value there - the negative one included (d3 writes the minus sign).
  expect_match(got$html, "^<b>3</b>")
  expect_match(got$html, "north: <b>3</b>")
  expect_match(got$html, "<b>south</b>: <b>[\u2212-]3</b>")
})
