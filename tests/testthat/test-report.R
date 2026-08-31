# pv_report writes a full HTML page, so these tests work at the file
# level: build a report into a temp dir, read it back, and grep for the
# section headings and the embedded widget payloads (each htmlwidget
# serialises its payload with a "type":"<renderer>" field).

build_report <- function(data, ..., env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  path <- file.path(dir, "report.html")
  ret <- pv_report(data, path, ...)
  list(dir = dir, path = ret,
       html = paste(readLines(path, warn = FALSE), collapse = "\n"))
}

count_widgets <- function(html, type) {
  hits <- gregexpr(sprintf('"type":"%s"', type), html, fixed = TRUE)[[1]]
  sum(hits > 0)
}

test_that("airquality report has the expected headings and widgets", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "report.html")
  # called directly (not through a helper) so the default title comes
  # from the actual argument expression. Ozone and Solar.R carry NAs,
  # but the report counts them in the histogram subtitles and drops
  # them before charting, so the build itself stays quiet.
  expect_no_warning(pv_report(airquality, path))
  r <- list(dir = dir, path = path,
            html = paste(readLines(path, warn = FALSE), collapse = "\n"))
  expect_match(r$html, "<h1>airquality</h1>", fixed = TRUE)
  expect_match(r$html, "153 rows and 6 columns", fixed = TRUE)
  expect_match(r$html, "Column summary", fixed = TRUE)
  expect_match(r$html, "Distributions", fixed = TRUE)
  expect_match(r$html, "Relationships", fixed = TRUE)
  # airquality has no categorical column, so that section must be absent
  expect_false(grepl("Categories", r$html, fixed = TRUE))
  expect_match(r$html, "polyviz", fixed = TRUE)
  # six numeric columns -> six histograms, plus one correlation heatmap
  expect_equal(count_widgets(r$html, "histogram"), 6)
  expect_equal(count_widgets(r$html, "heatmap"), 1)
  expect_gte(count_widgets(r$html, "histogram") +
               count_widgets(r$html, "heatmap"), 4)
  # widget assets land in lib/ alongside the page
  expect_true(dir.exists(file.path(r$dir, "lib")))
})

test_that("report returns the file path invisibly and can be re-titled", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "out.html")
  vis <- withVisible(pv_report(mtcars, path, title = "Motor Trend cars"))
  expect_false(vis$visible)
  expect_equal(vis$value, path)
  html <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_match(html, "<h1>Motor Trend cars</h1>", fixed = TRUE)
})

test_that("caps hold on a wide frame: 8 histograms, 4 bar charts", {
  set.seed(1)
  wide <- as.data.frame(c(
    stats::setNames(lapply(1:12, function(i) stats::rnorm(40, mean = i)),
                    paste0("num", 1:12)),
    stats::setNames(lapply(1:6, function(i) {
      sample(letters[1:5], 40, replace = TRUE)
    }), paste0("cat", 1:6))
  ))
  r <- build_report(wide)
  expect_equal(count_widgets(r$html, "histogram"), 8)
  expect_equal(count_widgets(r$html, "bar"), 4)
  expect_match(r$html, "first 8 of 12 numeric columns", fixed = TRUE)
  expect_match(r$html, "first 4 of 6 categorical columns", fixed = TRUE)
})

test_that("a frame with no numeric columns skips those sections", {
  chars <- data.frame(city = rep(c("Bern", "Zug", "Luzern"), 4),
                      grade = rep(c("A", "B"), 6))
  r <- build_report(chars)
  expect_false(grepl("Distributions", r$html, fixed = TRUE))
  expect_false(grepl("Relationships", r$html, fixed = TRUE))
  expect_match(r$html, "Categories", fixed = TRUE)
  expect_equal(count_widgets(r$html, "histogram"), 0)
  expect_equal(count_widgets(r$html, "heatmap"), 0)
  expect_equal(count_widgets(r$html, "bar"), 2)
})

test_that("a frame with no categorical columns skips that section", {
  nums <- data.frame(a = 1:10, b = (1:10)^2, c = sqrt(1:10))
  r <- build_report(nums)
  expect_false(grepl("Categories", r$html, fixed = TRUE))
  expect_equal(count_widgets(r$html, "bar"), 0)
  expect_equal(count_widgets(r$html, "histogram"), 3)
  # exactly 3 varying numerics is enough for the correlation heatmap
  expect_equal(count_widgets(r$html, "heatmap"), 1)
})

test_that("two numerics are too few for a correlation heatmap", {
  r <- build_report(data.frame(a = 1:10, b = stats::runif(10)))
  expect_false(grepl("Relationships", r$html, fixed = TRUE))
  expect_equal(count_widgets(r$html, "heatmap"), 0)
})

test_that("id-like categoricals (>30 levels) are left out", {
  ids <- data.frame(id = sprintf("row-%02d", 1:40),
                    group = rep(c("x", "y"), 20))
  r <- build_report(ids)
  # only "group" qualifies, so exactly one bar chart
  expect_equal(count_widgets(r$html, "bar"), 1)
  expect_match(r$html, "Categories", fixed = TRUE)
})

test_that("bad inputs fail with clear messages", {
  expect_error(pv_report(1:5, tempfile()), "data frame")
  expect_error(pv_report(mtcars, NA_character_), "single file path")
  expect_error(pv_report(mtcars, character(0)), "single file path")
  expect_error(pv_report(mtcars, tempfile(), mode = "sepia"), "mode")
})
