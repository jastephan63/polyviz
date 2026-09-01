# The generated chart descriptions (R/alt-text.R): plain string assembly
# on the widget payload, so everything here checks text on w$x without
# rendering anything - except the last test, which knits a document to
# prove the text reaches the included figure.

# The generated NAMESPACE registers knit_print.pvchart with knitr at
# install time; these tests run from load_all, so register it by hand.
if (requireNamespace("knitr", quietly = TRUE)) {
  registerS3method("knit_print", "pvchart", polyviz:::knit_print.pvchart,
                   envir = asNamespace("knitr"))
}

test_that("a bar chart's alt text names type, title, count, and extremes", {
  agg <- aggregate(revenue ~ region, pv_sales, sum)
  w <- pv_bar(agg, "region", "revenue", title = "Revenue by region")
  a <- w$x$alt
  expect_match(a, "^A bar chart titled \u201cRevenue by region\u201d")
  expect_match(a, sprintf("%d categories of region",
                          length(unique(agg$region))), fixed = TRUE)
  top <- agg$region[which.max(agg$revenue)]
  bottom <- agg$region[which.min(agg$revenue)]
  expect_match(a, paste0("(", top, ")"), fixed = TRUE)
  expect_match(a, paste0("(", bottom, ")"), fixed = TRUE)
  expect_match(a, format(round(max(agg$revenue)), big.mark = ","),
               fixed = TRUE)
  # the attached text and the generator agree
  expect_identical(pv_alt_text(w), a)
  # without a title, the lead sentence folds straight into the clause
  expect_match(pv_bar(agg, "region", "revenue")$x$alt,
               "^A bar chart showing revenue across ")
})

test_that("a line chart's alt text names the series and the final leader", {
  cities <- subset(pv_city_population, city %in% c("Luzern", "Zug"))
  w <- pv_line(cities, x = "year", y = "population", series = "city",
               title = "Two cities")
  a <- w$x$alt
  expect_match(a, "^A line chart titled \u201cTwo cities\u201d")
  expect_match(a, "2 lines (Luzern and Zug)", fixed = TRUE)
  expect_match(a, sprintf("from %d to %d", min(cities$year),
                          max(cities$year)), fixed = TRUE)
  fin <- cities[cities$year == max(cities$year), ]
  expect_match(a, sprintf("At the last point (%d), %s is highest",
                          max(cities$year),
                          fin$city[which.max(fin$population)]),
               fixed = TRUE)
})

test_that("a scatter plot's alt text counts points and names the top label", {
  f25 <- subset(pv_fiscal, year == 2025)
  w <- pv_scatter(f25, x = "resource_index", y = "equalization_chf",
                  label = "municipality")
  a <- w$x$alt
  expect_match(a, "^A scatter plot plotting equalization_chf against resource_index")
  expect_match(a, sprintf("%d points", nrow(f25)), fixed = TRUE)
  top <- f25$municipality[which.max(f25$equalization_chf)]
  expect_match(a, sprintf("%s has the highest equalization_chf", top),
               fixed = TRUE)
})

test_that("a donut's alt text carries the total and the largest share", {
  seats <- data.frame(party = c("A", "B", "C"), n = c(6, 3, 1))
  w <- pv_donut(seats, category = "party", value = "n", title = "Seats")
  a <- w$x$alt
  expect_match(a, "^A donut chart titled \u201cSeats\u201d")
  expect_match(a, "3 slices (A, B, and C)", fixed = TRUE)
  expect_match(a, "The total is 10.", fixed = TRUE)
  expect_match(a, "The largest slice, A, holds 6 (60% of the total).",
               fixed = TRUE)
  # a zero hole is a pie, and the description says so
  pie <- pv_donut(seats, category = "party", value = "n", inner_radius = 0)
  expect_match(pie$x$alt, "^A pie chart")
})

test_that("pv_alt overrides the generated text, and validates its input", {
  agg <- aggregate(revenue ~ region, pv_sales, sum)
  w <- pv_bar(agg, "region", "revenue")
  w2 <- pv_alt(w, "My own words.")
  expect_identical(w2$x$alt, "My own words.")
  # the generator itself is untouched - it still describes the data
  expect_match(pv_alt_text(w2), "^A bar chart showing revenue")
  expect_error(pv_alt(w, ""), "single non-empty string")
  expect_error(pv_alt(w, NA_character_), "single non-empty string")
  expect_error(pv_alt(w, c("a", "b")), "single non-empty string")
  expect_error(pv_alt(w, 42), "single non-empty string")
  expect_error(pv_alt("nope", "text"), "polyviz chart")
  expect_error(pv_alt_text("nope"), "polyviz chart")
})

test_that("every canonical chart carries clean, period-terminated alt text", {
  for (id in names(c(render_charts, render_variants))) {
    w <- c(render_charts, render_variants)[[id]]()
    a <- w$x$alt
    expect_true(is.character(a) && length(a) == 1 && !is.na(a) && nzchar(a),
                label = sprintf("%s alt text is a usable string", id))
    expect_true(grepl("\\.$", a),
                label = sprintf("%s alt text ends with a period", id))
    expect_false(grepl("\\bNA\\b|\\bNULL\\b|\\bNaN\\b|\\(no value\\)", a),
                 label = sprintf("%s alt text leaks a missing value", id))
    expect_false(grepl("  ", a, fixed = TRUE),
                 label = sprintf("%s alt text has doubled spaces", id))
  }
})

test_that("knitted markdown carries the alt text on the figure", {
  skip_on_cran()
  skip_if_not_installed("knitr")
  render_skip_if_no_chrome()
  dir <- withr::local_tempdir()
  writeLines(c(
    "```{r setup, include=FALSE}",
    "knitr::opts_chunk$set(screenshot.force = FALSE)",
    "# a pandoc-driven knit, the way rmarkdown::render() would set it up",
    "knitr::opts_knit$set(\"rmarkdown.pandoc.to\" = \"gfm\")",
    "```", "",
    "```{r auto, fig.width=6, fig.height=4, dpi=96}",
    "agg <- aggregate(revenue ~ region, pv_sales, sum)",
    "pv_bar(agg, \"region\", \"revenue\", title = \"Revenue by region\")",
    "```", "",
    "```{r manual, fig.width=6, fig.height=4, dpi=96, fig.alt=\"Author words here.\"}",
    "pv_bar(agg, \"region\", \"revenue\", title = \"Second chart\")",
    "```", ""), file.path(dir, "doc.Rmd"))
  withr::local_dir(dir)
  knitr::knit("doc.Rmd", "doc.md", quiet = TRUE,
              envir = new.env(parent = globalenv()))
  md <- paste(readLines("doc.md", warn = FALSE), collapse = "\n")
  # the generated description rides on the image as its alt attribute
  expect_match(md, "alt=\"A bar chart titled \u201cRevenue by region\u201d",
               fixed = TRUE)
  expect_match(md, "figure/auto-pv-1.png", fixed = TRUE)
  expect_match(md, "figure/manual-pv-1.png", fixed = TRUE)
  # an author's own fig.alt chunk option wins over the generated text
  expect_match(md, "alt=\"Author words here.\"", fixed = TRUE)
  expect_false(grepl("titled \u201cSecond chart\u201d", md, fixed = TRUE))
  # no stray figure wrappers: one image tag per chart, nothing doubled
  expect_length(gregexpr("<img ", md, fixed = TRUE)[[1]], 2)
  expect_false(grepl("<div class=\"figure\">", md, fixed = TRUE))
})
