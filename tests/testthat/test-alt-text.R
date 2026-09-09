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

test_that("a slope chart's alt text names the periods, the riser, and the faller", {
  two <- data.frame(year = rep(c(2000L, 2024L), each = 3),
                    g = rep(c("Up", "Down", "Flat"), 2),
                    v = c(10, 30, 20, 25, 12, 20))
  w <- pv_slope(two, x = "year", y = "v", group = "g", title = "Three paths")
  a <- w$x$alt
  expect_match(a, "^A slope chart titled \u201cThree paths\u201d")
  expect_match(a, "3 groups (Up, Down, and Flat) from 2000 to 2024",
               fixed = TRUE)
  expect_match(a, "Up rises the most, from 10 to 25", fixed = TRUE)
  expect_match(a, "Down falls the most, from 30 to 12", fixed = TRUE)
  # All-flat lines get the honest sentence, not an invented mover.
  flat <- data.frame(year = rep(c(2000L, 2024L), each = 2),
                     g = rep(c("A", "B"), 2), v = c(5, 7, 5, 7))
  expect_match(pv_slope(flat, x = "year", y = "v", group = "g")$x$alt,
               "No group changes between the two positions.", fixed = TRUE)
})

test_that("a dumbbell's alt text names the widest gap with both ends", {
  d <- data.frame(name = c("A", "B", "C"), before = c(10, 40, 25),
                  after = c(20, 90, 30))
  w <- pv_dumbbell(d, y = "name", x1 = "before", x2 = "after",
                   labels = c("Then", "Now"))
  a <- w$x$alt
  expect_match(a, "^A dumbbell chart comparing Then and Now across 3 categories")
  expect_match(a, "The widest gap is B, from 40 (Then) to 90 (Now).",
               fixed = TRUE)
})

test_that("a waterfall's alt text carries the largest contribution and the total", {
  steps <- data.frame(item = c("Wages", "Goods", "Fees"),
                      change = c(400, -150, 50))
  w <- pv_waterfall(steps, x = "item", y = "change")
  a <- w$x$alt
  expect_match(a, "^A waterfall chart building 3 signed contributions of change")
  expect_match(a, "The largest contribution is Wages, adding 400.",
               fixed = TRUE)
  expect_match(a, "The running total ends at 300.", fixed = TRUE)
  # The largest step can be a loss, and the verb says so.
  down <- data.frame(item = c("Wages", "Goods", "Fees"),
                     change = c(100, -400, 50))
  a2 <- pv_waterfall(down, x = "item", y = "change")$x$alt
  expect_match(a2, "The largest contribution is Goods, subtracting 400.",
               fixed = TRUE)
  expect_match(a2, "The running total ends at -250.", fixed = TRUE)
})

test_that("a bullet chart's alt text counts measures above, at, and below target", {
  b <- data.frame(m = c("A", "B", "C", "D"), v = c(120, 80, 100, 90),
                  t = c(100, 100, 100, 100))
  w <- pv_bullet(b, label = "m", value = "v", target = "t")
  a <- w$x$alt
  expect_match(a, "^A bullet chart comparing 4 measures")
  expect_match(a, "4 measures (A, B, C, and D) of v, each against its target",
               fixed = TRUE)
  expect_match(a, "1 sits above target, 1 hits the target exactly, and 2 fall short.",
               fixed = TRUE)
  # A single measure gets singular verbs.
  one <- data.frame(m = "A", v = 100, t = 100)
  expect_match(pv_bullet(one, label = "m", value = "v", target = "t")$x$alt,
               "1 hits the target exactly.", fixed = TRUE)
})

test_that("a waffle's alt text gives the leading share in squares and percent", {
  seats <- data.frame(party = c("A", "B", "C"), n = c(6, 3, 1))
  w <- pv_waffle(seats, category = "party", value = "n", title = "Seats")
  a <- w$x$alt
  expect_match(a, "^A waffle chart titled \u201cSeats\u201d")
  expect_match(a, "3 categories (A, B, and C) as a 10-by-10 grid of unit squares",
               fixed = TRUE)
  expect_match(a, "The largest category, A, fills 60 squares (60% of the total).",
               fixed = TRUE)
  # A coarser grid changes the square count but not the exact percent.
  w2 <- pv_waffle(seats, category = "party", value = "n", rows = 5)
  expect_match(w2$x$alt, "5-by-5 grid", fixed = TRUE)
  expect_match(w2$x$alt, "fills 15 squares (60% of the total)", fixed = TRUE)
})

test_that("an icicle's alt text mirrors the sunburst's structure", {
  luz <- pv_city_landuse[pv_city_landuse$city == "Luzern", ]
  ici <- pv_icicle(luz, levels = c("group", "category"),
                   value = "hectares")$x$alt
  sun <- pv_sunburst(luz, levels = c("group", "category"),
                     value = "hectares")$x$alt
  expect_match(ici, "^An icicle chart")
  n_leaf <- length(unique(luz$category))
  n_top <- length(unique(luz$group))
  expect_match(ici, sprintf(
    "showing a hierarchy of %d leaf segments in %d top-level groups as stacked rectangles",
    n_leaf, n_top), fixed = TRUE)
  agg <- aggregate(hectares ~ group, luz, sum)
  expect_match(ici, sprintf("The largest group, %s,",
                            agg$group[which.max(agg$hectares)]),
               fixed = TRUE)
  # Same payload, same closing fact: only the geometry words differ.
  expect_identical(sub("^.*? (The largest group,.*)$", "\\1", ici),
                   sub("^.*? (The largest group,.*)$", "\\1", sun))
})

test_that("a horizon chart's alt text counts ribbons, spans the axis, and names the deepest peak", {
  nights <- aggregate(nights ~ canton + year, pv_tourism, sum)
  w <- pv_horizon(nights, x = "year", y = "nights", series = "canton",
                  title = "Where the guests sleep")
  a <- w$x$alt
  expect_match(a, "^A horizon chart titled \u201cWhere the guests sleep\u201d")
  expect_match(a, "folding nights for 26 series", fixed = TRUE)
  expect_match(a, "3 bands of shading", fixed = TRUE)
  expect_match(a, sprintf("The year axis runs from %d to %d.",
                          min(nights$year), max(nights$year)), fixed = TRUE)
  peak <- nights[which.max(nights$nights), ]
  expect_match(a, sprintf("%s reaches the deepest band, peaking at %s.",
                          peak$canton,
                          format(round(peak$nights), big.mark = ",")),
               fixed = TRUE)
  expect_identical(pv_alt_text(w), a)
  # More bands show up in the text, and so does a suppressed axis title.
  five <- pv_horizon(nights, x = "year", y = "nights", series = "canton",
                     bands = 5, xlab = NA)
  expect_match(five$x$alt, "5 bands of shading", fixed = TRUE)
  expect_match(five$x$alt, "The x axis runs from", fixed = TRUE)
})

test_that("a mirrored horizon reports a negative peak honestly", {
  df <- data.frame(t = rep(1:4, 2), v = c(1, 2, -9, 3, 2, 1, 4, 2),
                   s = rep(c("cold", "warm"), each = 4))
  a <- pv_horizon(df, "t", "v", series = "s")$x$alt
  # The deepest band is picked by magnitude and reported signed.
  expect_match(a, "cold reaches the deepest band, peaking at -9.",
               fixed = TRUE)
  # A category axis spans its first and last level, in data order.
  cats <- data.frame(m = rep(c("Jan", "Feb", "Mar"), 2), v = 1:6,
                     s = rep(c("a", "b"), each = 3))
  expect_match(pv_horizon(cats, "m", "v", series = "s")$x$alt,
               "The m axis runs from Jan to Mar.", fixed = TRUE)
})

test_that("a flow map's alt text counts flows and places and names the largest flow", {
  latest <- subset(pv_commuters,
                   period == "2022-2024" & region != "Restliche Schweiz")
  flows <- data.frame(
    from = ifelse(latest$direction == "to Zug", latest$region, "Zug"),
    to = ifelse(latest$direction == "to Zug", "Zug", latest$region),
    commuters = latest$commuters)
  w <- pv_flow_map(flows, from = "from", to = "to", value = "commuters",
                   title = "Commuting with Zug")
  a <- w$x$alt
  expect_match(a, "^A flow map titled \u201cCommuting with Zug\u201d")
  expect_match(a, sprintf("tracing %d directed flows of commuters",
                          nrow(flows)), fixed = TRUE)
  expect_match(a, sprintf("among %d places",
                          length(unique(c(flows$from, flows$to)))),
               fixed = TRUE)
  top <- flows[which.max(flows$commuters), ]
  expect_match(a, sprintf("The largest flow runs from %s to %s, at %s.",
                          top$from, top$to,
                          format(round(top$commuters), big.mark = ",")),
               fixed = TRUE)
  expect_identical(pv_alt_text(w), a)
  # A single flow keeps its grammar singular.
  one <- data.frame(from = "Luzern", to = "Zug", n = 120)
  expect_match(pv_flow_map(one, from = "from", to = "to", value = "n")$x$alt,
               "tracing 1 directed flow of n among 2 places (Luzern and Zug).",
               fixed = TRUE)
})

# The canonical-loop hygiene checks, run over the newer chart types
# directly so their describers stay covered whatever the render helpers
# currently enumerate.
test_that("the new chart types carry clean, period-terminated alt text", {
  builders <- list(
    slope = function() {
      pop <- pv_city_population[
        pv_city_population$city %in% c("Luzern", "Emmen", "Zug") &
          pv_city_population$year %in% c(1930, 2024), ]
      pv_slope(pop, x = "year", y = "population", group = "city")
    },
    dumbbell = function() {
      both <- merge(
        pv_fiscal[pv_fiscal$year == 2020,
                  c("municipality", "resource_index")],
        pv_fiscal[pv_fiscal$year == 2027,
                  c("municipality", "resource_index")],
        by = "municipality", suffixes = c("_2020", "_2027"))
      pv_dumbbell(head(both, 12), y = "municipality",
                  x1 = "resource_index_2020", x2 = "resource_index_2027")
    },
    waterfall = function() {
      lu <- pv_city_population[pv_city_population$city == "Luzern", ]
      lu <- lu[order(lu$year), ]
      steps <- data.frame(
        period = paste(head(lu$year, -1), lu$year[-1], sep = "\u2013"),
        change = diff(lu$population))
      pv_waterfall(steps, x = "period", y = "change",
                   start = lu$population[1])
    },
    bullet = function() {
      rev <- aggregate(revenue ~ region, pv_sales, sum)
      rev$target <- round(1.08 * mean(rev$revenue), -4)
      pv_bullet(rev, label = "region", value = "revenue", target = "target")
    },
    waffle = function() {
      seats <- aggregate(elected ~ party,
                         pv_elections[pv_elections$year == 2024, ], sum)
      seats <- seats[order(-seats$elected), ]
      seats$party[-(1:7)] <- "Other"
      seats <- aggregate(elected ~ party, seats, sum)
      pv_waffle(seats, category = "party", value = "elected")
    },
    icicle = function() {
      pv_icicle(pv_city_landuse[pv_city_landuse$city == "Luzern", ],
                levels = c("group", "category"), value = "hectares")
    },
    horizon = function() {
      nights <- aggregate(nights ~ canton + year, pv_tourism, sum)
      pv_horizon(nights, x = "year", y = "nights", series = "canton")
    },
    flowmap = function() {
      latest <- subset(pv_commuters,
                       period == "2022-2024" &
                         region != "Restliche Schweiz")
      flows <- data.frame(
        from = ifelse(latest$direction == "to Zug", latest$region, "Zug"),
        to = ifelse(latest$direction == "to Zug", "Zug", latest$region),
        commuters = latest$commuters)
      pv_flow_map(flows, from = "from", to = "to", value = "commuters")
    })
  for (id in names(builders)) {
    w <- builders[[id]]()
    a <- w$x$alt
    expect_true(is.character(a) && length(a) == 1 && !is.na(a) && nzchar(a),
                label = sprintf("%s alt text is a usable string", id))
    expect_true(grepl("\\.$", a),
                label = sprintf("%s alt text ends with a period", id))
    expect_false(grepl("\\bNA\\b|\\bNULL\\b|\\bNaN\\b|\\(no value\\)", a),
                 label = sprintf("%s alt text leaks a missing value", id))
    expect_false(grepl("  ", a, fixed = TRUE),
                 label = sprintf("%s alt text has doubled spaces", id))
    expect_identical(pv_alt_text(w), a,
                     label = sprintf("%s generator agrees", id))
  }
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

test_that("pv_alt_text rebuilds the decompose description; plain facets keep theirs", {
  monthly <- aggregate(gwh ~ date, pv_electricity, sum)
  w <- pv_decompose(monthly, x = "date", y = "gwh", title = "Taken apart")
  expect_match(w$x$alt,
               "^A seasonal decomposition titled \u201cTaken apart\u201d")
  expect_match(w$x$alt, "(stl, frequency 12)", fixed = TRUE)
  # the generator rebuilds the same facts from the panels the payload
  # carries - it no longer falls back to "an interactive facet chart"
  expect_identical(pv_alt_text(w), w$x$alt)
  wc <- pv_decompose(monthly, x = "date", y = "gwh", method = "classical")
  expect_identical(pv_alt_text(wc), wc$x$alt)
  # after pv_alt() the generator still speaks for the data underneath
  own <- pv_alt(w, "My own words.")
  expect_match(pv_alt_text(own), "^A seasonal decomposition")
  # a plain facet keeps the honest generic sentence it always had
  pop <- subset(pv_city_population, city %in% c("Luzern", "Zug"))
  f <- pv_facet(pv_line(pop, x = "year", y = "population", series = "city"),
                pop$city)
  expect_identical(pv_alt_text(f), "An interactive facet chart.")
})

test_that("every panel's own alt text survives into a saved board page", {
  # A board is composed htmltools, not one widget, so it carries no alt
  # text of its own - a screen reader gets the board's heading markup and
  # then each panel as its own described image. What must survive the
  # save is each panel's generated description, riding in the payload
  # the renderer reads (role="img" plus aria-label at draw time).
  dir <- withr::local_tempdir()
  agg <- aggregate(revenue ~ region, pv_sales, sum)
  monthly <- aggregate(revenue ~ month, pv_sales, sum)
  bar <- pv_bar(agg, "region", "revenue", title = "Revenue by region")
  line <- pv_line(monthly, "month", "revenue", title = "Revenue over time")
  b <- pv_board(bar, line, title = "Sales at a glance")
  f <- file.path(dir, "board.html")
  pv_save(b, f, quiet = TRUE)
  html <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"),
                collapse = "\n")
  # both panels' alt strings sit in the saved page, ready for the
  # renderer to hand to assistive tech
  expect_match(html, substr(bar$x$alt, 1, 60), fixed = TRUE)
  expect_match(html, substr(line$x$alt, 1, 60), fixed = TRUE)
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
