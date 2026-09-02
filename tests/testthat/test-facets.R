# Same helper as test-widgets.R — each test file stands alone.
expect_pvchart <- function(w, type) {
  expect_s3_class(w, "htmlwidget")
  expect_equal(attr(w, "package"), "polyviz")
  expect_equal(w$x$type, type)
  invisible(w)
}

# Four cities, already grouped by city with years ascending inside each,
# so the line constructor's internal ordering is the identity and the
# facet vector stays row-aligned.
facet_pop <- function() {
  subset(pv_city_population,
         city %in% c("Luzern", "Emmen", "Kriens", "Zug"))
}

test_that("facet rewrites the payload into panels and keeps every row", {
  pop <- facet_pop()
  w <- pv_line(pop, "year", "population", series = "city")
  f <- expect_pvchart(pv_facet(w, pop$city), "facet")
  expect_equal(f$x$subtype, "line")
  expect_null(f$x$data)
  expect_length(f$x$panels, 4)
  expect_setequal(vapply(f$x$panels, `[[`, character(1), "name"),
                  unique(pop$city))
  expect_equal(sum(vapply(f$x$panels, function(p) nrow(p$data),
                          integer(1))),
               nrow(pop))
  # Each panel holds exactly its own city's rows.
  luzern <- Filter(function(p) p$name == "Luzern", f$x$panels)[[1]]
  expect_true(all(luzern$data$series == "Luzern"))
  expect_equal(nrow(luzern$data), sum(pop$city == "Luzern"))
  # Everything that isn't data stays once at the top level.
  expect_equal(f$x$xtype, "number")
  expect_equal(f$x$xlab, "year")
  expect_equal(f$x$ylab, "population")
})

test_that("panels keep the facet variable's first-appearance order", {
  mt <- mtcars
  by <- rep(c("zeta", "alpha", "mid"), length.out = nrow(mt))
  f <- pv_scatter(mt, "wt", "mpg") |> pv_facet(by)
  expect_equal(vapply(f$x$panels, `[[`, character(1), "name"),
               c("zeta", "alpha", "mid"))
})

test_that("shared numeric domains span all panels; opting out drops them", {
  pop <- facet_pop()
  f <- pv_line(pop, "year", "population", series = "city") |>
    pv_facet(pop$city)
  # Numeric limits arrive rounded outward to what d3 nice() would pick:
  # the renderers apply them verbatim. Years 1930-2024, peak 86 234.
  expect_equal(f$x$xlim, c(1930, 2030))
  # Lines anchor their y axis at zero, so the shared range does too.
  expect_equal(f$x$ylim, c(0, 90000))
  # Scatter fits both ends of y to the data instead.
  s <- pv_scatter(mtcars, "wt", "mpg") |>
    pv_facet(rep(c("a", "b"), 16))
  expect_equal(s$x$xlim, c(1.5, 5.5))
  expect_equal(s$x$ylim, c(10, 34))
  off <- pv_line(pop, "year", "population", series = "city") |>
    pv_facet(pop$city, share_x = FALSE, share_y = FALSE)
  expect_null(off$x$xlim)
  expect_null(off$x$ylim)
})

test_that("date and category x axes share their domains properly", {
  dated <- data.frame(
    d = rep(as.Date(c("2024-03-01", "2023-01-15", "2024-06-30")), 2),
    v = 1:6, g = rep(c("p1", "p2"), each = 3))
  f <- pv_line(dated, "d", "v", series = "g") |> pv_facet(dated$g)
  # A true date range, not a string sort, sent back as ISO strings.
  expect_equal(f$x$xlim, c("2023-01-15", "2024-06-30"))
  # Category union in first-appearance order across the panels. The
  # categories overlap across panels, so - like a facetable area - the
  # bars need a series that keeps one row per series/category pair;
  # pv_bar refuses duplicate pairs.
  bars <- data.frame(cat = c("west", "east", "east", "north"),
                     n = c(4, 2, 5, 3), s = c("s1", "s1", "s2", "s2"))
  fb <- pv_bar(bars, "cat", "n", series = "s") |>
    pv_facet(c("a", "a", "b", "b"))
  expect_equal(as.character(fb$x$xlim), c("west", "east", "north"))
  expect_equal(fb$x$ylim, c(0, 5))
})

test_that("stacked areas share the tallest stack, normalised areas skip y", {
  # Series are unique to their panel - pv_area refuses duplicate
  # series/x pairs, so a facetable area always nests its series inside
  # the facet variable.
  df <- data.frame(t = rep(1:2, 4), v = c(1, 2, 3, 4, 10, 12, 14, 16),
                   s = rep(c("a1", "a2", "b1", "b2"), each = 2),
                   g = rep(c("small", "big"), each = 4))
  f <- pv_area(df, "t", "v", series = "s") |> pv_facet(df$g)
  # Tallest stack: the "big" panel at t = 2 sums 12 + 16 = 28.
  expect_equal(f$x$ylim, c(0, 28))
  pct <- pv_area(df, "t", "v", series = "s", offset = "percent") |>
    pv_facet(df$g)
  expect_null(pct$x$ylim)
  expect_equal(pct$x$xlim, c(1, 2))
})

test_that("faceted stacked bars share the tallest bar; percent bars skip y", {
  # A facetable stacked bar keeps every series/category pair unique by
  # nesting the categories inside the panels - municipalities inside
  # cantons here - exactly like a facetable area nests its series.
  df <- data.frame(
    muni = rep(c("m1", "m2", "m3", "m4"), each = 2),
    seg = rep(c("s1", "s2"), 4),
    v = c(1, 2, 3, 4, 5, 6, 7, 8),
    canton = rep(c("A", "B"), each = 4))
  f <- pv_bar(df, "muni", "v", series = "seg", stack = "stack") |>
    pv_facet(df$canton)
  expect_equal(f$x$subtype, "bar")
  expect_length(f$x$panels, 2)
  # The shared ceiling is the tallest category TOTAL (m4: 7 + 8 = 15),
  # not the largest single segment (8), rounded outward like any other
  # shared limit.
  expect_equal(f$x$ylim, facet_nice(c(0, 15)))
  expect_gte(f$x$ylim[2], 15)
  # Percent-stacked panels normalise themselves - no shared range.
  p <- pv_bar(df, "muni", "v", series = "seg", stack = "percent") |>
    pv_facet(df$canton)
  expect_null(p$x$ylim)
  # Side-by-side bars keep the single-value ceiling they always had.
  g <- pv_bar(df, "muni", "v", series = "seg") |> pv_facet(df$canton)
  expect_equal(g$x$ylim, facet_nice(c(0, 8)))
})

test_that("faceting appends an honest panel sentence to the alt text", {
  pop <- facet_pop()
  w <- pv_line(pop, "year", "population", series = "city")
  base_alt <- w$x$alt
  # The pre-facet description survives, with one new sentence after it,
  # and the panel variable's name is read off the `by` argument.
  f <- pv_facet(w, pop$city)
  expect_equal(f$x$alt,
               paste(base_alt, "Shown as 4 small-multiple panels by city."))
  # A [["name"]] lookup and a bare vector name are legible too.
  expect_match(pv_facet(w, pop[["city"]])$x$alt,
               "Shown as 4 small-multiple panels by city\\.$")
  panels <- pop$city
  expect_match(pv_facet(w, panels)$x$alt,
               "Shown as 4 small-multiple panels by panels\\.$")
  # An anonymous expression keeps just the count.
  f3 <- pv_scatter(mtcars, "wt", "mpg") |> pv_facet(rep(c("a", "b"), 16))
  expect_match(f3$x$alt, "Shown as 2 small-multiple panels\\.$")
  # An author's own alt text is kept and extended the same way.
  own <- pv_alt(w, "Four cities, drawn one per panel.")
  expect_equal(
    pv_facet(own, pop$city)$x$alt,
    "Four cities, drawn one per panel. Shown as 4 small-multiple panels by city.")
})

test_that("negative values pull the shared y floor below zero", {
  df <- data.frame(cat = c("a", "b", "c", "d"), n = c(3, -2, 7, 1),
                   g = rep(c("p1", "p2"), each = 2))
  f <- pv_bar(df, "cat", "n") |> pv_facet(df$g)
  expect_equal(f$x$ylim, c(-2, 7))
})

test_that("the facet vector must line up with the data rows", {
  pop <- facet_pop()
  w <- pv_line(pop, "year", "population", series = "city")
  expect_error(pv_facet(w, head(pop$city, 3)), "data rows")
  expect_error(pv_facet(w, data.frame(a = pop$city)), "vector")
  na_by <- pop$city
  na_by[5] <- NA
  expect_error(pv_facet(w, na_by), "missing values")
})

test_that("unsupported chart types abort helpfully", {
  expect_error(pv_facet(pv_chord(pv_flows), c("a", "b")),
               "bar, line, scatter, area")
  expect_error(pv_facet(data.frame(x = 1), c("a", "b")), "polyviz chart")
  pop <- facet_pop()
  f <- pv_line(pop, "year", "population", series = "city") |>
    pv_facet(pop$city)
  expect_error(pv_facet(f, pop$city), "already faceted")
})

test_that("panel counts outside 2..16 abort", {
  mt <- mtcars
  w <- pv_scatter(mt, "wt", "mpg")
  expect_error(pv_facet(w, rep("only", nrow(mt))), "at least 2")
  expect_error(pv_facet(w, seq_len(nrow(mt))), "16")
})

test_that("ncol and the share flags are validated", {
  pop <- facet_pop()
  w <- pv_line(pop, "year", "population", series = "city")
  f <- pv_facet(w, pop$city, ncol = 2)
  expect_identical(f$x$ncol, 2L)
  expect_null(pv_facet(w, pop$city)$x$ncol)
  expect_error(pv_facet(w, pop$city, ncol = 0), "whole number")
  expect_error(pv_facet(w, pop$city, ncol = 2.5), "whole number")
  expect_error(pv_facet(w, pop$city, share_y = "auto"), "TRUE or FALSE")
  expect_error(pv_facet(w, pop$city, share_x = NA), "TRUE or FALSE")
})
