# pv_table(): payload construction, validation, the export guards, and
# (behind the usual skip) the real headless-Chrome render contract with
# its client-side sorting and paging.

expect_pvchart <- function(w, type) {
  expect_s3_class(w, "htmlwidget")
  expect_equal(attr(w, "package"), "polyviz")
  expect_equal(w$x$type, type)
  invisible(w)
}

# A small fiscal slice with one column per role: text, shaded number,
# plain number, barred number, and a sparkline list-column.
table_fixture <- function() {
  pop <- pv_city_population[order(pv_city_population$city,
                                  pv_city_population$year), ]
  trend <- split(pop$population, pop$city)
  f25 <- pv_fiscal[pv_fiscal$year == 2025, ]
  f25 <- f25[f25$municipality %in% names(trend), ]
  f25 <- f25[order(-f25$resource_per_capita), ]
  tbl <- f25[, c("municipality", "resource_index", "resource_per_capita",
                 "equalization_chf")]
  tbl$trend <- I(trend[tbl$municipality])
  rownames(tbl) <- NULL
  tbl
}

test_that("table packs column specs, rows, and chrome options", {
  tbl <- table_fixture()
  w <- expect_pvchart(
    pv_table(tbl, bars = "equalization_chf", shade = "resource_index",
             spark = "trend", title = "Fiscal table"), "table")
  specs <- w$x$columns
  expect_equal(vapply(specs, `[[`, character(1), "key"), names(tbl))
  expect_equal(vapply(specs, `[[`, character(1), "label"), names(tbl))
  expect_equal(vapply(specs, `[[`, character(1), "type"),
               c("text", "number", "number", "number", "spark"))
  expect_equal(nrow(w$x$data), nrow(tbl))
  expect_equal(w$x$data$municipality, tbl$municipality)
  # bars carry the column maximum, shade its range
  bar <- specs[[4]]
  expect_true(bar$bar)
  expect_equal(bar$barMax, max(tbl$equalization_chf))
  sh <- specs[[2]]
  expect_true(sh$shade)
  expect_equal(sh$shadeMin, min(tbl$resource_index))
  expect_equal(sh$shadeMax, max(tbl$resource_index))
  # a table never animates, never grows the SVG download control
  expect_identical(w$x$duration, 0)
  expect_false(w$x$downloads)
  expect_true(w$x$sortable)
  expect_null(w$x$pageSize)
})

test_that("columns selects and orders; bad selections error clearly", {
  tbl <- table_fixture()
  w <- pv_table(tbl, columns = c("resource_index", "municipality"))
  expect_equal(vapply(w$x$columns, `[[`, character(1), "key"),
               c("resource_index", "municipality"))
  expect_equal(names(w$x$data), c("resource_index", "municipality"))
  expect_error(pv_table(tbl, columns = "nope"), "not in `data`")
  expect_error(pv_table(tbl, columns = c("municipality", "municipality")),
               "repeats")
  expect_error(pv_table("not a frame"), "must be a data frame")
})

test_that("zero rows and oversize tables abort", {
  tbl <- table_fixture()
  expect_error(pv_table(tbl[0, ]), "no rows")
  big <- data.frame(x = seq_len(5001))
  expect_error(pv_table(big), "Aggregate")
  expect_error(pv_table(big), "5,000")
  # exactly at the cap still builds
  expect_pvchart(pv_table(data.frame(x = seq_len(5000))), "table")
})

test_that("automatic digits: none for whole numbers, up to two otherwise", {
  df <- data.frame(year = c(2020, 2021), whole = c(10, 20),
                   tenth = c(1.5, 2.5), fine = c(1.234, 5.678),
                   big = c(20000, 30000))
  w <- pv_table(df)
  specs <- w$x$columns
  names(specs) <- vapply(specs, `[[`, character(1), "key")
  expect_equal(specs$year$digits, 0L)
  expect_equal(specs$tenth$digits, 1L)
  expect_equal(specs$fine$digits, 2L)
  expect_false(specs$year$fixed)
  # the package-wide tick rule: whole numbers under 10'000 print
  # ungrouped, so years stay "2020" - the payload flags such columns
  expect_true(specs$year$small)
  expect_true(specs$whole$small)
  expect_false(specs$tenth$small)
  expect_false(specs$big$small)
})

test_that("digits: one count for all, or a named per-column vector", {
  df <- data.frame(name = c("a", "b"), v1 = c(1.5, 2), v2 = c(3, 4))
  w <- pv_table(df, digits = 1)
  specs <- w$x$columns
  expect_equal(specs[[2]]$digits, 1L)
  expect_equal(specs[[3]]$digits, 1L)
  expect_true(specs[[2]]$fixed)
  w2 <- pv_table(df, digits = c(v2 = 3))
  specs2 <- w2$x$columns
  expect_true(specs2[[3]]$fixed)
  expect_equal(specs2[[3]]$digits, 3L)
  # v1 keeps automatic formatting
  expect_false(specs2[[2]]$fixed)
  expect_error(pv_table(df, digits = -1), "between 0 and 12")
  expect_error(pv_table(df, digits = 1.5), "between 0 and 12")
  expect_error(pv_table(df, digits = c(1, 2)), "named vector")
  expect_error(pv_table(df, digits = c(name = 1)), "not numeric columns")
  expect_error(pv_table(df, digits = c(nope = 1)), "not numeric columns")
  expect_error(pv_table(data.frame(name = "a"), digits = 2),
               "no numeric columns")
})

test_that("bars and shade must name shown, numeric, distinct columns", {
  tbl <- table_fixture()
  expect_error(pv_table(tbl, bars = "municipality"), "not numeric")
  expect_error(pv_table(tbl, shade = "municipality"), "not numeric")
  expect_error(pv_table(tbl, bars = "trend"), "list-column")
  expect_error(pv_table(tbl, bars = "nope"), "not in `data`")
  expect_error(
    pv_table(tbl, columns = c("municipality", "resource_index"),
             bars = "equalization_chf"),
    "leaves out")
  expect_error(
    pv_table(tbl, bars = "resource_index", shade = "resource_index"),
    "one encoding")
  # an all-missing numeric column still builds - it just has no domain
  df <- data.frame(a = c("x", "y"), b = c(NA_real_, NA_real_))
  w <- pv_table(df, bars = "b")
  expect_equal(w$x$columns[[2]]$barMax, 0)
})

test_that("spark columns must be list-columns of numeric vectors", {
  tbl <- table_fixture()
  w <- pv_table(tbl, spark = "trend")
  expect_equal(w$x$columns[[5]]$type, "spark")
  # the vectors ride along as arrays, one per row
  expect_identical(unname(unclass(w$x$data$trend)),
                   unname(unclass(tbl$trend)))
  expect_error(pv_table(tbl, spark = "municipality"), "list-column")
  bad <- tbl
  bad$trend <- I(lapply(seq_len(nrow(bad)), function(i) {
    if (i == 3) "words" else c(1, 2)
  }))
  expect_error(pv_table(bad, spark = "trend"), "row 3 holds a character")
  # NULL cells are allowed - an empty sparkline, not an error
  sparse <- tbl
  sparse$trend <- I(lapply(seq_len(nrow(sparse)), function(i) {
    if (i == 1) NULL else c(1, 2, 3)
  }))
  expect_pvchart(pv_table(sparse, spark = "trend"), "table")
  # a list-column not named in `spark` has no honest rendering
  expect_error(pv_table(tbl), "name it in `spark`")
  expect_error(pv_table(tbl, spark = "trend", bars = "trend"),
               "cannot also carry")
})

test_that("sortable and page_size validate; page size reaches the payload", {
  tbl <- table_fixture()
  expect_error(pv_table(tbl, spark = "trend", sortable = "auto"),
               "TRUE or FALSE")
  expect_error(pv_table(tbl, spark = "trend", page_size = 0),
               "whole number")
  expect_error(pv_table(tbl, spark = "trend", page_size = 2.5),
               "whole number")
  w <- pv_table(tbl, spark = "trend", page_size = 3, sortable = FALSE)
  expect_identical(w$x$pageSize, 3L)
  expect_false(w$x$sortable)
})

test_that("dates print as ISO text and factors as their labels", {
  df <- data.frame(when = as.Date(c("2024-01-31", "2024-02-29")),
                   who = factor(c("a", "b")), v = c(1, 2))
  w <- pv_table(df)
  expect_identical(w$x$data$when, c("2024-01-31", "2024-02-29"))
  expect_identical(w$x$data$who, c("a", "b"))
  expect_equal(vapply(w$x$columns, `[[`, character(1), "type"),
               c("text", "text", "number"))
})

test_that("a table carries generated alt text and accepts pv_alt", {
  tbl <- table_fixture()
  w <- pv_table(tbl, spark = "trend", title = "Fiscal table")
  expect_match(pv_alt_text(w), "table")
  expect_match(pv_alt_text(w), "Fiscal table")
  w2 <- pv_alt(w, "My own words.")
  expect_identical(w2$x$alt, "My own words.")
})

test_that("the active locale rides along in the payload", {
  pv_locale("de-CH")
  on.exit(pv_locale(NULL))
  w <- pv_table(data.frame(a = c(10000, 20000)))
  expect_identical(w$x$locale$tag, "de-CH")
})

test_that("pv_save refuses .svg and .gif for tables, without a browser", {
  w <- pv_table(data.frame(a = 1:3))
  f <- function(ext) file.path(tempdir(), paste0("t", ext))
  expect_error(pv_save(w, f(".svg"), quiet = TRUE),
               "cannot hold an HTML table")
  expect_error(pv_save(w, f(".gif"), quiet = TRUE),
               "cannot hold an HTML table")
})

test_that("html export of a table is self-contained and needs no browser", {
  w <- pv_table(data.frame(city = c("A", "B"), pop = c(1, 2)),
                title = "Tiny table")
  f <- file.path(tempdir(), "table.html")
  on.exit(unlink(f))
  pv_save(w, f, quiet = TRUE)
  html <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"),
                collapse = "\n")
  expect_match(html, "\"type\":\"table\"", fixed = TRUE)
  expect_match(html, "pvRenderers.table", fixed = TRUE)
})

# ---- headless render contract --------------------------------------------

test_that("the table renders without JavaScript errors, light and dark", {
  render_skip_if_no_chrome()
  tbl <- table_fixture()
  w <- pv_table(tbl, bars = "equalization_chf", shade = "resource_index",
                spark = "trend", digits = c(resource_index = 1),
                title = "Fiscal equalization", source = "Source: LUSTAT")
  for (mode in c("light", "dark")) {
    path <- file.path(render_out_dir(), paste0("table-", mode, ".png"))
    expect_no_warning(pv_save(w, path, mode = mode, quiet = TRUE))
    expect_true(file.exists(path))
    expect_gt(file.size(path), 20000)
    render_publish(path)
  }
})

test_that("a table saves as a vector PDF through the shared serialiser", {
  render_skip_if_no_chrome()
  tbl <- table_fixture()
  w <- pv_table(tbl, bars = "equalization_chf", spark = "trend",
                title = "Fiscal equalization")
  path <- file.path(render_out_dir(), "table.pdf")
  expect_no_warning(pv_save(w, path, quiet = TRUE))
  expect_true(file.exists(path))
  bytes <- readBin(path, "raw", 5)
  expect_identical(rawToChar(bytes), "%PDF-")
})

test_that("header clicks sort and the pager pages, in a real browser", {
  render_skip_if_no_chrome()
  tbl <- table_fixture()
  tbl$trend <- NULL
  w <- pv_table(tbl, bars = "equalization_chf", page_size = 4,
                title = "Sortable table")
  page <- file.path(render_out_dir(), "table-sort.html")
  pv_save(w, page, quiet = TRUE)

  b <- chromote::ChromoteSession$new(width = 900L, height = 480L)
  on.exit(try(b$close(), silent = TRUE), add = TRUE)
  loaded <- b$Page$loadEventFired(wait_ = FALSE)
  b$Page$navigate(paste0("file://", normalizePath(page)), wait_ = FALSE)
  b$wait_for(loaded)

  eval_js <- function(js) {
    b$Runtime$evaluate(js, returnByValue = TRUE)$result$value
  }
  # The renderers draw synchronously once the scripts arrive; poll until
  # the table exists rather than sleeping a fixed (flaky) amount.
  for (i in 1:50) {
    if (isTRUE(eval_js(
      "!!document.querySelector('.pv-table tbody tr td')"))) break
    Sys.sleep(0.1)
  }
  first_cell <-
    "document.querySelector('.pv-table tbody tr td').textContent"
  expect_identical(eval_js(first_cell), tbl$municipality[[1]])

  click_th <- paste0(
    "(function () {",
    " var ths = document.querySelectorAll('.pv-table th');",
    " for (var i = 0; i < ths.length; i++) {",
    "  if (ths[i].textContent.indexOf('equalization_chf') === 0) {",
    "   ths[i].click(); return true; } }",
    " return false; })()")

  # First click on a numeric header sorts largest-first
  expect_true(isTRUE(eval_js(click_th)))
  biggest <- tbl$municipality[which.max(tbl$equalization_chf)]
  expect_identical(eval_js(first_cell), biggest)
  # Second click flips to ascending: the zero rows come first, in their
  # original (stable) order
  expect_true(isTRUE(eval_js(click_th)))
  smallest <- tbl$municipality[which.min(tbl$equalization_chf)]
  expect_identical(eval_js(first_cell), smallest)

  # Paging: four rows a page, and the pager range follows the clicks
  n <- nrow(tbl)
  expect_equal(eval_js(
    "document.querySelectorAll('.pv-table tbody tr').length"), 4)
  range_js <-
    "document.querySelector('.pv-table-pager span').textContent"
  expect_match(eval_js(range_js), paste0("^1.4 of ", n, " rows$"))
  eval_js(paste0(
    "document.querySelectorAll('.pv-table-pager button')[1].click()"))
  expect_match(eval_js(range_js),
               paste0("^5.", min(n, 8), " of ", n, " rows$"))
})
