# pv_board() composes finished widgets into one page, so most tests work
# at the HTML level: build a board, render it to text, and look for the
# layout classes and the serialised widget payloads. The interaction and
# capture tests drive a real headless Chrome, exactly like the render
# suite, and skip cleanly wherever the browser stack is missing.

board_bar <- function(...) {
  agg <- aggregate(revenue ~ region, pv_sales, sum)
  pv_bar(agg, "region", "revenue", title = "Revenue by region", ...)
}

board_line <- function(...) {
  monthly <- aggregate(revenue ~ month, pv_sales, sum)
  pv_line(monthly, "month", "revenue", title = "Revenue over time", ...)
}

# Pixel size straight from the PNG header, as in test-export.R (test
# files do not share their local helpers).
board_png_size <- function(path) {
  bytes <- readBin(path, "raw", n = 24)
  c(width = sum(as.integer(bytes[17:20]) * 256^(3:0)),
    height = sum(as.integer(bytes[21:24]) * 256^(3:0)))
}

test_that("pv_board validates its arguments", {
  w <- board_bar()
  expect_error(pv_board(), "at least one chart")
  expect_error(pv_board(data.frame()), "Panel 1")
  expect_error(pv_board(w, 1), "Panel 2")
  expect_error(pv_board(list(w, "x")), "Panel 2")
  expect_error(pv_board(w, ncol = 0), "`ncol`")
  expect_error(pv_board(w, ncol = 1.5), "`ncol`")
  expect_error(pv_board(w, ncol = c(1, 2)), "`ncol`")
  expect_error(pv_board(w, title = 1), "`title`")
  expect_error(pv_board(w, subtitle = "sub"), "needs a `title`")
  expect_error(pv_board(w, mode = "sepia"), '"auto", "light", or "dark"')
  expect_error(pv_board(w, heights = -1), "`heights`")
  expect_error(pv_board(w, heights = "tall"), "`heights`")
  expect_error(pv_board(w, heights = numeric(0)), "`heights`")
  expect_error(pv_board(w, heights = c(400, NA)), "`heights`")
})

test_that("a board is a browsable tag laying panels into a grid", {
  b <- pv_board(bar = board_bar(), line = board_line(),
                title = "Sales at a glance", subtitle = "Two views",
                source = "Simulated pv_sales data")
  expect_s3_class(b, "pv_board")
  expect_s3_class(b, "shiny.tag")
  expect_true(htmltools::is.browsable(b))
  html <- as.character(b)
  expect_match(html, 'class="pv-board"', fixed = TRUE)
  expect_match(html, "grid-template-columns: repeat(2, minmax(0, 1fr))",
               fixed = TRUE)
  # both panels, each captioned with its argument name
  expect_length(
    regmatches(html, gregexpr('class="pv-board-panel"', html))[[1]], 2)
  expect_match(html, '<div class="pv-board-caption">bar</div>',
               fixed = TRUE)
  expect_match(html, '<div class="pv-board-caption">line</div>',
               fixed = TRUE)
  # the heading block and the board's own source line
  expect_match(html, ">Sales at a glance<")
  expect_match(html, ">Two views<")
  expect_match(html, ">Simulated pv_sales data<")
  # the real widgets ride inside, full-width in their cells
  expect_match(html, '"type":"bar"', fixed = TRUE)
  expect_match(html, '"type":"line"', fixed = TRUE)
  expect_length(regmatches(html, gregexpr("width:100%", html))[[1]], 2)
  # the narrow-screen collapse to one column is part of the stylesheet
  expect_match(html, "max-width: 720px", fixed = TRUE)
})

test_that("a single list of charts works and ncol shapes the grid", {
  b <- pv_board(list(board_bar(), board_line(), board_bar()), ncol = 3)
  html <- as.character(b)
  expect_match(html, "repeat(3, minmax(0, 1fr))", fixed = TRUE)
  expect_length(
    regmatches(html, gregexpr('class="pv-board-panel"', html))[[1]], 3)
  # unnamed panels carry no captions, and the optional blocks are absent
  # (the class names always sit in the stylesheet, so look for elements)
  expect_false(grepl('class="pv-board-caption"', html, fixed = TRUE))
  expect_false(grepl('class="pv-board-header"', html, fixed = TRUE))
  expect_false(grepl('class="pv-board-source"', html, fixed = TRUE))
})

test_that("mode auto swaps surfaces by media query; a forced mode pins all", {
  auto <- as.character(pv_board(board_bar(), board_line()))
  expect_match(auto, "prefers-color-scheme: dark", fixed = TRUE)
  # the light surface is the default set on the container
  expect_match(auto, paste0(".pv-board { --surface:",
                            pv_colors$ink$light$surface), fixed = TRUE)
  # auto leaves each chart's own mode choice alone
  expect_match(auto, '"mode":"auto"', fixed = TRUE)

  dark <- as.character(pv_board(board_bar(), board_line(), mode = "dark"))
  expect_match(dark, paste0(".pv-board { --surface:",
                            pv_colors$ink$dark$surface), fixed = TRUE)
  expect_false(grepl("prefers-color-scheme", dark, fixed = TRUE))
  # a forced board mode pins every panel with it
  expect_length(regmatches(dark, gregexpr('"mode":"dark"', dark))[[1]], 2)
  expect_false(grepl('"mode":"auto"', dark, fixed = TRUE))
})

test_that("heights apply per grid row and recycle", {
  four <- list(board_bar(), board_line(), board_bar(), board_line())
  html <- as.character(pv_board(four, heights = c(300, 200)))
  expect_length(regmatches(html, gregexpr("height:300px", html))[[1]], 2)
  expect_length(regmatches(html, gregexpr("height:200px", html))[[1]], 2)
  # the first row's panels come before the second row's
  expect_lt(regexpr("height:300px", html), regexpr("height:200px", html))

  # a single number recycles to every row
  one <- as.character(pv_board(four, heights = 250))
  expect_length(regmatches(one, gregexpr("height:250px", one))[[1]], 4)

  # NULL keeps each widget's own height - a fixed build height where one
  # was given, the standard 420 otherwise
  own <- as.character(pv_board(board_bar(height = 380), board_line()))
  expect_match(own, "height:380px", fixed = TRUE)
  expect_match(own, "height:420px", fixed = TRUE)
})

test_that("printing a board writes the viewer page quietly", {
  b <- pv_board(board_bar(), mode = "dark")
  # browse = FALSE renders the page to a temp file without opening a
  # viewer and without printing anything - the htmlwidgets behaviour.
  # The method is called directly because bare print() only reaches it
  # through the NAMESPACE registration roxygen generates; until then a
  # board still shows in the viewer through htmltools' own browsable
  # printing.
  expect_output(polyviz:::print.pv_board(b, browse = FALSE), NA)
  expect_invisible(polyviz:::print.pv_board(b, browse = FALSE))
})

test_that("linked panels carry the crosstalk machinery exactly once", {
  pts <- data.frame(x = c(0, 10, 5, 2, 8), y = c(0, 10, 5, 8, 2))
  sd <- crosstalk::SharedData$new(pts, key = c("a", "b", "c", "d", "e"))
  b <- pv_board(pv_scatter(pts, x = "x", y = "y") |> pv_link(sd),
                pv_scatter(pts, x = "x", y = "y") |> pv_link(sd))
  rendered <- htmltools::renderTags(b)
  deps <- htmltools::resolveDependencies(rendered$dependencies)
  nm <- vapply(deps, function(d) d$name, character(1))
  expect_identical(sum(nm == "crosstalk"), 1L)
  # both panels point at the same selection group
  expect_length(regmatches(
    rendered$html, gregexpr(sd$groupName(), rendered$html))[[1]], 2)
})

test_that("pv_save writes a self-contained board html with no browser", {
  dir <- withr::local_tempdir()
  b <- pv_board(board_bar(), board_line(), title = "Sales at a glance")
  f <- file.path(dir, "board.html")
  expect_message(pv_save(b, f), "self-contained board html")
  # one file, no lib/ folder, and no leftover temp file from the write
  expect_identical(list.files(dir, all.files = TRUE, no.. = TRUE),
                   "board.html")
  html <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"),
                collapse = "\n")
  expect_match(html, 'class="pv-board"', fixed = TRUE)
  expect_match(html, "<title>Sales at a glance</title>", fixed = TRUE)
  expect_match(html, '"type":"bar"', fixed = TRUE)
  expect_match(html, '"type":"line"', fixed = TRUE)
  expect_match(html, "pvRenderers", fixed = TRUE)
  expect_match(html, "data:font/woff2;base64,", fixed = TRUE)
  expect_false(grepl('src="lib/', html, fixed = TRUE))
  # the default save mode pins everything light
  expect_length(regmatches(html, gregexpr('"mode":"light"', html))[[1]], 2)
  expect_gt(file.size(f), 500000)
})

test_that("board html honours mode auto and embed_fonts", {
  dir <- withr::local_tempdir()
  b <- pv_board(board_bar())
  f <- file.path(dir, "auto.html")
  expect_invisible(pv_save(b, f, mode = "auto", embed_fonts = FALSE,
                           quiet = TRUE))
  html <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"),
                collapse = "\n")
  expect_match(html, '"mode":"auto"', fixed = TRUE)
  expect_match(html, "prefers-color-scheme: dark", fixed = TRUE)
  expect_false(grepl("data:font/woff2", html, fixed = TRUE))
})

test_that("a linked board's saved html still carries crosstalk", {
  dir <- withr::local_tempdir()
  pts <- data.frame(x = c(0, 10, 5), y = c(0, 10, 5))
  sd <- crosstalk::SharedData$new(pts)
  b <- pv_board(pv_scatter(pts, x = "x", y = "y") |> pv_link(sd),
                pv_scatter(pts, x = "x", y = "y") |> pv_link(sd))
  f <- file.path(dir, "linked.html")
  pv_save(b, f, quiet = TRUE)
  html <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"),
                collapse = "\n")
  expect_match(html, "SelectionHandle", fixed = TRUE)
  expect_length(regmatches(html, gregexpr('"ctGroup"', html))[[1]], 2)
})

test_that("board saves validate and refuse the formats with nothing to hold", {
  b <- pv_board(board_bar())
  dir <- tempdir()
  expect_error(pv_save(b, file.path(dir, "b.svg")),
               "cannot hold a chart board")
  expect_error(pv_save(b, file.path(dir, "b.gif")),
               "cannot hold a chart board")
  expect_error(pv_save(b, file.path(dir, "b.docx")), "must end in")
  expect_error(pv_save(b, c("a.png", "b.png")), "single file path")
  expect_error(pv_save(b, file.path(dir, "b.png"), width = -1), "`width`")
  expect_error(pv_save(b, file.path(dir, "b.png"), height = 0), "`height`")
  expect_error(pv_save(b, file.path(dir, "b.png"), mode = "sepia"),
               '"auto", "light", or "dark"')
  expect_error(pv_save(b, file.path(dir, "b.png"), delay = -1), "`delay`")
  expect_error(pv_save(b, file.path(dir, "b.html"), embed_fonts = "yes"),
               "`embed_fonts`")
  expect_error(pv_save(b, file.path(dir, "b.html"), quiet = 1), "`quiet`")
  # a bare classed object without pv_board()'s recipe is refused
  fake <- structure(list(), class = "pv_board")
  expect_error(pv_save(fake, file.path(dir, "b.html")), "build recipe")
})

test_that("a clear error names chromote for board captures when missing", {
  local_mocked_bindings(export_has_chromote = function() FALSE)
  b <- pv_board(board_bar())
  expect_error(pv_save(b, file.path(tempdir(), "b.png"), quiet = TRUE),
               "chromote")
  expect_error(pv_save(b, file.path(tempdir(), "b.pdf"), quiet = TRUE),
               "chromote")
  # .html keeps working without a browser stack
  f <- file.path(withr::local_tempdir(), "still.html")
  expect_no_error(pv_save(b, f, quiet = TRUE))
  expect_true(file.exists(f))
})

test_that("a board knits into a markdown document via knit_print", {
  skip_if_not_installed("knitr")
  dir <- withr::local_tempdir()
  env <- new.env(parent = globalenv())
  env$board <- pv_board(board_bar(), board_line(), title = "Knitted board")
  rmd <- file.path(dir, "board.Rmd")
  writeLines(c("A board in a chunk:", "",
               "```{r board-chunk, echo=FALSE}", "board", "```"), rmd)
  md <- file.path(dir, "board.md")
  knitr::knit(rmd, md, envir = env, quiet = TRUE)
  out <- paste(readLines(md, warn = FALSE, encoding = "UTF-8"),
               collapse = "\n")
  expect_match(out, 'class="pv-board"', fixed = TRUE)
  expect_match(out, ">Knitted board<")
  expect_match(out, '"type":"bar"', fixed = TRUE)
  expect_match(out, '"type":"line"', fixed = TRUE)
})

# ---- headless-Chrome verification --------------------------------------

# A board of the four panel shapes that exercise every drawing path the
# capture must settle on: plain SVG charts, the geo renderer, and the one
# chart drawn as HTML rows rather than SVG.
board_four_panels <- function() {
  agg <- aggregate(revenue ~ region, pv_sales, sum)
  monthly <- aggregate(revenue ~ month + region, pv_sales, sum)
  pop <- pv_city_population[order(pv_city_population$city,
                                  pv_city_population$year), ]
  now <- pop[pop$year == 2024, c("city", "population")]
  then <- pop[pop$year == 1990, c("city", "population")]
  tab <- merge(now, then, by = "city", suffixes = c("", "_1990"))
  tab$growth <- 100 * (tab$population / tab$population_1990 - 1)
  tab <- head(tab[order(-tab$population), ], 8)
  pv_board(
    pv_bar(agg, "region", "revenue", title = "Revenue by region"),
    pv_line(monthly, "month", "revenue", series = "region",
            title = "Monthly revenue by region"),
    pv_choropleth(pv_fiscal[pv_fiscal$year == 2025, ],
                  id = "municipality_id", value = "resource_index",
                  palette = "diverging", center = 100,
                  title = "Municipal resource index"),
    pv_table(tab, columns = c("city", "population", "growth"),
             bars = "population", shade = "growth",
             title = "The largest Swiss cities"),
    title = "A four-panel board",
    source = "Simulated and public Swiss data")
}

test_that("a four-panel board captures as png, light and dark", {
  render_skip_if_no_chrome()
  dir <- withr::local_tempdir()
  b <- board_four_panels()
  light <- file.path(dir, "board-light.png")
  expect_no_warning(pv_save(b, light, quiet = TRUE, delay = 0.2))
  expect_identical(readBin(light, "raw", 4)[2:4], charToRaw("PNG"))
  size <- board_png_size(light)
  # scale 2 doubles the 900 css pixels; the natural height holds two
  # 420px rows plus the heading, gaps, padding, and source line
  expect_identical(size[["width"]], 1800)
  expect_gt(size[["height"]], 1700)
  expect_gt(file.size(light), 50000)

  dark <- file.path(dir, "board-dark.png")
  expect_no_warning(pv_save(b, dark, mode = "dark", quiet = TRUE,
                            delay = 0.2))
  expect_gt(file.size(dark), 50000)
  # the two captures really are different renders of the same page
  expect_false(identical(readBin(light, "raw", file.size(light)),
                         readBin(dark, "raw", file.size(dark))))
})

test_that("explicit board sizes pin the captured page", {
  render_skip_if_no_chrome()
  dir <- withr::local_tempdir()
  b <- pv_board(board_bar(), board_line())
  f <- file.path(dir, "sized.png")
  pv_save(b, f, width = 840, height = 700, scale = 1, quiet = TRUE,
          delay = 0.2)
  expect_identical(board_png_size(f), c(width = 840, height = 700))

  pdf <- file.path(dir, "board.pdf")
  expect_message(pv_save(b, pdf, delay = 0.2), "board pdf")
  expect_identical(readBin(pdf, "raw", 5), charToRaw("%PDF-"))
  raw <- readBin(pdf, "raw", file.size(pdf))
  ascii <- rawToChar(raw[raw > as.raw(0) & raw < as.raw(128)])
  # one page, with the charts' fonts embedded
  expect_match(ascii, "/Count 1", fixed = TRUE)
  expect_match(ascii, "FontFile", fixed = TRUE)
})

# Stages a board the way pv_save() stages its captures (light, entrance
# animations off), serves it from disk, and opens it in headless Chrome.
board_page_session <- function(board, width = 1000, height = 560) {
  spec <- attr(board, "pv_board")
  spec$mode <- "light"
  spec$widgets <- lapply(spec$widgets, function(w) {
    w$x$duration <- 0
    w
  })
  rebuilt <- polyviz:::board_build(spec)
  stage <- tempfile("pv-board-page-")
  dir.create(stage)
  page <- file.path(stage, "board.html")
  htmltools::save_html(rebuilt, file = page, libdir = "lib")
  b <- chromote::ChromoteSession$new(width = width, height = height)
  errors <- polyviz:::export_watch_errors(b)
  loaded <- b$Page$loadEventFired(wait_ = FALSE)
  b$Page$navigate(utils::URLencode(paste0("file://", normalizePath(page))),
                  wait_ = FALSE)
  b$wait_for(loaded)
  polyviz:::export_wait_settled(b, errors, 0,
                                polyviz:::board_settle_count_js())
  list(b = b, errors = errors)
}

test_that("a brush in one board panel dims the marks in another", {
  render_skip_if_no_chrome()
  # Five points on a [0, 10] square, which nice() keeps as the domain -
  # the same geometry as the single-chart brush test, twice over.
  pts <- data.frame(x = c(0, 10, 5, 2, 8), y = c(0, 10, 5, 8, 2))
  sd <- crosstalk::SharedData$new(pts, key = c("a", "b", "c", "d", "e"))
  board <- pv_board(pv_scatter(pts, x = "x", y = "y") |> pv_link(sd),
                    pv_scatter(pts, x = "x", y = "y") |> pv_link(sd))
  group <- attr(board, "pv_board")$widgets[[1]]$x$ctGroup
  s <- board_page_session(board)
  withr::defer(try(s$b$close(), silent = TRUE))
  expect_identical(s$errors$msgs, character())

  # Aim at the first panel's plot svg and drag a brush over its middle
  # third: only the centre point (key "c") lies inside it.
  rect <- s$b$Runtime$evaluate("
    (function () {
      var panel = document.querySelectorAll('.pv-board-panel')[0];
      var svg = null;
      panel.querySelectorAll('svg').forEach(function (el) {
        if (!svg || el.clientWidth > svg.clientWidth) { svg = el; }
      });
      return JSON.stringify(svg.getBoundingClientRect());
    })()", returnByValue = TRUE)$result$value
  r <- jsonlite::fromJSON(rect)
  x0 <- r$left + r$width * 0.35
  x1 <- r$left + r$width * 0.65
  y0 <- r$top + r$height * 0.35
  y1 <- r$top + r$height * 0.65
  s$b$Input$dispatchMouseEvent(type = "mouseMoved", x = x0, y = y0)
  s$b$Input$dispatchMouseEvent(type = "mousePressed", x = x0, y = y0,
                               button = "left", clickCount = 1)
  for (i in 1:8) {
    s$b$Input$dispatchMouseEvent(type = "mouseMoved",
                                 x = x0 + (x1 - x0) * i / 8,
                                 y = y0 + (y1 - y0) * i / 8,
                                 button = "left")
  }
  s$b$Input$dispatchMouseEvent(type = "mouseReleased", x = x1, y = y1,
                               button = "left", clickCount = 1)
  Sys.sleep(0.5)

  # The selection reached the shared group...
  sel <- s$b$Runtime$evaluate(sprintf(
    "JSON.stringify(window.crosstalk.group('%s').var('selection').get())",
    group), returnByValue = TRUE)$result$value
  expect_identical(jsonlite::fromJSON(sel), "c")

  # ...and the OTHER panel re-rendered against it: its centre mark keeps
  # the scatter's usual 0.62 opacity while the other four dim to the
  # linked-selection ghost of 0.12.
  ops <- s$b$Runtime$evaluate("
    (function () {
      var panel = document.querySelectorAll('.pv-board-panel')[1];
      return JSON.stringify(Array.prototype.map.call(
        panel.querySelectorAll('circle.pt'),
        function (c) { return +c.getAttribute('fill-opacity'); }));
    })()", returnByValue = TRUE)$result$value
  got <- sort(jsonlite::fromJSON(ops))
  expect_length(got, 5)
  expect_equal(got, c(0.12, 0.12, 0.12, 0.12, 0.62), tolerance = 1e-6)
  expect_identical(s$errors$msgs, character())
})

test_that("a saved board html renders alone in an empty directory", {
  render_skip_if_no_chrome()
  saved <- file.path(withr::local_tempdir(), "board.html")
  pv_save(pv_board(board_bar(), board_line(), title = "Standalone board"),
          saved, quiet = TRUE)
  # Reopen the file as the ONLY thing in a fresh directory, so nothing
  # can lean on assets that happened to sit next to the original.
  alone <- withr::local_tempdir()
  target <- file.path(alone, "board.html")
  file.copy(saved, target)
  expect_identical(list.files(alone), "board.html")

  b <- chromote::ChromoteSession$new(width = 1000, height = 700)
  withr::defer(try(b$close(), silent = TRUE))
  errors <- polyviz:::export_watch_errors(b)
  loaded <- b$Page$loadEventFired(wait_ = FALSE)
  b$Page$navigate(utils::URLencode(paste0("file://", normalizePath(target))),
                  wait_ = FALSE)
  b$wait_for(loaded)
  polyviz:::export_wait_settled(b, errors, 0,
                                polyviz:::board_settle_count_js())
  expect_identical(errors$msgs, character())
  got <- b$Runtime$evaluate("
    JSON.stringify({
      charts: document.querySelectorAll('.pvchart').length,
      svg: document.querySelectorAll('svg *').length,
      title: document.querySelector('.pv-board-title').textContent
    })", returnByValue = TRUE)$result$value
  info <- jsonlite::fromJSON(got)
  expect_identical(info$charts, 2L)
  expect_gt(info$svg, 50)
  expect_identical(info$title, "Standalone board")
})
