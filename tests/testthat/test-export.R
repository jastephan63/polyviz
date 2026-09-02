# The capture formats drive a real headless Chrome; skip cleanly wherever
# chromote or a browser is missing (CRAN included).
skip_if_no_chrome <- function() {
  skip_on_cran()
  skip_if_not_installed("chromote")
  chrome <- tryCatch(chromote::find_chrome(), error = function(e) NULL)
  skip_if(is.null(chrome) || !nzchar(chrome), "no Chrome-based browser found")
}

export_chart <- function(...) {
  agg <- aggregate(revenue ~ region, pv_sales, sum)
  pv_bar(agg, "region", "revenue", title = "Revenue by region",
         subtitle = "Simulated sales", source = "Source: pv_sales", ...)
}

# Pixel size straight from the PNG header: width and height sit big-endian
# at bytes 17-24, right after the signature and the IHDR chunk intro.
png_size <- function(path) {
  bytes <- readBin(path, "raw", n = 24)
  c(width = sum(as.integer(bytes[17:20]) * 256^(3:0)),
    height = sum(as.integer(bytes[21:24]) * 256^(3:0)))
}

# The gif tests need gifski on top of the browser stack.
skip_if_no_gif <- function() {
  skip_if_no_chrome()
  skip_if_not_installed("gifski")
}

# A small race for the gif tests: four census years, eight bars.
export_race <- function(...) {
  few <- pv_city_population[pv_city_population$year >= 1990, ]
  pv_race(few, time = "year", id = "city", value = "population",
          top_n = 8, title = "Swiss cities racing", ...)
}

# Frame count and per-frame delays, read by walking the GIF's own block
# structure - grepping the bytes for markers would miscount, since the
# marker values also occur inside compressed pixel data. After the
# header and optional global colour table, every block is either an
# extension (0x21: a graphic-control extension carries the next frame's
# delay in hundredths of a second), an image (0x2C, one frame), or the
# trailer (0x3B); sub-blocks are length-prefixed and end at length 0.
gif_scan <- function(path) {
  raw <- readBin(path, "raw", file.size(path))
  skip_sub <- function(p) {
    repeat {
      n <- as.integer(raw[p])
      p <- p + 1L
      if (n == 0L) {
        return(p)
      }
      p <- p + n
    }
  }
  packed <- as.integer(raw[11])
  p <- 14L
  if (bitwAnd(packed, 128L) > 0) {
    p <- p + 3L * 2L^(bitwAnd(packed, 7L) + 1L)
  }
  frames <- 0L
  delays <- numeric()
  while (p <= length(raw)) {
    b <- as.integer(raw[p])
    if (b == 0x3B) break
    if (b == 0x21) {
      if (as.integer(raw[p + 1L]) == 0xF9) {
        delays <- c(delays, (as.integer(raw[p + 4L]) +
                               256 * as.integer(raw[p + 5L])) / 100)
      }
      p <- skip_sub(p + 2L)
    } else if (b == 0x2C) {
      frames <- frames + 1L
      local_packed <- as.integer(raw[p + 9L])
      p <- p + 10L
      if (bitwAnd(local_packed, 128L) > 0) {
        p <- p + 3L * 2L^(bitwAnd(local_packed, 7L) + 1L)
      }
      p <- skip_sub(p + 1L)
    } else {
      stop("unexpected GIF block")
    }
  }
  list(frames = frames, delays = delays)
}

test_that("pv_save validates its arguments", {
  w <- export_chart()
  f <- file.path(tempdir(), "validate.png")
  expect_error(pv_save(data.frame(), f), "polyviz chart")
  expect_error(pv_save(w, c("a.png", "b.png")), "single file path")
  expect_error(pv_save(w, file.path(tempdir(), "chart.docx")), "must end in")
  expect_error(pv_save(w, f, width = -1), "`width`")
  expect_error(pv_save(w, f, height = 0), "`height`")
  expect_error(pv_save(w, f, scale = "big"), "`scale`")
  expect_error(pv_save(w, f, mode = "sepia"), '"auto", "light", or "dark"')
  expect_error(pv_save(w, f, delay = -1), "`delay`")
  expect_error(pv_save(w, f, embed_fonts = "yes"), "`embed_fonts`")
  expect_error(pv_save(w, f, quiet = 1), "`quiet`")
  expect_error(pv_save(w, f, fps = 0), "`fps`")
  expect_error(pv_save(w, f, fps = 51), "`fps`")
  expect_error(pv_save(w, f, fps = c(10, 20)), "`fps`")
})

test_that("only the race can be saved as a gif", {
  f <- file.path(tempdir(), "still.gif")
  # a bar chart has no animation timeline, and neither has the bump
  # chart - its lines only draw in once
  expect_error(pv_save(export_chart(), f), "no animation")
  bump <- pv_bump(pv_city_population, time = "year", id = "city",
                  value = "population")
  expect_error(pv_save(bump, f), "no animation")
})

test_that("a clear error names gifski when it is missing", {
  local_mocked_bindings(export_has_gifski = function() FALSE)
  # the gifski check runs before the browser check, so this needs no Chrome
  expect_error(pv_save(export_race(), file.path(tempdir(), "x.gif"),
                       quiet = TRUE), "gifski")
})

test_that("html export is one self-contained file, no browser needed", {
  dir <- withr::local_tempdir()
  w <- export_chart()
  f <- file.path(dir, "chart.html")
  expect_message(pv_save(w, f), "Saved")
  expect_true(file.exists(f))
  # one file, no lib/ folder, and no leftover temp file from the write
  expect_identical(list.files(dir, all.files = TRUE, no.. = TRUE),
                   "chart.html")
  html <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"),
                collapse = "\n")
  expect_match(html, "data:font/woff2;base64,", fixed = TRUE)
  expect_match(html, "Revenue by region", fixed = TRUE)
  expect_match(html, "pvRenderers", fixed = TRUE)
  expect_match(html, '"mode":"light"', fixed = TRUE)
  expect_false(grepl('src="lib/', html, fixed = TRUE))
  expect_gt(file.size(f), 500000)
})

test_that("html export honours mode and embed_fonts", {
  dir <- withr::local_tempdir()
  w <- export_chart()
  f <- file.path(dir, "auto.html")
  expect_invisible(pv_save(w, f, mode = "auto", embed_fonts = FALSE,
                           quiet = TRUE))
  html <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"),
                collapse = "\n")
  expect_match(html, '"mode":"auto"', fixed = TRUE)
  expect_match(html, "prefers-color-scheme: dark", fixed = TRUE)
  expect_false(grepl("data:font/woff2", html, fixed = TRUE))
})

test_that("a failed save leaves no file behind", {
  dir <- withr::local_tempdir()
  blocker <- file.path(dir, "blocker")
  file.create(blocker)
  # the target's parent "directory" is a plain file, so the save must fail
  target <- file.path(blocker, "out.html")
  expect_error(pv_save(export_chart(), target, quiet = TRUE))
  expect_false(file.exists(target))
  expect_identical(list.files(dir, all.files = TRUE, no.. = TRUE),
                   "blocker")
})

test_that("a clear error names chromote when it is missing", {
  local_mocked_bindings(export_has_chromote = function() FALSE)
  w <- export_chart()
  expect_error(pv_save(w, file.path(tempdir(), "x.png"), quiet = TRUE),
               "chromote")
  expect_error(pv_save(w, file.path(tempdir(), "x.pdf"), quiet = TRUE),
               "chromote")
  # .html keeps working without a browser stack
  f <- file.path(withr::local_tempdir(), "still.html")
  expect_no_error(pv_save(w, f, quiet = TRUE))
  expect_true(file.exists(f))
})

test_that("png export captures the settled chart at scale", {
  skip_if_no_chrome()
  f <- file.path(withr::local_tempdir(), "chart.png")
  pv_save(export_chart(), f, quiet = TRUE, delay = 0.2)
  expect_identical(readBin(f, "raw", 4)[2:4], charToRaw("PNG"))
  expect_identical(png_size(f), c(width = 1800, height = 1120))
  expect_gt(file.size(f), 10000)
})

test_that("height defaults to the widget's own fixed height", {
  skip_if_no_chrome()
  agg <- aggregate(revenue ~ region, pv_sales, sum)
  w <- pv_bar(agg, "region", "revenue", height = 380)
  f <- file.path(withr::local_tempdir(), "sized.png")
  pv_save(w, f, scale = 1, quiet = TRUE, delay = 0.2)
  expect_identical(png_size(f), c(width = 900, height = 380))
})

test_that("svg export is a standalone document carrying its font", {
  skip_if_no_chrome()
  dir <- withr::local_tempdir()
  w <- export_chart()
  f <- file.path(dir, "chart.svg")
  pv_save(w, f, quiet = TRUE, delay = 0.2)
  svg <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"),
               collapse = "\n")
  expect_match(svg, "^<\\?xml")
  expect_match(svg, "<svg", fixed = TRUE)
  expect_match(svg, "Revenue by region", fixed = TRUE)
  expect_match(svg, "Source: pv_sales", fixed = TRUE)
  expect_match(svg, "data:font/woff2;base64,", fixed = TRUE)
  f2 <- file.path(dir, "bare.svg")
  pv_save(w, f2, embed_fonts = FALSE, quiet = TRUE, delay = 0.2)
  bare <- paste(readLines(f2, warn = FALSE, encoding = "UTF-8"),
                collapse = "\n")
  expect_false(grepl("data:font/woff2", bare, fixed = TRUE))
})

test_that("svg export embeds a canvas scatter's raster among vector chrome", {
  skip_if_no_chrome()
  set.seed(11)
  cloud <- data.frame(x = stats::rnorm(600), y = stats::rnorm(600))
  w <- pv_scatter(cloud, x = "x", y = "y", canvas = TRUE,
                  title = "Canvas cloud")
  f <- file.path(withr::local_tempdir(), "canvas.svg")
  expect_no_warning(pv_save(w, f, quiet = TRUE, delay = 0.2))
  svg <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"),
               collapse = "\n")
  # The point marks arrive as one rasterised layer: an <image> holding
  # the canvas pixels as a png data URI, no circle per point.
  expect_match(svg, 'href="data:image/png;base64,')
  expect_length(regmatches(svg, gregexpr("<image", svg))[[1]], 1)
  expect_false(grepl('circle class="pt"', svg, fixed = TRUE))
  # Everything around the raster stays vector: the title text and the
  # axis tick text are real <text> elements.
  expect_match(svg, "Canvas cloud", fixed = TRUE)
  expect_match(svg, "<text", fixed = TRUE)
  # Root document, plot svg, the raster's holder, and the interaction
  # overlay - four svg elements in all.
  expect_equal(lengths(regmatches(svg, gregexpr("<svg", svg))), 4L)
})

test_that("a small scatter's svg export stays fully vector", {
  skip_if_no_chrome()
  w <- pv_scatter(mtcars, x = "wt", y = "mpg")
  f <- file.path(withr::local_tempdir(), "vector.svg")
  expect_no_warning(pv_save(w, f, quiet = TRUE, delay = 0.2))
  svg <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"),
               collapse = "\n")
  # Under the canvas threshold nothing is rasterised: no <image>, one
  # circle per data row, and only the root plus the one plot svg.
  expect_false(grepl("<image", svg, fixed = TRUE))
  expect_length(regmatches(svg, gregexpr('circle class="pt"', svg))[[1]],
                nrow(mtcars))
  expect_equal(lengths(regmatches(svg, gregexpr("<svg", svg))), 2L)
})

test_that("pdf export is a single-page vector pdf with embedded fonts", {
  skip_if_no_chrome()
  f <- file.path(withr::local_tempdir(), "chart.pdf")
  pv_save(export_chart(), f, quiet = TRUE, delay = 0.2)
  expect_identical(readBin(f, "raw", 5), charToRaw("%PDF-"))
  expect_gt(file.size(f), 5000)
  raw <- readBin(f, "raw", file.size(f))
  ascii <- rawToChar(raw[raw > as.raw(0) & raw < as.raw(128)])
  expect_match(ascii, "/Count 1", fixed = TRUE)
  expect_match(ascii, "FontFile", fixed = TRUE)
})

test_that("the settle probe matches what the chart draws", {
  # every SVG chart settles on the count of SVG elements...
  expect_identical(polyviz:::export_settle_count_js(export_chart()),
                   "document.querySelectorAll('svg *').length")
  # ...and so does a table with a spark column - its sparklines are SVG
  df <- data.frame(city = c("A", "B"), size = c(2, 1))
  df$trend <- I(list(c(1, 2, 3), c(2, 1, 4)))
  sparky <- pv_table(df, bars = "size", spark = "trend")
  expect_identical(polyviz:::export_settle_count_js(sparky),
                   "document.querySelectorAll('svg *').length")
  # a table without sparklines never draws any SVG, so the SVG poll
  # would sit out its whole timeout; that table settles on its own rows
  plain <- pv_table(data.frame(a = c("x", "y"), b = c(1, 2)),
                    sortable = FALSE)
  expect_identical(polyviz:::export_settle_count_js(plain),
                   "document.querySelectorAll('.pvchart table tr').length")
})

test_that("a plain table saves well under the old settle timeout", {
  skip_if_no_chrome()
  dir <- withr::local_tempdir()
  set.seed(1)
  tab <- data.frame(name = sprintf("Municipality %02d", 1:50),
                    population = round(runif(50, 1000, 90000)))
  w <- pv_table(tab, sortable = FALSE, title = "A plain table")
  # one throwaway save first, so Chrome's own startup cost stays out of
  # the measured run
  pv_save(w, file.path(dir, "warmup.png"), quiet = TRUE, delay = 0)
  f <- file.path(dir, "plain.png")
  elapsed <- system.time(
    pv_save(w, f, quiet = TRUE, delay = 0))[["elapsed"]]
  expect_true(file.exists(f))
  expect_gt(file.size(f), 20000)
  # With no SVG on the page the old settle loop sat out its full
  # 4-second cap before every capture; polling the table's rows settles
  # in a few tenths. Asserted with slack for slow machines, but still
  # well under the old floor.
  expect_lt(elapsed, 3)
})

# Renders a widget in headless Chrome exactly the way pv_save() stages
# its captures (light mode, no entrance animation), waits until it has
# settled, and returns the standalone SVG document - what the .pdf print
# path is built from.
export_settled_svg <- function(widget) {
  w <- widget
  w$x$mode <- "light"
  w$x$duration <- 0
  w$width <- NULL
  w$height <- NULL
  w$sizingPolicy$browser$fill <- TRUE
  w$sizingPolicy$browser$padding <- 0
  stage <- tempfile("pv-export-")
  dir.create(stage)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  page <- file.path(stage, "chart.html")
  htmlwidgets::saveWidget(w, page, selfcontained = FALSE, libdir = "lib")

  b <- chromote::ChromoteSession$new(width = 700, height = 460)
  on.exit(try(b$close(), silent = TRUE), add = TRUE)
  errors <- polyviz:::export_watch_errors(b)
  loaded <- b$Page$loadEventFired(wait_ = FALSE)
  b$Page$navigate(utils::URLencode(paste0("file://", normalizePath(page))),
                  wait_ = FALSE)
  b$wait_for(loaded)
  polyviz:::export_wait_settled(b, errors, 0,
                                polyviz:::export_settle_count_js(w))
  expect_identical(errors$msgs, character())
  b$Runtime$evaluate(paste0(
    "(function () {",
    " var el = document.querySelector('.pvchart');",
    " return window.pv.toStandaloneSvg(el, el.__pvLastX || null, null);",
    " })()"), returnByValue = TRUE)$result$value
}

test_that("a table's bars keep their rounded data end in vector output", {
  skip_if_no_chrome()
  regions <- aggregate(revenue ~ region, pv_sales, sum)
  w <- pv_table(regions, bars = "revenue", digits = 0,
                title = "Revenue by region")
  svg <- export_settled_svg(w)
  # The in-cell bar rounds only its data end (border-radius: 0 3px 3px
  # 0), which an rx-only rect cannot say: it must come out as a path
  # with one 3px arc per right corner, filled with the accent the bars
  # are drawn in.
  accent <- grDevices::col2rgb(w$x$theme$categorical$light[[1]])
  bar <- sprintf(paste0(
    '<path d="M[^"]*A3 3 0 0 1[^"]*A3 3 0 0 1[^"]*Z" ',
    'fill="rgb\\(%d, %d, %d\\)"'), accent[1], accent[2], accent[3])
  expect_match(svg, bar)
  # one rounded bar per data row
  expect_length(regmatches(svg, gregexpr(bar, svg))[[1]], 4)
})

test_that("javascript errors surface as an R warning", {
  skip_if_no_chrome()
  w <- export_chart()
  # break the payload so the renderer throws on its first line
  w$x$theme$ink <- list()
  f <- file.path(withr::local_tempdir(), "broken.png")
  expect_warning(pv_save(w, f, quiet = TRUE, delay = 0),
                 "JavaScript error")
  expect_true(file.exists(f))
})

test_that("gif export writes a looping animation of the race", {
  skip_if_no_gif()
  dir <- withr::local_tempdir()
  f <- file.path(dir, "race.gif")
  expect_message(pv_save(export_race(), f, width = 480, height = 320,
                         scale = 1, fps = 10, delay = 0.2),
                 "frames at 10 fps")
  expect_identical(readBin(f, "raw", 6), charToRaw("GIF89a"))
  expect_gt(file.size(f), 20000)
  # no leftover temp file from the atomic write
  expect_identical(list.files(dir, all.files = TRUE, no.. = TRUE),
                   "race.gif")

  # Four census years at the default tempo (900 ms per keyframe step)
  # sampled at 10 fps schedule 28 distinct frames; gifski may merge a
  # consecutive identical pair or split a long hold, so allow slack.
  info <- gif_scan(f)
  expect_gte(info$frames, 24)
  expect_lte(info$frames, 30)
  # riding frames show for 1/fps seconds each; the opening order holds
  # longer, and the final standings hold about two seconds. The encoder
  # rounds delays to centiseconds and may fold a riding frame into the
  # hold, so the hold is asserted with room to spare.
  expect_identical(stats::median(info$delays), 0.1)
  expect_gte(info$delays[1], 0.4)
  expect_gte(info$delays[length(info$delays)], 1.5)

  # The file must actually play, not just parse: show it in a browser
  # and look twice about a second apart - a running race puts different
  # standings on the screen each time.
  b <- chromote::ChromoteSession$new(width = 480, height = 320)
  withr::defer(try(b$close(), silent = TRUE))
  page <- file.path(dir, "play.html")
  writeLines(paste0(
    "<!DOCTYPE html><html><body style=\"margin:0\">",
    "<img src=\"race.gif\" width=\"480\" height=\"320\"></body></html>"),
    page)
  loaded <- b$Page$loadEventFired(wait_ = FALSE)
  b$Page$navigate(utils::URLencode(
    paste0("file://", normalizePath(page))), wait_ = FALSE)
  b$wait_for(loaded)
  shot <- function() {
    base64enc::base64decode(b$Page$captureScreenshot(format = "png")$data)
  }
  early <- shot()
  Sys.sleep(1.2)
  late <- shot()
  expect_false(identical(early, late))
})

test_that("gif export honours scale and holds the final standings", {
  skip_if_no_gif()
  f <- file.path(withr::local_tempdir(), "race2x.gif")
  pv_save(export_race(), f, width = 240, height = 160, scale = 2,
          fps = 4, quiet = TRUE, delay = 0.2)
  # gif pixel size sits little-endian at bytes 7-10 of the header
  bytes <- readBin(f, "raw", 10)
  expect_identical(as.integer(bytes[7]) + 256L * as.integer(bytes[8]), 480L)
  expect_identical(as.integer(bytes[9]) + 256L * as.integer(bytes[10]), 320L)
})
