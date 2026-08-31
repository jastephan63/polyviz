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
