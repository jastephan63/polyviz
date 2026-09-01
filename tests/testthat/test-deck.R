# pv_deck() builds a PowerPoint from real chart renders, so the deck
# tests need officer to pack the file and headless Chrome to draw the
# slides; skip cleanly wherever either is missing (CRAN included).
deck_skip_if_no_deps <- function() {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("chromote")
  chrome <- tryCatch(chromote::find_chrome(), error = function(e) NULL)
  skip_if(is.null(chrome) || !nzchar(chrome), "no Chrome-based browser found")
}

deck_chart <- function() {
  agg <- aggregate(revenue ~ region, pv_sales, sum)
  pv_bar(agg, "region", "revenue", title = "Revenue by region",
         source = "Source: pv_sales")
}

# Three different chart types, so the deck exercises more than one
# renderer; only the first carries a source line.
deck_charts <- function() {
  monthly <- aggregate(revenue ~ month, pv_sales, sum)
  agg <- aggregate(revenue ~ region, pv_sales, sum)
  list(
    deck_chart(),
    pv_line(monthly, x = "month", y = "revenue", title = "Monthly revenue"),
    pv_donut(agg, category = "region", value = "revenue",
             title = "Share of revenue")
  )
}

# Pixel size straight from the PNG header: width and height sit big-endian
# at bytes 17-24, right after the signature and the IHDR chunk intro.
deck_png_size <- function(path) {
  bytes <- readBin(path, "raw", n = 24)
  c(width = sum(as.integer(bytes[17:20]) * 256^(3:0)),
    height = sum(as.integer(bytes[21:24]) * 256^(3:0)))
}

deck_read_xml <- function(path) {
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

deck_count <- function(xml, pattern) {
  hits <- gregexpr(pattern, xml, fixed = TRUE)[[1]]
  if (length(hits) == 1 && hits == -1) 0L else length(hits)
}

test_that("pv_deck validates its arguments", {
  w <- deck_chart()
  f <- file.path(tempdir(), "validate.pptx")
  expect_error(pv_deck(42, f), "`charts` must be a list")
  expect_error(pv_deck(list(), f), "empty")
  expect_error(pv_deck(list(w, data.frame()), f), "charts\\[\\[2\\]\\]")
  expect_error(pv_deck(w, c("a.pptx", "b.pptx")), "single file path")
  expect_error(pv_deck(w, file.path(tempdir(), "deck.docx")),
               "must end in .pptx")
  expect_error(pv_deck(w, f, title = c("a", "b")), "`title`")
  expect_error(pv_deck(w, f, subtitle = "s"), "`subtitle` needs a `title`")
  expect_error(pv_deck(w, f, ratio = "21:9"), '"16:9" or "4:3"')
  expect_error(pv_deck(w, f, ratio = "4:3", width = 8), "not both")
  expect_error(pv_deck(w, f, width = -1), "`width`")
  expect_error(pv_deck(w, f, height = 0), "`height`")
  expect_error(pv_deck(w, f, scale = "big"), "`scale`")
  expect_error(pv_deck(w, f, mode = "sepia"), '"auto", "light", or "dark"')
  expect_error(pv_deck(w, f, quiet = 1), "`quiet`")
})

test_that("a clear error names officer when it is missing", {
  local_mocked_bindings(deck_has_officer = function() FALSE)
  expect_error(pv_deck(deck_chart(), file.path(tempdir(), "x.pptx")),
               "officer")
})

test_that("a clear error names chromote when it is missing", {
  # the officer guard runs first, so this test needs officer for real
  skip_if_not_installed("officer")
  local_mocked_bindings(export_has_chromote = function() FALSE)
  expect_error(pv_deck(deck_chart(), file.path(tempdir(), "x.pptx")),
               "chromote")
})

test_that("a titled deck is one 16:9 slide per chart plus the opener", {
  deck_skip_if_no_deps()
  dir <- withr::local_tempdir()
  f <- file.path(dir, "deck.pptx")
  expect_message(
    pv_deck(deck_charts(), f, title = "Quarterly review",
            subtitle = "Rendered by polyviz"),
    "Saved")
  expect_true(file.exists(f))
  # one file, and no leftover temp file from the atomic write
  expect_identical(list.files(dir, all.files = TRUE, no.. = TRUE),
                   "deck.pptx")

  # a valid zip holding four slides: the title slide plus one per chart
  listing <- unzip(f, list = TRUE)
  expect_true(all(sprintf("ppt/slides/slide%d.xml", 1:4) %in% listing$Name))
  expect_false("ppt/slides/slide5.xml" %in% listing$Name)

  ex <- withr::local_tempdir()
  unzip(f, exdir = ex)

  # the deck is 16:9: 10 x 5.63 inches in EMUs (914400 to the inch)
  pres <- deck_read_xml(file.path(ex, "ppt", "presentation.xml"))
  expect_match(pres, '<p:sldSz cx="9144000" cy="5148072"/>', fixed = TRUE)

  # the opener carries the heading text and no image
  opener <- deck_read_xml(file.path(ex, "ppt", "slides", "slide1.xml"))
  expect_match(opener, "Quarterly review", fixed = TRUE)
  expect_match(opener, "Rendered by polyviz", fixed = TRUE)
  expect_identical(deck_count(opener, "<p:pic>"), 0L)

  # each content slide places exactly one image
  for (i in 2:4) {
    slide <- deck_read_xml(
      file.path(ex, "ppt", "slides", sprintf("slide%d.xml", i)))
    expect_identical(deck_count(slide, "<p:pic>"), 1L)
  }

  # the embedded media are the three real renders at slide size x scale:
  # 960 x 540 CSS pixels captured at the default 2x
  media <- list.files(file.path(ex, "ppt", "media"),
                      pattern = "\\.png$", full.names = TRUE)
  expect_length(media, 3)
  for (m in media) {
    expect_identical(readBin(m, "raw", 4)[2:4], charToRaw("PNG"))
    expect_gt(file.size(m), 20000)
    expect_identical(deck_png_size(m), c(width = 1920, height = 1080))
  }

  # every chart slide's notes carry the chart's title (and source line,
  # where the chart has one)
  notes <- list.files(file.path(ex, "ppt", "notesSlides"),
                      pattern = "^notesSlide[0-9]+\\.xml$",
                      full.names = TRUE)
  expect_length(notes, 3)
  all_notes <- paste(vapply(notes, deck_read_xml, character(1)),
                     collapse = "\n")
  expect_match(all_notes, "Revenue by region", fixed = TRUE)
  expect_match(all_notes, "Source: pv_sales", fixed = TRUE)
  expect_match(all_notes, "Monthly revenue", fixed = TRUE)
  expect_match(all_notes, "Share of revenue", fixed = TRUE)
})

test_that("ratio 4:3 works, and a bare widget is wrapped", {
  deck_skip_if_no_deps()
  dir <- withr::local_tempdir()
  f <- file.path(dir, "classic.pptx")
  expect_invisible(pv_deck(deck_chart(), f, ratio = "4:3", scale = 1,
                           quiet = TRUE))
  ex <- withr::local_tempdir()
  unzip(f, exdir = ex)
  pres <- deck_read_xml(file.path(ex, "ppt", "presentation.xml"))
  expect_match(pres, '<p:sldSz cx="9144000" cy="6858000"/>', fixed = TRUE)
  # no title given, so the one chart is the whole deck
  expect_true(file.exists(file.path(ex, "ppt", "slides", "slide1.xml")))
  expect_false(file.exists(file.path(ex, "ppt", "slides", "slide2.xml")))
  media <- list.files(file.path(ex, "ppt", "media"),
                      pattern = "\\.png$", full.names = TRUE)
  expect_length(media, 1)
  expect_identical(deck_png_size(media), c(width = 960, height = 720))
})

test_that("a failed deck write leaves no file behind", {
  deck_skip_if_no_deps()
  dir <- withr::local_tempdir()
  blocker <- file.path(dir, "blocker")
  file.create(blocker)
  # the target's parent "directory" is a plain file, so the save must fail
  target <- file.path(blocker, "out.pptx")
  expect_error(pv_deck(deck_chart(), target, quiet = TRUE))
  expect_false(file.exists(target))
  expect_identical(list.files(dir, all.files = TRUE, no.. = TRUE),
                   "blocker")
})
