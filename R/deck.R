# pv_deck() turns a list of charts into a PowerPoint file, one chart per
# slide. Every slide is a real render: the chart goes through pv_save()'s
# headless-Chrome pipeline at slide dimensions, and the settled PNG is
# placed edge to edge on a blank layout, so the deck shows exactly what a
# pv_save() capture shows. The R side renders and assembles; officer only
# packs slides, which is why it lives in Suggests.

# Thin wrapper so tests can pretend officer is not installed.
deck_has_officer <- function() {
  requireNamespace("officer", quietly = TRUE)
}

deck_need_officer <- function() {
  if (!deck_has_officer()) {
    rlang::abort(paste(
      "Building .pptx decks needs the officer package.",
      'Install it with install.packages("officer").'))
  }
}

# Same failure path as pv_save()'s capture formats - the slides are PNG
# renders - but worded for decks, which have no browserless fallback.
deck_need_chrome <- function() {
  if (!export_has_chromote()) {
    rlang::abort(paste(
      "Building .pptx decks needs the chromote package (and a",
      "Chrome-based browser) to render the slides. Install it with",
      'install.packages("chromote").'))
  }
  chrome <- tryCatch(chromote::find_chrome(), error = function(e) NULL)
  if (is.null(chrome) || !nzchar(chrome)) {
    rlang::abort(paste(
      "Building .pptx decks needs a Chrome-based browser to render the",
      "slides and none was found. Install Google Chrome, or point the",
      "CHROMOTE_CHROME environment variable at a Chromium binary."))
  }
}

deck_check_inches <- function(value, name) {
  if (!is.numeric(value) || length(value) != 1 || !is.finite(value) ||
      value <= 0) {
    rlang::abort(sprintf(
      "`%s` must be a single positive number of inches.", name))
  }
  as.numeric(value)
}

deck_check_text <- function(value, name) {
  if (is.null(value)) {
    return(NULL)
  }
  if (!is.character(value) || length(value) != 1 || is.na(value)) {
    rlang::abort(sprintf("`%s` must be a single string.", name))
  }
  value
}

# officer has no public slide-size setter, so the deck's dimensions are
# patched straight into the presentation part: <p:sldSz> carries them in
# EMUs (914400 to the inch), and the part's own replace_xml() reads the
# patched document back in. The exported slide_size() getter then has to
# agree - if a future officer stores the part differently, this fails
# loudly instead of quietly building a 4:3 deck.
deck_set_slide_size <- function(doc, width, height) {
  emu <- function(inches) {
    format(round(inches * 914400), scientific = FALSE, trim = TRUE)
  }
  part <- doc$presentation
  xml <- as.character(part$get())
  patched <- sub(
    "<p:sldSz[^>]*/>",
    sprintf('<p:sldSz cx="%s" cy="%s"/>', emu(width), emu(height)),
    xml)
  tmp <- tempfile(fileext = ".xml")
  on.exit(unlink(tmp), add = TRUE)
  writeBin(charToRaw(enc2utf8(patched)), tmp)
  part$replace_xml(tmp)
  got <- officer::slide_size(doc)
  if (!isTRUE(all.equal(c(got$width, got$height), c(width, height),
                        tolerance = 1e-4))) {
    rlang::abort(paste(
      "Failed to set the slide size; this version of officer stores the",
      "presentation part differently than pv_deck() expects."))
  }
  doc
}

# The opening slide is composed on the blank layout rather than the
# template's "Title Slide" one, whose placeholders sit where a 4:3 slide
# put them. A full-bleed rectangle first paints the same surface the
# rendered charts sit on, then the heading text goes just above the
# vertical centre. The text names Inter, the face every rendered chart
# is set in - left to the template it would come out in Calibri and the
# opener would clash with its own deck. PowerPoint substitutes a system
# font silently on machines without Inter, which beats the guaranteed
# mismatch.
deck_title_slide <- function(doc, title, subtitle, width, height, ink) {
  doc <- officer::add_slide(doc, layout = "Blank", master = "Office Theme")
  doc <- officer::ph_with(
    doc, officer::empty_content(),
    location = officer::ph_location(left = 0, top = 0, width = width,
                                    height = height,
                                    bg = ink$surface %||% "#ffffff"))
  margin <- 0.07 * width
  doc <- officer::ph_with(
    doc,
    officer::fpar(
      officer::ftext(title, officer::fp_text_lite(
        color = ink$primary %||% "#000000", font.size = 36, bold = TRUE,
        font.family = "Inter")),
      fp_p = officer::fp_par(text.align = "left")),
    location = officer::ph_location(left = margin, top = 0.36 * height,
                                    width = width - 2 * margin, height = 1))
  if (!is.null(subtitle)) {
    doc <- officer::ph_with(
      doc,
      officer::fpar(
        officer::ftext(subtitle, officer::fp_text_lite(
          color = ink$secondary %||% "#666666", font.size = 18,
          font.family = "Inter")),
        fp_p = officer::fp_par(text.align = "left")),
      location = officer::ph_location(left = margin,
                                      top = 0.36 * height + 1,
                                      width = width - 2 * margin,
                                      height = 0.6))
  }
  doc
}

# The speaker notes for one chart: its title and its source line, each as
# its own paragraph. NULL when the chart carries neither.
deck_notes <- function(widget) {
  ok <- function(v) {
    is.character(v) && length(v) == 1 && !is.na(v) && nzchar(v)
  }
  parts <- c(if (ok(widget$x$title)) widget$x$title,
             if (ok(widget$x$source)) widget$x$source)
  if (!length(parts)) {
    return(NULL)
  }
  do.call(officer::block_list, lapply(parts, officer::fpar))
}

#' Save a list of charts as a PowerPoint deck
#'
#' Writes one `.pptx` file with one slide per chart. Every slide is a real
#' render: the chart goes through the same headless-Chrome pipeline as
#' [pv_save()] at exactly the slide's dimensions (96 pixels to the inch,
#' times `scale`), and the settled PNG is placed edge to edge on a blank
#' layout — what lands in the deck is pixel for pixel what a `pv_save()`
#' capture shows. When `title` is given the deck opens with a title slide
#' on the same chart surface, set in Inter like the charts themselves;
#' on machines without Inter installed PowerPoint silently substitutes a
#' system font. Each chart's title and source line also go
#' into its slide's speaker notes, so the deck stays searchable and
#' presentable even though the slides are images.
#'
#' Slides are 16:9 by default (10 x 5.63 inches). `ratio = "4:3"` switches
#' to the classic 10 x 7.5, or give explicit `width`/`height` for anything
#' else.
#'
#' Needs the officer package to write the file and chromote (with a
#' Chrome-based browser) to render the slides; both install with
#' `install.packages()`.
#'
#' @param charts A list of polyviz charts, one per slide, in slide order.
#'   A single chart may be given bare.
#' @param file Output path, ending in `.pptx`.
#' @param title Optional deck title; when given, the deck opens with a
#'   title slide.
#' @param subtitle Optional line under the title on the title slide;
#'   needs `title`.
#' @param width,height Slide size in inches. The default is the standard
#'   16:9 slide.
#' @param scale Resolution multiplier for the slide renders: each slide
#'   PNG is `scale` times its pixel size (the default 2 is a crisp
#'   "retina" image, 192 pixels to the inch).
#' @param mode `"light"` (default), `"dark"`, or `"auto"`, forced onto
#'   every rendered slide. Slides are static captures, so `"auto"`
#'   renders as light, exactly as in [pv_save()].
#' @param ratio Convenience preset for the slide size: `"16:9"` or
#'   `"4:3"`. Give either `ratio` or explicit `width`/`height`, not both.
#' @param quiet Skip the one-line message saying what was saved?
#' @return The output path, invisibly.
#' @examplesIf interactive()
#' agg <- aggregate(revenue ~ region, pv_sales, sum)
#' deck <- list(
#'   pv_bar(agg, "region", "revenue", title = "Revenue by region"),
#'   pv_donut(agg, "region", "revenue", title = "Share of revenue")
#' )
#' pv_deck(deck, file.path(tempdir(), "review.pptx"),
#'         title = "Quarterly review")
#' @export
pv_deck <- function(charts, file, title = NULL, subtitle = NULL,
                    width = 10, height = 5.63, scale = 2, mode = "light",
                    ratio = NULL, quiet = FALSE) {
  if (inherits(charts, "pvchart")) {
    charts <- list(charts)
  }
  if (!is.list(charts)) {
    rlang::abort(paste(
      "`charts` must be a list of polyviz charts (the return values of",
      "pv_bar() and friends)."))
  }
  if (!length(charts)) {
    rlang::abort("`charts` is empty; nothing to build.")
  }
  for (i in seq_along(charts)) {
    if (!inherits(charts[[i]], "pvchart")) {
      rlang::abort(sprintf(paste(
        "`charts[[%d]]` must be a polyviz chart (the return value of",
        "pv_bar() and friends)."), i))
    }
  }
  if (!is.character(file) || length(file) != 1 || is.na(file) ||
      !nzchar(file)) {
    rlang::abort("`file` must be a single file path.")
  }
  if (tolower(tools::file_ext(file)) != "pptx") {
    rlang::abort(sprintf('`file` must end in .pptx (got "%s").',
                         basename(file)))
  }
  title <- deck_check_text(title, "title")
  subtitle <- deck_check_text(subtitle, "subtitle")
  if (!is.null(subtitle) && is.null(title)) {
    rlang::abort(paste(
      "`subtitle` needs a `title` - the opening slide only exists when",
      "`title` is given."))
  }
  if (!is.null(ratio)) {
    if (!is.character(ratio) || length(ratio) != 1 || is.na(ratio) ||
        !ratio %in% c("16:9", "4:3")) {
      rlang::abort('`ratio` must be "16:9" or "4:3".')
    }
    if (!missing(width) || !missing(height)) {
      rlang::abort(paste(
        "Give either `ratio` or explicit `width`/`height`, not both -",
        "they are two ways to say the same thing."))
    }
    size <- switch(ratio, "16:9" = c(10, 5.63), "4:3" = c(10, 7.5))
    width <- size[[1]]
    height <- size[[2]]
  }
  width <- deck_check_inches(width, "width")
  height <- deck_check_inches(height, "height")
  scale <- export_check_size(scale, "scale")
  if (!is.character(mode) || length(mode) != 1 || is.na(mode) ||
      !mode %in% c("auto", "light", "dark")) {
    rlang::abort('`mode` must be "auto", "light", or "dark".')
  }
  if (!isTRUE(quiet) && !isFALSE(quiet)) {
    rlang::abort("`quiet` must be TRUE or FALSE.")
  }
  deck_need_officer()
  deck_need_chrome()

  stage <- tempfile("pv-deck-")
  dir.create(stage)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)

  # Every capture is taken at the slide's own dimensions, so nothing is
  # stretched at placement time: 96 CSS pixels make an inch of slide.
  px_width <- round(width * 96)
  px_height <- round(height * 96)
  pngs <- character(length(charts))
  for (i in seq_along(charts)) {
    pngs[[i]] <- file.path(stage, sprintf("slide-%03d.png", i))
    pv_save(charts[[i]], pngs[[i]], width = px_width, height = px_height,
            scale = scale, mode = mode, quiet = TRUE)
  }

  # The title slide matches the surface the charts were actually rendered
  # on - the first chart's own ink tokens, not whatever theme happens to
  # be active when the deck is assembled.
  ink <- charts[[1]]$x$theme$ink %||% pv_colors$ink
  ink <- (if (mode == "dark") ink$dark else ink$light) %||% list()

  doc <- officer::read_pptx()
  doc <- deck_set_slide_size(doc, width, height)
  if (!is.null(title)) {
    doc <- deck_title_slide(doc, title, subtitle, width, height, ink)
  }
  for (i in seq_along(charts)) {
    chart_title <- charts[[i]]$x$title
    if (!is.character(chart_title) || length(chart_title) != 1 ||
        is.na(chart_title) || !nzchar(chart_title)) {
      chart_title <- "polyviz chart"
    }
    doc <- officer::add_slide(doc, layout = "Blank",
                              master = "Office Theme")
    doc <- officer::ph_with(
      doc,
      officer::external_img(pngs[[i]], width = width, height = height,
                            unit = "in", alt = chart_title),
      location = officer::ph_location(left = 0, top = 0, width = width,
                                      height = height))
    notes <- deck_notes(charts[[i]])
    if (!is.null(notes)) {
      doc <- officer::set_notes(
        doc, notes, location = officer::notes_location_type("body"))
    }
  }

  # officer writes its own file; route it through the staging directory
  # and hand the bytes to the same atomic writer pv_save() uses, so a
  # failure part-way never leaves a half-written deck at `file`.
  staged <- file.path(stage, "deck.pptx")
  print(doc, target = staged)
  export_write_atomic(file, readBin(staged, "raw", file.size(staged)))
  n <- length(charts) + !is.null(title)
  if (!quiet) {
    message(sprintf("Saved %s (%d slide%s, %s x %s in)", file, n,
                    if (n == 1) "" else "s", format_px(width),
                    format_px(height)))
  }
  invisible(file)
}
