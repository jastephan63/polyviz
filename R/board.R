# pv_board() composes several finished charts into one page straight from
# R - no Shiny, no R Markdown needed. The R side only arranges: every
# panel is the untouched widget its chart function built, laid into a CSS
# grid on the theme's surface tokens (the same token approach R/report.R
# uses), with one optional heading block and one optional source line for
# the board as a whole. Each chart keeps its own source line; the board's
# is additional. Charts linked through crosstalk (pv_link) keep their
# linking across panels, because htmltools carries each dependency once
# no matter how many panels ask for it.

# The board's stylesheet, generated from the ink tokens the board was
# built under. Everything is scoped to the .pv-board container, so a
# board dropped into an R Markdown page styles itself and nothing else.
# With mode "auto" the light tokens are the default and a
# prefers-color-scheme block swaps in the dark set - the same media-query
# approach as the report and the widgets themselves; a forced mode bakes
# in one set and skips the media query.
board_css <- function(mode, ncol, ink) {
  vars <- function(i) {
    paste0("--surface:", i$surface, ";--primary:", i$primary,
           ";--secondary:", i$secondary, ";--muted:", i$muted,
           ";--grid:", i$grid, ";--baseline:", i$baseline, ";")
  }
  base <- if (mode == "dark") ink$dark else ink$light
  dark_block <- if (mode == "auto") {
    paste0("@media (prefers-color-scheme: dark) { .pv-board { ",
           vars(ink$dark), " } }\n")
  } else {
    ""
  }
  paste0(
    ".pv-board { ", vars(base), " }\n",
    dark_block,
    ".pv-board { box-sizing: border-box; background: var(--surface);\n",
    "            color: var(--primary);\n",
    "            font-family: ", pv_font_stack(), ";\n",
    "            padding: 20px; }\n",
    ".pv-board-header { margin: 2px 2px 18px; }\n",
    ".pv-board-title { font-size: 24px; font-weight: 700;\n",
    "                  letter-spacing: -0.02em; margin: 0; }\n",
    ".pv-board-subtitle { color: var(--secondary); line-height: 1.5;\n",
    "                     margin: 4px 0 0; max-width: 640px;\n",
    "                     font-size: 14px; }\n",
    ".pv-board-grid { display: grid; gap: 18px;\n",
    "                 grid-template-columns: repeat(",
    format_px(ncol), ", minmax(0, 1fr)); }\n",
    # One column on narrow screens, same breakpoint as the report's
    # two-up grid.
    "@media (max-width: 720px) {\n",
    "  .pv-board-grid { grid-template-columns: 1fr; }\n",
    "}\n",
    # min-width lets a grid cell shrink below its content's natural
    # width, so a wide panel scrolls inside itself instead of blowing
    # the whole grid apart.
    ".pv-board-panel { min-width: 0; }\n",
    ".pv-board-caption { color: var(--secondary); font-size: 13px;\n",
    "                    font-weight: 600; margin: 0 0 6px; }\n",
    ".pv-board-source { color: var(--muted); font-size: 12.5px;\n",
    "                   line-height: 1.6;\n",
    "                   border-top: 1px solid var(--grid);\n",
    "                   padding-top: 12px; margin: 20px 2px 0; }\n")
}

# Assembles the board object from its recipe (the validated arguments
# pv_board() collected). Kept separate from pv_board() because pv_save()
# rebuilds the board from the same recipe with the mode pinned and the
# entrance animations off - one builder, two callers, identical layout.
board_build <- function(spec) {
  tg <- htmltools::tags
  widgets <- spec$widgets
  n <- length(widgets)
  rows <- ceiling(n / spec$ncol)
  row_heights <- if (!is.null(spec$heights)) {
    rep(spec$heights, length.out = rows)
  }
  captions <- names(widgets) %||% rep("", n)
  panels <- lapply(seq_len(n), function(i) {
    w <- widgets[[i]]
    # A forced board mode pins every panel too, so the page and its
    # charts can never disagree about the surface; "auto" leaves each
    # chart's own choice alone.
    if (spec$mode != "auto") {
      w$x$mode <- spec$mode
    }
    w$width <- "100%"
    if (!is.null(row_heights)) {
      w$height <- row_heights[[ceiling(i / spec$ncol)]]
    }
    caption <- captions[[i]]
    tg$div(class = "pv-board-panel",
           if (!is.na(caption) && nzchar(caption)) {
             tg$div(class = "pv-board-caption", caption)
           },
           w)
  })
  header <- if (!is.null(spec$title)) {
    tg$div(class = "pv-board-header",
           tg$div(class = "pv-board-title", spec$title),
           if (!is.null(spec$subtitle)) {
             tg$div(class = "pv-board-subtitle", spec$subtitle)
           })
  }
  source_line <- if (!is.null(spec$source)) {
    tg$div(class = "pv-board-source", spec$source)
  }
  board <- tg$div(
    class = "pv-board",
    tg$style(htmltools::HTML(board_css(spec$mode, spec$ncol, spec$ink))),
    header,
    tg$div(class = "pv-board-grid", panels),
    source_line)
  board <- htmltools::browsable(board)
  class(board) <- c("pv_board", class(board))
  # The recipe rides along so pv_save() can rebuild the board with the
  # capture mode pinned, without re-validating anything.
  attr(board, "pv_board") <- spec
  board
}

#' Compose charts into one board
#'
#' Lays several polyviz charts out as one page - a responsive CSS grid of
#' panels on the theme's surface, with an optional heading block and an
#' optional source line for the board as a whole. No Shiny and no
#' R Markdown are needed: the return value is a browsable htmltools
#' object, so it shows in the RStudio viewer when printed, drops into an
#' R Markdown or Quarto chunk like any widget, and saves to a standalone
#' page with `htmltools::save_html(board, "board.html")` (widget assets
#' land in a `lib/` folder next to the file). [pv_save()] also takes a
#' board directly: `.html` writes one self-contained file, `.png` and
#' `.pdf` capture the rendered page.
#'
#' Each chart keeps everything it was built with - its own title,
#' subtitle, and source line included; the board's `source` is an
#' additional line for the page as a whole. Naming an argument
#' (`pv_board(revenue = pv_bar(...))`) puts that name above the panel as
#' a caption.
#'
#' Charts linked with [pv_link()] stay linked on the board: a selection
#' brushed in one panel dims the unselected marks in every other panel
#' sharing the [crosstalk::SharedData] group, exactly as on any static
#' crosstalk page. The board carries the crosstalk machinery once, no
#' matter how many panels use it.
#'
#' The grid collapses to a single column on narrow screens (under
#' 720 pixels), so a board embedded in a document stays readable on a
#' phone.
#'
#' @param ... Polyviz charts, one per panel, in reading order (left to
#'   right, then down). A single list of charts is also accepted. Names
#'   become panel captions.
#' @param ncol Number of grid columns (panels per row).
#' @param title Optional board heading, shown once above the grid.
#' @param subtitle Optional line under the heading; needs `title`.
#' @param source Optional source/credit line for the whole board, shown
#'   small and grey under the grid. Each chart's own source line is
#'   untouched - this one is additional.
#' @param mode `"auto"` (default: the board and every panel follow the
#'   viewer's light/dark setting), `"light"`, or `"dark"`. A forced mode
#'   pins the board's surface and every panel's mode together, so the
#'   page can never disagree with its charts.
#' @param heights Panel heights in pixels, one number per row of the
#'   grid, recycled across rows (so a single number sets every row).
#'   `NULL` (default) keeps each widget's own height - the height it was
#'   built with, or the standard 420.
#' @return A browsable htmltools object of class `"pv_board"`. Print it
#'   to see it in the viewer, return it from an R Markdown chunk to embed
#'   it, or hand it to [pv_save()] / [htmltools::save_html()] to write it
#'   to disk.
#' @examples
#' agg <- aggregate(revenue ~ region, pv_sales, sum)
#' monthly <- aggregate(revenue ~ month, pv_sales, sum)
#' pv_board(
#'   pv_bar(agg, "region", "revenue", title = "Revenue by region"),
#'   pv_line(monthly, "month", "revenue", title = "Revenue over time"),
#'   title = "Sales at a glance",
#'   source = "Simulated pv_sales data"
#' )
#' @export
pv_board <- function(..., ncol = 2, title = NULL, subtitle = NULL,
                     source = NULL, mode = "auto", heights = NULL) {
  widgets <- list(...)
  # A single bare list of charts is unwrapped, so both spellings work:
  # pv_board(a, b) and pv_board(list(a, b)). Only a plain unclassed list
  # unwraps - a chart is itself a list, and so is a data frame, and both
  # must fall through to the panel check for its clear message.
  if (length(widgets) == 1 && is.list(widgets[[1]]) &&
      !is.object(widgets[[1]])) {
    widgets <- widgets[[1]]
  }
  if (!length(widgets)) {
    rlang::abort("`...` is empty; give pv_board() at least one chart.")
  }
  for (i in seq_along(widgets)) {
    if (!inherits(widgets[[i]], "pvchart")) {
      rlang::abort(sprintf(paste(
        "Panel %d is not a polyviz chart - every panel must be the",
        "return value of pv_bar() and friends."), i))
    }
  }
  if (!is.numeric(ncol) || length(ncol) != 1 || !is.finite(ncol) ||
      ncol < 1 || ncol != round(ncol)) {
    rlang::abort(
      "`ncol` must be a single whole number of columns, 1 or more.")
  }
  title <- deck_check_text(title, "title")
  subtitle <- deck_check_text(subtitle, "subtitle")
  source <- deck_check_text(source, "source")
  if (!is.null(subtitle) && is.null(title)) {
    rlang::abort(paste(
      "`subtitle` needs a `title` - the heading block only exists when",
      "`title` is given."))
  }
  if (!is.character(mode) || length(mode) != 1 || is.na(mode) ||
      !mode %in% c("auto", "light", "dark")) {
    rlang::abort('`mode` must be "auto", "light", or "dark".')
  }
  if (!is.null(heights)) {
    if (!is.numeric(heights) || !length(heights) ||
        any(!is.finite(heights)) || any(heights <= 0)) {
      rlang::abort(paste(
        "`heights` must be positive pixel heights, one number per row",
        "of the grid."))
    }
    heights <- as.numeric(heights)
  }
  # The board chrome takes its ink from the theme active right now, the
  # same moment the panels' own payloads took theirs.
  tokens <- the$theme %||% pv_colors
  spec <- list(widgets = widgets, ncol = as.numeric(ncol), title = title,
               subtitle = subtitle, source = source, mode = mode,
               heights = heights, ink = tokens$ink)
  board_build(spec)
}

#' Print a chart board
#'
#' Shows the board in the RStudio viewer (or the default browser), the
#' way htmlwidgets print: the page is rendered to a temporary file and
#' handed to the viewer. In a non-interactive session the file is still
#' written but no viewer opens - the same behaviour as printing a chart.
#'
#' @param x A board built by [pv_board()].
#' @param ... Ignored.
#' @param browse Open the board in the viewer? Defaults to
#'   [interactive()].
#' @return `x`, invisibly.
#' @keywords internal
#' @export
print.pv_board <- function(x, ..., browse = interactive()) {
  # The viewer page's own background matches the board surface, so no
  # white ring sits around a dark board.
  spec <- attr(x, "pv_board")
  ink <- spec$ink %||% pv_colors$ink
  surface <- if (identical(spec$mode, "dark")) {
    ink$dark$surface %||% "#1b1a18"
  } else {
    ink$light$surface %||% "#ffffff"
  }
  htmltools::html_print(x, background = surface, viewer = if (browse) {
    getOption("viewer", utils::browseURL)
  })
  invisible(x)
}

# ---- saving boards (called from pv_save in R/export.R) -----------------

# The one-file HTML page for a board: the board's rendered tags plus
# every dependency - d3, the renderers, htmlwidgets, crosstalk when any
# panel is linked, the font css - inlined into the head through the same
# machinery the single-chart path uses.
board_standalone_html <- function(board, spec, mode, embed_fonts) {
  rendered <- htmltools::renderTags(board)
  deps <- htmltools::resolveDependencies(rendered$dependencies)
  parts <- export_inline_dependencies(deps, embed_fonts)
  title <- spec$title %||% "polyviz board"
  export_standalone_page(rendered, parts,
                         export_body_css(spec$ink, mode), title)
}

# What to poll while waiting for a board to settle: the SVG count covers
# every chart, and the table-row count covers plain-table panels, which
# never grow any SVG of their own.
board_settle_count_js <- function() {
  paste0("document.querySelectorAll('svg *').length + ",
         "document.querySelectorAll('.pvchart table tr').length")
}

# The board PDF is printed from a static snapshot of the settled page,
# not from the live one, for the same reason the single-chart path
# prints a static SVG: print layout re-lays the page out, the widgets
# re-render, and the print engine catches them mid-redraw - the axes
# come out but the marks are still flat. Unlike a chart, a board has no
# one SVG to lift out (several svgs mixed with HTML chrome, possibly a
# table), so the snapshot is the page's own DOM with every script
# stripped: the drawn marks sit in the serialised svgs, nothing can
# redraw under the printer, and Chrome prints the svgs as true vectors
# with the fonts embedded. A canvas layer (the big-cloud scatter) would
# serialise empty, so each one is swapped for an image of its pixels
# first - the same trade the SVG exporter makes.
board_print_pdf <- function(b, stage, width, height) {
  res <- b$Runtime$evaluate(paste0(
    "(function () {",
    " var doc = document.documentElement.cloneNode(true);",
    " var kill = doc.querySelectorAll('script');",
    " for (var i = 0; i < kill.length; i++) {",
    "   kill[i].parentNode.removeChild(kill[i]);",
    " }",
    " var live = document.querySelectorAll('canvas');",
    " var copies = doc.querySelectorAll('canvas');",
    " for (var j = 0; j < live.length; j++) {",
    "   var img = document.createElement('img');",
    "   try { img.src = live[j].toDataURL('image/png'); } catch (e) {}",
    "   img.setAttribute('style', live[j].getAttribute('style') || '');",
    "   if (live[j].getAttribute('class')) {",
    "     img.setAttribute('class', live[j].getAttribute('class'));",
    "   }",
    "   img.style.width = live[j].clientWidth + 'px';",
    "   img.style.height = live[j].clientHeight + 'px';",
    "   copies[j].parentNode.replaceChild(img, copies[j]);",
    " }",
    " return '<!DOCTYPE html>' + doc.outerHTML;",
    " })()"), returnByValue = TRUE)
  html <- res$result$value
  if (!is.character(html) || length(html) != 1 || !nzchar(html)) {
    rlang::abort(
      "The rendered page could not be snapshotted for printing.")
  }
  # The snapshot lands next to the capture page, so its relative lib/
  # links (the font stylesheet) keep resolving.
  page <- file.path(stage, "print.html")
  writeBin(charToRaw(enc2utf8(html)), page)
  loaded <- b$Page$loadEventFired(wait_ = FALSE)
  b$Page$navigate(utils::URLencode(paste0("file://", normalizePath(page))),
                  wait_ = FALSE)
  b$wait_for(loaded)
  # The fonts still have to be decoded before printing.
  tryCatch(b$Runtime$evaluate(
    "document.fonts.ready.then(function () { return true; })",
    awaitPromise = TRUE), error = function(e) NULL)
  base64enc::base64decode(b$Page$printToPDF(
    paperWidth = width / 96, paperHeight = height / 96,
    marginTop = 0, marginBottom = 0, marginLeft = 0, marginRight = 0,
    printBackground = TRUE, pageRanges = "1")$data)
}

# pv_save()'s board path. Same argument meanings as the chart path, but
# the whole page is what gets sized and captured: `width` is the page
# width, and `height = NULL` captures the page at the natural height the
# board renders to at that width.
board_save <- function(board, file, width, height, scale, mode, delay,
                       embed_fonts, quiet) {
  spec <- attr(board, "pv_board")
  if (!is.list(spec) || !length(spec$widgets)) {
    rlang::abort(paste(
      "`widget` is a pv_board without its build recipe - build boards",
      "with pv_board()."))
  }
  if (!is.character(file) || length(file) != 1 || is.na(file) ||
      !nzchar(file)) {
    rlang::abort("`file` must be a single file path.")
  }
  format <- export_format(file)
  # A board has no single vector drawing to serialise and no animation
  # timeline to sample, so the two formats built from those have nothing
  # to build from - the same reasoning as the plain table.
  if (format %in% c("svg", "gif")) {
    rlang::abort(sprintf(paste(
      "A .%s file cannot hold a chart board - save it as .png, .pdf,",
      "or .html instead."), format))
  }
  width <- export_check_size(width, "width")
  if (!is.null(height)) {
    height <- export_check_size(height, "height")
  }
  scale <- export_check_size(scale, "scale")
  if (!is.character(mode) || length(mode) != 1 || is.na(mode) ||
      !mode %in% c("auto", "light", "dark")) {
    rlang::abort('`mode` must be "auto", "light", or "dark".')
  }
  if (!is.numeric(delay) || length(delay) != 1 || !is.finite(delay) ||
      delay < 0) {
    rlang::abort("`delay` must be a single non-negative number of seconds.")
  }
  if (!isTRUE(embed_fonts) && !isFALSE(embed_fonts)) {
    rlang::abort("`embed_fonts` must be TRUE or FALSE.")
  }
  if (!isTRUE(quiet) && !isFALSE(quiet)) {
    rlang::abort("`quiet` must be TRUE or FALSE.")
  }

  # The board is rebuilt from its recipe with the save mode pinned -
  # board_build() pins every panel along with the chrome.
  spec$mode <- mode

  if (format == "html") {
    rebuilt <- board_build(spec)
    html <- board_standalone_html(rebuilt, spec, mode, embed_fonts)
    export_write_atomic(file, charToRaw(enc2utf8(html)))
    if (!quiet) {
      message(sprintf("Saved %s (self-contained board html)", file))
    }
    return(invisible(file))
  }

  export_need_chrome(format)

  # The capture copy: every panel with its entrance animation off, so
  # the settled page can never show a half-grown chart.
  spec$widgets <- lapply(spec$widgets, function(w) {
    w$x$duration <- 0
    w
  })
  rebuilt <- board_build(spec)

  stage <- tempfile("pv-board-")
  dir.create(stage)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  page <- file.path(stage, "board.html")
  surface_ink <- if (mode == "dark") spec$ink$dark else spec$ink$light
  surface <- surface_ink$surface %||% "#ffffff"
  # save_html builds the page shell and copies every widget dependency
  # into lib/ next to it; the head style clears the body margin so the
  # capture is exactly the board, edge to edge.
  page_tags <- htmltools::tagList(
    htmltools::tags$head(
      if (!is.null(spec$title)) htmltools::tags$title(spec$title),
      htmltools::tags$style(htmltools::HTML("body{margin:0;}"))),
    rebuilt)
  htmltools::save_html(page_tags, file = page, background = surface,
                       libdir = "lib")

  # Same extra patience for Chrome's debugging port as the chart path.
  if (is.null(getOption("chromote.timeout"))) {
    old_opts <- options(chromote.timeout = 30)
    on.exit(options(old_opts), add = TRUE)
  }
  b <- export_chrome_session(as.integer(round(width)),
                             as.integer(round(height %||% 800)))
  on.exit(try(b$close(), silent = TRUE), add = TRUE)
  errors <- export_watch_errors(b)
  loaded <- b$Page$loadEventFired(wait_ = FALSE)
  b$Page$navigate(utils::URLencode(paste0("file://", normalizePath(page))),
                  wait_ = FALSE)
  b$wait_for(loaded)
  count_js <- board_settle_count_js()
  export_wait_settled(b, errors, delay, count_js)

  # With no height given, the page is captured at its natural height:
  # the board's own rendered size at this width.
  if (is.null(height)) {
    h <- tryCatch(b$Runtime$evaluate(paste0(
      "Math.ceil(document.querySelector('.pv-board')",
      ".getBoundingClientRect().height)"))$result$value,
      error = function(e) NULL)
    if (!is.numeric(h) || !is.finite(h) || h <= 0) {
      rlang::abort(
        "The rendered page holds no board; the capture failed.")
    }
    height <- as.numeric(h)
  }
  if (format == "png") {
    # Resize the viewport to the capture size (and resolution). The
    # resize re-renders every panel, so wait for the new drawing to
    # settle - with the caller's own delay again, since the panels'
    # entrance stagger runs on wall-clock delays even at duration zero.
    b$Emulation$setDeviceMetricsOverride(
      width = as.integer(round(width)), height = as.integer(round(height)),
      deviceScaleFactor = scale, mobile = FALSE)
    export_wait_settled(b, errors, delay, count_js)
  }
  if (length(errors$msgs)) {
    rlang::warn(sprintf(
      "JavaScript error while rendering the board: %s",
      sub("\n[\\s\\S]*$", "", errors$msgs[[1]], perl = TRUE)))
  }

  bytes <- switch(format,
    png = base64enc::base64decode(
      b$Page$captureScreenshot(format = "png")$data),
    pdf = board_print_pdf(b, stage, width, height))
  export_write_atomic(file, bytes)
  if (!quiet) {
    extra <- if (format == "png") {
      sprintf(" at %sx", format_px(scale))
    } else {
      ""
    }
    message(sprintf("Saved %s (board %s, %s x %s px%s)", file, format,
                    format_px(width), format_px(height), extra))
  }
  invisible(file)
}
