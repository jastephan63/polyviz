# Charts dropped into an R Markdown document "just work" in every output
# format. HTML documents get the live interactive widget, exactly as
# before. Static formats - Word, PDF, plain markdown - cannot run d3, so
# there the chart is rendered to a PNG by pv_save() at the chunk's figure
# size and included like any other figure. Knitting a chart into a paper
# therefore needs no extra code in the document.

# One knit can print several charts from the same chunk; each needs its
# own file name. The counter lives in opts_knit, which knitr snapshots at
# the start of every knit and restores at the end, so numbering restarts
# cleanly for each document and never collides within one.
knit_fig_number <- function(options) {
  key <- paste0(options$fig.path %||% "", "\r", options$label %||% "")
  state <- knitr::opts_knit$get("polyviz.fig.count")
  if (is.null(state) || !identical(state$key, key)) {
    state <- list(key = key, n = 0L)
  }
  state$n <- state$n + 1L
  knitr::opts_knit$set(polyviz.fig.count = state)
  state$n
}

# The pixel geometry for a knitted capture. The chunk's fig.width and
# fig.height (inches) set the CSS pixel size at 96 px per inch - the size
# the chart lays itself out for - and dpi scales the capture up or down
# from there, so the file ends up fig.width * dpi pixels wide. A chunk
# with no usable figure options falls back to pv_save()'s own defaults.
knit_fig_size <- function(options) {
  fw <- options$fig.width[1]
  fh <- options$fig.height[1]
  dpi <- options$dpi[1] %||% 96
  usable <- function(v) {
    is.numeric(v) && length(v) == 1 && is.finite(v) && v > 0
  }
  if (usable(fw) && usable(fh) && usable(dpi)) {
    list(width = fw * 96, height = fh * 96, scale = dpi / 96)
  } else {
    list(width = 900, height = NULL, scale = 2)
  }
}

#' Print a chart from a knitr document
#'
#' The method knitr calls when a chunk's value is a polyviz chart. When
#' the document is becoming real HTML the live interactive widget is
#' embedded, exactly as htmlwidgets always does. For every other output —
#' Word, PDF via LaTeX, plain or GitHub-flavoured markdown, epub — the
#' chart is rendered to a PNG by [pv_save()] at the chunk's `fig.width`
#' by `fig.height` and `dpi`, written into the chunk's figure path, and
#' included like an ordinary figure. The capture waits for the chart to
#' settle, so it can never show a half-grown entrance animation. Charts
#' built with `mode = "auto"` print light; a chart forced dark stays
#' dark.
#'
#' One line of setup is needed in the document, because knitr otherwise
#' reaches for its own generic widget screenshotter (the webshot package)
#' before asking the chart how it wants to be printed:
#'
#' ```
#' knitr::opts_chunk$set(screenshot.force = FALSE)
#' ```
#'
#' Put it in the setup chunk of any document knitted to Word, PDF, or
#' markdown. (Without it, documents still knit on machines where webshot
#' is not installed — knitr then falls through to this method by itself.)
#'
#' The static path needs the chromote package and a Chrome-based browser,
#' the same as [pv_save()].
#'
#' The chart's alt text (generated at build time, or set with
#' [pv_alt()]) rides along as the figure's `fig.alt`, so a pandoc-driven
#' knit to markdown, GitHub-flavoured markdown, or epub keeps an
#' accessible description of the chart on the image. `fig.alt` or
#' `fig.cap` chunk options set by the author always win — the figure
#' then goes through knitr's stock handling. Word, PDF, and the other
#' pandoc office formats keep their usual figure markup untouched, as
#' knitr itself declares `fig.alt` unsupported there.
#'
#' @param x A polyviz chart.
#' @param ... Passed on to the next method.
#' @param options The knitr chunk options.
#' @return An object for knitr to include in the document: the widget's
#'   HTML, or the path of the rendered PNG.
#' @keywords internal
#' @exportS3Method knitr::knit_print
knit_print.pvchart <- function(x, ..., options = NULL) {
  # knitr counts plain markdown as HTML-ish because it usually ends up
  # there, but a .md file cannot run d3 any more than Word can, so the
  # markdown flavours are excluded here and get the PNG too.
  html_out <- knitr::is_html_output(
    excludes = c("markdown", "epub", "epub2", "gfm"))
  if (!isTRUE(getOption("knitr.in.progress")) || html_out) {
    return(NextMethod())
  }
  options <- options %||% list()
  fig <- knit_fig_size(options)
  path <- paste0(options$fig.path %||% "figure/",
                 options$label %||% "polyviz",
                 "-pv-", knit_fig_number(options), ".png")
  mode <- if (identical(x$x$mode, "dark")) "dark" else "light"
  pv_save(x, path, width = fig$width, height = fig$height,
          scale = fig$scale, mode = mode, quiet = TRUE)
  img <- knitr::include_graphics(path)
  # The chart's alt text goes out through knitr's own fig.alt mechanism,
  # but only where that mechanism is clean. A pandoc-driven knit to a
  # markdown-family or HTML-carrying output (gfm, epub, plain markdown)
  # renders it as the image's alt attribute. Everything else keeps the
  # untouched include_graphics() path: for Word and the other office
  # formats knitr declares fig.alt unsupported and would warn on every
  # chunk; for LaTeX a set fig.alt reroutes the figure through a
  # different hook; without pandoc (or with a fig.cap) knitr auto-writes
  # a caption, and its htmlwidget handling would then wrap the figure
  # twice. An author's own fig.alt chunk option also takes the untouched
  # path: knitr applies it there by itself.
  alt <- x$x$alt
  to <- knitr::pandoc_to()
  clean <- !is.null(to) &&
    !to %in% c("docx", "pptx", "rtf", "odt", "latex", "beamer", "context")
  if (clean && is.null(options$fig.alt) && is.null(options$fig.cap) &&
      alt_str(alt) && !is.null(options$fig.show) &&
      "sew" %in% getNamespaceExports("knitr")) {
    options$fig.alt <- alt
    # Sewing here, with the amended options, is exactly what knitr would
    # do with this object one step later - just with fig.alt filled in.
    return(knitr::asis_output(paste(knitr::sew(img, options),
                                    collapse = "")))
  }
  img
}
