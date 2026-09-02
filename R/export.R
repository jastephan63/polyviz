# pv_save() writes a chart to disk in the formats a paper or a slide deck
# needs. For .png, .svg, and .pdf the chart is rendered for real - headless
# Chrome loads the same HTML/d3 pipeline the interactive widget uses - and
# the settled result is captured as pixels, a standalone SVG document, or a
# true vector PDF. For .gif the same page is asked for exact frames along
# the bar-chart race's timeline, and gifski strings them into a looping
# animation. The .html format is different: it is assembled purely in R,
# with every script, stylesheet, and font the widget needs inlined into
# one file, so it works with no browser and no pandoc installed.

# The output format is named by the file extension alone.
export_format <- function(file) {
  ext <- tolower(tools::file_ext(file))
  if (!ext %in% c("png", "svg", "pdf", "gif", "html")) {
    rlang::abort(sprintf(
      '`file` must end in .png, .svg, .pdf, .gif, or .html (got "%s").',
      basename(file)))
  }
  ext
}

# The height a widget was built with, when it was a fixed pixel number,
# else the package's standard 560. Widgets usually leave height NULL and
# let the page decide, so most saves land on the default.
export_fixed_height <- function(widget) {
  h <- widget$height
  if (is.numeric(h) && length(h) == 1 && is.finite(h) && h > 0) {
    return(as.numeric(h))
  }
  if (is.character(h) && length(h) == 1 && !is.na(h) &&
      grepl("^[0-9.]+(px)?$", h)) {
    return(as.numeric(sub("px$", "", h)))
  }
  560
}

export_check_size <- function(value, name) {
  if (!is.numeric(value) || length(value) != 1 || !is.finite(value) ||
      value <= 0) {
    rlang::abort(sprintf(
      "`%s` must be a single positive number of pixels.", name))
  }
  as.numeric(value)
}

export_mime <- function(path) {
  switch(tolower(tools::file_ext(path)),
         woff2 = "font/woff2",
         woff = "font/woff",
         ttf = "font/ttf",
         otf = "font/otf",
         png = "image/png",
         jpg = ,
         jpeg = "image/jpeg",
         gif = "image/gif",
         svg = "image/svg+xml",
         css = "text/css",
         js = "application/javascript",
         "application/octet-stream")
}

export_data_uri <- function(path) {
  paste0("data:", export_mime(path), ";base64,",
         base64enc::base64encode(path))
}

export_read_file <- function(path) {
  if (!file.exists(path)) {
    rlang::abort(sprintf("Widget dependency file not found: %s.", path))
  }
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

# Replaces relative url(...) references in a stylesheet with base64 data
# URIs, so the css carries its font (or image) files inside itself. Refs
# that are already absolute, or that point at files that are not there,
# are left alone.
export_embed_css_urls <- function(css, dir) {
  pattern <- "url\\(\\s*['\"]?([^)'\"]+?)['\"]?\\s*\\)"
  found <- regmatches(css, gregexpr(pattern, css, perl = TRUE))[[1]]
  for (ref_call in unique(found)) {
    ref <- sub(pattern, "\\1", ref_call, perl = TRUE)
    if (grepl("^(data:|https?:|//)", ref)) next
    path <- file.path(dir, ref)
    if (!file.exists(path)) next
    css <- gsub(ref_call, paste0("url(", export_data_uri(path), ")"),
                css, fixed = TRUE)
  }
  css
}

# The @font-face rule for the bundled Inter, with the woff2 inlined as a
# data URI - what makes an exported SVG carry its own font.
export_font_css <- function() {
  font <- system.file("htmlwidgets", "lib", "pv-fonts",
                      "InterVariable.woff2", package = "polyviz")
  if (!nzchar(font)) {
    return("")
  }
  paste0('@font-face{font-family:"InterVariable";font-style:normal;',
         "font-weight:100 900;src:url(", export_data_uri(font),
         ') format("woff2");}')
}

export_svg_embed_font <- function(svg) {
  css <- export_font_css()
  if (!nzchar(css)) {
    return(svg)
  }
  # A <style> element directly inside the root <svg> applies to the whole
  # document, nested plot svgs included.
  sub("(<svg[^>]*>)", paste0("\\1<style>", css, "</style>"), svg)
}

# A dependency's script/stylesheet entry is usually a character vector of
# file names, but htmltools also allows list entries carrying attributes.
export_dep_files <- function(entry) {
  if (is.null(entry)) {
    return(character())
  }
  if (is.character(entry)) {
    return(entry)
  }
  if (is.list(entry) && !is.null(entry$src)) {
    return(as.character(entry$src))
  }
  vapply(entry, function(e) {
    if (is.list(e)) as.character(e$src) else as.character(e)
  }, character(1))
}

# Builds the one-file HTML page: the widget's rendered tags plus every
# dependency (d3, the renderers, the font css) inlined into the head, in
# the same order a saveWidget() page would load them from disk.
export_standalone_html <- function(widget, mode, embed_fonts) {
  tags <- htmltools::as.tags(widget, standalone = TRUE)
  rendered <- htmltools::renderTags(tags)
  deps <- htmltools::resolveDependencies(rendered$dependencies)
  parts <- character()
  for (dep in deps) {
    root <- if (is.null(dep$package)) {
      dep$src$file
    } else {
      system.file(dep$src$file, package = dep$package)
    }
    for (sheet in export_dep_files(dep$stylesheet)) {
      path <- file.path(root, sheet)
      css <- export_read_file(path)
      if (embed_fonts) {
        css <- export_embed_css_urls(css, dirname(path))
      }
      parts <- c(parts, paste0("<style>\n", css, "\n</style>"))
    }
    for (script in export_dep_files(dep$script)) {
      path <- file.path(root, script)
      js <- export_read_file(path)
      parts <- c(parts, if (grepl("</script", js, fixed = TRUE)) {
        # A literal closing tag inside the code would cut the inline
        # element short, so such a file rides along as a data URL.
        paste0('<script src="', export_data_uri(path), '"></script>')
      } else {
        paste0("<script>\n", js, "\n</script>")
      })
    }
    if (!is.null(dep$head)) {
      parts <- c(parts, as.character(dep$head))
    }
  }
  # The page background matches the chart surface, so nothing flashes and
  # the margins around the widget blend in. "auto" keeps both surfaces
  # live through a media query, the way the widget itself behaves.
  ink <- widget$x$theme$ink
  light_bg <- ink$light$surface %||% "#ffffff"
  dark_bg <- ink$dark$surface %||% "#1b1a18"
  body_css <- switch(mode,
    light = paste0("body{margin:0;background-color:", light_bg, ";}"),
    dark = paste0("body{margin:0;background-color:", dark_bg, ";}"),
    paste0("body{margin:0;background-color:", light_bg, ";}",
           "@media (prefers-color-scheme: dark){body{background-color:",
           dark_bg, ";}}"))
  title <- widget$x$title
  if (!is.character(title) || length(title) != 1 || is.na(title) ||
      !nzchar(title)) {
    title <- "polyviz chart"
  }
  paste0(
    "<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n",
    "<meta charset=\"utf-8\"/>\n",
    "<meta name=\"viewport\" content=\"width=device-width, ",
    "initial-scale=1\"/>\n",
    "<title>", htmltools::htmlEscape(title), "</title>\n",
    "<style>", body_css, "</style>\n",
    paste(parts, collapse = "\n"), "\n",
    paste(as.character(rendered$head), collapse = "\n"), "\n",
    "</head>\n<body>\n",
    paste(as.character(rendered$html), collapse = "\n"),
    "\n</body>\n</html>\n")
}

# Thin wrapper so tests can pretend chromote is not installed.
export_has_chromote <- function() {
  requireNamespace("chromote", quietly = TRUE)
}

# The three capture formats need headless Chrome; fail before any work
# with a message that says what to install and which format works anyway.
export_need_chrome <- function(format) {
  if (!export_has_chromote()) {
    rlang::abort(sprintf(paste(
      "Saving .%s files needs the chromote package (and a Chrome-based",
      'browser). Install it with install.packages("chromote"), or save',
      "as .html, which works without a browser."), format))
  }
  chrome <- tryCatch(chromote::find_chrome(), error = function(e) NULL)
  if (is.null(chrome) || !nzchar(chrome)) {
    rlang::abort(sprintf(paste(
      "Saving .%s files needs a Chrome-based browser and none was found.",
      "Install Google Chrome (or point the CHROMOTE_CHROME environment",
      "variable at a Chromium binary), or save as .html, which works",
      "without a browser."), format))
  }
}

# Thin wrapper so tests can pretend gifski is not installed.
export_has_gifski <- function() {
  requireNamespace("gifski", quietly = TRUE)
}

# The GIF encoder is optional like the browser stack; fail before any
# browser work with a message that says what to install.
export_need_gifski <- function() {
  if (!export_has_gifski()) {
    rlang::abort(paste(
      "Saving .gif files needs the gifski package. Install it with",
      'install.packages("gifski").'))
  }
}

# Collects JavaScript failures from the page: uncaught exceptions and
# console.error calls both land in one growing character vector.
export_watch_errors <- function(b) {
  errors <- new.env(parent = emptyenv())
  errors$msgs <- character()
  b$Runtime$enable()
  b$Runtime$exceptionThrown(callback_ = function(params) {
    d <- params$exceptionDetails
    msg <- NULL
    if (!is.null(d$exception)) {
      msg <- d$exception$description %||% d$exception$value
    }
    msg <- msg %||% d$text %||% "unknown JavaScript error"
    errors$msgs <- c(errors$msgs, paste(format(msg), collapse = " "))
  })
  b$Runtime$consoleAPICalled(callback_ = function(params) {
    if (!identical(params$type, "error")) {
      return()
    }
    text <- vapply(params$args, function(a) {
      paste(format(a$value %||% a$description %||% ""), collapse = " ")
    }, character(1))
    text <- trimws(paste(text, collapse = " "))
    if (nzchar(text)) {
      errors$msgs <- c(errors$msgs, text)
    }
  })
  errors
}

# What to poll while waiting for the chart to settle. Every chart draws
# into SVG except the table, so counting SVG elements is the general
# signal - but a table without sparklines never grows any, and polling
# for them would sit out the whole timeout. That table settles on its
# own DOM: the count of <table> rows. A table with a spark column draws
# real SVG (the sparklines) and keeps the general signal.
export_settle_count_js <- function(widget) {
  if (identical(widget$x$type, "table")) {
    spark <- vapply(widget$x$columns, function(col) {
      identical(col$type, "spark")
    }, logical(1))
    if (!any(spark)) {
      return("document.querySelectorAll('.pvchart table tr').length")
    }
  }
  "document.querySelectorAll('svg *').length"
}

# Waits until the page has actually drawn the chart. The renderers draw
# synchronously once their scripts run, but scripts and fonts arrive
# asynchronously even from file://, so poll the number of drawn elements
# (`count_js`, SVG children for every chart but the plain table) until it
# stops changing (or a JavaScript error makes waiting pointless), wait
# for the web font, then honour the caller's extra delay.
export_wait_settled <- function(b, errors, delay, count_js = NULL) {
  count_js <- count_js %||% "document.querySelectorAll('svg *').length"
  last <- -1
  stable <- 0
  for (i in seq_len(40)) {
    if (length(errors$msgs)) break
    n <- tryCatch(b$Runtime$evaluate(count_js)$result$value,
                  error = function(e) NULL)
    n <- if (is.numeric(n)) n else -1
    if (n > 0 && n == last) {
      stable <- stable + 1
      if (stable >= 2) break
    } else {
      stable <- 0
    }
    last <- n
    Sys.sleep(0.1)
  }
  # A capture taken a frame before the font finishes loading would fall
  # back to the system font, so wait for it explicitly.
  tryCatch(b$Runtime$evaluate(
    "document.fonts.ready.then(function () { return true; })",
    awaitPromise = TRUE), error = function(e) NULL)
  if (delay > 0) {
    Sys.sleep(delay)
  }
}

# Serialises the settled chart on the page into one standalone SVG
# document, via the exporter that ships with the widget's JavaScript.
export_page_svg <- function(b) {
  res <- b$Runtime$evaluate(paste0(
    "(function () {",
    " var el = document.querySelector('.pvchart');",
    " if (!el || !window.pv || !window.pv.toStandaloneSvg) return '';",
    " return window.pv.toStandaloneSvg(el, el.__pvLastX || null, null);",
    " })()"), returnByValue = TRUE)
  svg <- res$result$value
  if (!is.character(svg) || length(svg) != 1 || !nzchar(svg)) {
    rlang::abort(
      "The rendered page produced no SVG; the chart failed to draw.")
  }
  svg
}

# The PDF is printed from a static page holding the settled chart as one
# SVG, not from the live widget: print layout resizes the page, which
# would make the live widget re-render and the print engine would catch
# it mid-redraw. A page with no JavaScript cannot change under the
# printer, and Chrome prints the SVG as true vectors, embedding the
# chart's font. 96 CSS pixels make an inch of paper, so the page is
# pinned to exactly the capture size and nothing paginates or scales.
export_print_pdf <- function(b, stage, width, height, title) {
  svg <- export_svg_embed_font(export_page_svg(b))
  if (!is.character(title) || length(title) != 1 || is.na(title) ||
      !nzchar(title)) {
    title <- "polyviz chart"
  }
  page <- file.path(stage, "print.html")
  writeBin(charToRaw(enc2utf8(paste0(
    "<!DOCTYPE html>\n<html><head><meta charset=\"utf-8\"/>",
    # The page title becomes the PDF's document title.
    "<title>", htmltools::htmlEscape(title), "</title><style>",
    sprintf("@page{size:%spx %spx;margin:0;}",
            format_px(width), format_px(height)),
    "html,body{margin:0;padding:0;}svg{display:block;}",
    "</style></head><body>", svg, "</body></html>"))), page)
  loaded <- b$Page$loadEventFired(wait_ = FALSE)
  b$Page$navigate(utils::URLencode(paste0("file://", normalizePath(page))),
                  wait_ = FALSE)
  b$wait_for(loaded)
  # The embedded font still has to be decoded before printing.
  tryCatch(b$Runtime$evaluate(
    "document.fonts.ready.then(function () { return true; })",
    awaitPromise = TRUE), error = function(e) NULL)
  base64enc::base64decode(b$Page$printToPDF(
    paperWidth = width / 96, paperHeight = height / 96,
    marginTop = 0, marginBottom = 0, marginLeft = 0, marginRight = 0,
    printBackground = TRUE, preferCSSPageSize = TRUE,
    pageRanges = "1")$data)
}

# The GIF is not captured off the live animation. The race runs on one
# d3.timer, and screenshots of a running page - even under Chrome's
# virtual-time clock - tie every frame to when the compositor happens to
# produce one, which is exactly the timing flakiness a capture must not
# have. Instead the race renderer exposes its draw() as a seek hook on
# the chart element (motion.js), the page loads settled like every other
# capture with nothing animating on its own, and each frame is the chart
# drawn at one exact position on the race's own timeline, screenshotted,
# and handed to gifski. Same chart, same frames, every run.
export_capture_gif <- function(b, widget, width, height, scale, fps,
                               stage) {
  seek <- function(s) {
    res <- b$Runtime$evaluate(sprintf(paste0(
      "(function () {",
      " var el = document.querySelector('.pvchart');",
      " return (el && el.__pvRaceSeek) ? el.__pvRaceSeek(%.6f) : -1;",
      " })()"), s), returnByValue = TRUE)
    res$result$value
  }
  if (!identical(seek(0) > 1, TRUE)) {
    rlang::abort(
      "The rendered page exposed no race to seek; the chart failed to draw.")
  }

  # The frame schedule mirrors the interactive tempo (motion.js): one
  # keyframe step takes max(200, 900 * duration / 500) ms, and a
  # duration of 0 - the no-autoplay still - replays at the default
  # tempo, so its GIF runs at that tempo too.
  k <- length(widget$x$times)
  duration <- widget$x$duration %||% 500
  step_ms <- if (duration > 0) max(200, 900 * duration / 500) else 900
  pos <- seq(0, (k - 1) * step_ms, by = 1000 / fps) / step_ms
  if (pos[length(pos)] < k - 1) {
    pos <- c(pos, k - 1)
  }

  frame_dir <- file.path(stage, "frames")
  dir.create(frame_dir)
  files <- file.path(frame_dir, sprintf("frame-%05d.png", seq_along(pos)))
  for (i in seq_along(pos)) {
    seek(pos[i])
    writeBin(base64enc::base64decode(
      b$Page$captureScreenshot(format = "png")$data), files[i])
  }

  # Two holds bracket the run: the starting order shows briefly (as the
  # interactive race holds its opening frame) and the final standings
  # hold about two seconds before the loop restarts. gifski takes one
  # delay for all frames but merges consecutive identical images into a
  # single longer-showing frame, so the holds ride in as repeated file
  # names rather than a delay vector it would ignore.
  files <- c(rep(files[1], round(0.4 * fps)), files,
             rep(files[length(files)], round(2 * fps)))
  gif <- file.path(stage, "animation.gif")
  gifski::gifski(files, gif, width = round(width * scale),
                 height = round(height * scale), delay = 1 / fps,
                 loop = TRUE, progress = FALSE)
  bytes <- readBin(gif, "raw", file.size(gif))
  attr(bytes, "frames") <- length(pos)
  bytes
}

# Writes through a temporary file in the target directory and renames it
# into place, so a failure part-way never leaves a half-written file.
export_write_atomic <- function(file, bytes) {
  dir <- dirname(file)
  if (!dir.exists(dir) &&
      !dir.create(dir, recursive = TRUE, showWarnings = FALSE)) {
    rlang::abort(sprintf(
      "Cannot create the directory for `file` (%s).", dir))
  }
  tmp <- tempfile(pattern = ".pv-save-", tmpdir = dir)
  on.exit(if (file.exists(tmp)) unlink(tmp), add = TRUE)
  writeBin(bytes, tmp)
  # On Windows a rename onto an existing file refuses; clear the target
  # and try once more before giving up.
  if (!suppressWarnings(file.rename(tmp, file))) {
    if (file.exists(file)) unlink(file)
    if (!suppressWarnings(file.rename(tmp, file)) &&
        !file.copy(tmp, file, overwrite = TRUE)) {
      rlang::abort(sprintf("Failed to write %s.", file))
    }
  }
  invisible(file)
}

format_px <- function(v) {
  format(v, scientific = FALSE, trim = TRUE)
}

#' Save a chart as PNG, SVG, PDF, GIF, or self-contained HTML
#'
#' Writes a polyviz chart to disk in the format named by the file
#' extension, ready to drop into a paper, a slide deck, or an email. The
#' pixel and vector formats are captured from a real render: headless
#' Chrome loads the same d3 pipeline the interactive widget uses, the
#' entrance animation is switched off and the colour `mode` is pinned, and
#' the settled chart is saved once nothing is still drawing — a capture
#' can never catch a mid-animation frame or inherit the machine's dark
#' mode. JavaScript errors raised while the page renders are reported as
#' an R warning quoting the error.
#'
#' The formats:
#'
#' * `.png` — raster capture at `scale` times the pixel size (the default
#'   2 is a crisp "retina" image).
#' * `.svg` — one standalone SVG document with the title, legend, plot,
#'   and source line all included as vector shapes and text.
#' * `.pdf` — a true vector PDF from Chrome's print engine, sized exactly
#'   to the chart; ideal for LaTeX.
#' * `.gif` — the bar-chart race as a looping animated GIF (needs the
#'   gifski package). Frames are rendered one by one at exact positions
#'   on the race's timeline — never screenshotted off the running
#'   animation, so the file comes out identical on every run — at the
#'   tempo the chart's `duration` sets, `fps` frames per second. The
#'   starting order holds briefly and the final standings hold about two
#'   seconds before the loop restarts. Only [pv_race()] charts can be
#'   saved this way: every other chart, the bump chart included, only
#'   animates its entrance, and a GIF of an entrance effect is no use in
#'   a paper or a deck — asking for one is an error pointing at `.png`.
#' * `.html` — the interactive widget as one self-contained file (every
#'   script, stylesheet, and font inlined). This format needs neither
#'   Chrome nor pandoc, and it keeps the entrance animation.
#'
#' @param widget A polyviz chart, as returned by [pv_bar()] and friends.
#' @param file Output path; the extension (`.png`, `.svg`, `.pdf`, `.gif`,
#'   or `.html`) picks the format.
#' @param width Chart width in CSS pixels.
#' @param height Chart height in CSS pixels. `NULL` (default) uses the
#'   fixed height the widget was built with, if any, and 560 otherwise.
#' @param scale Resolution multiplier for `.png` output: the saved image
#'   is `scale` times `width` by `scale` times `height` device pixels.
#'   `.gif` frames are captured at the same multiplier. The other formats
#'   are resolution-independent and ignore it.
#' @param mode `"light"` (default), `"dark"`, or `"auto"`, forced onto the
#'   saved chart. In an `.html` file `"auto"` keeps the light/dark
#'   switching live; for captures it renders as light.
#' @param delay Extra seconds to wait after the chart looks settled,
#'   before capturing. Raise it for charts that keep loading things.
#' @param embed_fonts Inline the bundled Inter font into `.svg` and
#'   `.html` output as a base64 data URI, so the file renders with the
#'   right type anywhere. `FALSE` keeps the file smaller and falls back
#'   to a locally installed Inter (or the system font); print workflows
#'   that post-process the SVG with Inter installed may prefer that.
#'   `.png` and `.pdf` always carry their fonts and ignore this.
#' @param quiet Skip the one-line message saying what was saved?
#' @param fps Frames per second for `.gif` output, from 1 to 50 (GIF time
#'   steps are hundredths of a second, so 50 is the format's practical
#'   ceiling). The other formats ignore it.
#' @return The output path, invisibly.
#' @examplesIf interactive()
#' agg <- aggregate(revenue ~ region, pv_sales, sum)
#' w <- pv_bar(agg, "region", "revenue", title = "Revenue by region")
#' pv_save(w, file.path(tempdir(), "revenue.png"))
#' pv_save(w, file.path(tempdir(), "revenue.svg"))
#' pv_save(w, file.path(tempdir(), "revenue.pdf"))
#' pv_save(w, file.path(tempdir(), "revenue.html"))
#'
#' r <- pv_race(pv_city_population, time = "year", id = "city",
#'              value = "population", top_n = 8)
#' pv_save(r, file.path(tempdir(), "race.gif"))
#' @export
pv_save <- function(widget, file, width = 900, height = NULL, scale = 2,
                    mode = "light", delay = 0.5, embed_fonts = TRUE,
                    quiet = FALSE, fps = 20) {
  if (!inherits(widget, "pvchart")) {
    rlang::abort(paste(
      "`widget` must be a polyviz chart (the return value of pv_bar()",
      "and friends)."))
  }
  if (!is.character(file) || length(file) != 1 || is.na(file) ||
      !nzchar(file)) {
    rlang::abort("`file` must be a single file path.")
  }
  format <- export_format(file)
  width <- export_check_size(width, "width")
  height <- export_check_size(height %||% export_fixed_height(widget),
                              "height")
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
  if (!is.numeric(fps) || length(fps) != 1 || !is.finite(fps) ||
      fps < 1 || fps > 50) {
    rlang::abort(
      "`fps` must be a single number of frames per second from 1 to 50.")
  }
  # The table is the one chart drawn as HTML rather than SVG, so the two
  # formats built from the chart's vector drawing have nothing to build
  # from: an .svg would carry the text but none of the table itself, and
  # a .gif captures an animation a table does not have. The raster, PDF,
  # and page formats all work.
  if (format %in% c("svg", "gif") && identical(widget$x$type, "table")) {
    rlang::abort(sprintf(paste(
      "A .%s file cannot hold an HTML table - save it as .png, .pdf,",
      "or .html instead."), format))
  }
  # The race is the only chart whose animation is a data timeline; every
  # other chart merely animates its entrance, and nobody needs a GIF of
  # a fade-in when a .png shows the same finished chart.
  if (format == "gif" && !identical(widget$x$type, "race")) {
    rlang::abort(sprintf(paste(
      'A .gif captures the bar-chart race animation, and a "%s" chart',
      "has no animation to capture - save it as .png instead."),
      widget$x$type))
  }

  w <- widget
  w$x$mode <- mode

  if (format == "html") {
    html <- export_standalone_html(w, mode, embed_fonts)
    export_write_atomic(file, charToRaw(enc2utf8(html)))
    if (!quiet) {
      message(sprintf("Saved %s (self-contained html)", file))
    }
    return(invisible(file))
  }

  if (format == "gif") {
    export_need_gifski()
  }
  export_need_chrome(format)

  # The capture copy: entrance animation off, and no fixed size of its
  # own - the widget fills the page, and the page is opened at exactly
  # width x height, so the browser never resizes (a resize would
  # re-render the chart mid-capture). The GIF starts from the same still
  # page - its frames come from the seek hook, never from live playback.
  w$x$duration <- 0
  w$width <- NULL
  w$height <- NULL
  w$sizingPolicy$browser$fill <- TRUE
  w$sizingPolicy$browser$padding <- 0

  stage <- tempfile("pv-save-")
  dir.create(stage)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  page <- file.path(stage, "chart.html")
  htmlwidgets::saveWidget(w, page, selfcontained = FALSE, libdir = "lib")

  b <- chromote::ChromoteSession$new(width = as.integer(round(width)),
                                     height = as.integer(round(height)))
  on.exit(try(b$close(), silent = TRUE), add = TRUE)
  errors <- export_watch_errors(b)
  if (format %in% c("png", "gif") && scale != 1) {
    b$Emulation$setDeviceMetricsOverride(
      width = as.integer(round(width)), height = as.integer(round(height)),
      deviceScaleFactor = scale, mobile = FALSE)
  }
  loaded <- b$Page$loadEventFired(wait_ = FALSE)
  b$Page$navigate(utils::URLencode(paste0("file://", normalizePath(page))),
                  wait_ = FALSE)
  b$wait_for(loaded)
  export_wait_settled(b, errors, delay, export_settle_count_js(widget))
  if (length(errors$msgs)) {
    rlang::warn(sprintf(
      "JavaScript error while rendering the chart: %s",
      sub("\n[\\s\\S]*$", "", errors$msgs[[1]], perl = TRUE)))
  }

  bytes <- switch(format,
    png = base64enc::base64decode(
      b$Page$captureScreenshot(format = "png")$data),
    pdf = export_print_pdf(b, stage, width, height, widget$x$title),
    gif = export_capture_gif(b, widget, width, height, scale, fps, stage),
    svg = {
      svg <- export_page_svg(b)
      if (embed_fonts) {
        svg <- export_svg_embed_font(svg)
      }
      charToRaw(enc2utf8(svg))
    })
  # The gif bytes carry their frame count as an attribute for the message
  # below; writeBin() refuses raw vectors with attributes, so lift it off.
  frames <- attr(bytes, "frames")
  attributes(bytes) <- NULL
  export_write_atomic(file, bytes)
  if (!quiet) {
    extra <- switch(format,
      png = sprintf(" at %sx", format_px(scale)),
      gif = sprintf(" at %sx, %s frames at %s fps", format_px(scale),
                    format_px(frames), format_px(fps)),
      "")
    message(sprintf("Saved %s (%s, %s x %s px%s)", file, format,
                    format_px(width), format_px(height), extra))
  }
  invisible(file)
}
