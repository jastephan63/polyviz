# pv_story() composes finished charts and prose into one scrollytelling
# page straight from R - no Shiny, no R Markdown needed. The page reads
# the way the newsroom pieces do: prose cards scroll up the left-hand
# column past a chart pinned on the right, and as the reader reaches
# each card the pinned chart swaps to that step's widget. The R side
# only arranges, exactly as pv_board() does: every step carries the
# untouched widget its chart function built, consecutive steps group
# into one "scene" sharing one pinned pane, and full-width breaks -
# headings, tables, standalone charts - flow between the scenes.
#
# The mechanism is deliberately boring. Every step's widget is rendered
# up front inside its scene's pane by htmlwidgets' normal static render,
# absolutely stacked and hidden with opacity/visibility (never
# display:none, which would zero the boxes the widgets measure
# themselves against). The scroll driver in
# inst/htmlwidgets/lib/pv-story/pv-story.js is one IntersectionObserver
# per scene watching the prose cards; activating a step is a class
# toggle and a ~300ms crossfade. Entrance animations run once at page
# load (mostly while hidden) and are not replayed per step - the
# crossfade is the transition. Where the driver cannot run - a browser
# without IntersectionObserver, or a reader whose system asks for
# reduced motion - it stands down and the stylesheet's fallback shows
# each scene's LAST step's widget statically, with all prose flowing
# normally, so no pane is ever blank. (With scripts blocked entirely no
# htmlwidget can draw at all, stories and boards alike; the prose still
# reads in order.)

# The scroller's stylesheet and script, carried as an htmlDependency the
# way the widgets carry theirs, so htmltools includes them once no
# matter how many stories share a page. Version follows the other pv-*
# libs in pvchart.yaml.
story_dependency <- function() {
  htmltools::htmlDependency(
    name = "pv-story", version = "0.2.0",
    src = system.file("htmlwidgets", "lib", "pv-story",
                      package = "polyviz"),
    script = "pv-story.js", stylesheet = "pv-story.css")
}

# The story's generated stylesheet: only what depends on the build - the
# theme tokens the page was built under and the font stack - scoped to
# .pv-story like the board's CSS is scoped to .pv-board. The structural
# rules (the sticky pane, the stacked widgets, the no-script fallback)
# live in the static pv-story.css and paint themselves with these vars.
# Mode "auto" sets the light tokens and swaps in the dark set by media
# query; a forced mode bakes in one set and skips the query.
story_css <- function(mode, ink) {
  vars <- function(i) {
    paste0("--surface:", i$surface, ";--primary:", i$primary,
           ";--secondary:", i$secondary, ";--muted:", i$muted,
           ";--grid:", i$grid, ";--baseline:", i$baseline, ";")
  }
  base <- if (mode == "dark") ink$dark else ink$light
  dark_block <- if (mode == "auto") {
    paste0("@media (prefers-color-scheme: dark) { .pv-story { ",
           vars(ink$dark), " } }\n")
  } else {
    ""
  }
  paste0(
    ".pv-story { ", vars(base), " }\n",
    dark_block,
    ".pv-story { font-family: ", pv_font_stack(), "; }\n")
}

# The pixel height a step's widget will render at inside the pane: a
# fixed build height when one was given, else the 420 every widget
# defaults to on a static page (the sizingPolicy in R/widgets.R - not
# export.R's 560, which is the capture default, a different question).
# The scene's pane is sized to its tallest step, so shorter widgets
# simply sit at the top of the pane.
story_widget_height <- function(w) {
  h <- w$height
  if (is.numeric(h) && length(h) == 1 && is.finite(h) && h > 0) {
    return(as.numeric(h))
  }
  if (is.character(h) && length(h) == 1 && !is.na(h) &&
      grepl("^[0-9.]+(px)?$", h)) {
    return(as.numeric(sub("px$", "", h)))
  }
  420
}

# Groups the validated blocks into what the page actually lays out:
# every run of consecutive steps becomes one scene (one pinned pane),
# and each break stands alone between them.
story_scenes <- function(blocks) {
  runs <- list()
  steps <- list()
  flush <- function() {
    if (length(steps)) {
      runs[[length(runs) + 1L]] <<- list(kind = "scene", steps = steps)
      steps <<- list()
    }
  }
  for (b in blocks) {
    if (inherits(b, "pv_story_step")) {
      steps[[length(steps) + 1L]] <- b
    } else {
      flush()
      runs[[length(runs) + 1L]] <- list(kind = "break",
                                        content = b$content)
    }
  }
  flush()
  runs
}

# Assembles the story object from its recipe (the validated arguments
# pv_story() collected). Kept separate from pv_story() for the same
# reason board_build() is: story_save() rebuilds the story from the same
# recipe with the mode pinned - one builder, two callers, identical
# layout.
story_build <- function(spec) {
  tg <- htmltools::tags
  # A forced story mode pins every widget too - steps and break charts
  # alike - so the page and its charts can never disagree about the
  # surface; "auto" leaves each chart's own choice alone. Widgets always
  # fill their container's width; heights stay their own.
  pin <- function(w) {
    if (spec$mode != "auto") {
      w$x$mode <- spec$mode
    }
    w$width <- "100%"
    w
  }
  blocks <- lapply(story_scenes(spec$blocks), function(run) {
    if (run$kind == "break") {
      content <- run$content
      if (inherits(content, "pvchart")) {
        content <- pin(content)
      } else if (is.character(content)) {
        content <- htmltools::HTML(content)
      }
      return(tg$div(class = "pv-story-break", content))
    }
    steps <- run$steps
    pane_h <- max(vapply(steps, function(s) {
      story_widget_height(s$widget)
    }, numeric(1)))
    # The pane comes before the prose in the DOM so the narrow-screen
    # layout can stick it on top; the desktop grid places both into one
    # row regardless of order. data-pv-step ties card i to widget i for
    # the scroll driver.
    widgets <- lapply(seq_along(steps), function(i) {
      tg$div(class = "pv-story-widget", `data-pv-step` = i - 1L,
             pin(steps[[i]]$widget))
    })
    cards <- lapply(seq_along(steps), function(i) {
      tg$div(class = "pv-story-step", `data-pv-step` = i - 1L,
             htmltools::HTML(steps[[i]]$html))
    })
    tg$div(class = "pv-story-scene",
           # The scene's one build-time measurement: its pane's height,
           # read by the stylesheet for the sticky centring too.
           style = sprintf("--pv-pane-h:%spx", format_px(pane_h)),
           tg$div(class = "pv-story-pane", widgets),
           tg$div(class = "pv-story-prose", cards))
  })
  header <- if (!is.null(spec$title)) {
    tg$div(class = "pv-story-header",
           tg$div(class = "pv-story-title", spec$title),
           if (!is.null(spec$subtitle)) {
             tg$div(class = "pv-story-subtitle", spec$subtitle)
           })
  }
  source_line <- if (!is.null(spec$source)) {
    tg$div(class = "pv-story-source", spec$source)
  }
  story <- tg$div(
    class = "pv-story",
    tg$style(htmltools::HTML(story_css(spec$mode, spec$ink))),
    header,
    blocks,
    source_line)
  # The scroller itself, plus the bundled Inter face so the prose wears
  # the same type as the charts before the first widget loads (the same
  # dependency the demo page carries; htmltools de-duplicates it against
  # the widgets' own copy).
  story <- htmltools::attachDependencies(
    story, list(story_dependency(), demo_font_dependency()),
    append = TRUE)
  story <- htmltools::browsable(story)
  class(story) <- c("pv_story", class(story))
  # The recipe rides along so story_save() can rebuild the story with
  # the save mode pinned, without re-validating anything.
  attr(story, "pv_story") <- spec
  story
}

#' One step of a scrollytelling story
#'
#' Pairs a chart with the prose card that introduces it. Steps are the
#' beats of a [pv_story()]: as the reader scrolls the card into the
#' middle of the viewport, the scene's pinned pane crossfades to this
#' step's widget. Consecutive steps typically carry variants of one
#' chart - the same data with more annotations, a changed subtitle, a
#' different year - and each step simply carries its complete widget;
#' nothing is diffed or patched between them.
#'
#' @param w The polyviz chart the pinned pane shows while this step is
#'   active - a complete widget from [pv_bar()] and friends, built with
#'   everything it needs.
#' @param html The step's prose, as a single string. It may contain HTML
#'   markup (`<p>`, `<strong>`, `<h3>`, ...), which is passed through
#'   verbatim - write it as you would any trusted fragment of your own
#'   page.
#' @return A story step, for [pv_story()]'s `...`.
#' @seealso [pv_story()], [pv_story_break()]
#' @export
pv_story_step <- function(w, html) {
  if (!inherits(w, "pvchart")) {
    rlang::abort(paste(
      "`w` must be a polyviz chart (the return value of pv_bar() and",
      "friends)."))
  }
  if (!is.character(html) || length(html) != 1 || is.na(html) ||
      !nzchar(html)) {
    rlang::abort(paste(
      "`html` must be a single non-empty string - the step's prose,",
      "plain or with HTML markup."))
  }
  structure(list(widget = w, html = html), class = "pv_story_step")
}

#' A full-width break between story scenes
#'
#' Ends the current run of steps and lays `x` across the page's full
#' column before the next scene begins. This is how everything that is
#' not a pinned beat interleaves with the scenes: section headings,
#' connecting prose, tables, and standalone charts that should scroll
#' with the page instead of pinning.
#'
#' @param x What to show: any htmltools tag (or tag list), a raw HTML
#'   string (passed through verbatim), or a bare polyviz chart, which is
#'   rendered inline at full column width - not pinned.
#' @return A story break, for [pv_story()]'s `...`.
#' @seealso [pv_story()], [pv_story_step()]
#' @export
pv_story_break <- function(x) {
  tag_like <- inherits(x, c("shiny.tag", "shiny.tag.list", "html"))
  text_like <- is.character(x) && length(x) == 1 && !is.na(x) &&
    nzchar(x)
  if (!inherits(x, "pvchart") && !tag_like && !text_like) {
    rlang::abort(paste(
      "`x` must be an htmltools tag, a single HTML string, or a",
      "polyviz chart."))
  }
  structure(list(content = x), class = "pv_story_break")
}

#' Compose charts and prose into a scrollytelling story
#'
#' Lays story blocks out as one long-form page: prose cards scroll up a
#' column past a pinned chart pane that changes as the reader reaches
#' each step. Consecutive [pv_story_step()]s form one *scene* - one
#' sticky pane on the right (pinned on top on narrow screens), their
#' cards scrolling on the left - and every [pv_story_break()] ends the
#' scene and flows full-width before the next begins. No Shiny and no
#' R Markdown are needed: the return value is a browsable htmltools
#' object, so it shows in the RStudio viewer when printed, drops into an
#' R Markdown or Quarto chunk like any widget, and saves to a standalone
#' page with `htmltools::save_html(story, "story.html")` (widget assets
#' land in a `lib/` folder next to the file).
#'
#' Every step's widget is rendered when the page loads, stacked inside
#' its scene's pane; reaching a step's card crossfades the pane to that
#' step's widget (about 300ms). Each chart keeps everything it was built
#' with - title, subtitle, source line, tooltips, downloads. The pane is
#' sized to the scene's tallest widget and stays vertically centred in
#' the viewport while its scene scrolls past.
#'
#' The page degrades honestly: where the scroll driver cannot run - a
#' browser without IntersectionObserver, or a reader whose system asks
#' for reduced motion - all prose flows normally and each scene shows
#' its **last** step's widget statically, never a blank pane. (With
#' scripts blocked entirely no htmlwidget can draw at all, stories and
#' boards alike; the prose still reads in order.) Entrance animations
#' run once at page load and are not replayed as steps activate; the
#' crossfade is the step transition.
#'
#' @param ... Story blocks in reading order: [pv_story_step()]s and
#'   [pv_story_break()]s. A single list of blocks is also accepted. At
#'   least one step is required - a story with no scene is just a page.
#' @param title Optional story heading, shown once at the top.
#' @param subtitle Optional line under the heading; needs `title`.
#' @param source Optional source/credit line for the whole story, shown
#'   small and grey at the very end. Each chart's own source line is
#'   untouched - this one is additional.
#' @param mode `"auto"` (default: the page and every chart follow the
#'   viewer's light/dark setting), `"light"`, or `"dark"`. A forced mode
#'   pins the page's surface and every widget's mode together, break
#'   charts included.
#' @return A browsable htmltools object of class `"pv_story"`. Print it
#'   to see it in the viewer, return it from an R Markdown chunk to
#'   embed it, or write it to disk with [htmltools::save_html()].
#' @examples
#' agg <- aggregate(revenue ~ region, pv_sales, sum)
#' monthly <- aggregate(revenue ~ month, pv_sales, sum)
#' pv_story(
#'   pv_story_step(
#'     pv_bar(agg, "region", "revenue", title = "Revenue by region"),
#'     "<p>Four regions carry the year, and one of them carries
#'      most of it.</p>"),
#'   pv_story_step(
#'     pv_bar(agg, "region", "revenue", sort = TRUE,
#'            title = "Revenue by region, sorted"),
#'     "<p>Sorted, the gap is plainer still.</p>"),
#'   pv_story_break("<h2>The year underneath</h2>"),
#'   pv_story_step(
#'     pv_line(monthly, "month", "revenue", title = "Monthly revenue"),
#'     "<p>Month by month, the same total breathes.</p>"),
#'   title = "Revenue, told in steps",
#'   source = "Simulated pv_sales data"
#' )
#' @export
pv_story <- function(..., title = NULL, subtitle = NULL, source = NULL,
                     mode = "auto") {
  blocks <- list(...)
  # A single bare list of blocks is unwrapped, so both spellings work,
  # exactly as in pv_board(). Only a plain unclassed list unwraps - a
  # step and a break are themselves classed lists and must fall through
  # to the block check for its clear message.
  if (length(blocks) == 1 && is.list(blocks[[1]]) &&
      !is.object(blocks[[1]])) {
    blocks <- blocks[[1]]
  }
  if (!length(blocks)) {
    rlang::abort(
      "`...` is empty; give pv_story() at least one pv_story_step().")
  }
  for (i in seq_along(blocks)) {
    if (!inherits(blocks[[i]], c("pv_story_step", "pv_story_break"))) {
      rlang::abort(sprintf(paste(
        "Block %d is neither a step nor a break - build story blocks",
        "with pv_story_step() and pv_story_break()."), i))
    }
  }
  if (!any(vapply(blocks, inherits, logical(1), "pv_story_step"))) {
    rlang::abort(paste(
      "A story needs at least one pv_story_step() - with only breaks",
      "there is nothing to pin."))
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
  # The story chrome takes its ink from the theme active right now, the
  # same moment the widgets' own payloads took theirs.
  tokens <- the$theme %||% pv_colors
  spec <- list(blocks = blocks, title = title, subtitle = subtitle,
               source = source, mode = mode, ink = tokens$ink)
  story_build(spec)
}

#' Print a scrollytelling story
#'
#' Shows the story in the RStudio viewer (or the default browser), the
#' way htmlwidgets print: the page is rendered to a temporary file and
#' handed to the viewer. In a non-interactive session the file is still
#' written but no viewer opens - the same behaviour as printing a chart
#' or a board.
#'
#' @param x A story built by [pv_story()].
#' @param ... Ignored.
#' @param browse Open the story in the viewer? Defaults to
#'   [interactive()].
#' @return `x`, invisibly.
#' @keywords internal
#' @export
print.pv_story <- function(x, ..., browse = interactive()) {
  # The viewer page's own background matches the story surface, so no
  # white ring sits around a dark story.
  spec <- attr(x, "pv_story")
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

# ---- saving stories ----------------------------------------------------

# The story save hook, shaped exactly like board_save() so pv_save() can
# dispatch to it with the same one-line inherits() check R/export.R uses
# for boards; until that line exists, stories save through
# htmltools::save_html() (assets in lib/) or straight through here.
#
# Only .html makes sense: a story IS scrolling - the pinned scenes exist
# only on a live page - so there is no settled single frame for the
# capture formats to hold, and the refusal below says so loudly, the way
# boards refuse .svg and .gif. The written file is self-contained: the
# story's rendered tags plus every dependency - d3, the renderers,
# htmlwidgets, the scroller, the font css - inlined into the head
# through the same machinery the chart and board paths use.
story_save <- function(story, file, mode = "auto", embed_fonts = TRUE,
                       quiet = FALSE) {
  spec <- attr(story, "pv_story")
  if (!is.list(spec) || !length(spec$blocks)) {
    rlang::abort(paste(
      "`story` is a pv_story without its build recipe - build stories",
      "with pv_story()."))
  }
  if (!is.character(file) || length(file) != 1 || is.na(file) ||
      !nzchar(file)) {
    rlang::abort("`file` must be a single file path.")
  }
  format <- export_format(file)
  # The scenes only exist while the reader scrolls, so a story has no
  # single settled frame for the capture formats to hold and no vector
  # drawing to serialise - .html is the one format that IS the story.
  if (format != "html") {
    rlang::abort(sprintf(paste(
      "A .%s file cannot hold a scrollytelling story - the pinned",
      "scenes only exist on a live scrolling page. Save it as .html",
      "instead."), format))
  }
  if (!is.character(mode) || length(mode) != 1 || is.na(mode) ||
      !mode %in% c("auto", "light", "dark")) {
    rlang::abort('`mode` must be "auto", "light", or "dark".')
  }
  if (!isTRUE(embed_fonts) && !isFALSE(embed_fonts)) {
    rlang::abort("`embed_fonts` must be TRUE or FALSE.")
  }
  if (!isTRUE(quiet) && !isFALSE(quiet)) {
    rlang::abort("`quiet` must be TRUE or FALSE.")
  }
  # The story is rebuilt from its recipe with the save mode pinned -
  # story_build() pins every widget along with the chrome. The default
  # "auto" keeps the live light/dark switching, the natural choice for
  # a page that is itself alive.
  spec$mode <- mode
  rebuilt <- story_build(spec)
  rendered <- htmltools::renderTags(rebuilt)
  deps <- htmltools::resolveDependencies(rendered$dependencies)
  parts <- export_inline_dependencies(deps, embed_fonts)
  html <- export_standalone_page(rendered, parts,
                                 export_body_css(spec$ink, mode),
                                 spec$title %||% "polyviz story")
  export_write_atomic(file, charToRaw(enc2utf8(html)))
  if (!quiet) {
    message(sprintf("Saved %s (self-contained story html)", file))
  }
  invisible(file)
}
