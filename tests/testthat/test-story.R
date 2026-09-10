# pv_story() composes finished widgets and prose into a scrollytelling
# page, so most tests work at the HTML level: build a story, render it
# to text, and look for the scene structure, the stacked widgets, and
# the serialised payloads. The scroll driver itself is verified in a
# real headless Chrome at the end, exactly like the board suite, and
# skips cleanly wherever the browser stack is missing.

story_bar <- function(...) {
  agg <- aggregate(revenue ~ region, pv_sales, sum)
  pv_bar(agg, "region", "revenue", title = "Revenue by region", ...)
}

story_line <- function(...) {
  monthly <- aggregate(revenue ~ month, pv_sales, sum)
  pv_line(monthly, "month", "revenue", title = "Revenue over time", ...)
}

test_that("pv_story_step validates its widget and prose", {
  w <- story_bar()
  expect_error(pv_story_step("x", "<p>hi</p>"), "polyviz chart")
  expect_error(pv_story_step(data.frame(), "<p>hi</p>"), "polyviz chart")
  expect_error(pv_story_step(w, 1), "`html`")
  expect_error(pv_story_step(w, NA_character_), "`html`")
  expect_error(pv_story_step(w, ""), "`html`")
  expect_error(pv_story_step(w, c("a", "b")), "`html`")
  s <- pv_story_step(w, "<p>hi</p>")
  expect_s3_class(s, "pv_story_step")
  expect_identical(s$html, "<p>hi</p>")
})

test_that("pv_story_break takes tags, HTML strings, and bare charts", {
  expect_s3_class(pv_story_break("<h2>Chapter</h2>"), "pv_story_break")
  expect_s3_class(pv_story_break(htmltools::tags$h2("Chapter")),
                  "pv_story_break")
  expect_s3_class(pv_story_break(htmltools::HTML("<p>x</p>")),
                  "pv_story_break")
  expect_s3_class(pv_story_break(story_bar()), "pv_story_break")
  expect_error(pv_story_break(1), "htmltools tag")
  expect_error(pv_story_break(""), "htmltools tag")
  expect_error(pv_story_break(NA_character_), "htmltools tag")
  expect_error(pv_story_break(c("a", "b")), "htmltools tag")
})

test_that("pv_story validates its blocks and arguments", {
  step <- pv_story_step(story_bar(), "<p>one</p>")
  expect_error(pv_story(), "at least one")
  # a bare widget is not a block - the message points at the wrappers
  expect_error(pv_story(story_bar()), "Block 1")
  expect_error(pv_story(step, 1), "Block 2")
  expect_error(pv_story(list(step, "x")), "Block 2")
  # breaks alone give the pinned pane nothing to do
  expect_error(pv_story(pv_story_break("<h2>only</h2>")),
               "at least one pv_story_step")
  expect_error(pv_story(step, title = 1), "`title`")
  expect_error(pv_story(step, subtitle = "sub"), "needs a `title`")
  expect_error(pv_story(step, source = 1), "`source`")
  expect_error(pv_story(step, mode = "sepia"),
               '"auto", "light", or "dark"')
})

test_that("a story is a browsable tag grouping steps into scenes", {
  s <- pv_story(
    pv_story_step(story_bar(), "<p>Step one.</p>"),
    pv_story_step(story_bar(sort = TRUE), "<p>Step two.</p>"),
    pv_story_break("<h2>An interlude</h2>"),
    pv_story_step(story_line(), "<p>Step three.</p>"),
    title = "A two-scene story", subtitle = "With a break",
    source = "Simulated pv_sales data")
  expect_s3_class(s, "pv_story")
  expect_s3_class(s, "shiny.tag")
  expect_true(htmltools::is.browsable(s))
  # the recipe rides along for story_save()
  expect_true(is.list(attr(s, "pv_story")))
  expect_length(attr(s, "pv_story")$blocks, 4)

  html <- as.character(s)
  # two scenes: steps 1+2 share the first pane, step 3 gets its own
  expect_length(
    regmatches(html, gregexpr('class="pv-story-scene"', html))[[1]], 2)
  expect_length(
    regmatches(html, gregexpr('class="pv-story-widget"', html))[[1]], 3)
  expect_length(
    regmatches(html, gregexpr('class="pv-story-step"', html))[[1]], 3)
  # cards and widgets are tied together by step index within the scene
  expect_length(
    regmatches(html, gregexpr('data-pv-step="0"', html))[[1]], 4)
  expect_length(
    regmatches(html, gregexpr('data-pv-step="1"', html))[[1]], 2)
  # the break flows between the scenes, verbatim
  expect_match(html, '<div class="pv-story-break"><h2>An interlude</h2>',
               fixed = TRUE)
  # heading block and the story's own source line
  expect_match(html, ">A two-scene story<")
  expect_match(html, ">With a break<")
  expect_match(html, ">Simulated pv_sales data<")
  # the real widgets ride inside, full-width in the pane
  expect_length(regmatches(html, gregexpr('"type":"bar"', html))[[1]], 2)
  expect_match(html, '"type":"line"', fixed = TRUE)
  expect_length(regmatches(html, gregexpr("width:100%", html))[[1]], 3)
  # the prose card content is passed through as written
  expect_match(html, "<p>Step one.</p>", fixed = TRUE)
})

test_that("a single list of blocks works, as in pv_board", {
  s <- pv_story(list(pv_story_step(story_bar(), "<p>one</p>"),
                     pv_story_step(story_line(), "<p>two</p>")))
  html <- as.character(s)
  expect_length(
    regmatches(html, gregexpr('class="pv-story-scene"', html))[[1]], 1)
  expect_length(
    regmatches(html, gregexpr('class="pv-story-widget"', html))[[1]], 2)
  # no title given: the optional blocks are absent
  expect_false(grepl('class="pv-story-header"', html, fixed = TRUE))
  expect_false(grepl('class="pv-story-source"', html, fixed = TRUE))
})

test_that("the pane is sized to the scene's tallest step", {
  s <- pv_story(
    pv_story_step(story_bar(height = 500), "<p>tall</p>"),
    pv_story_step(story_bar(), "<p>default</p>"),
    pv_story_break("<h2>x</h2>"),
    pv_story_step(story_line(), "<p>own scene</p>"))
  html <- as.character(s)
  # scene one: max(500, the standard 420); scene two: the standard 420
  expect_match(html, "--pv-pane-h:500px", fixed = TRUE)
  expect_match(html, "--pv-pane-h:420px", fixed = TRUE)
})

test_that("break content is rendered by kind: tag, HTML, chart", {
  s <- pv_story(
    pv_story_step(story_bar(), "<p>one</p>"),
    pv_story_break(htmltools::tags$h2("A tag heading")),
    pv_story_step(story_bar(), "<p>two</p>"),
    pv_story_break(story_line()),
    pv_story_step(story_line(), "<p>three</p>"))
  html <- as.character(s)
  expect_match(html, "<h2>A tag heading</h2>", fixed = TRUE)
  # the break chart renders inline (not pinned): three scenes around
  # the two breaks, and the break's widget is full-width too
  expect_length(
    regmatches(html, gregexpr('class="pv-story-scene"', html))[[1]], 3)
  expect_length(regmatches(html, gregexpr('"type":"line"', html))[[1]], 2)
  expect_length(regmatches(html, gregexpr("width:100%", html))[[1]], 4)
})

test_that("mode auto swaps tokens by media query; a forced mode pins all", {
  step <- function() pv_story_step(story_bar(), "<p>x</p>")
  auto <- as.character(pv_story(step(), step(),
                                pv_story_break(story_line())))
  expect_match(auto, "prefers-color-scheme: dark", fixed = TRUE)
  # the light surface is the default set on the container
  expect_match(auto, paste0(".pv-story { --surface:",
                            pv_colors$ink$light$surface), fixed = TRUE)
  # auto leaves each chart's own mode choice alone
  expect_match(auto, '"mode":"auto"', fixed = TRUE)

  dark <- as.character(pv_story(step(), step(),
                                pv_story_break(story_line()),
                                mode = "dark"))
  expect_match(dark, paste0(".pv-story { --surface:",
                            pv_colors$ink$dark$surface), fixed = TRUE)
  expect_false(grepl("prefers-color-scheme", dark, fixed = TRUE))
  # a forced story mode pins every widget with it, break charts included
  expect_length(regmatches(dark, gregexpr('"mode":"dark"', dark))[[1]], 3)
  expect_false(grepl('"mode":"auto"', dark, fixed = TRUE))
})

test_that("a story carries the scroller and font dependencies once", {
  s <- pv_story(pv_story_step(story_bar(), "<p>one</p>"),
                pv_story_step(story_line(), "<p>two</p>"))
  rendered <- htmltools::renderTags(s)
  deps <- htmltools::resolveDependencies(rendered$dependencies)
  nm <- vapply(deps, function(d) d$name, character(1))
  expect_identical(sum(nm == "pv-story"), 1L)
  expect_identical(sum(nm == "pv-fonts"), 1L)
  expect_true(all(c("d3", "pv-common", "pv-renderers") %in% nm))
})

test_that("the packaged stylesheet keeps the no-script fallback", {
  css <- paste(readLines(system.file("htmlwidgets", "lib", "pv-story",
                                     "pv-story.css", package = "polyviz"),
                         warn = FALSE, encoding = "UTF-8"),
               collapse = "\n")
  # without the driver each scene shows its last step's widget...
  expect_match(css, ".pv-story-widget:last-child", fixed = TRUE)
  # ...and reduced motion turns the fades off entirely
  expect_match(css, "prefers-reduced-motion", fixed = TRUE)
  js <- paste(readLines(system.file("htmlwidgets", "lib", "pv-story",
                                    "pv-story.js", package = "polyviz"),
                        warn = FALSE, encoding = "UTF-8"),
              collapse = "\n")
  # the driver stands down under reduced motion instead of overriding it
  expect_match(js, "prefers-reduced-motion", fixed = TRUE)
  expect_match(js, "IntersectionObserver", fixed = TRUE)
})

test_that("printing a story writes the viewer page quietly", {
  s <- pv_story(pv_story_step(story_bar(), "<p>one</p>"), mode = "dark")
  expect_output(polyviz:::print.pv_story(s, browse = FALSE), NA)
  expect_invisible(polyviz:::print.pv_story(s, browse = FALSE))
})

test_that("story_save writes one self-contained html file", {
  dir <- withr::local_tempdir()
  s <- pv_story(pv_story_step(story_bar(), "<p>one</p>"),
                pv_story_step(story_line(), "<p>two</p>"),
                title = "A saved story")
  f <- file.path(dir, "story.html")
  expect_message(polyviz:::story_save(s, f), "self-contained story html")
  # one file, no lib/ folder, and no leftover temp file from the write
  expect_identical(list.files(dir, all.files = TRUE, no.. = TRUE),
                   "story.html")
  html <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"),
                collapse = "\n")
  expect_match(html, "<title>A saved story</title>", fixed = TRUE)
  expect_match(html, 'class="pv-story"', fixed = TRUE)
  expect_match(html, "pvRenderers", fixed = TRUE)
  # the scroller and its stylesheet are inlined with everything else
  expect_match(html, "IntersectionObserver", fixed = TRUE)
  expect_match(html, ".pv-story-widget:last-child", fixed = TRUE)
  expect_match(html, "data:font/woff2;base64,", fixed = TRUE)
  expect_false(grepl('src="lib/', html, fixed = TRUE))
  # the default save mode keeps the live light/dark switching
  expect_match(html, '"mode":"auto"', fixed = TRUE)
  expect_match(html, "prefers-color-scheme: dark", fixed = TRUE)
  expect_gt(file.size(f), 500000)
})

test_that("story saves validate and refuse the capture formats loudly", {
  s <- pv_story(pv_story_step(story_bar(), "<p>one</p>"))
  dir <- tempdir()
  for (ext in c("png", "svg", "pdf", "gif")) {
    expect_error(
      polyviz:::story_save(s, file.path(dir, paste0("s.", ext))),
      "cannot hold a scrollytelling story")
  }
  expect_error(polyviz:::story_save(s, file.path(dir, "s.docx")),
               "must end in")
  expect_error(polyviz:::story_save(s, c("a.html", "b.html")),
               "single file path")
  expect_error(polyviz:::story_save(s, file.path(dir, "s.html"),
                                    mode = "sepia"),
               '"auto", "light", or "dark"')
  expect_error(polyviz:::story_save(s, file.path(dir, "s.html"),
                                    embed_fonts = "yes"), "`embed_fonts`")
  expect_error(polyviz:::story_save(s, file.path(dir, "s.html"),
                                    quiet = 1), "`quiet`")
  # a bare classed object without pv_story()'s recipe is refused
  fake <- structure(list(), class = "pv_story")
  expect_error(polyviz:::story_save(fake, file.path(dir, "s.html")),
               "build recipe")
})

test_that("a story knits into a markdown document via knit_print", {
  skip_if_not_installed("knitr")
  dir <- withr::local_tempdir()
  env <- new.env(parent = globalenv())
  env$story <- pv_story(pv_story_step(story_bar(), "<p>one</p>"),
                        title = "Knitted story")
  rmd <- file.path(dir, "story.Rmd")
  writeLines(c("A story in a chunk:", "",
               "```{r story-chunk, echo=FALSE}", "story", "```"), rmd)
  md <- file.path(dir, "story.md")
  knitr::knit(rmd, md, envir = env, quiet = TRUE)
  out <- paste(readLines(md, warn = FALSE, encoding = "UTF-8"),
               collapse = "\n")
  expect_match(out, 'class="pv-story"', fixed = TRUE)
  expect_match(out, ">Knitted story<")
  expect_match(out, '"type":"bar"', fixed = TRUE)
})

# ---- headless-Chrome verification --------------------------------------

# Stages a story the way the board tests stage boards (light, entrance
# animations off), serves it from disk, and opens it in headless Chrome.
# reduced_motion emulates the prefers-reduced-motion media feature, the
# state in which the scroll driver must stand down.
story_page_session <- function(story, width = 1200, height = 800,
                               reduced_motion = FALSE) {
  spec <- attr(story, "pv_story")
  spec$mode <- "light"
  spec$blocks <- lapply(spec$blocks, function(b) {
    if (inherits(b, "pv_story_step")) {
      b$widget$x$duration <- 0
    }
    b
  })
  rebuilt <- polyviz:::story_build(spec)
  stage <- tempfile("pv-story-page-")
  dir.create(stage)
  page <- file.path(stage, "story.html")
  htmltools::save_html(rebuilt, file = page, libdir = "lib")
  b <- chromote::ChromoteSession$new(width = width, height = height)
  if (reduced_motion) {
    b$Emulation$setEmulatedMedia(features = list(
      list(name = "prefers-reduced-motion", value = "reduce")))
  }
  errors <- polyviz:::export_watch_errors(b)
  loaded <- b$Page$loadEventFired(wait_ = FALSE)
  b$Page$navigate(utils::URLencode(paste0("file://", normalizePath(page))),
                  wait_ = FALSE)
  b$wait_for(loaded)
  polyviz:::export_wait_settled(b, errors, 0,
                                polyviz:::board_settle_count_js())
  list(b = b, errors = errors)
}

# One JSON snapshot of the driver's state: per widget, its scene, its
# rendered SVG node count, and whether it is visible right now.
story_state_js <- "
  JSON.stringify({
    driver: document.querySelectorAll('.pv-story-js').length,
    scenes: document.querySelectorAll('.pv-story-scene').length,
    widgets: Array.prototype.map.call(
      document.querySelectorAll('.pv-story-scene'),
      function (sc, i) {
        return Array.prototype.map.call(
          sc.querySelectorAll('.pv-story-widget'),
          function (w) {
            var cs = getComputedStyle(w);
            return { scene: i,
                     svg: w.querySelectorAll('svg *').length,
                     visible: cs.visibility === 'visible' &&
                       +cs.opacity > 0.9 };
          });
      }).flat()
  })"

test_that("a two-scene story renders every step and pins one at a time", {
  render_skip_if_no_chrome()
  s <- pv_story(
    pv_story_step(story_bar(), "<p>Step one.</p>"),
    pv_story_step(story_bar(sort = TRUE), "<p>Step two.</p>"),
    pv_story_break("<h2>An interlude</h2>"),
    pv_story_step(story_line(), "<p>Step three.</p>"),
    pv_story_step(story_line(curve = "monotone"), "<p>Step four.</p>"),
    title = "A two-scene story")
  sess <- story_page_session(s)
  withr::defer(try(sess$b$close(), silent = TRUE))
  expect_identical(sess$errors$msgs, character())
  # let the boot activation's 300ms crossfades finish
  Sys.sleep(0.7)

  state <- function() {
    jsonlite::fromJSON(sess$b$Runtime$evaluate(
      story_state_js, returnByValue = TRUE)$result$value)
  }
  st <- state()
  expect_identical(st$driver, 1L)
  expect_identical(st$scenes, 2L)
  expect_identical(nrow(st$widgets), 4L)
  # every step's widget rendered for real, the hidden ones included -
  # the layout-preserving hiding is exactly what this asserts
  expect_true(all(st$widgets$svg > 20))
  # exactly one widget visible per scene, and at the top of the page it
  # is each scene's first step
  vis <- st$widgets[st$widgets$visible, ]
  expect_identical(vis$scene, c(0L, 1L))
  expect_identical(which(st$widgets$visible), c(1L, 3L))

  # scroll the first scene's second card to the viewport centre: the
  # pane crossfades to that step's widget
  sess$b$Runtime$evaluate(paste0(
    "void document.querySelectorAll('.pv-story-step')[1]",
    ".scrollIntoView({block:'center'})"))
  Sys.sleep(1)
  st2 <- state()
  expect_true(st2$widgets$visible[[2]])
  expect_false(st2$widgets$visible[[1]])
  expect_identical(sum(st2$widgets$visible[st2$widgets$scene == 0]), 1L)

  # and down into the second scene's final step
  sess$b$Runtime$evaluate(paste0(
    "void document.querySelectorAll('.pv-story-step')[3]",
    ".scrollIntoView({block:'center'})"))
  Sys.sleep(1)
  st3 <- state()
  expect_true(st3$widgets$visible[[4]])
  expect_identical(sum(st3$widgets$visible[st3$widgets$scene == 1]), 1L)

  expect_identical(sess$errors$msgs, character())
})

test_that("under reduced motion the driver stands down to the fallback", {
  render_skip_if_no_chrome()
  s <- pv_story(
    pv_story_step(story_bar(), "<p>Step one.</p>"),
    pv_story_step(story_bar(sort = TRUE), "<p>Step two.</p>"),
    pv_story_break("<h2>An interlude</h2>"),
    pv_story_step(story_line(), "<p>Step three.</p>"))
  sess <- story_page_session(s, reduced_motion = TRUE)
  withr::defer(try(sess$b$close(), silent = TRUE))
  st <- jsonlite::fromJSON(sess$b$Runtime$evaluate(
    story_state_js, returnByValue = TRUE)$result$value)
  # the driver never took over...
  expect_identical(st$driver, 0L)
  # ...every widget still rendered, and each scene statically shows its
  # LAST step's widget - the no-driver contract: never a blank pane
  expect_true(all(st$widgets$svg > 20))
  expect_identical(which(st$widgets$visible), c(2L, 3L))
  expect_identical(sess$errors$msgs, character())
})
