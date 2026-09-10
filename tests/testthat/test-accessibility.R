# Keyboard navigation and screen-reader wiring, beyond the alt text that
# test-alt-text.R covers: the rendered container must be focusable and
# described, the arrow keys must walk the marks with the real tooltip
# following along, an aria-live region must speak what the tooltip shows,
# and none of it may leak into exports or static charts. These drive the
# actual JavaScript in headless Chrome, so they skip wherever a browser
# is missing - exactly like the render tests.

# Stages a widget the way pv_save() does (light mode, no entrance
# animation, filling a page opened at a fixed size) and hands back the
# live Chrome session - the same staging test-widgets.R uses for its
# hover tests. The caller closes the session.
a11y_page_session <- function(w, width = 700, height = 460) {
  w$x$mode <- "light"
  w$x$duration <- 0
  w$width <- NULL
  w$height <- NULL
  w$sizingPolicy$browser$fill <- TRUE
  w$sizingPolicy$browser$padding <- 0
  stage <- tempfile("pv-a11y-page-")
  dir.create(stage)
  page <- file.path(stage, "chart.html")
  htmlwidgets::saveWidget(w, page, selfcontained = FALSE, libdir = "lib")
  b <- chromote::ChromoteSession$new(width = width, height = height)
  errors <- polyviz:::export_watch_errors(b)
  loaded <- b$Page$loadEventFired(wait_ = FALSE)
  b$Page$navigate(utils::URLencode(paste0("file://", normalizePath(page))),
                  wait_ = FALSE)
  b$wait_for(loaded)
  polyviz:::export_wait_settled(b, errors, 0,
                                polyviz:::export_settle_count_js(w))
  list(b = b, errors = errors)
}

# A tiny bar chart with known marks in a known order. The values run
# descending on purpose, so any sorting the builder might apply leaves
# the order alone and "first mark" stays Bern.
a11y_chart <- function() {
  towns <- data.frame(city = c("Bern", "Luzern", "Zug"),
                      n = c(3302, 1200, 800))
  pv_bar(towns, x = "city", y = "n", title = "Commuters")
}

test_that("a rendered chart is focusable, described, and quiet at rest", {
  render_skip_if_no_chrome()
  w <- a11y_chart()
  s <- a11y_page_session(w)
  withr::defer(try(s$b$close(), silent = TRUE))
  expect_identical(s$errors$msgs, character())
  res <- s$b$Runtime$evaluate("
    (function () {
      var el = document.querySelector('.pvchart');
      var live = el.querySelector('.pv-live');
      var descId = el.getAttribute('aria-describedby');
      var desc = descId ? document.getElementById(descId) : null;
      return JSON.stringify({
        tabindex: el.getAttribute('tabindex'),
        liveThere: !!live,
        livePolite: live ? live.getAttribute('aria-live') : '',
        liveText: live ? live.textContent : 'missing',
        descText: desc ? desc.textContent : 'missing',
        marks: el.__pvNav ? el.__pvNav.marks.length : -1
      });
    })()", returnByValue = TRUE)$result$value
  got <- jsonlite::fromJSON(res)
  # focusable, with the live region present and empty until a key moves
  expect_identical(got$tabindex, "0")
  expect_true(got$liveThere)
  expect_identical(got$livePolite, "polite")
  expect_identical(got$liveText, "")
  # aria-describedby resolves to a node carrying the generated alt text
  expect_identical(got$descText, w$x$alt)
  # one registered mark per bar
  expect_identical(got$marks, 3L)
})

test_that("arrow keys walk the marks, show the tooltip, and feed the live region", {
  render_skip_if_no_chrome()
  s <- a11y_page_session(a11y_chart())
  withr::defer(try(s$b$close(), silent = TRUE))
  expect_identical(s$errors$msgs, character())
  res <- s$b$Runtime$evaluate("
    (function () {
      var el = document.querySelector('.pvchart');
      el.focus();
      function key(k) {
        el.dispatchEvent(new KeyboardEvent('keydown', { key: k }));
      }
      var live = el.querySelector('.pv-live');
      var tip = el.querySelector('.pv-tooltip');
      var out = {};
      key('ArrowRight');
      out.first = live.textContent;
      out.tipShown = tip.style.opacity;
      var ring = el.querySelector('.pv-focus-ring');
      out.ringShown = !!ring && ring.style.display !== 'none';
      out.containerRing = el.style.boxShadow !== '';
      key('ArrowRight');
      out.second = live.textContent;
      key('End');
      out.last = live.textContent;
      key('Home');
      out.home = live.textContent;
      key('Escape');
      out.afterEscape = live.textContent;
      out.tipAfterEscape = tip.style.opacity;
      ring = el.querySelector('.pv-focus-ring');
      out.ringAfterEscape = !ring || ring.style.display === 'none';
      return JSON.stringify(out);
    })()", returnByValue = TRUE)$result$value
  got <- jsonlite::fromJSON(res)
  # the first arrow lands on the first bar: the real tooltip appears and
  # its text - category, then the formatted value - reaches the reader
  expect_match(got$first, "Bern", fixed = TRUE)
  expect_match(got$first, "3,302", fixed = TRUE)
  expect_identical(got$tipShown, "1")
  # both focus indicators are up: the mark's ring and the container's
  expect_true(got$ringShown)
  expect_true(got$containerRing)
  # stepping and jumping move through the data in order
  expect_match(got$second, "Luzern", fixed = TRUE)
  expect_match(got$last, "Zug", fixed = TRUE)
  expect_match(got$home, "Bern", fixed = TRUE)
  # Escape clears the focused mark completely
  expect_identical(got$afterEscape, "")
  expect_identical(got$tipAfterEscape, "0")
  expect_true(got$ringAfterEscape)
  # none of the key handling raised a JavaScript error
  expect_identical(s$errors$msgs, character())
})

test_that("the accessibility chrome never reaches a standalone svg export", {
  render_skip_if_no_chrome()
  w <- a11y_chart()
  s <- a11y_page_session(w)
  withr::defer(try(s$b$close(), silent = TRUE))
  # export mid-navigation, with the live region full and the rings up -
  # the worst case for leakage
  svg <- s$b$Runtime$evaluate("
    (function () {
      var el = document.querySelector('.pvchart');
      el.focus();
      el.dispatchEvent(new KeyboardEvent('keydown', { key: 'ArrowRight' }));
      return window.pv.toStandaloneSvg(el, el.__pvLastX || null, null);
    })()", returnByValue = TRUE)$result$value
  # a real export came back, with the plot embedded
  expect_match(svg, "^<\\?xml")
  expect_gt(lengths(regmatches(svg, gregexpr("<svg", svg))), 1L)
  # the hidden description (the alt text) stays out of the drawing
  expect_false(grepl(substr(w$x$alt, 1, 40), svg, fixed = TRUE))
  # and "Bern" appears only as the axis tick label, not doubled by the
  # live region's announcement
  expect_length(regmatches(svg, gregexpr("Bern", svg, fixed = TRUE))[[1]], 1)
})

test_that("a static chart stays out of the tab order entirely", {
  render_skip_if_no_chrome()
  s <- a11y_page_session(pv_static(a11y_chart()))
  withr::defer(try(s$b$close(), silent = TRUE))
  expect_identical(s$errors$msgs, character())
  res <- s$b$Runtime$evaluate("
    (function () {
      var el = document.querySelector('.pvchart');
      return JSON.stringify({
        hasTab: el.hasAttribute('tabindex'),
        hasDescribedby: el.hasAttribute('aria-describedby'),
        liveThere: !!el.querySelector('.pv-live'),
        inert: el.style.pointerEvents
      });
    })()", returnByValue = TRUE)$result$value
  got <- jsonlite::fromJSON(res)
  expect_false(got$hasTab)
  expect_false(got$hasDescribedby)
  expect_false(got$liveThere)
  expect_identical(got$inert, "none")
})
