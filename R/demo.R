# The Shiny demo gallery: one app, every chart family, live controls.
# The app itself lives in inst/shiny/app.R as a plain single-file Shiny
# app; pv_demo() below is the front door. The helpers here are the parts
# the app cannot do politely from the outside - swapping the session
# theme and locale around a widget build and putting them back, and
# handing the page the bundled Inter font - so the app reaches for them
# with polyviz:::, the same way the tests do.

# Thin wrapper so tests can pretend shiny is not installed.
demo_has_shiny <- function() {
  requireNamespace("shiny", quietly = TRUE)
}

demo_need_shiny <- function() {
  if (!demo_has_shiny()) {
    rlang::abort(paste(
      "The demo app needs the shiny package.",
      'Install it with install.packages("shiny").'))
  }
}

# Builds one widget under the demo's header-row choices. The theme and
# locale are session state (theme-api.R, locale.R), and the demo runs
# inside whatever session launched it, so this snapshots that state,
# applies the demo's own settings, builds, and puts everything back on
# the way out - on.exit, so even a build that errors cannot leak the
# demo's theme into the user's session.
demo_with_settings <- function(theme = "default", locale = "none", build) {
  old_theme <- the$theme
  old_font <- the$font
  old_locale <- the$locale
  on.exit({
    the$theme <- old_theme
    the$font <- old_font
    the$locale <- old_locale
  })
  if (identical(theme, "paper")) {
    pv_set_theme(pv_theme_paper())
  } else {
    pv_reset_theme()
  }
  if (is.character(locale) && length(locale) == 1 && !is.na(locale) &&
      locale %in% names(pv_locales)) {
    pv_locale(locale)
  } else {
    pv_locale(NULL)
  }
  build()
}

# The ink tokens the demo page paints its own chrome with - the active
# theme's when the header says paper, the packaged ones otherwise - so
# the page and the widgets on it always share a surface.
demo_ink <- function(theme = "default") {
  if (identical(theme, "paper")) pv_theme_paper()$ink else pv_colors$ink
}

# The bundled Inter font as a dependency the demo page itself can carry,
# so the page chrome wears the same face as the widgets before the first
# one loads. Name and version match the pv-fonts entry in pvchart.yaml;
# htmltools then de-duplicates it against the widgets' own copy.
demo_font_dependency <- function() {
  htmltools::htmlDependency(
    name = "pv-fonts", version = "0.2.0",
    src = system.file("htmlwidgets", "lib", "pv-fonts", package = "polyviz"),
    stylesheet = "inter.css")
}

#' Launch the polyviz demo gallery
#'
#' A Shiny app that shows every chart family working in one place, on
#' the bundled Swiss data: a page per family (comparison, distribution,
#' evolution, composition, relational, geo, motion, tables and
#' matrices), each chart with live controls for its main options. The
#' header row switches the theme (packaged or [pv_theme_paper()]), the
#' locale ([pv_locale()]), and light/dark mode for every chart at once,
#' and an always-visible events panel prints the `input$<id>_<event>`
#' values the charts send back - the Shiny round-trip made visible.
#'
#' The global controls work by rebuilding the widgets server-side with
#' [pv_set_theme()] and [pv_locale()] around each build; the app
#' snapshots and restores the session's own theme and locale around
#' every build, so running the demo never changes the settings of the
#' session that launched it.
#'
#' @param launch Run the app right away? `TRUE` (default) blocks until
#'   the app is closed, as [shiny::runApp()] does. `FALSE` only returns
#'   the app object - the form programmatic drivers want: hand it to
#'   [shiny::testServer()] or a `shinytest2::AppDriver` to script the
#'   app without a browser, or to [shiny::runApp()] yourself to pick the
#'   port and host.
#' @return The [shiny::shinyAppDir()] app object, invisibly.
#' @examples
#' if (interactive()) {
#'   pv_demo()
#' }
#'
#' \dontrun{
#' # Programmatic use: get the app object without launching it, then
#' # drive it yourself - shiny::testServer, shinytest2, or runApp with
#' # your own port.
#' app <- pv_demo(launch = FALSE)
#' shiny::runApp(app, port = 4321)
#' }
#' @export
pv_demo <- function(launch = TRUE) {
  if (!isTRUE(launch) && !isFALSE(launch)) {
    rlang::abort("`launch` must be TRUE or FALSE.")
  }
  demo_need_shiny()
  app <- shiny::shinyAppDir(system.file("shiny", package = "polyviz"))
  if (launch) {
    shiny::runApp(app)
  }
  invisible(app)
}
