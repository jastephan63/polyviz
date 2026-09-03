# The polyviz demo gallery - the living proof that every widget works,
# and a manual test bench in one place. One page per chart family, every
# chart on the bundled Swiss data, each with live controls for the
# options that teach the most. The header row swaps the theme, locale,
# and light/dark mode for the whole page by rebuilding the widgets
# server-side, and the events panel in the corner prints the input
# values charts send back to R as you click and hover them.
#
# Launch it with pv_demo(); the page chrome below reuses the package's
# own ink tokens (pv_colors) and the bundled Inter font, so the gallery
# looks like a polyviz report, not a default Shiny app.

library(shiny)
library(polyviz)

`%||%` <- function(a, b) if (is.null(a)) b else a

# ---- the data on show -------------------------------------------------
# Everything is prepared once, at app load, from the bundled datasets.

sales_mix <- aggregate(revenue ~ region + product, pv_sales, sum)

fiscal25 <- pv_fiscal[pv_fiscal$year == 2025, ]
equal_top <- utils::head(fiscal25[order(-fiscal25$equalization_chf), ], 25)

commuters_latest <- pv_commuters[
  pv_commuters$period == max(pv_commuters$period), ]

growth_cities <- pv_city_population[
  pv_city_population$city %in% c("Luzern", "Emmen", "Kriens", "Zug"), ]

landuse_lucerne <- pv_city_landuse[pv_city_landuse$city == "Luzern", ]

# Council seats by party, the seven biggest parties plus "Other" - the
# same fold the render suite uses, so the donut never runs out of
# palette slots.
seats <- aggregate(elected ~ party,
                   pv_elections[pv_elections$year == 2024, ], sum)
seats <- seats[order(-seats$elected), ]
seats$party[-(1:7)] <- "Other"
seats <- aggregate(elected ~ party, seats, sum)
seats <- seats[order(-seats$elected), ]

# Two-stage path to a council seat: party to sex, sex to outcome.
elect24 <- pv_elections[pv_elections$year == 2024, ]
outcome <- ifelse(elect24$elected, "elected", "not elected")
seat_links <- rbind(
  aggregate(list(value = rep(1, nrow(elect24))),
            list(source = elect24$party, target = elect24$sex), sum),
  aggregate(list(value = rep(1, nrow(elect24))),
            list(source = elect24$sex, target = outcome), sum))

# A generated point cloud big enough to make the scatter's three drawing
# modes worth comparing: two overlapping regimes, six thousand points.
# The fixed seed keeps the cloud identical across rebuilds, so switching
# modes compares drawings, not data.
demo_cloud <- local({
  set.seed(1291)
  x <- c(rnorm(3600, -0.8, 0.9), rnorm(2400, 1.6, 0.7))
  y <- 0.8 * x + c(rnorm(3600, 0, 0.7), rnorm(2400, 1.2, 0.5))
  data.frame(x = round(x, 3), y = round(y, 3))
})

# Urban population by canton for the country-wide choropleth. The BFS
# issues municipality numbers in cantonal blocks, so each city's canton
# number is a findInterval() lookup away.
canton_pop <- local({
  blocks <- c(1, 301, 1001, 1201, 1301, 1401, 1501, 1601, 1701, 2001,
              2401, 2701, 2761, 2901, 3001, 3101, 3201, 3501, 4001,
              4401, 5001, 5401, 6001, 6401, 6601, 6701)
  cities <- merge(pv_city_population[pv_city_population$year == 2024, ],
                  pv_city_coords[, c("city", "id")], by = "city")
  aggregate(list(population = cities$population),
            list(canton = findInterval(cities$id, blocks)), sum)
})

city_bubbles <- merge(pv_city_coords,
                      pv_city_population[pv_city_population$year == 2024, ],
                      by = "city")

# The largest cities as a table: today's population, growth since 1990,
# and the full census series as a sparkline per row.
city_table <- local({
  pop <- pv_city_population[order(pv_city_population$city,
                                  pv_city_population$year), ]
  now <- pop[pop$year == 2024, c("city", "population")]
  then <- pop[pop$year == 1990, c("city", "population")]
  tab <- merge(now, then, by = "city", suffixes = c("", "_1990"))
  tab$growth <- 100 * (tab$population / tab$population_1990 - 1)
  tab$trend <- I(split(pop$population, pop$city)[tab$city])
  utils::head(tab[order(-tab$population), ], 30)
})

# Employment shares for the twelve biggest cities, for the matrix view.
sector_matrix <- local({
  pop24 <- pv_city_population[pv_city_population$year == 2024, ]
  top12 <- utils::head(pop24$city[order(-pop24$population)], 12)
  pv_city_sectors[pv_city_sectors$city %in% top12, ]
})

bfs <- "Swiss Federal Statistical Office"

# ---- page chrome ------------------------------------------------------

# CSS custom properties from one set of ink tokens, shared with the
# report stylesheet's approach so the page and the widgets always agree
# on colours.
demo_vars <- function(i) {
  paste0("--surface:", i$surface, ";--primary:", i$primary,
         ";--secondary:", i$secondary, ";--muted:", i$muted,
         ";--grid:", i$grid, ";--baseline:", i$baseline, ";")
}

# The :root variable block for the current theme and mode choices. With
# mode "auto" the light tokens are the default and a media query swaps
# in the dark set, mirroring what the widgets do; a forced mode bakes in
# one set. Re-rendered by the server whenever the header row changes.
demo_root_css <- function(theme = "default", mode = "auto") {
  ink <- polyviz:::demo_ink(theme)
  base <- if (mode == "dark") ink$dark else ink$light
  scheme <- switch(mode, auto = "light dark", light = "light",
                   dark = "dark")
  dark_block <- if (mode == "auto") {
    paste0("@media (prefers-color-scheme: dark) { :root { ",
           demo_vars(ink$dark), " } }\n")
  } else {
    ""
  }
  paste0(":root { color-scheme: ", scheme, "; ", demo_vars(base), " }\n",
         dark_block)
}

demo_css <- paste0(
  "* { box-sizing: border-box; }\n",
  "body { margin: 0; background: var(--surface); color: var(--primary);\n",
  "       font-family: ", polyviz:::pv_font_stack(), ";\n",
  "       font-size: 14px; }\n",
  ".pv-shell { max-width: 1200px; margin: 0 auto;\n",
  "            padding: 0 22px 260px; }\n",
  # header row: brand on the left, the three global controls on the right
  ".pv-header { display: flex; flex-wrap: wrap; align-items: flex-end;\n",
  "             justify-content: space-between; gap: 14px 32px;\n",
  "             padding: 30px 0 16px; }\n",
  ".pv-brand h1 { font-size: 26px; letter-spacing: -0.02em; margin: 0; }\n",
  ".pv-brand p { margin: 3px 0 0; color: var(--secondary);\n",
  "              font-size: 13.5px; }\n",
  ".pv-globals { display: flex; gap: 26px; flex-wrap: wrap;\n",
  "              padding-bottom: 2px; }\n",
  ".pv-global-label { display: block; font-size: 10.5px;\n",
  "                   letter-spacing: 0.08em; text-transform: uppercase;\n",
  "                   color: var(--muted); margin-bottom: 1px; }\n",
  # shiny form controls, stripped back to hairlines and small type
  ".form-group { margin: 0; }\n",
  ".shiny-options-group { display: flex; gap: 12px; flex-wrap: wrap; }\n",
  "label.radio-inline, .checkbox label { font-weight: 400;\n",
  "    font-size: 12.5px; color: var(--secondary); cursor: pointer;\n",
  "    padding-left: 0; margin: 0; display: inline-flex;\n",
  "    align-items: center; gap: 5px; }\n",
  ".radio-inline input[type=radio], .checkbox input[type=checkbox] {\n",
  "    accent-color: var(--primary); margin: 0; position: static; }\n",
  ".radio-inline + .radio-inline { margin-left: 0; }\n",
  ".checkbox { margin: 0; }\n",
  # family navigation: quiet tabs on one baseline hairline
  ".nav-tabs { border-bottom: 1px solid var(--baseline);\n",
  "            display: flex; flex-wrap: wrap; }\n",
  ".nav-tabs > li { float: none; }\n",
  ".nav-tabs > li > a { border: none; border-radius: 0;\n",
  "    padding: 8px 2px 9px; margin: 0 24px 0 0; font-size: 13.5px;\n",
  "    color: var(--secondary); background: transparent;\n",
  "    border-bottom: 2px solid transparent; }\n",
  ".nav-tabs > li > a:hover, .nav-tabs > li > a:focus {\n",
  "    color: var(--primary); background: transparent;\n",
  "    border-color: transparent; }\n",
  ".nav-tabs > li.active > a, .nav-tabs > li.active > a:hover,\n",
  ".nav-tabs > li.active > a:focus { color: var(--primary);\n",
  "    background: transparent; border: none;\n",
  "    border-bottom: 2px solid var(--primary); }\n",
  ".tab-content { padding-top: 20px; }\n",
  # the chart grid and its cards
  ".pv-grid { display: grid; gap: 18px;\n",
  "           grid-template-columns: repeat(auto-fit,\n",
  "                                         minmax(430px, 1fr)); }\n",
  "@media (max-width: 720px) { .pv-grid { grid-template-columns: 1fr; } }\n",
  ".pv-card { border: 1px solid var(--grid); border-radius: 10px;\n",
  "           padding: 12px 16px 8px; min-width: 0; }\n",
  ".pv-wide { grid-column: 1 / -1; }\n",
  ".pv-card-controls { display: flex; align-items: center; gap: 20px;\n",
  "    flex-wrap: wrap; border-bottom: 1px solid var(--grid);\n",
  "    padding: 0 0 9px; margin-bottom: 10px; min-height: 26px; }\n",
  ".pv-ctl { display: flex; align-items: center; gap: 8px; }\n",
  ".pv-ctl .control-label { font-weight: 400; font-size: 10.5px;\n",
  "    text-transform: uppercase; letter-spacing: 0.07em;\n",
  "    color: var(--muted); margin: 0; }\n",
  ".pv-ctl .form-group { display: flex; align-items: center;\n",
  "                      gap: 8px; }\n",
  # the events panel: fixed in the corner, last few events newest first
  ".pv-events { position: fixed; right: 18px; bottom: 18px;\n",
  "    width: 380px; max-width: calc(100vw - 36px);\n",
  "    background: var(--surface); border: 1px solid var(--baseline);\n",
  "    border-radius: 10px; box-shadow: 0 8px 28px rgba(0, 0, 0, 0.16);\n",
  "    padding: 10px 14px 12px; z-index: 50; }\n",
  ".pv-events h2 { font-size: 11px; margin: 0 0 7px; font-weight: 600;\n",
  "    letter-spacing: 0.07em; text-transform: uppercase;\n",
  "    color: var(--muted); }\n",
  ".pv-event { font-family: ui-monospace, 'SF Mono', Menlo, Consolas,\n",
  "    monospace; font-size: 11px; line-height: 1.6;\n",
  "    color: var(--secondary); white-space: nowrap; overflow: hidden;\n",
  "    text-overflow: ellipsis; }\n",
  ".pv-event-name { color: var(--primary); }\n",
  ".pv-events-empty { color: var(--muted); font-size: 12px; margin: 0;\n",
  "                   line-height: 1.5; }\n")

# ---- small UI builders ------------------------------------------------

# One global control in the header row: a tiny caption over an inline
# radio group.
demo_global <- function(id, label, choices, selected = choices[[1]]) {
  div(class = "pv-global",
      tags$span(class = "pv-global-label", label),
      radioButtons(id, NULL, choices, selected = selected, inline = TRUE))
}

# One chart card: an optional control row, then the widget.
demo_card <- function(outputId, ..., height = "400px", wide = FALSE) {
  controls <- list(...)
  div(class = paste(c("pv-card", if (wide) "pv-wide"), collapse = " "),
      if (length(controls)) div(class = "pv-card-controls", controls),
      pvchartOutput(outputId, height = height))
}

demo_radio <- function(id, label, choices, selected = choices[[1]]) {
  div(class = "pv-ctl",
      radioButtons(id, label, choices, selected = selected, inline = TRUE))
}

demo_check <- function(id, label, value = FALSE) {
  div(class = "pv-ctl", checkboxInput(id, label, value = value))
}

# Every widget the app renders; the events panel watches these ids.
demo_chart_ids <- c(
  "cmp_bar", "cmp_lollipop", "cmp_commuters",
  "dist_hist", "dist_violin", "dist_ridge",
  "evo_line", "evo_zoom", "evo_area", "evo_calendar",
  "comp_donut", "comp_treemap", "comp_sunburst",
  "rel_scatter", "rel_force", "rel_arc", "rel_sankey",
  "geo_lucerne", "geo_cantons", "geo_bubbles",
  "mot_race", "mot_bump",
  "tab_table", "tab_heatmap", "tab_pairs")

# ---- UI ---------------------------------------------------------------

ui <- fluidPage(
  title = "polyviz demo",
  polyviz:::demo_font_dependency(),
  tags$head(
    # The default look, baked in so the page never flashes unstyled;
    # the server swaps the :root block when the header row changes.
    tags$style(HTML(paste0(demo_root_css(), demo_css)))),
  uiOutput("theme_css"),
  div(
    class = "pv-shell",
    div(class = "pv-header",
        div(class = "pv-brand",
            h1("polyviz"),
            p(paste("Every chart family, live, on the bundled Swiss",
                    "data. The header controls rebuild each widget",
                    "server-side."))),
        div(class = "pv-globals",
            demo_global("theme", "Theme", c("default", "paper")),
            demo_global("locale", "Locale",
                        c("none", "de-CH", "fr-CH", "it-CH")),
            demo_global("mode", "Mode", c("auto", "light", "dark")))),
    tabsetPanel(
      id = "family",
      type = "tabs",
      tabPanel(
        "Comparison",
        div(class = "pv-grid",
            demo_card("cmp_bar",
                      demo_radio("cmp_stack", "stack",
                                 c("none", "stack", "percent")),
                      demo_check("cmp_textures", "textures")),
            demo_card("cmp_lollipop",
                      demo_check("cmp_sort", "sort by value",
                                 value = TRUE)),
            demo_card("cmp_commuters",
                      demo_check("cmp_labels", "value labels"),
                      wide = TRUE))),
      tabPanel(
        "Distribution",
        div(class = "pv-grid",
            demo_card("dist_hist",
                      demo_radio("dist_bins", "bins",
                                 c("10", "20", "40"), selected = "20"),
                      demo_check("dist_density", "density curve")),
            demo_card("dist_violin",
                      demo_check("dist_box", "box overlay",
                                 value = TRUE),
                      demo_check("dist_points", "point overlay")),
            demo_card("dist_ridge", wide = TRUE))),
      tabPanel(
        "Evolution",
        div(class = "pv-grid",
            demo_card("evo_line",
                      demo_radio("evo_curve", "curve",
                                 c("linear", "monotone", "step")),
                      demo_check("evo_points", "show points")),
            demo_card("evo_zoom",
                      demo_check("evo_zoom_on", "zoom & pan",
                                 value = TRUE)),
            demo_card("evo_area",
                      demo_radio("evo_offset", "offset",
                                 c("stacked", "percent", "stream")),
                      wide = TRUE),
            demo_card("evo_calendar", wide = TRUE))),
      tabPanel(
        "Composition",
        div(class = "pv-grid",
            demo_card("comp_donut",
                      demo_radio("comp_inner", "inner radius",
                                 c("0", "0.62", "0.8"),
                                 selected = "0.62")),
            demo_card("comp_treemap",
                      demo_radio("comp_labels", "labels",
                                 c("auto", "on", "off"))),
            demo_card("comp_sunburst", wide = TRUE))),
      tabPanel(
        "Relational",
        div(class = "pv-grid",
            demo_card("rel_scatter",
                      demo_radio("rel_draw", "drawn as",
                                 c("points", "canvas", "contours"))),
            demo_card("rel_force"),
            demo_card("rel_arc",
                      demo_radio("rel_order", "node order",
                                 c("auto", "none")),
                      wide = TRUE),
            demo_card("rel_sankey",
                      demo_radio("rel_align", "align",
                                 c("justify", "left", "right", "center")),
                      wide = TRUE))),
      tabPanel(
        "Geo",
        div(class = "pv-grid",
            demo_card("geo_lucerne",
                      demo_radio("geo_palette", "palette",
                                 c("sequential", "diverging"),
                                 selected = "diverging")),
            demo_card("geo_cantons"),
            demo_card("geo_bubbles", wide = TRUE, height = "460px"))),
      tabPanel(
        "Motion",
        div(class = "pv-grid",
            demo_card("mot_race",
                      demo_radio("mot_race_n", "bars",
                                 c("5", "10", "15"), selected = "10"),
                      height = "460px"),
            demo_card("mot_bump",
                      demo_radio("mot_bump_n", "ranks",
                                 c("5", "8", "10"), selected = "8"),
                      height = "460px"))),
      tabPanel(
        "Tables & matrices",
        div(class = "pv-grid",
            demo_card("tab_table",
                      demo_check("tab_bars", "bars", value = TRUE),
                      demo_check("tab_shade", "shade", value = TRUE),
                      demo_radio("tab_paging", "rows",
                                 c("all", "10 per page"),
                                 selected = "10 per page"),
                      wide = TRUE, height = "520px"),
            demo_card("tab_heatmap",
                      demo_check("tab_values", "cell values",
                                 value = TRUE),
                      wide = TRUE, height = "460px"),
            demo_card("tab_pairs", wide = TRUE, height = "520px")))),
    div(class = "pv-events",
        h2("Shiny events"),
        uiOutput("events"))))

# ---- server -----------------------------------------------------------

server <- function(input, output, session) {

  # Every widget goes through here: built under the header row's theme
  # and locale, with the session's own settings restored the moment the
  # widget exists. The %||% fallbacks keep programmatic drivers
  # (shiny::testServer) working before any inputs are set.
  demo_widget <- function(build) {
    polyviz:::demo_with_settings(
      theme = input$theme %||% "default",
      locale = input$locale %||% "none",
      build = build)
  }

  mode <- reactive(input$mode %||% "auto")

  # --- comparison ------------------------------------------------------

  output$cmp_bar <- renderPvchart(demo_widget(function() {
    w <- pv_bar(sales_mix, x = "region", y = "revenue",
                series = "product",
                stack = input$cmp_stack %||% "none",
                title = "Revenue by region and product",
                subtitle = "Two years of simulated sales",
                mode = mode())
    if (isTRUE(input$cmp_textures)) pv_textures(w) else w
  }))

  output$cmp_lollipop <- renderPvchart(demo_widget(function() {
    pv_lollipop(equal_top, x = "municipality", y = "equalization_chf",
                sort = isTRUE(input$cmp_sort %||% TRUE),
                title = "Where fiscal equalization flows",
                subtitle = "The 25 largest payments, 2025",
                source = "LUSTAT Statistik Luzern", mode = mode())
  }))

  output$cmp_commuters <- renderPvchart(demo_widget(function() {
    pv_bar(commuters_latest, x = "region", y = "commuters",
           series = "direction",
           value_labels = isTRUE(input$cmp_labels),
           title = "Commuter exchange with Canton Zug",
           subtitle = paste("Both directions,",
                            max(pv_commuters$period)),
           source = bfs, mode = mode())
  }))

  # --- distribution ----------------------------------------------------

  output$dist_hist <- renderPvchart(demo_widget(function() {
    pv_histogram(fiscal25, x = "resource_per_capita",
                 bins = as.numeric(input$dist_bins %||% "20"),
                 density = isTRUE(input$dist_density),
                 title = "Municipal tax bases",
                 subtitle = "Resource potential per resident, 2025",
                 source = "LUSTAT Statistik Luzern", mode = mode())
  }))

  output$dist_violin <- renderPvchart(demo_widget(function() {
    pv_violin(pv_fiscal, value = "resource_per_capita", group = "year",
              box = isTRUE(input$dist_box %||% TRUE),
              points = isTRUE(input$dist_points),
              title = "The same skewed shape, year after year",
              subtitle = "Tax resources across 79 municipalities",
              mode = mode())
  }))

  output$dist_ridge <- renderPvchart(demo_widget(function() {
    pv_ridgeline(pv_fiscal, value = "resource_index", group = "year",
                 title = "Resource index across municipalities",
                 subtitle = "One ridge per year; 100 is the cantonal mean",
                 mode = mode())
  }))

  # --- evolution -------------------------------------------------------

  output$evo_line <- renderPvchart(demo_widget(function() {
    pv_line(growth_cities, x = "year", y = "population", series = "city",
            show_points = isTRUE(input$evo_points),
            curve = input$evo_curve %||% "linear",
            title = "A century of urban growth",
            subtitle = "Census population since 1930",
            source = bfs, mode = mode())
  }))

  output$evo_zoom <- renderPvchart(demo_widget(function() {
    pv_line(pv_weather, x = "date", y = "temp_mean",
            zoom = isTRUE(input$evo_zoom_on %||% TRUE),
            title = "Six years of daily means",
            subtitle = "MeteoSwiss station Luzern; drag to zoom",
            source = "MeteoSwiss", mode = mode())
  }))

  output$evo_area <- renderPvchart(demo_widget(function() {
    pv_area(pv_electricity, x = "date", y = "gwh", series = "source",
            offset = input$evo_offset %||% "stacked",
            title = "Where Swiss electricity comes from",
            subtitle = "Monthly production by source, GWh",
            source = "Swiss Federal Office of Energy", mode = mode())
  }))

  output$evo_calendar <- renderPvchart(demo_widget(function() {
    pv_calendar(pv_weather, date = "date", value = "temp_max",
                years = 2024:2025,
                title = "Two years of daily maximums",
                subtitle = "One cell per day, one row per weekday",
                source = "MeteoSwiss", mode = mode())
  }))

  # --- composition -----------------------------------------------------

  output$comp_donut <- renderPvchart(demo_widget(function() {
    pv_donut(seats, category = "party", value = "elected",
             inner_radius = as.numeric(input$comp_inner %||% "0.62"),
             title = "Council seats by party",
             subtitle = "Lucerne municipal elections, 2024",
             source = "LUSTAT Statistik Luzern", mode = mode())
  }))

  output$comp_treemap <- renderPvchart(demo_widget(function() {
    pv_treemap(landuse_lucerne, levels = c("group", "category"),
               value = "hectares",
               labels = switch(input$comp_labels %||% "auto",
                               auto = "auto", on = TRUE, off = FALSE),
               title = "How Lucerne uses its land",
               subtitle = "Hectares by land-use category",
               source = bfs, mode = mode())
  }))

  output$comp_sunburst <- renderPvchart(demo_widget(function() {
    pv_sunburst(landuse_lucerne, levels = c("group", "category"),
                value = "hectares",
                title = "Lucerne's land, ring by ring",
                subtitle = "Click a segment to zoom in, the centre to back out",
                source = bfs, mode = mode())
  }))

  # --- relational ------------------------------------------------------

  output$rel_scatter <- renderPvchart(demo_widget(function() {
    draw <- input$rel_draw %||% "points"
    pv_scatter(demo_cloud, x = "x", y = "y",
               density = identical(draw, "contours"),
               canvas = identical(draw, "canvas"),
               title = "Six thousand generated points",
               subtitle = switch(
                 draw,
                 points = "Every point its own SVG circle",
                 canvas = "The same cloud on a single canvas layer",
                 contours = "The cloud summarised as density contours"),
               mode = mode())
  }))

  output$rel_force <- renderPvchart(demo_widget(function() {
    pv_force(pv_network$nodes, pv_network$links, group = "group",
             title = "Collaboration network",
             subtitle = "Drag a node; click one to log it",
             mode = mode())
  }))

  output$rel_arc <- renderPvchart(demo_widget(function() {
    pv_arc(pv_network$nodes, pv_network$links, group = "group",
           order = input$rel_order %||% "auto",
           title = "The same network, name by name",
           subtitle = paste("\"auto\" reduces arc crossings;",
                            "\"none\" keeps arrival order"),
           mode = mode())
  }))

  output$rel_sankey <- renderPvchart(demo_widget(function() {
    pv_sankey(seat_links, align = input$rel_align %||% "justify",
              title = "The path to a council seat",
              subtitle = "Candidacies by party, sex, and outcome, 2024",
              note = "LUSTAT Statistik Luzern", mode = mode())
  }))

  # --- geo -------------------------------------------------------------

  output$geo_lucerne <- renderPvchart(demo_widget(function() {
    diverging <- identical(input$geo_palette %||% "diverging",
                           "diverging")
    pv_choropleth(fiscal25, id = "municipality_id",
                  value = "resource_index",
                  palette = if (diverging) "diverging" else "sequential",
                  center = if (diverging) 100,
                  title = "Lucerne's wealth wears a lakeside ring",
                  subtitle = if (diverging) {
                    "Resource index, diverging around the mean of 100"
                  } else {
                    "Resource index on the sequential ramp"
                  },
                  source = "LUSTAT Statistik Luzern", mode = mode())
  }))

  output$geo_cantons <- renderPvchart(demo_widget(function() {
    pv_choropleth(canton_pop, map = "cantons", id = "canton",
                  value = "population",
                  title = "Where urban Switzerland lives",
                  subtitle = "City population by canton, 2024",
                  source = bfs, mode = mode())
  }))

  output$geo_bubbles <- renderPvchart(demo_widget(function() {
    pv_bubble_map(city_bubbles, lon = "lon", lat = "lat",
                  size = "population", label = "city",
                  title = "Every Swiss city is a circle",
                  subtitle = "Sized by 2024 population; hover to read out",
                  source = bfs, mode = mode())
  }))

  # --- motion ----------------------------------------------------------

  output$mot_race <- renderPvchart(demo_widget(function() {
    pv_race(pv_city_population, time = "year", id = "city",
            value = "population",
            top_n = as.numeric(input$mot_race_n %||% "10"),
            title = "Swiss cities racing through a century",
            subtitle = "Census population since 1930",
            source = bfs, mode = mode())
  }))

  output$mot_bump <- renderPvchart(demo_widget(function() {
    pv_bump(pv_city_population, time = "year", id = "city",
            value = "population",
            top_n = as.numeric(input$mot_bump_n %||% "8"),
            title = "Who overtook whom since 1930",
            subtitle = "Rank by census population",
            source = bfs, mode = mode())
  }))

  # --- tables & matrices -----------------------------------------------

  output$tab_table <- renderPvchart(demo_widget(function() {
    paged <- identical(input$tab_paging %||% "10 per page",
                       "10 per page")
    pv_table(city_table,
             columns = c("city", "population", "growth", "trend"),
             bars = if (isTRUE(input$tab_bars %||% TRUE)) "population",
             shade = if (isTRUE(input$tab_shade %||% TRUE)) "growth",
             spark = "trend",
             page_size = if (paged) 10,
             title = "The largest Swiss cities in numbers",
             subtitle = "Population 2024, growth since 1990, and the full series",
             source = bfs, mode = mode())
  }))

  output$tab_heatmap <- renderPvchart(demo_widget(function() {
    pv_heatmap(sector_matrix, x = "city", y = "sector", value = "share",
               cell_values = if (isTRUE(input$tab_values %||% TRUE)) {
                 "auto"
               } else {
                 FALSE
               },
               title = "Where Swiss city jobs are",
               subtitle = "Share of employment by sector, twelve largest cities",
               source = bfs, mode = mode())
  }))

  output$tab_pairs <- renderPvchart(demo_widget(function() {
    f <- fiscal25
    f$side <- ifelse(f$equalization_chf > 0, "receives", "contributes")
    pv_pairs(f, columns = c("resource_per_capita", "resource_index",
                            "equalization_chf"),
             color = "side", label = "municipality",
             title = "Every pair of fiscal measures at once",
             mode = mode())
  }))

  # --- page chrome follows the header row ------------------------------

  output$theme_css <- renderUI({
    tags$style(HTML(demo_root_css(theme = input$theme %||% "default",
                                  mode = mode())))
  })

  # --- the events panel ------------------------------------------------

  # The charts report interactions as input$<outputId>_<event>; one
  # observer per chart and event kind funnels them all into a short log,
  # newest first, so the Shiny round-trip is visible while you test.
  demo_events <- reactiveVal(list())

  demo_event_text <- function(payload) {
    text <- tryCatch(
      as.character(jsonlite::toJSON(payload, auto_unbox = TRUE,
                                    digits = 4, null = "null")),
      error = function(e) paste(utils::capture.output(str(payload)),
                                collapse = " "))
    if (nchar(text) > 120) paste0(substr(text, 1, 120), "\u2026") else text
  }

  for (chart_id in demo_chart_ids) {
    for (kind in c("click", "hover", "brush")) {
      local({
        input_name <- paste(chart_id, kind, sep = "_")
        observeEvent(input[[input_name]], {
          entry <- list(time = format(Sys.time(), "%H:%M:%S"),
                        name = input_name,
                        text = demo_event_text(input[[input_name]]))
          demo_events(utils::head(c(list(entry), demo_events()), 5))
        }, ignoreInit = TRUE)
      })
    }
  }

  output$events <- renderUI({
    entries <- demo_events()
    if (!length(entries)) {
      return(p(class = "pv-events-empty",
               paste("Interact with a chart - click a bar, a slice, a",
                     "municipality - and the value it sends to R as",
                     "input$<id>_<event> appears here.")))
    }
    div(lapply(entries, function(e) {
      div(class = "pv-event",
          paste0(e$time, "  "),
          tags$span(class = "pv-event-name",
                    paste0("input$", e$name)),
          paste0("  ", e$text))
    }))
  })
}

shinyApp(ui, server)
