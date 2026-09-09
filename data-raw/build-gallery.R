# Builds the live demo gallery (docs/index.html, served by GitHub Pages).
# Every chart on the page is a real interactive widget running on the
# package's bundled open datasets, with the explanation text pulled from
# the data-raw/gallery-*.R snippet files.
#
# Snippet format (see gallery-core.R):
#   ## <id>
#   # explain: <text, continued on "#"-prefixed lines>
#   <R expression(s); the last value must be a polyviz widget>
#
# Run from the package root: LANG=en_US.UTF-8 Rscript data-raw/build-gallery.R

suppressMessages(pkgload::load_all(".", quiet = TRUE))
library(htmltools)

display_names <- c(
  bar = "Bar chart", line = "Line chart", scatter = "Scatter plot",
  `scatter-density` = "Density contours",
  `scatter-hex` = "Hexagonal binning",
  force = "Force-directed network", chord = "Chord diagram",
  arc = "Arc diagram",
  sunburst = "Zoomable sunburst", icicle = "Zoomable icicle",
  slope = "Slope chart", dumbbell = "Dumbbell chart",
  waterfall = "Waterfall chart", bullet = "Bullet chart",
  waffle = "Waffle chart", histogram = "Histogram",
  boxplot = "Boxplot", ridgeline = "Ridgeline", donut = "Donut chart",
  treemap = "Treemap", lollipop = "Lollipop chart", area = "Stacked area",
  `area-stream` = "Streamgraph",
  heatmap = "Heatmap", horizon = "Horizon chart",
  sankey = "Sankey diagram",
  parallel = "Parallel coordinates", pack = "Circle packing",
  dendrogram = "Dendrogram", choropleth = "Choropleth map",
  `choropleth-cantons` = "Country-wide choropleth",
  `bubble-map` = "Bubble map", `flow-map` = "Flow map",
  race = "Bar-chart race", bump = "Bump chart", beeswarm = "Beeswarm",
  pairs = "Scatterplot matrix", table = "Data table",
  violin = "Violin plot", calendar = "Calendar heatmap",
  annotated = "Annotations & trends", facet = "Small multiples",
  `bar-stacked` = "Stacked bars", `line-points` = "Connected scatter",
  `line-zoom` = "Brush to zoom", `violin-points` = "Violin with raw points",
  `bar-textured` = "Print-ready", `bar-locale` = "Swiss number format",
  decompose = "Seasonal decomposition", forecast = "Forecast fan",
  changepoints = "Level shifts"
)

# Almost every section shows at the same standard height; the ones that
# genuinely need more room - four stacked decomposition panels would be
# unreadable at 470px - name their own here.
section_heights <- c(decompose = 760)

# Pull the id / explanation / code blocks out of one snippet file.
parse_snippets <- function(path) {
  lines <- readLines(path, warn = FALSE)
  starts <- grep("^## ", lines)
  lapply(seq_along(starts), function(i) {
    from <- starts[i]
    to <- if (i < length(starts)) starts[i + 1] - 1 else length(lines)
    block <- lines[from:to]
    id <- sub("^## ", "", block[1])
    comment <- grepl("^#", block[-1])
    explain <- block[-1][comment]
    explain <- paste(trimws(sub("^#( +explain:)?", "", explain)),
                     collapse = " ")
    code <- block[-1][!comment]
    list(id = id, explain = trimws(explain),
         code = paste(code, collapse = "\n"))
  })
}

files <- c("data-raw/gallery-core.R",
           Sys.glob("data-raw/gallery-*.R"))
files <- unique(files[file.exists(files)])

sections <- list()
for (f in files) {
  for (s in parse_snippets(f)) {
    widget <- eval(parse(text = s$code), envir = new.env(parent = globalenv()))
    widget$width <- "100%"
    widget$height <- if (s$id %in% names(section_heights)) {
      section_heights[[s$id]]
    } else {
      470
    }
    nm <- display_names[[s$id]] %||% s$id
    sections[[length(sections) + 1]] <- tags$section(
      id = s$id, class = "chart",
      tags$h2(nm),
      tags$p(class = "explain", s$explain),
      widget,
      tags$details(
        tags$summary("Show the R code"),
        tags$pre(tags$code(s$code))
      )
    )
    cat("built:", s$id, "\n")
  }
}

toc <- tags$nav(lapply(sections, function(s) {
  tags$a(href = paste0("#", s$attribs$id),
         display_names[[s$attribs$id]] %||% s$attribs$id)
}))

page <- tags$html(lang = "en", tags$head(
  tags$meta(charset = "utf-8"),
  tags$meta(name = "viewport",
            content = "width=device-width, initial-scale=1"),
  tags$title("polyviz gallery"),
  tags$style(HTML("
    :root { color-scheme: light dark; }
    body { margin: 0; background: #fbf9f5; color: #161511;
           font-family: 'InterVariable', 'Inter', system-ui, sans-serif; }
    @media (prefers-color-scheme: dark) {
      body { background: #1b1a18; color: #f6f4ef; }
      header p, .explain, nav a, footer { color: #c7c3b8 !important; }
      details { border-color: #45433d !important; }
    }
    main { max-width: 880px; margin: 0 auto; padding: 0 20px 60px; }
    header { padding: 48px 0 8px; }
    header h1 { font-size: 34px; letter-spacing: -0.02em; margin: 0; }
    header p { color: #57544b; max-width: 640px; line-height: 1.55; }
    nav { display: flex; flex-wrap: wrap; gap: 6px 16px; padding: 10px 0 26px; }
    nav a { color: #57544b; font-size: 13px; text-decoration: none;
            border-bottom: 1px solid transparent; }
    nav a:hover { border-bottom-color: #006ba2; color: #006ba2; }
    header a { color: #006ba2; text-decoration: none;
               border-bottom: 1px solid transparent; }
    header a:hover { border-bottom-color: #006ba2; }
    section.chart { margin: 34px 0; }
    section.chart h2 { font-size: 21px; letter-spacing: -0.01em;
                       margin: 0 0 6px; }
    .explain { color: #57544b; line-height: 1.6; max-width: 720px;
               margin: 0 0 14px; }
    details { margin-top: 10px; border: 1px solid #ddd8cc;
              border-radius: 8px; padding: 8px 12px; }
    summary { cursor: pointer; font-size: 13px; }
    pre { overflow-x: auto; font-size: 12.5px; line-height: 1.5; }
    footer { color: #8b8779; font-size: 12.5px; line-height: 1.6;
             border-top: 1px solid #ddd8cc; padding-top: 18px;
             margin-top: 48px; }
    footer a { color: inherit; }
  "))
), tags$body(tags$main(
  tags$header(
    tags$h1("polyviz"),
    tags$p(paste(
      "Interactive d3.js visualisations driven entirely from R.",
      "Every chart below is live - hover, drag, click, and zoom -",
      "and runs on real Swiss open government data bundled with the",
      "package. Every chart can also be taken along: hover it and the",
      "button in its top-right corner downloads it as a standalone SVG",
      "or a high-resolution PNG. Install with",
      'devtools::install_github("jastephan63/polyviz").')),
    tags$p(tags$a(href = "story.html",
      "Anatomy of a Swiss canton — a data investigation built with polyviz")),
    tags$p(tags$a(href = "board.html",
      "A chart board — several finished charts composed into one page with pv_board()"))
  ),
  toc,
  sections,
  tags$footer(HTML(paste(
    "Data: Bundesamt f&uuml;r Statistik (stats.swiss), MeteoSwiss,",
    "LUSTAT Statistik Luzern, Bundesamt f&uuml;r Energie, and Fachstelle",
    "Statistik Kanton Zug via opendata.swiss - open data, cited per",
    "dataset above; LUSTAT data",
    "additionally requires the owner's permission for commercial use.",
    "Charts rendered by <a href='https://d3js.org'>d3.js</a> v7,",
    "type set in <a href='https://rsms.me/inter/'>Inter</a>.",
    "MIT-licensed R package:",
    "<a href='https://github.com/jastephan63/polyviz'>jastephan63/polyviz</a>."
  )))
)))

dir.create("docs", showWarnings = FALSE)
save_html(page, "docs/index.html", libdir = "lib")
cat("gallery written to docs/index.html with", length(sections),
    "charts\n")

# ---------------------------------------------------------------------
# The board demo page (docs/board.html). A pv_board() is one htmltools
# page, not one widget, so it cannot sit inside the gallery's
# single-widget sections - it gets its own small page instead, linked
# from the gallery header. Built exactly like a gallery section: the
# code below is both evaluated and printed on the page.

board_code <- '# Four finished charts, one page - no Shiny, no R Markdown.
nuclear <- subset(pv_electricity, source == "Nuclear")
chg <- merge(aggregate(gwh ~ source, subset(pv_electricity, year == 2020), sum),
             aggregate(gwh ~ source, subset(pv_electricity, year == 2025), sum),
             by = "source", suffixes = c("_2020", "_2025"))
chg$change <- chg$gwh_2025 - chg$gwh_2020
chg <- chg[order(-chg$change), ]
# Short category labels so the narrow panel keeps them apart; the other
# panels spell the sources out in full.
chg$source <- sub(" hydro", "", chg$source)
mix25 <- aggregate(gwh ~ source, subset(pv_electricity, year == 2025), sum)

pv_board(
  pv_area(pv_electricity, x = "date", y = "gwh", series = "source", offset = "stream",
          xlab = NA, title = "How the mix breathes",
          subtitle = "Monthly production by source, GWh"),
  pv_line(nuclear, x = "date", y = "gwh", ylab = "GWh", xlab = NA,
          title = "Nuclear, with its 2025 level shift",
          subtitle = "Monthly production and the changepoint the data supports") |>
    pv_changepoints(levels = TRUE),
  pv_waterfall(chg, x = "source", y = "change", start = sum(chg$gwh_2020),
               total = "2025", xlab = NA, ylab = "GWh",
               title = "What changed, 2020 to 2025",
               subtitle = "Annual production by source, GWh"),
  pv_donut(mix25, category = "source", value = "gwh",
           title = "The 2025 mix",
           subtitle = "Share of annual production by source"),
  ncol = 2,
  title = "Swiss electricity at a glance",
  subtitle = paste("One pv_board() call: every panel keeps its own",
                   "interactivity, downloads, and alt text."),
  source = "Source: Bundesamt für Energie via opendata.swiss"
)'
board <- eval(parse(text = board_code), envir = new.env(parent = globalenv()))

board_page <- tags$html(lang = "en", tags$head(
  tags$meta(charset = "utf-8"),
  tags$meta(name = "viewport",
            content = "width=device-width, initial-scale=1"),
  tags$title("polyviz chart board"),
  tags$style(HTML("
    :root { color-scheme: light dark; }
    body { margin: 0; background: #fbf9f5; color: #161511;
           font-family: 'InterVariable', 'Inter', system-ui, sans-serif; }
    @media (prefers-color-scheme: dark) {
      body { background: #1b1a18; color: #f6f4ef; }
      header p, .explain { color: #c7c3b8 !important; }
      details { border-color: #45433d !important; }
    }
    main { max-width: 1080px; margin: 0 auto; padding: 0 20px 60px; }
    header { padding: 40px 0 10px; }
    header h1 { font-size: 30px; letter-spacing: -0.02em; margin: 0; }
    header p { color: #57544b; max-width: 720px; line-height: 1.55; }
    header a { color: #006ba2; text-decoration: none;
               border-bottom: 1px solid transparent; }
    header a:hover { border-bottom-color: #006ba2; }
    .explain { color: #57544b; line-height: 1.6; max-width: 720px;
               margin: 0 0 18px; }
    details { margin-top: 14px; border: 1px solid #ddd8cc;
              border-radius: 8px; padding: 8px 12px; }
    summary { cursor: pointer; font-size: 13px; }
    pre { overflow-x: auto; font-size: 12.5px; line-height: 1.5; }
  "))
), tags$body(tags$main(
  tags$header(
    tags$h1("A chart board"),
    tags$p(tags$a(href = "index.html", "← back to the gallery"))
  ),
  tags$p(class = "explain", paste(
    "pv_board() lays finished polyviz charts out as one responsive",
    "page: a CSS grid of panels on the theme's surface, with one",
    "optional heading and one optional source line for the board as a",
    "whole. Nothing else is needed - no Shiny, no R Markdown - and the",
    "result prints in the RStudio viewer, drops into a document chunk,",
    "or saves through pv_save() as a self-contained .html, a .png, or",
    "a .pdf. Every panel below is the untouched widget its chart",
    "function built: hover for tooltips, use each panel's own download",
    "button, and each panel keeps its own generated alt text for",
    "screen readers. The grid collapses to one column on a phone.")),
  board,
  tags$details(
    tags$summary("Show the R code"),
    tags$pre(tags$code(board_code))
  )
)))

save_html(board_page, "docs/board.html", libdir = "lib")
cat("board demo written to docs/board.html\n")
