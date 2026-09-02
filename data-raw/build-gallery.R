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
  force = "Force-directed network", chord = "Chord diagram",
  arc = "Arc diagram",
  sunburst = "Zoomable sunburst", histogram = "Histogram",
  boxplot = "Boxplot", ridgeline = "Ridgeline", donut = "Donut chart",
  treemap = "Treemap", lollipop = "Lollipop chart", area = "Stacked area",
  `area-stream` = "Streamgraph",
  heatmap = "Heatmap", sankey = "Sankey diagram",
  parallel = "Parallel coordinates", pack = "Circle packing",
  dendrogram = "Dendrogram", choropleth = "Choropleth map",
  `choropleth-cantons` = "Country-wide choropleth",
  `bubble-map` = "Bubble map",
  race = "Bar-chart race", bump = "Bump chart", beeswarm = "Beeswarm",
  pairs = "Scatterplot matrix", table = "Data table",
  violin = "Violin plot", calendar = "Calendar heatmap",
  annotated = "Annotations & trends", facet = "Small multiples",
  `bar-stacked` = "Stacked bars", `line-points` = "Connected scatter",
  `line-zoom` = "Brush to zoom", `violin-points` = "Violin with raw points",
  `bar-textured` = "Print-ready", `bar-locale` = "Swiss number format"
)

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
    widget$height <- 470
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
      'devtools::install_github("jastephan63/polyviz").'))
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
