# Builds the printable cheatsheet: one A4 landscape page that groups all
# 34 chart constructors by intent, states the shared grammar, and lists
# the ways in (data) and out (paper). The page is laid out as HTML in the
# package's own light-mode design tokens (pv_colors), set in the bundled
# Inter, and printed to a true vector PDF by headless Chrome - the same
# way pv_save() prints charts.
#
# Two guarantees are enforced, not hoped for: every pv_* name on the
# sheet must exist in the loaded namespace (or be a bundled dataset),
# and the rendered page must actually fit - any column that overflows the
# page, or any code token wider than its column, stops the build with a
# message naming the culprit.
#
# The intermediate page lands in data-raw/cheatsheet.html; the shipped
# file is docs/polyviz-cheatsheet.pdf.
#
# Run from the package root:
#   LANG=en_US.UTF-8 Rscript data-raw/build-cheatsheet.R

suppressMessages(pkgload::load_all(".", quiet = TRUE))

tag <- htmltools::tags
version <- unname(read.dcf("DESCRIPTION")[1, "Version"])

# ---------------------------------------------------------------------
# Content. The chooser is data, not markup, so the completeness check
# below can hold the sheet to the namespace.

chooser <- list(
  list(
    label = "Compare",
    entries = list(
      c("pv_bar()", "magnitudes by category; stacks by value or to 100%"),
      c("pv_lollipop()", "a ranking with less ink than bars"),
      c("pv_slope()", "two moments, one line per group; the slope is the change"),
      c("pv_dumbbell()", "two values per category, the gap on every row"),
      c("pv_waterfall()", "signed contributions building to a total"),
      c("pv_bullet()", "measures against their targets, in compact rows"),
      c("pv_heatmap()", "one value across two category axes"))),
  list(
    label = "Change over time",
    entries = list(
      c("pv_line()", "a measure over time, a few series"),
      c("pv_area()", "totals over time: stacked, percent or stream"),
      c("pv_bump()", "ranks trading places, period by period"),
      c("pv_race()", "the animated ranking, one frame per period"),
      c("pv_calendar()", "a daily value, laid out year by year"))),
  list(
    label = "Distribution",
    entries = list(
      c("pv_histogram()", "the shape of one numeric variable"),
      c("pv_boxplot()", "five-number summaries across groups"),
      c("pv_violin()", "full densities, group against group"),
      c("pv_ridgeline()", "many densities stacked in rows"),
      c("pv_beeswarm()", "every point visible, none overlapping"))),
  list(
    label = "Composition",
    entries = list(
      c("pv_donut()", "a few shares of one whole"),
      c("pv_waffle()", "shares as countable unit squares"),
      c("pv_treemap()", "many parts sized by value, one or two levels"),
      c("pv_sunburst()", "a hierarchy in rings from the root; zoomable"),
      c("pv_icicle()", "the same hierarchy as columns, labels legible"),
      c("pv_pack()", "a hierarchy as nested circles; zoomable"))),
  list(
    label = "Relationships",
    entries = list(
      c("pv_scatter()", "two numerics; density contours when crowded"),
      c("pv_pairs()", "every numeric pair at once, as a matrix"),
      c("pv_parallel()", "many numeric columns, brushable"),
      c("pv_sankey()", "flows between stages, width carrying volume"),
      c("pv_chord()", "flows around one closed set, from a matrix"),
      c("pv_arc()", "a network on one line, crossings minimised"),
      c("pv_force()", "a network left free to find its clusters"))),
  list(
    label = "Maps",
    entries = list(
      c("pv_choropleth()", "a value per region, shaded on its map"),
      c("pv_bubble_map()", "sized points at real coordinates"))),
  list(
    label = "Special",
    entries = list(
      c("pv_table()",
        "the numbers themselves: sortable, in-cell bars, sparklines"),
      c("pv_dendrogram()", "an hclust tree, cut into k groups"))))

n_charts <- sum(vapply(chooser, function(g) length(g$entries), integer(1)))
stopifnot(n_charts == 34L)

# ---------------------------------------------------------------------
# Small builders, so the markup below reads like the sheet.

fn <- function(x) tag$code(class = "fn", x)

entry <- function(e) {
  tag$div(class = "entry", fn(e[[1]]), tag$span(class = "when", e[[2]]))
}

# An intent group: colour dot, small-caps label, hairline, entries.
group <- function(g, color) {
  tag$div(class = "group",
    tag$div(class = "ghead",
      tag$span(class = "dot", style = paste0("background:", color)),
      tag$span(class = "glabel", g$label)),
    lapply(g$entries, entry))
}

# A utility section in the right-hand columns: no dot - colour belongs
# to the chart intents alone.
section <- function(label, ...) {
  tag$div(class = "group",
    tag$div(class = "ghead", tag$span(class = "glabel", label)),
    ...)
}

# A definition line: code, an em dash, then the clause, wrapping with a
# hanging indent.
def <- function(code, text) {
  tag$p(class = "def", fn(code), " \u2014 ", text)
}

# ---------------------------------------------------------------------
# The four columns.

accents <- pv_colors$categorical$light
ink <- pv_colors$ink$light

col1 <- tag$div(class = "col pick",
  group(chooser[[1]], accents[1]),
  group(chooser[[2]], accents[2]),
  group(chooser[[3]], accents[3]))

col2 <- tag$div(class = "col pick",
  group(chooser[[4]], accents[4]),
  group(chooser[[5]], accents[5]),
  group(chooser[[6]], accents[6]),
  group(chooser[[7]], accents[7]))

col3 <- tag$div(class = "col",
  section("Every chart",
    tag$pre(class = "code",
      paste0('pv_bar(data, x, y, series = NULL,\n',
             '       title, subtitle, source,\n',
             '       mode = "auto")')),
    tag$p(class = "note",
      sprintf("All %d constructors share the title block, the source line and ",
              n_charts),
      fn("mode"), ": ", fn('"auto"'),
      " follows the reader\u2019s light or dark side, ", fn('"light"'),
      " and ", fn('"dark"'), " pin it. Wherever a chart decides for ",
      "itself \u2014 orientation, legends, value labels \u2014 the ",
      "argument takes ", fn("TRUE"), ", ", fn("FALSE"), " or ",
      fn('"auto"'), "."),
    tag$p(class = "note",
      "Degenerate data fails at once with a plain R message, never a ",
      "broken chart.")),
  section("Pipe-able layers",
    tag$p(class = "note lead",
      "Every modifier takes a chart and hands it back:"),
    tag$p(class = "pipe", fn("w |> pv_trend() |> pv_annotate(...)")),
    def("pv_annotate(...)",
        list("layers ", fn("pv_hline()"), ", ", fn("pv_vline()"), ", ",
             fn("pv_band()"), " and ", fn("pv_note()"),
             " onto the plot, in data units")),
    def("pv_trend()",
        list("a fitted trend and its ribbon: ", fn('"loess"'), " or ",
             fn('"lm"'))),
    def("pv_facet(by)", "small multiples on one variable, scales shared"),
    def("pv_textures()",
        "hatches each series, so print and photocopies keep identity"),
    def("pv_static()", "no animation, tooltips or controls"),
    def("pv_downloads(FALSE)", "hides the hover download button"),
    def("pv_link(sd)", "crosstalk selection linking across charts"),
    def('pv_alt("...")',
        list("your own words for screen readers; ", fn("pv_alt_text(w)"),
             " reads what a chart would say"))),
  section("Theme & locale",
    def("pv_set_theme(pv_theme_paper())",
        list("the print-first theme: white surface, luminance-laddered ",
             "colours; ", fn("pv_reset_theme()"), " undoes it")),
    def('pv_locale("de-CH")',
        list("numbers and dates the Swiss way (10\u2019000, M\u00e4rz); ",
             "also ", fn('"fr-CH"'), ", ", fn('"it-CH"'), ", ",
             fn('"en-CH"'), "; ", fn("NULL"), " resets"))))

col4 <- tag$div(class = "col",
  section("Onto paper",
    tag$p(class = "note lead",
      fn('pv_save(w, "chart.png")'),
      " \u2014 the extension picks the format:"),
    tag$div(class = "exts",
      fn(".png"), tag$span(class = "when",
        "crisp 2\u00d7 raster, for Word and slides"),
      fn(".svg"), tag$span(class = "when",
        "standalone vector, font embedded"),
      fn(".pdf"), tag$span(class = "when",
        "a true vector print \u2014 made for LaTeX"),
      fn(".gif"), tag$span(class = "when",
        "the bar-chart race, a frame-exact loop"),
      fn(".html"), tag$span(class = "when",
        "still interactive; needs no Chrome")),
    def('pv_deck(charts, "review.pptx")',
        list("a list of charts to PowerPoint \u2014 one 2\u00d7 capture ",
             "per 16:9 slide, title slide and notes"))),
  section("Knitting to Word & PDF",
    tag$pre(class = "code",
      "knitr::opts_chunk$set(\n  screenshot.force = FALSE)"),
    tag$p(class = "note",
      "The one setup line. In output that cannot run d3, a ",
      "chunk\u2019s chart becomes a print-quality figure at the ",
      "chunk\u2019s ", fn("fig.width"), ", ", fn("fig.height"),
      " and ", fn("dpi"), ".")),
  section("Data in",
    def('pv_read("f.csv")',
        list("one reader for ", fn(".csv"), ", ", fn(".sqlite"), ", ",
             fn(".sas7bdat"), " and ", fn(".xpt"))),
    def("pv_search_opendata(q)",
        list("search the federal catalogue; ",
             fn("pv_fetch_opendata()"), " fetches what it lists")),
    def("pv_fetch_bfs(id)", "a stats.swiss SDMX dataflow, tidied"),
    def("pv_fetch_lustat(id)", "a LUSTAT Statistik Luzern table"),
    def('pv_fetch_map("municipalities")',
        "the one boundary layer too big to bundle"),
    tag$p(class = "note",
      "Each fetch states its source and licence; downloads cache ",
      "once (", fn("pv_cache_status()"), ", ",
      fn("pv_cache_clear()"), ").")),
  section("On board",
    tag$p(class = "note",
      "17 datasets ship with the package. ", fn("pv_sales"), ", ",
      fn("pv_network"), " and ", fn("pv_flows"),
      " are simulated; the rest is real Swiss open data \u2014 ",
      fn("pv_city_population"), ", ", fn("pv_city_sectors"), ", ",
      fn("pv_city_landuse"), ", ", fn("pv_commuters"), ", ",
      fn("pv_fiscal"), ", ", fn("pv_elections"), ", ",
      fn("pv_weather"), ", ", fn("pv_electricity"), ", ",
      fn("pv_tourism"), " \u2014 and the map layers ",
      fn("pv_lucerne_map"), ", ", fn("pv_swiss_cantons"), ", ",
      fn("pv_swiss_districts"), ", ", fn("pv_swiss_lakes"), ", ",
      fn("pv_city_coords"), ".")))

# ---------------------------------------------------------------------
# Page frame: header, column heads, the grid, footer.

header <- tag$header(
  tag$div(class = "brand",
    tag$h1("polyviz"),
    tag$p(class = "tagline",
      "The reference card \u2014 d3-quality charts and a polyglot ",
      "data toolkit, driven entirely from R.")),
  tag$div(class = "install",
    fn('devtools::install_github("jastephan63/polyviz")'),
    tag$span(class = "sub",
      "Works in the Viewer, R Markdown, Quarto and Shiny \u00b7 every ",
      "chart carries its own alt text")))

colheads <- tag$div(class = "colheads",
  tag$div(class = "colhead span2",
    sprintf("Pick a chart \u2014 all %d types, by intent", n_charts)),
  tag$div(class = "colhead", "The shared grammar"),
  tag$div(class = "colhead", "Paper, decks & data"))

footer <- tag$footer(
  tag$span(tag$b("Every chart live and explained:"),
           " jastephan63.github.io/polyviz"),
  tag$span(tag$b("Reference:"), " jastephan63.github.io/polyviz/reference"),
  tag$span(paste0("polyviz v", version)),
  tag$span("MIT \u00a9 Jake Stephan"),
  tag$span("Bundled: d3.js v7 (ISC), d3-sankey (BSD-3), Inter (SIL OFL 1.1)"))

sheet <- tag$div(class = "sheet",
  header, colheads,
  tag$div(class = "cols", col1, col2, col3, col4),
  footer)

# ---------------------------------------------------------------------
# Styles. Light mode only - this page is for paper. Tokens come from
# pv_colors, so a theme change re-flows into the sheet on rebuild.

font_file <- system.file("htmlwidgets", "lib", "pv-fonts",
                         "InterVariable.woff2", package = "polyviz")
stopifnot(nzchar(font_file))
font_uri <- paste0("data:font/woff2;base64,",
                   base64enc::base64encode(font_file))

css <- paste0('
@font-face { font-family: "InterVariable"; font-style: normal;
  font-weight: 100 900; src: url(', font_uri, ') format("woff2"); }
@page { size: 297mm 210mm; margin: 0; }
* { margin: 0; padding: 0; box-sizing: border-box; }
html, body { width: 297mm; height: 210mm; }
body {
  font-family: "InterVariable", "Inter", system-ui, sans-serif;
  background: ', ink$surface, '; color: ', ink$primary, ';
  font-size: 7.2pt; line-height: 1.38;
  -webkit-print-color-adjust: exact; print-color-adjust: exact;
}
code, pre {
  font-family: "Menlo", "Consolas", "DejaVu Sans Mono", monospace;
  font-variant-numeric: tabular-nums;
}
.sheet { width: 297mm; height: 210mm; padding: 7.5mm 10mm 6mm;
  display: flex; flex-direction: column; overflow: hidden; }

header { display: flex; justify-content: space-between;
  align-items: flex-end; padding-bottom: 2.2mm;
  border-bottom: 0.75pt solid ', ink$primary, '; }
header h1 { font-size: 19pt; font-weight: 760; letter-spacing: -0.022em;
  line-height: 1; }
.tagline { color: ', ink$secondary, '; font-size: 8pt; margin-top: 1.4mm; }
.install { text-align: right; }
.install .fn { font-size: 7.4pt; }
.install .sub { display: block; color: ', ink$muted, '; font-size: 7pt;
  margin-top: 1mm; }

.colheads { display: grid;
  grid-template-columns: 1fr 1fr 1.12fr 1.16fr;
  gap: 0; padding: 1.8mm 0 1.4mm; }
.colhead { font-size: 7.4pt; font-weight: 730; text-transform: uppercase;
  letter-spacing: 0.085em; color: ', ink$primary, '; padding: 0 3mm; }
.colhead:first-child { padding-left: 0; }
.span2 { grid-column: 1 / 3; }

.cols { display: grid; grid-template-columns: 1fr 1fr 1.12fr 1.16fr;
  gap: 0; flex: 1; min-height: 0; }
.col { padding: 0 3mm; min-width: 0; }
.col:first-child { padding-left: 0; }
.col:last-child { padding-right: 0; }
.col + .col { border-left: 0.5pt solid ', ink$baseline, '; }

.group { margin-bottom: 2.2mm; }
.group:last-child { margin-bottom: 0; }
.ghead { display: flex; align-items: center; gap: 1.6mm;
  border-bottom: 0.5pt solid ', ink$grid, ';
  padding-bottom: 0.9mm; margin-bottom: 1.1mm; }
.dot { width: 2.2mm; height: 2.2mm; border-radius: 0.45mm; flex: none; }
.glabel { font-size: 7pt; font-weight: 700; text-transform: uppercase;
  letter-spacing: 0.07em; color: ', ink$secondary, '; }

.entry { display: grid; grid-template-columns: 22.5mm 1fr; gap: 0 2mm;
  padding: 0.5mm 0; }
.pick .entry { padding: 0.45mm 0; }
.pick .group { margin-bottom: 2.2mm; }
.fn { font-size: 7pt; color: ', ink$primary, '; white-space: nowrap; }
.when { color: ', ink$secondary, '; }

.def { padding: 0.3mm 0 0.3mm 3mm; text-indent: -3mm;
  color: ', ink$secondary, '; }
.note { color: ', ink$secondary, '; padding: 0.3mm 0; }
.lead { padding-top: 0; }
.pipe { padding: 0.2mm 0 0.8mm; }
.code { font-size: 7pt; line-height: 1.45; color: ', ink$primary, ';
  background: ', ink$grid, '66; border-left: 0.75pt solid ',
  ink$baseline, '; padding: 1mm 0 1mm 2mm; margin: 0.5mm 0 0.8mm;
  white-space: pre; }

.exts { display: grid; grid-template-columns: 9mm 1fr; gap: 0 2mm;
  padding: 0.4mm 0 0.8mm; }
.exts .fn, .exts .when { padding: 0.35mm 0; }

footer { display: flex; flex-wrap: wrap; gap: 0 4.5mm;
  border-top: 0.5pt solid ', ink$baseline, '; padding-top: 1.5mm;
  margin-top: 1.6mm; color: ', ink$muted, '; font-size: 7pt; }
footer b { font-weight: 640; color: ', ink$secondary, '; }
')

page <- paste0(
  "<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n",
  "<meta charset=\"utf-8\"/>\n",
  "<title>polyviz cheatsheet</title>\n",
  "<style>", css, "</style>\n</head>\n<body>\n",
  as.character(sheet),
  "\n</body>\n</html>\n")

# htmltools pretty-prints each child on its own line, and in HTML a
# newline is a space - which puts a gap between an inline code token and
# the comma or bracket beside it. Close those gaps.
page <- gsub("</code>\\s+([,.;:)])", "</code>\\1", page, perl = TRUE)
page <- gsub("\\(\\s+<code", "(<code", page, perl = TRUE)

# ---------------------------------------------------------------------
# No fiction on a reference sheet: every pv_* token in the final markup
# must exist - as an exported function or as a bundled dataset.

used <- setdiff(
  unique(regmatches(page, gregexpr("pv_[a-z0-9_]+", page))[[1]]),
  "pv_fonts")  # the font library's path name, not an R object
# The namespace's declared exports, plus everything living in the dev
# load - between adding a chart and regenerating NAMESPACE, the new
# constructor exists in the namespace before it shows up in the export
# list. A typo'd or invented name still fails either way.
known <- c(getNamespaceExports("polyviz"),
           ls(envir = asNamespace("polyviz")),
           utils::data(package = "polyviz")$results[, "Item"])
missing <- setdiff(used, known)
if (length(missing)) {
  stop("The sheet names things the package does not have: ",
       paste(missing, collapse = ", "))
}
constructors <- unlist(lapply(chooser, function(g) {
  vapply(g$entries, function(e) sub("\\(\\)$", "", e[[1]]), character(1))
}))
stopifnot(length(constructors) == 34L,
          !anyDuplicated(constructors),
          all(vapply(constructors, function(f) {
            is.function(get0(f, envir = asNamespace("polyviz")))
          }, logical(1))))
cat(sprintf("checked: %d pv_* names on the sheet, %d chart constructors\n",
            length(used), n_charts))

html_path <- file.path("data-raw", "cheatsheet.html")
writeBin(charToRaw(enc2utf8(page)), html_path)

# ---------------------------------------------------------------------
# Print it. A4 landscape is 297 x 210 mm = 11.69 x 8.27 inches; the
# @page rule above says the same thing, and preferCSSPageSize lets it
# win. The fonts must be decoded before the print, exactly as in
# pv_save()'s pdf path.

b <- chromote::ChromoteSession$new(width = 1123L, height = 794L)
loaded <- b$Page$loadEventFired(wait_ = FALSE)
invisible(b$Page$navigate(
  utils::URLencode(paste0("file://", normalizePath(html_path))),
  wait_ = FALSE))
invisible(b$wait_for(loaded))
invisible(tryCatch(b$Runtime$evaluate(
  "document.fonts.ready.then(function () { return true; })",
  awaitPromise = TRUE), error = function(e) NULL))

# The fit check: a sheet that overflows is not a cheatsheet, it is a
# clipping. Any column taller than the page, any code token wider than
# its column, and the build stops naming the offender.
overflow <- b$Runtime$evaluate(paste0(
  "(function () {",
  " var out = [];",
  " var sheet = document.querySelector('.sheet');",
  " if (sheet.scrollHeight > sheet.clientHeight + 1)",
  "   out.push('sheet is ' + (sheet.scrollHeight - sheet.clientHeight) +",
  "            'px too tall');",
  " if (sheet.scrollWidth > sheet.clientWidth + 1)",
  "   out.push('sheet is ' + (sheet.scrollWidth - sheet.clientWidth) +",
  "            'px too wide');",
  " var cols = document.querySelector('.cols');",
  " if (cols.scrollHeight > cols.clientHeight + 1)",
  "   out.push('columns run ' + (cols.scrollHeight - cols.clientHeight) +",
  "            'px past the page');",
  " document.querySelectorAll('code.fn').forEach(function (el) {",
  "   if (el.scrollWidth > el.clientWidth + 1)",
  "     out.push('code too wide for its column: ' + el.textContent);",
  " });",
  " return out.join('; ');",
  " })()"), returnByValue = TRUE)$result$value
if (is.character(overflow) && nzchar(overflow)) {
  stop("The sheet does not fit: ", overflow)
}

pdf_bytes <- base64enc::base64decode(b$Page$printToPDF(
  paperWidth = 297 / 25.4, paperHeight = 210 / 25.4,
  marginTop = 0, marginBottom = 0, marginLeft = 0, marginRight = 0,
  printBackground = TRUE, preferCSSPageSize = TRUE)$data)

out <- file.path("docs", "polyviz-cheatsheet.pdf")
tmp <- tempfile(pattern = ".pv-cheatsheet-", tmpdir = "docs")
writeBin(pdf_bytes, tmp)
if (!suppressWarnings(file.rename(tmp, out))) {
  if (file.exists(out)) unlink(out)
  stopifnot(file.rename(tmp, out))
}
cat(sprintf("wrote %s (A4 landscape, %d bytes)\n", out, length(pdf_bytes)))
invisible(try(b$close(), silent = TRUE))
