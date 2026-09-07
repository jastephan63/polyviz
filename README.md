# polyviz

[![R-CMD-check](https://github.com/jastephan63/polyviz/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jastephan63/polyviz/actions/workflows/R-CMD-check.yaml)

**D3-quality interactive visualisation and polyglot data analysis, with a pure R interface.**

polyviz was born from loving [d3.js](https://d3js.org) visualisations but not wanting to write JavaScript. Every chart is rendered by a bundled copy of D3 v7 — real d3 scales, transitions, tooltips, force simulations — but you drive it entirely from R data frames.

**→ [Anatomy of a Swiss canton](https://jastephan63.github.io/polyviz/story.html)** — a six-chapter data story built with polyviz on live-fetched Swiss open data: convergence that isn't happening, rising inequality and the transfers that compress it, four statistical families of municipalities, and an ageing no scenario escapes — every number computed from the data.

**→ [Live demo gallery](https://jastephan63.github.io/polyviz/)** — all 28 chart types, interactive, each explained and running on real Swiss open government data, with the R code that made it.

**Getting started:** `vignette("polyviz")` is the five-minute tour, `pv_demo()` launches the live gallery as a Shiny app, `pv_suggest(data)` prints runnable chart calls that fit your data frame, and the [cheatsheet (PDF)](https://jastephan63.github.io/polyviz/polyviz-cheatsheet.pdf) fits the whole package on a desk-side sheet.

Behind the R interface, the package deliberately spans four backend languages:

| Language | Where it lives | What it does |
|---|---|---|
| **JavaScript (D3 v7)** | `inst/htmlwidgets/` | Renders all 28 interactive chart types |
| **SQL** | `R/sql.R`, `inst/sql/` | SQLite querying, parameterised queries, runnable `.sql` script files |
| **Python** | `inst/python/polyviz.py` | Numeric profiling and outlier detection (stdlib only — no pandas needed), with an identical pure-R fallback |
| **SAS** | `R/sas.R` | Reads/writes `sas7bdat` and `xpt` datasets with variable labels, no SAS licence required |

## Installation

```r
# install.packages("devtools")
devtools::install_github("jastephan63/polyviz")
```

Python is optional. If `reticulate` finds any Python ≥ 3.8, the profiling functions use it; otherwise they transparently run the same computation in R.

## The charts

Every chart is an htmlwidget: it animates in, responds to hover with tooltips, follows light/dark mode, re-renders when its container resizes, and works in the RStudio Viewer, R Markdown, Quarto, and Shiny (`pvchartOutput()` / `renderPvchart()`). Automatic corrections adapt each chart to its data and size — orientation, labels, legends, opacity — and every automatic behaviour has an explicit `TRUE`/`FALSE`/`"auto"` override. Options add depth where the form supports it, without changing any chart that doesn't ask: bars stack by value or to 100%, lines take observation markers, curved or stepped interpolation, and a brush-to-zoom strip (areas zoom too), violins pin the raw values inside their silhouettes as jittered dots, and scatters trade their marks for filled density contours (`density = TRUE`) or move them to a canvas layer (`canvas`, automatic past 8,000 points) so huge clouds stay fluid. Invalid or degenerate data — a missing column, text where numbers belong, two rows for one bar, nothing left to draw — fails immediately with a clear R-side message instead of rendering a broken chart. See each one live, explained, in the [gallery](https://jastephan63.github.io/polyviz/).

| | | |
|---|---|---|
| `pv_bar()` | `pv_line()` | `pv_scatter()` |
| `pv_lollipop()` | `pv_area()` — stacked/percent/stream | `pv_histogram()` |
| `pv_boxplot()` | `pv_violin()` | `pv_ridgeline()` |
| `pv_beeswarm()` | `pv_donut()` | `pv_treemap()` |
| `pv_sunburst()` — zoomable | `pv_pack()` — zoomable | `pv_heatmap()` |
| `pv_calendar()` | `pv_choropleth()` | `pv_bubble_map()` |
| `pv_force()` | `pv_chord()` | `pv_arc()` — crossing-minimised order |
| `pv_sankey()` | `pv_parallel()` — brushable | `pv_race()` — animated |
| `pv_bump()` | `pv_dendrogram()` — from `hclust` | `pv_pairs()` — scatterplot matrix |
| `pv_table()` — sortable, in-cell bars, shading & sparklines | | |

## Charts in papers and documents

Interactive is the default, but every chart also leaves the browser as a print-quality file — `pv_save()` picks the format from the extension:

```r
agg <- aggregate(revenue ~ region, pv_sales, sum)
w <- pv_bar(agg, "region", "revenue", title = "Revenue by region")

pv_save(w, "revenue.png")   # crisp 2x raster — Word, Google Docs, slides
pv_save(w, "revenue.svg")   # standalone vector — web pages, Inkscape/Illustrator
pv_save(w, "revenue.pdf")   # true vector PDF — LaTeX
pv_save(w, "revenue.html")  # one self-contained interactive file — sharing
```

The capture formats render the chart for real in headless Chrome (the chromote package plus a Chrome-based browser required), with the entrance animation switched off and light mode forced by default — a save never catches a mid-animation frame or your machine's dark theme. The `.html` file is assembled purely in R: no Chrome, no pandoc, interactivity intact.

Knitting to Word or PDF needs no extra code: in any output format that can't run d3, the chart becomes a print-quality PNG at the chunk's `fig.width`/`fig.height`/`dpi` (put `knitr::opts_chunk$set(screenshot.force = FALSE)` in the setup chunk; chromote + Chrome required — without them the error saying what to install stands where the chart would be, or stops the knit). And every rendered chart carries a hover download button in its top-right corner — standalone SVG or 2x PNG — which `pv_downloads(w, FALSE)` hides. When a chart should simply sit still on the page — no animation, tooltips, or controls at all — pipe it through `pv_static()`.

Beyond single figures: `pv_deck(charts, "review.pptx")` renders a list of charts straight into a PowerPoint deck — one real 2x capture per 16:9 slide, title slide and speaker notes included (needs the officer package) — and `pv_save(race, "race.gif")` turns a bar-chart race into a looping GIF rendered frame by frame, identical on every run. For pages printed in greyscale, `pv_set_theme(pv_theme_paper())` swaps in a print-first theme — pure white surface, five colours climbing a luminance ladder, optional serif type — and piping a chart through `pv_textures()` hatches each series so identity survives toner and colour-vision deficiency alike. `pv_locale("de-CH")` (or `fr`/`it`/`en-CH`) makes every chart write numbers and dates the Swiss way: `10'000` on the axis, the language's own month names on date scales. And every chart describes itself — automatically generated alt text reaches screen readers and knitted documents, with `pv_alt()` to supply your own words.

One habit to unlearn: `print(w)` in a non-interactive script displays nothing — there is no viewer to open. Save instead. Details, print sizing included, in `vignette("embedding-charts")`.

## Design

The look is research-backed, not taste-backed. The typeface is [Inter](https://rsms.me/inter/) (bundled, SIL OFL) — the open-licence counterpart of the grotesques used by the FT, The Economist, and the NYT graphics desks — with true tabular numerals on every axis. The categorical palette is anchored on The Economist's published web palette, then re-stepped in OKLCH colour space and slot-ordered by exhaustive search so that adjacent series remain distinguishable under the common colour-vision deficiencies, in light **and** dark mode. Chart anatomy follows newsroom craft: left-aligned title block, horizontal hairline gridlines only, a zero baseline, direct labels over legends, compact "12.8k" numbers on axes with exact values in tooltips, and a source line on every chart.

## Getting data in

```r
# One reader for everything — dispatches on the file extension
pv_read("results.csv")
pv_read("trial.sas7bdat")
pv_read("warehouse.sqlite", table = "sales")

# SQL: connect, write, query (with ? parameter binding), run script files
con <- pv_db_connect("warehouse.sqlite")
pv_db_write(con, "sales", pv_sales)
pv_query(con, "SELECT * FROM sales WHERE region = ?", params = list("North"))
pv_run_sql_file(con, system.file("sql", "demo.sql", package = "polyviz"))
pv_db_disconnect(con)

# SAS: round-trip datasets with variable labels intact
df <- pv_set_labels(pv_sales, c(revenue = "Net revenue, EUR"))
pv_write_sas(df, "sales.xpt")
pv_labels(pv_read_sas("sales.xpt"))

# Swiss open data, live from the official portals (downloads cached)
pv_search_opendata("Finanzausgleich Luzern")        # the federal catalogue
pv_fetch_opendata("finanzausgleich-kanton-luzern")  # fetch a dataset it lists
pv_fetch_bfs("DF_SSV_POP_1930")   # a stats.swiss SDMX dataflow, tidied
pv_fetch_lustat("fa-lu-ra")       # a LUSTAT Statistik Luzern CSV
```

Every fetch prints the data's source and licence terms and attaches them
to the result; resources without an open licence are refused rather than
delivered. Portals, cache, and obligations: `vignette("swiss-open-data")`.

## Analysing it

```r
# One-look data-quality summary (dimensions, missingness, value sketches)
pv_summary(airquality)

# Numeric profiling — runs in Python when available, R otherwise
pv_profile(airquality)

# Outlier flags by IQR fence or z-score
pv_outliers(airquality$Ozone, method = "iqr")
```

The profile also states each variable's shape — skewness, excess
kurtosis, and the Jarque-Bera normality test. And wherever a
correlation is computed (`pv_pairs()`, `pv_plot_corr()`, the
`pv_report()` heatmap), `method = "spearman"` or `"kendall"` swaps
Pearson's *r* for a rank coefficient — the honest choice for
relationships that are monotone but not linear.

## Bundled data

Fourteen real open-government datasets ship with the package, covering the
population, economy, energy, territory, tourism, and weather of Switzerland:

- `pv_city_population` — 180 Swiss cities at census years 1930–2024 (BFS)
- `pv_city_sectors` — employment shares by economic sector per city (BFS/STATENT)
- `pv_city_landuse` — land use in hectares, two hierarchy levels (BFS Arealstatistik)
- `pv_commuters` — commuter flows between Canton Zug and its neighbours (Fachstelle Statistik Zug)
- `pv_fiscal` — fiscal equalization of Lucerne municipalities (LUSTAT Statistik Luzern)
- `pv_elections` — Lucerne municipal council candidacies and seats (LUSTAT Statistik Luzern)
- `pv_lucerne_map` — municipal boundary polygons of Canton Lucerne, 1.1.2025 (BFS ThemaKart)
- `pv_swiss_cantons` / `pv_swiss_districts` / `pv_swiss_lakes` — country-wide boundary layers for `pv_choropleth()` and `pv_bubble_map()` (BFS ThemaKart)
- `pv_city_coords` — one WGS84 point per city, the bubble-map companion (derived from BFS ThemaKart)
- `pv_weather` — daily temperature, precipitation, and sunshine in Lucerne, 2020–2025 (MeteoSwiss)
- `pv_electricity` — monthly Swiss electricity production by source, 2020–2025 (Bundesamt für Energie)
- `pv_tourism` — hotel arrivals and nights per canton by guest origin, 2005–2025 (BFS/HESTA)

The one layer too big to bundle — all ~2100 municipal boundaries — is
downloaded and cached on first use by `pv_fetch_map("municipalities")`,
or simply by passing `map = "municipalities"` to a geo chart.

All are openly licensed with source citation required (see each dataset's
help page for the exact attribution); the LUSTAT datasets additionally
require the data owner's permission for commercial use. Three simulated
datasets (`pv_sales`, `pv_network`, `pv_flows`) remain for examples that
need shapes the real data doesn't provide.

## Licence

MIT © Jake Stephan. Bundled: [d3.js](https://d3js.org) (ISC),
[d3-sankey](https://github.com/d3/d3-sankey) (BSD-3),
[Inter](https://rsms.me/inter/) (SIL OFL 1.1).
