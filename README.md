# polyviz

[![R-CMD-check](https://github.com/jastephan63/polyviz/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jastephan63/polyviz/actions/workflows/R-CMD-check.yaml)

**D3-quality interactive visualisation and polyglot data analysis, with a pure R interface.**

polyviz was born from loving [d3.js](https://d3js.org) visualisations but not wanting to write JavaScript. Every chart is rendered by a bundled copy of D3 v7 — real d3 scales, transitions, tooltips, force simulations — but you drive it entirely from R data frames.

**→ [Live demo gallery](https://jastephan63.github.io/polyviz/)** — all 24 chart types, interactive, each explained and running on real Swiss open government data, with the R code that made it.

Behind the R interface, the package deliberately spans four backend languages:

| Language | Where it lives | What it does |
|---|---|---|
| **JavaScript (D3 v7)** | `inst/htmlwidgets/` | Renders all 24 interactive chart types |
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

Every chart is an htmlwidget: it animates in, responds to hover with tooltips, follows light/dark mode, re-renders when its container resizes, and works in the RStudio Viewer, R Markdown, Quarto, and Shiny (`pvchartOutput()` / `renderPvchart()`). Automatic corrections adapt each chart to its data and size — orientation, labels, legends, opacity — and every automatic behaviour has an explicit `TRUE`/`FALSE`/`"auto"` override. Invalid or degenerate data — a missing column, text where numbers belong, two rows for one bar, nothing left to draw — fails immediately with a clear R-side message instead of rendering a broken chart. See each one live, explained, in the [gallery](https://jastephan63.github.io/polyviz/).

| | | |
|---|---|---|
| `pv_bar()` | `pv_line()` | `pv_scatter()` |
| `pv_lollipop()` | `pv_area()` — stacked/percent/stream | `pv_histogram()` |
| `pv_boxplot()` | `pv_violin()` | `pv_ridgeline()` |
| `pv_beeswarm()` | `pv_donut()` | `pv_treemap()` |
| `pv_sunburst()` — zoomable | `pv_pack()` — zoomable | `pv_heatmap()` |
| `pv_calendar()` | `pv_choropleth()` | `pv_force()` |
| `pv_chord()` | `pv_sankey()` | `pv_parallel()` — brushable |
| `pv_race()` — animated | `pv_bump()` | `pv_dendrogram()` — from `hclust` |

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

Knitting to Word or PDF needs no extra code: in any output format that can't run d3, the chart becomes a print-quality PNG at the chunk's `fig.width`/`fig.height`/`dpi` (put `knitr::opts_chunk$set(screenshot.force = FALSE)` in the setup chunk; chromote + Chrome required — without them the error saying what to install stands where the chart would be, or stops the knit). And every rendered chart carries a hover download button in its top-right corner — standalone SVG or 2x PNG — which `pv_downloads(w, FALSE)` hides.

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
```

## Analysing it

```r
# One-look data-quality summary (dimensions, missingness, value sketches)
pv_summary(airquality)

# Numeric profiling — runs in Python when available, R otherwise
pv_profile(airquality)

# Outlier flags by IQR fence or z-score
pv_outliers(airquality$Ozone, method = "iqr")
```

## Bundled data

Eight real open-government datasets ship with the package, covering the
population, economy, territory, and weather of Switzerland:

- `pv_city_population` — 181 Swiss cities at census years 1930–2024 (BFS)
- `pv_city_sectors` — employment shares by economic sector per city (BFS/STATENT)
- `pv_city_landuse` — land use in hectares, two hierarchy levels (BFS Arealstatistik)
- `pv_commuters` — commuter flows between Canton Zug and its neighbours (Fachstelle Statistik Zug)
- `pv_fiscal` — fiscal equalization of Lucerne municipalities (LUSTAT Statistik Luzern)
- `pv_elections` — Lucerne municipal council candidacies and seats (LUSTAT Statistik Luzern)
- `pv_lucerne_map` — municipal boundary polygons, 1.1.2025 (BFS ThemaKart)
- `pv_weather` — daily temperature, precipitation, and sunshine in Lucerne, 2020–2025 (MeteoSwiss)

All are openly licensed with source citation required (see each dataset's
help page for the exact attribution); the LUSTAT datasets additionally
require the data owner's permission for commercial use. Three simulated
datasets (`pv_sales`, `pv_network`, `pv_flows`) remain for examples that
need shapes the real data doesn't provide.

## Licence

MIT © Jake Stephan. Bundled: [d3.js](https://d3js.org) (ISC),
[d3-sankey](https://github.com/d3/d3-sankey) (BSD-3),
[Inter](https://rsms.me/inter/) (SIL OFL 1.1).
