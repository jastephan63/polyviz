# polyviz

**D3-quality interactive visualisation and polyglot data analysis, with a pure R interface.**

polyviz was born from loving [d3.js](https://d3js.org) visualisations but not wanting to write JavaScript. Every chart is rendered by a bundled copy of D3 v7 — real d3 scales, transitions, tooltips, force simulations — but you drive it entirely from R data frames.

**→ [Live demo gallery](https://jastephan63.github.io/polyviz/)** — every chart type, interactive, on real Swiss open government data.

Behind the R interface, the package deliberately spans four backend languages:

| Language | Where it lives | What it does |
|---|---|---|
| **JavaScript (D3 v7)** | `inst/htmlwidgets/` | Renders all sixteen interactive chart types |
| **SQL** | `R/sql.R`, `inst/sql/` | SQLite querying, parameterised queries, runnable `.sql` script files |
| **Python** | `inst/python/polyviz.py` | Numeric profiling and outlier detection (stdlib only — no pandas needed), with an identical pure-R fallback |
| **SAS** | `R/sas.R` | Reads/writes `sas7bdat` and `xpt` datasets with variable labels, no SAS licence required |

## Installation

```r
# install.packages("devtools")
devtools::install_github("jastephan63/polyviz")
```

Python is optional. If `reticulate` finds any Python ≥ 3.8, the profiling functions use it; otherwise they transparently run the same computation in R.

## Design

The look is research-backed, not taste-backed. The typeface is [Inter](https://rsms.me/inter/) (bundled, SIL OFL) — the open-licence counterpart of the grotesques used by the FT, The Economist, and the NYT graphics desks — with true tabular numerals on every axis. The categorical palette is anchored on The Economist's published web palette, then re-stepped in OKLCH colour space and slot-ordered by exhaustive search so that adjacent series remain distinguishable under the common colour-vision deficiencies, in light **and** dark mode (every chart follows your system theme automatically). Chart anatomy follows newsroom craft: left-aligned title block, horizontal hairline gridlines only, a zero baseline, direct labels over legends, compact "12.8k" numbers on axes with exact values in tooltips, and a source line on every chart.

## The charts

All charts are interactive htmlwidgets: they animate in, respond to hover with tooltips, follow light/dark mode, and work in the RStudio Viewer, R Markdown, Quarto, and Shiny (`pvchartOutput()` / `renderPvchart()`). Each example below runs as-is on data bundled with the package.

<!-- gallery:start -->
<!-- gallery:end -->

## Getting data in

```r
# One reader for everything — dispatches on the file extension
pv_read("results.csv")
pv_read("trial.sas7bdat")
pv_read("warehouse.sqlite", table = "sales")

# SQL: connect, write, query (with ? parameter binding), run script files
con <- pv_db_connect("warehouse.sqlite")
pv_db_write(con, "sales", pv_sales)
pv_query(con, "SELECT region, SUM(revenue) AS revenue
               FROM sales GROUP BY region")
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
attr(pv_profile(airquality), "engine")   # which backend did the work

# Outlier flags by IQR fence or z-score
pv_outliers(airquality$Ozone, method = "iqr")
```

## Bundled data

Six real open-government datasets ship with the package, covering the
population, economy, and territory themes of Swiss official statistics:

- `pv_city_population` — 181 Swiss cities at census years 1930–2024 (BFS)
- `pv_city_sectors` — employment shares by economic sector per city (BFS/STATENT)
- `pv_city_landuse` — land use in hectares, two hierarchy levels (BFS Arealstatistik)
- `pv_commuters` — commuter flows between Canton Zug and its neighbours (Fachstelle Statistik Zug)
- `pv_fiscal` — fiscal equalization of Lucerne municipalities (LUSTAT Statistik Luzern)
- `pv_elections` — Lucerne municipal council candidacies and seats (LUSTAT Statistik Luzern)

All are openly licensed with source citation required (see each dataset's
help page for the exact attribution); the LUSTAT datasets additionally
require the data owner's permission for commercial use. Three simulated
datasets (`pv_sales`, `pv_network`, `pv_flows`) remain for examples that
need shapes the real data doesn't provide.

## Licence

MIT © Jake Stephan. Bundled: [d3.js](https://d3js.org) (ISC),
[d3-sankey](https://github.com/d3/d3-sankey) (BSD-3),
[Inter](https://rsms.me/inter/) (SIL OFL 1.1).
