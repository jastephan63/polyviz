# polyviz

[![R-CMD-check](https://github.com/jastephan63/polyviz/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jastephan63/polyviz/actions/workflows/R-CMD-check.yaml)

**D3-quality interactive visualisation and polyglot data analysis, with a pure R interface.**

polyviz was born from loving [d3.js](https://d3js.org) visualisations but not wanting to write JavaScript. Every chart is rendered by a bundled copy of D3 v7 — real d3 scales, transitions, tooltips, force simulations — but you drive it entirely from R data frames.

**→ [Anatomy of a Swiss canton](https://jastephan63.github.io/polyviz/story.html)** — an eight-chapter data story built with polyviz on live-fetched Swiss open data: convergence that isn't happening, rising inequality and the transfers that compress it, four statistical families of municipalities, an ageing no scenario escapes, a housing market read through thirty years of vacancy counts, and a fleet caught mid-electrification — every number computed from the data.

**→ [Live demo gallery](https://jastephan63.github.io/polyviz/)** — all 37 chart types, interactive, each explained and running on real Swiss open government data, with the R code that made it.

**Getting started:** `vignette("polyviz")` is the five-minute tour, `pv_demo()` launches the live gallery as a Shiny app, `pv_suggest(data)` prints runnable chart calls that fit your data frame, and the [cheatsheet (PDF)](https://jastephan63.github.io/polyviz/polyviz-cheatsheet.pdf) fits the whole package on a desk-side sheet.

## Contents

- [Installation](#installation) · [A first chart](#a-first-chart)
- [The charts — all 37](#the-charts) · [Layers: annotate, trend, facet, link](#layers-annotate-trend-facet-link)
- [Time-series statistics](#time-series-statistics) · [Boards and decks](#boards-and-decks)
- [Charts in papers and documents](#charts-in-papers-and-documents)
- [Design](#design) · [Accessibility](#accessibility)
- [Getting data in](#getting-data-in) · [Analysing it](#analysing-it)
- [Shiny](#shiny) · [Bundled data](#bundled-data) · [Documentation map](#documentation-map)

Behind the R interface, the package deliberately spans four backend languages:

| Language | Where it lives | What it does |
|---|---|---|
| **JavaScript (D3 v7)** | `inst/htmlwidgets/` | Renders all 37 interactive chart types |
| **SQL** | `R/sql.R`, `inst/sql/` | SQLite querying, parameterised queries, runnable `.sql` script files |
| **Python** | `inst/python/polyviz.py` | Numeric profiling and outlier detection (stdlib only — no pandas needed), with an identical pure-R fallback |
| **SAS** | `R/sas.R` | Reads/writes `sas7bdat` and `xpt` datasets with variable labels, no SAS licence required |

## Installation

```r
# install.packages("devtools")
devtools::install_github("jastephan63/polyviz")
```

Each [release](https://github.com/jastephan63/polyviz/releases) also attaches the exact source tarball that passed `R CMD check --as-cran`, installable with `install.packages("polyviz_x.y.z.tar.gz", repos = NULL)`.

Python is optional. If `reticulate` finds any Python ≥ 3.8, the profiling functions use it; otherwise they transparently run the same computation in R.

## A first chart

```r
library(polyviz)

agg <- aggregate(revenue ~ region, pv_sales, sum)
pv_bar(agg, x = "region", y = "revenue",
       title = "Revenue by region",
       subtitle = "Simulated sales, twelve months",
       source = "Source: pv_sales (simulated)")
```

That's the whole grammar: a data frame, column names, and optional title anatomy. The result animates in, shows exact values on hover, offers a download button in its corner, follows the viewer's light/dark mode, and resizes with its container. Every other chart type works the same way — swap `pv_bar` for `pv_line`, `pv_scatter`, or any constructor below. When you're not sure which form fits, `pv_suggest(your_data)` prints runnable calls ranked by how well each chart's requirements match your columns.

## The charts

Every chart is an htmlwidget: it animates in, responds to hover with tooltips, follows light/dark mode, re-renders when its container resizes, and works in the RStudio Viewer, R Markdown, Quarto, and Shiny. Automatic corrections adapt each chart to its data and size — orientation, labels, legends, opacity — and every automatic behaviour has an explicit `TRUE`/`FALSE`/`"auto"` override. Invalid or degenerate data — a missing column, text where numbers belong, two rows for one bar, nothing left to draw — fails immediately with a clear R-side message instead of rendering a broken chart. See each one live, explained, in the [gallery](https://jastephan63.github.io/polyviz/).

**Compare amounts**

| Chart | What it's for |
|---|---|
| `pv_bar()` | The workhorse: grouped, stacked (`stack = "stack"`), or 100% (`"percent"`) bars, horizontal when labels are long |
| `pv_lollipop()` | A bar chart that breathes — thin stems, dot ends, for many categories |
| `pv_slope()` | Two moments in time joined by one line per entity |
| `pv_dumbbell()` | The gap between two values, on every row |
| `pv_bullet()` | A value against its target and qualitative bands |
| `pv_waterfall()` | How gains and losses walk a running total |
| `pv_pyramid()` | Two opposing flows mirrored around a centre spine — in/out commuters, age pyramids |
| `pv_waffle()` | Parts of a whole as countable squares |

**Follow things over time**

| Chart | What it's for |
|---|---|
| `pv_line()` | Markers (`show_points`), step or monotone curves, and a brush-to-zoom strip (`zoom = TRUE`) |
| `pv_area()` | Stacked, 100%, or streamgraph via `offset = "stacked" / "percent" / "stream"` |
| `pv_calendar()` | A year of days, coloured by value |
| `pv_horizon()` | Dozens of series folded into compact ribbons — the 26-canton chart |
| `pv_race()` | An animated bar-chart race; `pv_save(w, "race.gif")` makes it a GIF |
| `pv_bump()` | Rank trajectories — who overtook whom |

**See a distribution**

| Chart | What it's for |
|---|---|
| `pv_histogram()` | Binned counts with sensible break rules |
| `pv_boxplot()` | Five-number summaries side by side |
| `pv_violin()` | Full densities; `points =` pins the raw values inside the silhouettes |
| `pv_ridgeline()` | Many densities stacked into a skyline |
| `pv_beeswarm()` | Every single observation, packed without overlap |

**Relate variables**

| Chart | What it's for |
|---|---|
| `pv_scatter()` | Labels, colour and size encodings; `density = "contours"` or `"hex"` replaces overplotted marks, `canvas` keeps 100k points fluid, and hover always finds the nearest mark |
| `pv_heatmap()` | A value on a grid of two categories |
| `pv_pairs()` | The scatterplot matrix, with Pearson, Spearman, or Kendall coefficients |
| `pv_parallel()` | Parallel coordinates, brushable per axis |

**Show parts and hierarchies**

| Chart | What it's for |
|---|---|
| `pv_donut()` | A simple share of a whole |
| `pv_treemap()` | Nested rectangles by size |
| `pv_sunburst()` | Zoomable rings — click to descend |
| `pv_pack()` | Zoomable nested circles |
| `pv_icicle()` | The hierarchy as zoomable slabs |
| `pv_dendrogram()` | An `hclust` tree, straight from the object |

**Trace networks and flows**

| Chart | What it's for |
|---|---|
| `pv_force()` | A force-directed network you can drag |
| `pv_sankey()` | Flows between stages, widths true to volume |
| `pv_chord()` | A square flow matrix wrapped around a circle |
| `pv_arc()` | The linear network, node order chosen to minimise crossings |

**Put it on a map**

| Chart | What it's for |
|---|---|
| `pv_choropleth()` | Regions coloured by value — Lucerne municipalities bundled; cantons, districts, or all ~2,100 Swiss municipalities via `map =` |
| `pv_bubble_map()` | Sized circles at coordinates on the Swiss layers |
| `pv_flow_map()` | Tapered movement arcs between places |

**Read exact values**

| Chart | What it's for |
|---|---|
| `pv_table()` | The design-system table: click-to-sort, in-cell bars, value shading, sparkline columns, paging |

## Layers: annotate, trend, facet, link

Charts are pipeable values, and a small set of modifiers adds depth after construction — each returns the chart, so they chain:

```r
w <- pv_scatter(cities, x = "vacancy", y = "rent", label = "city") |>
  pv_trend(method = "lm") |>                       # fitted line with its CI band
  pv_annotate(
    pv_hline(1500, label = "median rent"),         # reference line
    pv_band(x0 = 0, x1 = 1, label = "shortage"),   # shaded region
    pv_note(1.01, 1454, "Luzern", dx = 14)         # anchored callout
  )

pv_facet(w, by = cities$region, ncol = 2)          # small multiples, shared scales
```

`pv_trend()` fits loess or a linear model in R and draws the band honestly. `pv_annotate()` accepts `pv_hline()`, `pv_vline()`, `pv_band()`, and `pv_note()` in any combination. `pv_facet()` turns one chart into aligned small multiples. And `pv_link(w, sd)` binds a chart to a [crosstalk](https://rstudio.github.io/crosstalk/) `SharedData` object, so brushing one chart highlights the same entities on every chart sharing it — no Shiny required ([the story's cluster chapter](https://jastephan63.github.io/polyviz/story.html#familien) links its map and profiles this way).

## Time-series statistics

Three tools keep the statistics in R, on base `stats` alone, and draw through the chart machinery:

```r
w <- pv_line(monthly, x = "month", y = "value")

pv_decompose(monthly, x = "month", y = "value",
             method = "stl")          # aligned trend / seasonal / remainder panels
pv_forecast(w, horizon = 12)          # Holt-Winters, small ARIMA search, or naive —
                                      # nested 50/80/95% fans, dashed projection line
pv_changepoints(w, levels = TRUE)     # sustained level shifts by binary segmentation
                                      # with a BIC stopping rule, marked as date lines
```

Each states plainly what it assumes and what its intervals don't know: the fitted model is always recorded in the chart's payload, a forecast is drawn as the extrapolation it is, and `pv_changepoints()` reports finding nothing when nothing is there.

## Boards and decks

`pv_board(a, b, ...)` lays finished charts out as one responsive page — no Shiny, no R Markdown; print it, embed it in a chunk, or `pv_save()` it as `.html`, `.png`, or `.pdf` ([live example](https://jastephan63.github.io/polyviz/board.html)). A board is plain HTML rather than one widget, so a screen reader gets its headings and then each panel's own generated description. Crosstalk-linked panels keep their linking inside a board.

`pv_deck(charts, "review.pptx")` renders a list of charts straight into a PowerPoint deck — one real 2x capture per 16:9 slide, title slide and speaker notes included (needs the officer package).

## Charts in papers and documents

Interactive is the default, but every chart also leaves the browser as a print-quality file — `pv_save()` picks the format from the extension:

```r
pv_save(w, "revenue.png")   # crisp 2x raster — Word, Google Docs, slides
pv_save(w, "revenue.svg")   # standalone vector — web pages, Inkscape/Illustrator
pv_save(w, "revenue.pdf")   # true vector PDF — LaTeX
pv_save(w, "revenue.html")  # one self-contained interactive file — sharing
pv_save(race, "race.gif")   # a bar-chart race as a looping GIF
```

The capture formats render the chart for real in headless Chrome (the chromote package plus a Chrome-based browser required), with the entrance animation switched off and light mode forced by default — a save never catches a mid-animation frame or your machine's dark theme. The `.html` file is assembled purely in R: no Chrome, no pandoc, interactivity intact.

Knitting to Word or PDF needs no extra code: in any output format that can't run d3, the chart becomes a print-quality PNG at the chunk's `fig.width`/`fig.height`/`dpi` (put `knitr::opts_chunk$set(screenshot.force = FALSE)` in the setup chunk; chromote + Chrome required — without them the error saying what to install stands where the chart would be, or stops the knit). And every rendered chart carries a hover download button in its top-right corner — standalone SVG or 2x PNG — which `pv_downloads(w, FALSE)` hides. When a chart should simply sit still on the page — no animation, tooltips, or controls at all — pipe it through `pv_static()`.

For pages printed in greyscale, `pv_set_theme(pv_theme_paper())` swaps in a print-first theme — pure white surface, five colours climbing a luminance ladder, optional serif type — and piping a chart through `pv_textures()` hatches each series so identity survives toner and colour-vision deficiency alike.

One habit to unlearn: `print(w)` in a non-interactive script displays nothing — there is no viewer to open. Save instead. Details, print sizing included, in `vignette("embedding-charts")`.

## Design

The look is research-backed, not taste-backed. The typeface is [Inter](https://rsms.me/inter/) (bundled, SIL OFL) — the open-licence counterpart of the grotesques used by the FT, The Economist, and the NYT graphics desks — with true tabular numerals on every axis. The categorical palette is anchored on The Economist's published web palette, then re-stepped in OKLCH colour space and slot-ordered by exhaustive search so that adjacent series remain distinguishable under the common colour-vision deficiencies, in light **and** dark mode. Chart anatomy follows newsroom craft: left-aligned title block, horizontal hairline gridlines only, a zero baseline, direct labels over legends, compact "12.8k" numbers on axes with exact values in tooltips, and a source line on every chart.

The system is programmable. `pv_set_theme()` restyles every subsequent chart (and accepts whole theme objects such as `pv_theme_paper()`); `pv_reset_theme()` returns to stock; `pv_check_palette(c("#...", ...))` runs any candidate palette through the colour-vision-deficiency validator before you commit to it; `pv_colors` and `pv_palette()` expose the tokens. `pv_locale("de-CH")` (or `fr`/`it`/`en-CH`) makes every chart write numbers and dates the Swiss way — `10'000` on the axis, the language's own month names on date scales. And for the times you're in ggplot2 instead, the same identity travels: `scale_colour_pv()` / `scale_fill_pv()` apply the palette and `theme_polyviz()` applies the chart anatomy to any ggplot.

## Accessibility

Every chart describes itself: an automatically generated description of type, variables, extremes, and shape is attached as `aria` alt text for screen readers and flows into knitted documents as `fig.alt`. `pv_alt(w, "...")` replaces it with your own words when the data deserves better ones; `pv_alt_text(w)` shows what a chart will say. Charts carry `role="img"` with the description as their accessible name, boards expose real headings, and `pv_static()` produces a still figure for contexts where motion is unwelcome. The palette's colour-vision-deficiency guarantees and the `pv_textures()` hatching layer mean series identity never rides on colour alone.

## Getting data in

```r
# One reader for everything — dispatches on the file extension
pv_read("results.csv")
pv_read("trial.sas7bdat")
pv_read("warehouse.sqlite", table = "sales")

# SQL: connect, write, query (with ? parameter binding), run script files
con <- pv_db_connect("warehouse.sqlite")
pv_db_write(con, "sales", pv_sales)
pv_db_tables(con)
pv_query(con, "SELECT * FROM sales WHERE region = ?", params = list("North"))
pv_run_sql_file(con, system.file("sql", "demo.sql", package = "polyviz"))
pv_db_disconnect(con)

# SAS: round-trip datasets with variable labels intact
df <- pv_set_labels(pv_sales, c(revenue = "Net revenue, EUR"))
pv_write_sas(df, "sales.xpt")
pv_labels(pv_read_sas("sales.xpt"))
```

### Live Swiss open data

```r
pv_search_opendata("Finanzausgleich Luzern")        # the federal catalogue
pv_fetch_opendata("finanzausgleich-kanton-luzern")  # fetch a dataset it lists
pv_fetch_bfs("DF_SSV_POP_1930")   # a stats.swiss SDMX dataflow, tidied
pv_fetch_lustat("fa-lu-ra")       # a LUSTAT Statistik Luzern CSV

# Big registers, sliced on the server: an SDMX data key names dimension
# values in the dataflow's dimension order (empty segment = all values,
# '+' = or). DF_LWZ_1 is hundreds of MB whole; canton Lucerne's vacancy
# rate since 2015 is a few KB:
pv_fetch_bfs("CH1.LWZ,DF_LWZ_1", filter = "LU._T._T.PC.A", start = "2015")
```

Every fetch prints the data's source and licence terms and attaches them to the result; resources without an open licence are refused rather than delivered. Downloads are cached under the URL's hash — `pv_cache_status()` lists the cache, `pv_cache_clear()` empties it, and a cached build re-runs offline. Portals, keys, and obligations: `vignette("swiss-open-data")`.

## Analysing it

```r
pv_summary(airquality)                    # dimensions, missingness, value sketches
pv_profile(airquality)                    # numeric profiling — Python when available,
                                          # the same computation in R otherwise;
                                          # skewness, kurtosis, Jarque-Bera included
pv_outliers(airquality$Ozone, "iqr")      # outlier flags by IQR fence or z-score
pv_plot_corr(mtcars, method = "spearman") # correlation heatmap
pv_plot_missing(airquality)               # where the NAs live
pv_report(airquality, "airquality.html")  # all of the above as one HTML report
```

Wherever a correlation is computed (`pv_pairs()`, `pv_plot_corr()`, the `pv_report()` heatmap), `method = "spearman"` or `"kendall"` swaps Pearson's *r* for a rank coefficient — the honest choice for relationships that are monotone but not linear. The exploratory workflow end to end: `vignette("polyviz-eda")`; the polyglot backends: `vignette("polyglot-backend")`.

## Shiny

Charts are first-class Shiny citizens: `pvchartOutput("id")` in the UI, `renderPvchart({ ... })` in the server. Interactions come back as inputs — hovering, clicking, and brushing marks fire `input$id_hover`, `input$id_click`, and `input$id_brush` with the underlying data, so a chart can drive the rest of an app. `pv_demo()` is itself a Shiny app built this way: every chart family with live option controls, theme, locale, and mode switches, and an event log showing exactly what your server would receive.

## Bundled data

Fourteen real open-government datasets ship with the package, covering the population, economy, energy, territory, tourism, and weather of Switzerland:

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

The one layer too big to bundle — all ~2100 municipal boundaries — is downloaded and cached on first use by `pv_fetch_map("municipalities")`, or simply by passing `map = "municipalities"` to a geo chart.

All are openly licensed with source citation required (see each dataset's help page for the exact attribution); the LUSTAT datasets additionally require the data owner's permission for commercial use. Three simulated datasets (`pv_sales`, `pv_network`, `pv_flows`) remain for examples that need shapes the real data doesn't provide.

## Documentation map

| Where | What |
|---|---|
| `vignette("polyviz")` | The five-minute tour — first chart to first export |
| `vignette("embedding-charts")` | Charts in R Markdown, Quarto, Word, LaTeX; print sizing |
| `vignette("swiss-open-data")` | The fetchers, server-side filtering, the cache, licence obligations |
| `vignette("polyviz-eda")` | The exploratory workflow: summary → profile → suggest → chart |
| `vignette("polyglot-backend")` | How the SQL, Python, and SAS backends work and when they engage |
| [Function reference](https://jastephan63.github.io/polyviz/reference/) | Every exported function, rendered |
| [Cheatsheet (PDF)](https://jastephan63.github.io/polyviz/polyviz-cheatsheet.pdf) | The whole package on one A4 sheet |
| [Gallery](https://jastephan63.github.io/polyviz/) · [Story](https://jastephan63.github.io/polyviz/story.html) · [Board demo](https://jastephan63.github.io/polyviz/board.html) | Everything live |
| [NEWS](NEWS.md) · [Releases](https://github.com/jastephan63/polyviz/releases) | What changed, with checked tarballs |

## Licence

MIT © Jake Stephan. Bundled: [d3.js](https://d3js.org) (ISC),
[d3-sankey](https://github.com/d3/d3-sankey) (BSD-3),
[d3-contour](https://github.com/d3/d3-contour) (ISC),
[d3-hexbin](https://github.com/d3/d3-hexbin) (BSD-3),
[Inter](https://rsms.me/inter/) (SIL OFL 1.1).
