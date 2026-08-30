# polyviz

**D3-quality interactive visualisation and polyglot data analysis, with a pure R interface.**

polyviz was born from loving [d3.js](https://d3js.org) visualisations but not wanting to write JavaScript. Every chart below is rendered by a bundled copy of D3 v7 — real d3 scales, transitions, tooltips, force simulations — but you drive it entirely from R data frames.

Behind the R interface, the package deliberately spans four backend languages:

| Language | Where it lives | What it does |
|---|---|---|
| **JavaScript (D3 v7)** | `inst/htmlwidgets/pvchart.js` | Renders all six interactive chart types |
| **SQL** | `R/sql.R`, `inst/sql/` | SQLite querying, parameterised queries, runnable `.sql` script files |
| **Python** | `inst/python/polyviz.py` | Numeric profiling and outlier detection (stdlib only — no pandas needed), with an identical pure-R fallback when Python isn't available |
| **SAS** | `R/sas.R` | Reads/writes `sas7bdat` and `xpt` datasets with variable labels, no SAS licence required |

## Installation

```r
# install.packages("devtools")
devtools::install_github("jastephan63/polyviz")
```

Python is optional. If `reticulate` finds any Python ≥ 3.8, the profiling functions use it; otherwise they transparently run the same computation in R.

## The charts

All six are interactive htmlwidgets: they animate in, respond to hover with tooltips, follow your system's light/dark mode, and work in the RStudio Viewer, R Markdown, Quarto, and Shiny.

```r
library(polyviz)

# Animated bar chart
totals <- aggregate(revenue ~ region, pv_sales, sum)
pv_bar(totals, x = "region", y = "revenue", sort = TRUE,
       title = "Revenue by region")

# Multi-series line chart with a crosshair tooltip
monthly <- aggregate(revenue ~ month + region, pv_sales, sum)
pv_line(monthly, x = "month", y = "revenue", series = "region",
        title = "Monthly revenue")

# Scatter with size encoding
pv_scatter(mtcars, x = "wt", y = "mpg", size = "hp",
           title = "Weight vs efficiency")

# Force-directed network — drag the nodes
pv_force(pv_network$nodes, pv_network$links, group = "group",
         title = "Collaboration network")

# Chord diagram of flows between entities
pv_chord(pv_flows, title = "Inter-warehouse shipments")

# Zoomable sunburst — click a segment to zoom in, the centre to zoom out
pv_sunburst(pv_sales, levels = c("region", "product"), value = "revenue",
            title = "Revenue hierarchy")
```

There are also two static ggplot2 companions, `pv_plot_missing()` and `pv_plot_corr()`, plus `theme_polyviz()` and `scale_colour_pv()` / `scale_fill_pv()` if you want the same look on your own ggplots.

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

## Demo data

Three simulated datasets ship with the package so every example runs out of the box: `pv_sales` (two years of monthly sales), `pv_network` (a 16-person collaboration graph), and `pv_flows` (a warehouse shipment matrix). The same sales table is also bundled as `inst/extdata/demo.sqlite` and `inst/extdata/demo_sales.xpt` for practising the SQL and SAS readers.

## Design notes

The colour palette is colourblind-checked: adjacent series colours keep a safe perceptual distance under the common colour-vision deficiencies, in both light and dark mode, and series always take colours in the same fixed order. Charts cap or refuse encodings that would break that guarantee (for example, more than 8 bar series, or more than 3 scatter colour groups).

## Licence

MIT © Jake Stephan
