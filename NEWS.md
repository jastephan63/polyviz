# polyviz 0.7.0

Swiss open data, live from R — and maps of the whole country.

## Fetching data

* `pv_fetch_bfs()` and `pv_fetch_lustat()` download any dataset from
  the stats.swiss and LUSTAT portals and return it as a tidy data
  frame, with the source and licence printed on every fetch and
  attached to the result (LUSTAT's open-by-ask terms included).
* `pv_search_opendata()` and `pv_fetch_opendata()` search and download
  from opendata.swiss; resources whose licence is not open are refused
  with a plain message instead of delivered.
* Downloads land in a local cache (`pv_cache_status()`,
  `pv_cache_clear()`), so repeat fetches are instant and offline-safe.

## Maps

* Bundled Switzerland-wide layers: `pv_swiss_cantons`,
  `pv_swiss_districts`, and `pv_swiss_lakes` (BFS ThemaKart, ~250KB
  total), plus `pv_city_coords` — the 180 statistical cities with
  their coordinates.
* `pv_choropleth(map = )` now takes a bundled layer name, your own
  `sf` object (reprojected automatically), or `"municipalities"`,
  which `pv_fetch_map()` downloads once and caches. Mismatched joins
  fail loudly, naming the unmatched ids. A `lakes` option draws the
  water, which makes country-wide maps read as Switzerland at a
  glance.
* `pv_bubble_map()` — the 25th chart type: sized circles over a quiet
  base map, with a circle-size legend, adaptive overlap opacity, and
  the full chart chrome.

A new vignette, "Swiss open data from R", walks the whole path:
fetch a dataset, chart it, map it, and save the figure for a paper.

# polyviz 0.6.0

More control over the core charts: the four chart-option items from the
public roadmap, every one of them off by default so existing charts are
untouched.

* `pv_bar(stack = "stack")` piles a series into one bar per category —
  2px gaps between segments, values labelled where they fit, the
  category total at the bar's end; `stack = "percent"` normalises each
  bar to 100% for composition comparisons. Both work horizontally. (#2)
* `pv_line(show_points = TRUE)` marks every observation on the line
  (`"auto"` turns markers on for short series), and
  `curve = "monotone"` or `"step"` picks the interpolation — together
  they give the connected scatter. (#4)
* `pv_line(zoom = TRUE)` and `pv_area(zoom = TRUE)` add a muted
  context strip under the chart: drag across it to zoom the main
  panel, double-click to reset. Charts open at the full range, so
  saved and exported figures always show the whole series. (#5)
* `pv_violin(points = TRUE)` jitters the raw observations inside each
  violin's silhouette with adaptive opacity, complementing the
  boxplot overlay; `"auto"` shows them when groups are small. (#1)

The live gallery gained four sections showing each option on the Swiss
data, and the CI render tests now exercise all the new code paths.

# polyviz 0.5.0

Charts that work when they are not interactive, and a package that
fails loudly instead of drawing something wrong.

## Static export

* `pv_save()` — save any chart as a file: `.png` (retina raster for
  Word and slides), `.svg` (a standalone vector of the whole chart —
  title, legend, and source line included — for the web and vector
  editors), `.pdf` (true vector through Chrome's print engine, ready
  for LaTeX), or `.html` (one fully self-contained file, built without
  pandoc). Captures force the animation off and default to light mode,
  so the file always shows the finished chart.
* Knitting to Word or PDF now embeds a print-quality image of the
  chart automatically (chromote and Chrome required); see the new
  "Embedding polyviz charts in papers and documents" vignette.
* Every rendered chart has a download control in its top-right corner
  offering the same standalone SVG and a 2x PNG; turn it off per chart
  with `pv_downloads(w, FALSE)`.
* Headless browsers — knitr screenshots, `pv_save()`, test suites —
  now always get the finished chart: the entry animation is skipped
  and `mode = "auto"` resolves to light, so an automated capture can
  no longer freeze a half-drawn or dark-themed frame.

## Clear failures

* Wrong input now stops in R with a plain message instead of drawing
  something wrong: factor or character columns in numeric roles,
  matrix or NULL data, misspelled columns (with a "did you mean"
  hint), empty data, invalid `mode`/`duration` values, negative
  values where areas or angles encode size (treemap, pack, sunburst,
  stacked area), and duplicate category/x keys on bar, line, and
  sankey. Rows with missing values are dropped with a warning that
  says how many.
* When rendering itself fails in the browser, the chart now shows
  "polyviz: rendering failed" with the reason in the widget — no more
  silent blank boxes — and empty payloads say "no data to display".

## Big data

* Heatmaps thin their row labels and scale their cell gaps on dense
  grids, so a 150x150 matrix reads as a continuous colour field.
* Scatter opacity and point size keep adapting past 10,000 points;
  a 50,000-point cloud keeps its density structure visible.
* Line charts with more series than the palette switch to a grey
  spaghetti view with hover highlighting instead of recycling colours.

## Infrastructure

* Continuous integration now renders all 24 chart types in a headless
  browser on every push and fails on any JavaScript error; the
  rendered images are published as a build artifact.

# polyviz 0.4.0

Depth over breadth: layers, links, and infrastructure that work across
the existing 24 chart types, composed with the pipe.

## Chart layers (pipe-able modifiers)

* `pv_annotate()` with `pv_hline()`, `pv_vline()`, `pv_band()`, and
  `pv_note()` — reference lines, shaded bands, and leader-line callouts
  on the cartesian charts.
* `pv_trend()` — loess or lm fits with confidence ribbons on scatter
  and line charts, computed in R, drawn by d3.
* `pv_facet()` — small multiples with shared scales for bar, line,
  scatter, and area.

## Interactivity

* Shiny round-trips: clicks (and scatter/bar hovers) arrive as
  `input$<outputId>_<event>` values on every chart type.
* Crosstalk linking via `pv_link()`: brush a scatter or beeswarm, click
  regions on a choropleth, and every linked polyviz widget (and any
  other crosstalk widget) highlights the same rows.

## Theming

* `pv_set_theme()` / `pv_reset_theme()` — bring your own brand palette;
  dark-mode colours derive automatically via OKLCH re-stepping.
* `pv_check_palette()` — the colour-vision-deficiency validator behind
  polyviz's own palette, as a public R function: lightness band, chroma
  floor, Machado-simulated CVD separation, contrast, and a suggested
  slot ordering.

## Reporting and infrastructure

* `pv_report()` — one call turns a data frame into a themed HTML report
  of real widgets.
* Continuous integration (GitHub Actions `R CMD check`), a pkgdown
  reference site at `/reference`, two vignettes, and CRAN submission
  notes.

# polyviz 0.3.0

Eight new chart types — 24 in total — plus map boundaries and daily
weather data to power them.

## New charts

* `pv_choropleth()` — GeoJSON choropleth with sequential or
  centre-pinned diverging colouring; ships with the Lucerne municipal
  boundaries (`pv_lucerne_map`) joined by official BFS numbers.
* `pv_race()` — the animated bar-chart race, with interpolated
  keyframes, entering/exiting entities, and a replay control.
* `pv_bump()` — ranks over time as crossing lines.
* `pv_pack()` — zoomable circle packing.
* `pv_dendrogram()` — feed `stats::hclust()` output straight in;
  optional `k` colours the cut clusters.
* `pv_beeswarm()` — force-packed dot strips, one dot per observation.
* `pv_violin()` — mirrored densities with optional inner boxplots.
* `pv_calendar()` — GitHub-style calendar heatmap, one block per year.

## Data

* `pv_lucerne_map` — boundary polygons of the 79 Lucerne municipalities
  (BFS Generalisierte Gemeindegrenzen, 1.1.2025, WGS84).
* `pv_weather` — six years of daily temperature, precipitation, and
  sunshine from the MeteoSwiss Luzern station.
* `pv_fiscal` and `pv_elections` gain `municipality_id` (the official
  BFS number), joining the map exactly.

# polyviz 0.2.1

Charts now correct themselves to fit their data and their container, and
every automatic behaviour has an explicit override.

## Adaptive rendering

* Every chart re-renders whenever its container changes size (a
  `ResizeObserver`, not just window resizes), so a chart is never laid
  out for a width it no longer has.
* Label-derived margins are capped at 45% of the chart width everywhere;
  labels truncate with an ellipsis and the tooltip keeps the full text.
* Scatter and parallel-coordinate opacity scales with row count; force
  network physics scale with node count, links are clearly visible, and
  node labels wear a halo and dodge collisions.
* Sunburst labels only render where the arc genuinely fits them; light
  mode reads richer.
* Compact numbers round to three significant digits ("23.3M");
  tooltips keep full precision. Years no longer render as "2,020".

## New options (TRUE / FALSE / "auto")

* `pv_bar(horizontal, value_labels)` — auto flips long-labelled
  single-series charts horizontal and prints values when there is room.
* `pv_line(legend)`, `pv_scatter(legend)`, `pv_area(legend)`.
* `pv_boxplot(points)` — auto shows raw points up to 600 values.
* `pv_donut(labels)`, `pv_treemap(labels)`, `pv_lollipop(value_labels)`.
* `pv_heatmap(cell_values, truncate_labels)`.
* `xlab` / `ylab` overrides on all cartesian charts (`NULL` keeps the
  column name, `NA` or `""` suppresses the title).

# polyviz 0.2.0

Ten new d3 chart types, a research-backed design system, and real Swiss
open-government data.

## New charts

* Distribution: `pv_histogram()` (optional density overlay),
  `pv_boxplot()` (optional jittered points), `pv_ridgeline()`.
* Composition and ranking: `pv_donut()` (pie via `inner_radius = 0`),
  `pv_treemap()`, `pv_lollipop()`.
* Evolution and matrix: `pv_area()` (stacked, percent, or streamgraph via
  `offset`), `pv_heatmap()` (sequential or diverging).
* Flow and multivariate: `pv_sankey()` (bundled official d3-sankey
  plugin), `pv_parallel()` with per-axis brush filtering.
* `pv_bar()` gains a `horizontal` orientation with value labels at the
  bar ends; every chart gains a `source` credit line.

## Design system

* New categorical palette anchored on The Economist's published web
  palette, re-stepped in OKLCH and slot-ordered by exhaustive search so
  adjacent series stay separable under common colour-vision deficiencies
  in both light and dark mode; matched sequential and diverging ramps.
* Inter (SIL OFL) is bundled and used everywhere, with tabular numerals
  on axes; warm editorial surface and ink tokens; newsroom chart anatomy
  (left-aligned titles, horizontal-only hairline gridlines, direct
  labels, compact axis numbers, source lines).

## Data

* Six real open datasets covering population, economy, and territory:
  `pv_city_population`, `pv_city_sectors`, `pv_city_landuse`,
  `pv_commuters`, `pv_fiscal`, `pv_elections` — from the Bundesamt für
  Statistik, Fachstelle Statistik Kanton Zug, and LUSTAT Statistik
  Luzern, each documented with its licence and attribution.

## Documentation

* Live demo gallery (GitHub Pages) where every chart runs interactively
  on the real data with an explanation and its code; the README gallery
  shows every chart in light or dark to match your GitHub theme.

# polyviz 0.1.0

First release: six interactive d3 chart types (bar, line, scatter,
force-directed network, chord, zoomable sunburst); unified ingestion
across CSV, SQLite, and SAS files; Python-backed profiling with a pure-R
fallback; simulated demo data; clean R CMD check.
