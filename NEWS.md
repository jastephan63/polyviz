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
