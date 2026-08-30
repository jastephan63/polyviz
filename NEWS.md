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
