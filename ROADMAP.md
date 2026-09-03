# polyviz roadmap

The daily development queue. Each item is scoped to be one focused session:
implement, test, document, then commit. Items get ticked off as they land. Every open item is tracked as a
GitHub issue - the numbers below link to them.

## Chart types & options

- [x] Violin plot: add optional boxplot overlay and jittered raw points (#1) — shipped in 0.6.0
- [x] Stacked and percent-stacked modes for `pv_bar` (plus horizontal orientation) (#2) — shipped in 0.6.0
- [x] Correlogram / scatterplot-matrix widget (`pv_pairs`) composing scatter, histogram, and correlation cells (#3) — shipped in 0.9.0
- [x] Dendrogram widget that accepts `hclust` objects directly — shipped in 0.3.0 as `pv_dendrogram()`
- [x] Zoomable circular-packing widget (`pv_pack`) — shipped in 0.3.0
- [x] Connected-scatter option for `pv_line` (`show_points`, curve styles) (#4) — shipped in 0.6.0
- [x] Brush-to-zoom (focus + context) option for `pv_line` and area charts (#5) — shipped in 0.6.0
- [ ] Contour-density mode for `pv_scatter` to handle overplotting on large data (#6)
- [ ] Arc diagram as a label-friendly alternative network view (#7)
- [ ] Optional texture/pattern fills as a colour-blindness accessibility channel (#8)
- [x] "Download as PNG" button option on every widget — shipped in 0.5.0, as a hover control offering both SVG and 2x PNG, toggleable per chart with `pv_downloads()`; `pv_save()` covers PNG/SVG/PDF/HTML from R
- [ ] Canvas rendering fallback for scatters beyond ~5,000 points (#9)

## Data & analysis

- [ ] Add more LUSTAT / opendata.swiss datasets with documented examples (#10)
- [x] `pv_report()`: one call that builds a full HTML EDA report from a data frame — shipped in 0.4.0
- [x] Correlation method options (Spearman, Kendall) for `pv_plot_corr` and heatmaps (#11) — shipped in 0.9.0
- [x] Python backend: add skewness/kurtosis and normality checks to `pv_profile` (#12) — shipped in 0.9.0

## Documentation & infrastructure

- [x] GitHub Actions CI: R CMD check + testthat on every push — shipped in 0.4.0, extended in 0.5.0 with headless render tests of all 24 charts
- [x] pkgdown site published via GitHub Pages — shipped in 0.4.0 at /reference
- [x] Vignette: "Exploring a dataset with polyviz" end-to-end walkthrough — shipped in 0.4.0
- [x] Vignette: "The polyglot backend" — how R drives SQL, SAS, Python, and d3 — shipped in 0.4.0
- [x] Shiny demo app in `inst/shiny` showcasing every widget with live controls (#13) — shipped in 1.0.0 as `pv_demo()`
- [x] CRAN submission preparation: spell check, URL checks, win-builder run (#14) — decided against submitting: polyviz is a personal, repo-distributed package. The hygiene shipped anyway (spell and URL checks run clean as of 1.0.0; CI checks at CRAN strictness on every push).
