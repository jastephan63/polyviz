# polyviz roadmap

The daily development queue. Each item is scoped to be one focused session:
implement, test, document, then commit. Items get ticked off as they land.

## Chart types & options

- [ ] Violin plot: add optional boxplot overlay and jittered raw points
- [ ] Stacked and percent-stacked modes for `pv_bar` (plus horizontal orientation)
- [ ] Correlogram / scatterplot-matrix widget (`pv_pairs`) composing scatter, histogram, and correlation cells
- [x] Dendrogram widget that accepts `hclust` objects directly — shipped in 0.3.0 as `pv_dendrogram()`
- [x] Zoomable circular-packing widget (`pv_pack`) — shipped in 0.3.0
- [ ] Connected-scatter option for `pv_line` (`show_points`, curve styles)
- [ ] Brush-to-zoom (focus + context) option for `pv_line` and area charts
- [ ] Contour-density mode for `pv_scatter` to handle overplotting on large data
- [ ] Arc diagram as a label-friendly alternative network view
- [ ] Optional texture/pattern fills as a colour-blindness accessibility channel
- [x] "Download as PNG" button option on every widget — shipped in 0.5.0, as a hover control offering both SVG and 2x PNG, toggleable per chart with `pv_downloads()`; `pv_save()` covers PNG/SVG/PDF/HTML from R
- [ ] Canvas rendering fallback for scatters beyond ~5,000 points

## Data & analysis

- [ ] Add more LUSTAT / opendata.swiss datasets with documented examples
- [x] `pv_report()`: one call that builds a full HTML EDA report from a data frame — shipped in 0.4.0
- [ ] Correlation method options (Spearman, Kendall) for `pv_plot_corr` and heatmaps
- [ ] Python backend: add skewness/kurtosis and normality checks to `pv_profile`

## Documentation & infrastructure

- [x] GitHub Actions CI: R CMD check + testthat on every push — shipped in 0.4.0, extended in 0.5.0 with headless render tests of all 24 charts
- [x] pkgdown site published via GitHub Pages — shipped in 0.4.0 at /reference
- [x] Vignette: "Exploring a dataset with polyviz" end-to-end walkthrough — shipped in 0.4.0
- [x] Vignette: "The polyglot backend" — how R drives SQL, SAS, Python, and d3 — shipped in 0.4.0
- [ ] Shiny demo app in `inst/shiny` showcasing every widget with live controls
- [ ] CRAN submission preparation: spell check, URL checks, win-builder run
