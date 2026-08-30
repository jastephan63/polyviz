# polyviz roadmap

The daily development queue. Each item is scoped to be one focused session:
implement, test, document, then commit. Items get ticked off as they land.

## Chart types & options

- [ ] Violin plot: add optional boxplot overlay and jittered raw points
- [ ] Stacked and percent-stacked modes for `pv_bar` (plus horizontal orientation)
- [ ] Correlogram / scatterplot-matrix widget (`pv_pairs`) composing scatter, histogram, and correlation cells
- [ ] Dendrogram widget that accepts `hclust` objects directly
- [ ] Zoomable circular-packing widget (`pv_pack`)
- [ ] Connected-scatter option for `pv_line` (`show_points`, curve styles)
- [ ] Brush-to-zoom (focus + context) option for `pv_line` and area charts
- [ ] Contour-density mode for `pv_scatter` to handle overplotting on large data
- [ ] Arc diagram as a label-friendly alternative network view
- [ ] Optional texture/pattern fills as a colour-blindness accessibility channel
- [ ] "Download as PNG" button option on every widget
- [ ] Canvas rendering fallback for scatters beyond ~5,000 points

## Data & analysis

- [ ] Add more LUSTAT / opendata.swiss datasets with documented examples
- [ ] `pv_report()`: one call that builds a full HTML EDA report from a data frame
- [ ] Correlation method options (Spearman, Kendall) for `pv_plot_corr` and heatmaps
- [ ] Python backend: add skewness/kurtosis and normality checks to `pv_profile`

## Documentation & infrastructure

- [ ] GitHub Actions CI: R CMD check + testthat on every push
- [ ] pkgdown site published via GitHub Pages
- [ ] Vignette: "Exploring a dataset with polyviz" end-to-end walkthrough
- [ ] Vignette: "The polyglot backend" — how R drives SQL, SAS, Python, and d3
- [ ] Shiny demo app in `inst/shiny` showcasing every widget with live controls
- [ ] CRAN submission preparation: spell check, URL checks, win-builder run
