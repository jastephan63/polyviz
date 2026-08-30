# Demo gallery examples for the chart-depth layer: the pipe-able
# modifiers that add statistics and editorial marks to a finished chart.
# Each block below is one gallery entry: an explanation for the demo
# page, then a single runnable expression built on a bundled dataset.

## annotated
# explain: A chart shows the data; annotations say what it means. Every
#   polyviz cartesian chart can be piped through pv_trend(), which fits a
#   loess smoother or regression line in R and overlays it with its
#   confidence ribbon, and pv_annotate(), which places reference lines,
#   shaded bands, and short notes in data coordinates - so they stay put
#   at every chart width. Here each dot is a Lucerne municipality in
#   2025: fiscal strength on the x axis, equalization received on the y.
#   The loess curve makes the mechanism visible - payments fall steadily
#   as resources rise and stop at the dashed cantonal average - and its
#   ribbon is honest about uncertainty, widening on the right where rich
#   municipalities are few. The note singles out Emmen, a below-average
#   city of 32,000 that draws the canton's largest single payment.
f25 <- subset(pv_fiscal, year == 2025)
pv_scatter(f25, x = "resource_index", y = "equalization_chf",
           label = "municipality",
           xlab = "Resource index (cantonal average = 100)",
           ylab = "Equalization received (CHF)",
           title = "The weaker the tax base, the larger the payment",
           subtitle = "Lucerne municipalities: fiscal resources vs. equalization received, 2025",
           source = "Source: LUSTAT Statistik Luzern") |>
  pv_trend("loess") |>
  pv_annotate(
    pv_vline(100, label = "cantonal average"),
    pv_note(65.9, 23278722, "Emmen: CHF 23.3m", dx = 18, dy = 14))
