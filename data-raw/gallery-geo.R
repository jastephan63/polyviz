# Gallery snippets for the geographic charts. Each block: an id, an
# explanation for the demo page, and one runnable expression on the real
# bundled data. data-raw/build-gallery.R assembles these into docs/.

## choropleth
# explain: The choropleth map colours each region of a map by a numeric
#   value - the chart for showing how a measure varies across space, here
#   on a diverging palette pinned to the cantonal average of 100 so blue
#   means fiscally weaker and red stronger. Hover any municipality for
#   its name and exact index; regions without data stay neutral grey with
#   a dashed outline. The 2025 picture splits Lucerne in two: a ring of
#   wealthy lakeside municipalities around the city - Meggen at index
#   292, Weggis, Vitznau, Horw - against a rural west where Romoos and
#   Luthern sit below 42, and only 15 of 79 municipalities reach the
#   average at all.
local({
  f <- pv_fiscal[pv_fiscal$year == 2025, ]
  pv_choropleth(f, id = "municipality_id", value = "resource_index",
                palette = "diverging", center = 100,
                title = "Lucerne's wealth wears a lakeside ring",
                subtitle = "Municipal resource index 2025 - cantonal average = 100",
                source = "Source: LUSTAT Statistik Luzern; boundaries © BFS, ThemaKart")
})
