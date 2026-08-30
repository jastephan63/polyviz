# Gallery snippets for the hierarchy charts. Each block: an id, an
# explanation for the demo page, and one runnable expression on the real
# bundled data. data-raw/build-gallery.R assembles these into docs/.

## pack
# explain: Circle packing nests a hierarchy as circles within circles -
#   area encodes value, hue names the top-level branch, and deeper levels
#   fade toward the background. Click any circle to fly into it (the
#   classic d3 zoom), click the background to step back out; labels
#   appear only where a name honestly fits at the current zoom, and
#   hovering any circle gives the full path and share. Summed over all
#   181 Swiss cities, urban ground is greener than the word "urban"
#   suggests: cultivated land is nearly two thirds of it (forest 32% and
#   agriculture 32%), the whole settlement ring is under a quarter, and
#   buildings proper cover just 12%.
pv_pack(
  pv_city_landuse, levels = c("group", "category"), value = "hectares",
  title = "What Swiss urban ground is made of",
  subtitle = "Hectares across all 181 statistical cities — click a circle to zoom",
  source = "Source: Bundesamt für Statistik – Arealstatistik"
)

## dendrogram
# explain: The dendrogram draws a hierarchical clustering as a tree -
#   leaves on the right, and every merge drawn at its actual height, so
#   the taller the elbow, the more dissimilar the two groups it joins.
#   Cutting the tree into k clusters colours each cluster and leaves the
#   branches above the cut neutral; hovering a junction highlights its
#   subtree and reads out its size and merge height. Ward-clustering the
#   25 most populous Swiss cities by employment mix finds four families
#   of city economies: the big service-and-culture centres (Basel, Bern,
#   Genève, Lausanne, Luzern...), a broad band of regional towns, a
#   high-finance quartet around Zürich and Zug - and Vernier, whose
#   airport-side logistics economy merges last, resembling no one.
local({
  latest <- pv_city_population[pv_city_population$year ==
                                 max(pv_city_population$year), ]
  top <- head(latest$city[order(-latest$population)], 25)
  sect <- pv_city_sectors[pv_city_sectors$city %in% top, ]
  # xtabs keeps sectors aligned even where a city has no row for a sector.
  wide <- stats::xtabs(share ~ city + sector_code, data = sect)
  hc <- stats::hclust(stats::dist(scale(wide)), method = "ward.D2")
  pv_dendrogram(hc, k = 4,
                title = "Families of city economies",
                subtitle = "Ward clustering of the 25 largest cities by employment mix across 19 sectors, cut into 4",
                source = "Source: Bundesamt für Statistik – STATENT")
})
