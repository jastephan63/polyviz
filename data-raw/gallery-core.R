# Gallery snippets for the original six chart types. Each block: an id,
# an explanation for the demo page, and one runnable expression on the
# real bundled data. data-raw/build-gallery.R assembles these into docs/.

## bar
# explain: The bar chart is the workhorse of comparison - one bar per
#   category, length encoding the value. polyviz draws bars with rounded
#   value-ends growing from a zero baseline, and the horizontal
#   orientation keeps long category names upright and readable, with the
#   exact value at each bar's end. Here it ranks how the city of Lucerne
#   uses its land: agriculture and buildings each cover more ground than
#   forest, and unproductive land is nearly absent.
pv_bar(
  aggregate(hectares ~ category,
            pv_city_landuse[pv_city_landuse$city == "Luzern", ], sum),
  x = "category", y = "hectares", sort = TRUE, horizontal = TRUE,
  title = "Land use in the city of Lucerne",
  subtitle = "Hectares by category, 2013–2025 survey",
  source = "Source: Bundesamt für Statistik – Arealstatistik"
)

## line
# explain: The line chart shows how values evolve - polyviz draws each
#   series in with a left-to-right animation, labels lines directly at
#   their right ends (no legend hunting), and a crosshair tooltip reads
#   out every series at the hovered year. The data here is nearly a
#   century of population counts: the city of Lucerne grew until about
#   1970 and then plateaued, while its suburbs kept climbing.
pv_line(
  pv_city_population[pv_city_population$city %in%
                       c("Luzern", "Emmen", "Kriens", "Zug"), ],
  x = "year", y = "population", series = "city",
  title = "A century of urban growth",
  subtitle = "Permanent resident population at census years since 1930",
  source = "Source: Bundesamt für Statistik – Statistik der Schweizer Städte"
)

## scatter
# explain: The scatter plot reveals the relationship between two numeric
#   variables, one dot per observation, with hover tooltips for exact
#   values. This one shows how Lucerne's fiscal equalization works as
#   designed: municipalities with a low resource index (weak tax base)
#   receive large equalization payments, and everyone above the
#   100-index line receives nothing.
pv_scatter(
  pv_fiscal[pv_fiscal$year == 2025, ],
  x = "resource_index", y = "equalization_chf", label = "municipality",
  title = "Fiscal equalization evens out municipal wealth",
  subtitle = "Lucerne municipalities, 2025 — resource index 100 = cantonal average",
  source = "Source: LUSTAT Statistik Luzern"
)

## force
# explain: The force-directed network is d3's signature physics
#   simulation: nodes repel, links pull, and you can grab any node and
#   drag it. Nodes here are large Swiss cities, linked when their
#   economic structures are highly similar (correlation of employment
#   shares across all 19 economic sectors above 0.94), sized by how many
#   similar cities each has, and coloured by population class. Hovering
#   a city highlights its economic look-alikes.
local({
  # Wide matrix of sector shares per city, for the 30 most populous cities.
  # xtabs keeps sectors aligned even where a city has no row for a sector.
  latest <- pv_city_population[pv_city_population$year ==
                                 max(pv_city_population$year), ]
  top <- head(latest$city[order(-latest$population)], 30)
  sect <- pv_city_sectors[pv_city_sectors$city %in% top, ]
  wide <- stats::xtabs(share ~ city + sector_code, data = sect)
  sim <- stats::cor(t(wide))
  pairs <- which(sim > 0.94 & upper.tri(sim), arr.ind = TRUE)
  cities <- rownames(sim)
  links <- data.frame(source = cities[pairs[, 1]],
                      target = cities[pairs[, 2]])
  nodes <- data.frame(
    id = cities,
    class = latest$size_class[match(cities, latest$city)]
  )
  keep <- nodes$id %in% c(links$source, links$target)
  pv_force(nodes[keep, ], links, group = "class",
           title = "Cities with similar economies",
           subtitle = "Linked when employment mix across 19 sectors correlates above 0.94",
           source = "Source: Bundesamt für Statistik – STATENT")
})

## chord
# explain: The chord diagram shows flows between entities around a
#   circle - ribbon width encodes volume, and hovering a group fades
#   everything unrelated. Here it shows commuting between Canton Zug and
#   its neighbours: ribbons leaving the Zug arc are people commuting out,
#   ribbons arriving are people commuting in, and the asymmetry is the
#   story - Zug pulls in far more workers than it sends out.
local({
  latest <- pv_commuters[pv_commuters$period ==
                           max(pv_commuters$period), ]
  regions <- unique(latest$region)
  m <- matrix(0, length(regions) + 1, length(regions) + 1,
              dimnames = list(c(regions, "Zug"), c(regions, "Zug")))
  to_zug <- latest[latest$direction == "to Zug", ]
  from_zug <- latest[latest$direction == "from Zug", ]
  m[to_zug$region, "Zug"] <- to_zug$commuters
  m["Zug", from_zug$region] <- from_zug$commuters
  pv_chord(m,
           title = "Commuting to and from Canton Zug",
           subtitle = paste("Daily commuters,", max(latest$period)),
           source = "Source: Fachstelle Statistik Kanton Zug")
})

## sunburst
# explain: The sunburst lays a hierarchy out as concentric rings - the
#   inner ring is the top level, outer rings its parts, and clicking any
#   segment zooms into that branch (the centre zooms back out). The
#   hierarchy here is Lucerne's land use in two levels: settlement,
#   cultivated, and natural land, each split into its categories, so you
#   can see at a glance that cultivated land dominates and then zoom
#   into how the settlement area subdivides.
pv_sunburst(
  pv_city_landuse[pv_city_landuse$city == "Luzern", ],
  levels = c("group", "category"), value = "hectares",
  title = "Lucerne's land, ring by ring",
  subtitle = "Click a segment to zoom in; click the centre to zoom out",
  source = "Source: Bundesamt für Statistik – Arealstatistik"
)
