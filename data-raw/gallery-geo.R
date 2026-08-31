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

## choropleth-cantons
# explain: The same pv_choropleth() call scales up to the whole country:
#   map = "cantons" (or "districts", or "municipalities") draws the
#   Switzerland-wide layer, major lakes included, and joins the data on
#   the official BFS region number. Here each canton is coloured by how
#   much its statistical cities grew over nearly a century, on a
#   diverging palette centred at the +121% of urban Switzerland as a
#   whole - red cantons out-grew the urban average, blue ones lagged it.
#   The map splits along the country's economic fault lines: Zug's
#   cities more than quadrupled (+316%) on the canton's low-tax boom and
#   the Valais valley towns follow at +277%, while Basel-Stadt (+30%) -
#   a city hemmed in by its own cantonal border - watched the century's
#   growth spill next door into Basel-Landschaft (+216%), and the old
#   textile east around Appenzell Ausserrhoden (+17%) barely moved.
local({
  # The BFS issues municipality numbers in cantonal blocks - Zürich's
  # start at 1, Bern's at 301, and so on - so each city's canton number
  # is one findInterval() away from its municipality number.
  blocks <- c(1, 301, 1001, 1201, 1301, 1401, 1501, 1601, 1701, 2001,
              2401, 2701, 2761, 2901, 3001, 3101, 3201, 3501, 4001,
              4401, 5001, 5401, 6001, 6401, 6601, 6701)
  pop <- merge(pv_city_population, pv_city_coords[, c("city", "id")],
               by = "city")
  pop <- pop[pop$city %in% pop$city[pop$year == 1930], ]
  pop$canton <- findInterval(pop$id, blocks)
  then <- tapply(pop$population[pop$year == 1930],
                 pop$canton[pop$year == 1930], sum)
  now <- tapply(pop$population[pop$year == 2024],
                pop$canton[pop$year == 2024], sum)
  growth <- data.frame(canton = as.integer(names(now)),
                       pct = round(100 * (now / then[names(now)] - 1)))
  pv_choropleth(growth, map = "cantons", id = "canton", value = "pct",
                palette = "diverging",
                center = round(100 * (sum(now) / sum(then) - 1)),
                title = "The urban century belonged to Zug, not Basel",
                subtitle = "Growth of city residents by canton, 1930–2024 - urban Switzerland overall: +121%",
                source = "Source: Bundesamt für Statistik; boundaries © BFS, ThemaKart")
})

## bubble-map
# explain: The bubble map answers where the choropleth cannot: it is for
#   quantities anchored to points rather than regions - city populations,
#   plant capacities, event counts. Each place becomes a circle whose
#   area (not radius - the honest encoding for sizes) scales with the
#   value, every circle wears a thin surface-coloured ring so overlaps
#   stay readable, and a size legend sits in the map's corner; hovering
#   a circle names it with its exact value. The 180 statistical cities
#   of Switzerland, sized by their 2024 population, redraw the country's
#   human geography: a chain of cities across the Plateau from Genève to
#   St. Gallen, Zürich's 437,000 residents dwarfing everything around
#   it, and the Alps in between left almost empty - except where valley
#   floors let towns like Chur, Sion, and Lugano through.
local({
  cities <- merge(pv_city_coords,
                  subset(pv_city_population, year == 2024), by = "city")
  pv_bubble_map(cities, lon = "lon", lat = "lat", size = "population",
                label = "city",
                title = "Half of Switzerland lives in these 180 dots",
                subtitle = "Permanent residents of the statistical cities, 2024",
                source = "Source: Bundesamt für Statistik; boundaries © BFS, ThemaKart")
})
