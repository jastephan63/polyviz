# Gallery snippets for the original six chart types. Each block: an id,
# an explanation for the demo page, and one runnable expression on the
# real bundled data. data-raw/build-gallery.R assembles these into docs/.

## bar
# explain: The bar chart is the workhorse of comparison - one bar per
#   category, length encoding the value. polyviz draws bars with rounded
#   value-ends growing from a zero baseline. Orientation defaults to
#   "auto": a single-series chart flips itself horizontal when its
#   category labels are too long to sit under vertical bars, and vertical
#   bars print their values on top when there is room for the numbers.
#   Here the horizontal orientation is pinned explicitly - the readable
#   choice for ranked categories, labels upright and the exact value at
#   each bar's end - ranking how the city of Lucerne uses its land:
#   agriculture and buildings each cover more ground than forest, and
#   unproductive land is nearly absent.
pv_bar(
  aggregate(hectares ~ category,
            pv_city_landuse[pv_city_landuse$city == "Luzern", ], sum),
  x = "category", y = "hectares", sort = TRUE, horizontal = TRUE,
  title = "Land use in the city of Lucerne",
  subtitle = "Hectares by category, 2013–2025 survey",
  source = "Source: Bundesamt für Statistik – Arealstatistik"
)

## bar-stacked
# explain: Hand the bar chart a series and stack = "percent", and it stops
#   comparing sizes and starts comparing recipes: one bar per category, a
#   segment per series level, every bar normalised to 100%. Reach for it
#   when the question is what each whole is made of - here, city economies
#   of very different sizes sit on equal footing because absolute
#   employment is deliberately thrown away. (stack = "stack" is the
#   halfway option: the segments pile up but keep their raw values, so
#   each bar's full height is its total, printed at the bar's end.)
#   Segments print their share wherever they have the room for it, the
#   tooltip always has the exact numbers, and segments stack in the order
#   the series first appear in the data - so the group to read most
#   precisely belongs on the baseline. Zürich and Zug run on knowledge
#   work and finance - nearly half of all jobs - Biel/Bienne is the
#   industrial outlier with a quarter of its employment in industry and
#   construction, and no public sector comes close to the capital's 38%.
local({
  # Fold the 19 NOGA sectors into five groups a reader can hold at once.
  fold <- c(A = "Other", B = "Industry & construction",
            C = "Industry & construction", D = "Industry & construction",
            E = "Industry & construction", F = "Industry & construction",
            G = "Trade, transport & hospitality",
            H = "Trade, transport & hospitality",
            I = "Trade, transport & hospitality",
            J = "Knowledge & finance", K = "Knowledge & finance",
            L = "Knowledge & finance", M = "Knowledge & finance",
            N = "Knowledge & finance",
            O = "Public, education & health",
            P = "Public, education & health",
            Q = "Public, education & health",
            R = "Other", S = "Other")
  cities <- c("Zürich", "Zug", "Lugano", "Genève", "Bern", "Biel/Bienne")
  emp <- subset(pv_city_sectors, city %in% cities)
  emp$group <- fold[emp$sector_code]
  agg <- aggregate(share ~ city + group, emp, sum)
  # Bars and segments both follow first-appearance order: cities ranked
  # by knowledge share, and the knowledge segment on the baseline.
  groups <- c("Knowledge & finance", "Public, education & health",
              "Trade, transport & hospitality", "Industry & construction",
              "Other")
  agg <- agg[order(match(agg$city, cities), match(agg$group, groups)), ]
  pv_bar(agg, x = "city", y = "share", series = "group", stack = "percent",
         xlab = NA,
         title = "What six city economies are made of",
         subtitle = "Employment shares by broad sector group",
         source = "Source: Bundesamt für Statistik – STATENT")
})

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

## line-points
# explain: Two options change what a line chart claims. show_points = TRUE
#   marks every observation with a dot in its line's colour - the
#   connected-scatter treatment. Reach for it when the data is a handful
#   of real measurements rather than a continuous stream: the dots show
#   where the knowledge actually sits, and the line owns up to being
#   interpolation between them ("auto" makes that call from the data,
#   drawing dots only when no series runs past 30 points and neighbours
#   sit at least about 12px apart). curve = "monotone" then bends the
#   interpolation into a smooth curve that still passes through every
#   point without overshooting - right whenever the quantity genuinely
#   moves smoothly between observations, as a temperature does; "step"
#   instead holds each value flat until the next, the honest shape for
#   rates that change at discrete moments. Twelve monthly averages per
#   series here: Lucerne afternoons reach 25 °C from June to August, but
#   even an August night cools to 15 °C, and January nights average just
#   below freezing.
local({
  # Twelve monthly averages apiece for the daily highs and lows.
  mon <- month.abb[as.integer(format(pv_weather$date, "%m"))]
  clim <- rbind(
    data.frame(month = mon, series = "Daily high",
               temp = pv_weather$temp_max),
    data.frame(month = mon, series = "Daily low",
               temp = pv_weather$temp_min))
  clim <- aggregate(temp ~ month + series, clim, function(v) round(mean(v), 1))
  # A category x axis keeps the rows' arrival order, so put the months
  # back into calendar order before charting.
  clim <- clim[order(match(clim$month, month.abb)), ]
  pv_line(clim, x = "month", y = "temp", series = "series",
          show_points = TRUE, curve = "monotone",
          xlab = NA, ylab = "°C",
          title = "Lucerne's climate year",
          subtitle = "Monthly averages of the daily high and low temperature, 2020–2025",
          source = "Source: MeteoSwiss")
})

## line-zoom
# explain: zoom = TRUE hangs a brush strip below a line (or stacked area)
#   chart - a muted miniature of the whole series. Reach for it when a
#   series is too long for its details to survive at full width: six
#   years of daily temperatures is 2,192 points, so a typical screen
#   spends less than a pixel per day and keeps only the seasonal
#   sawtooth. Drag across the strip and the main panel narrows to that
#   window - down to single weeks, where the day-to-day swings reappear -
#   then double-click the strip to snap back to the full range. Somewhere
#   in here: the hottest daily mean of the six years, 27.1 °C on 19 June
#   2022, and the coldest, -6.3 °C in the February 2021 cold snap. The
#   chart always opens at the full range, so a pv_save() capture always
#   shows the complete series.
pv_line(pv_weather, x = "date", y = "temp_mean", zoom = TRUE,
        xlab = NA, ylab = "°C",
        title = "Six years of Lucerne days",
        subtitle = "Daily mean temperature — drag across the strip below to zoom in",
        source = "Source: MeteoSwiss")

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
#   a city highlights its economic look-alikes. Labels wear a halo of the
#   background and drop below their node when two would collide, and a
#   gentle pull toward the centre keeps separate components in frame.
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
#   into how the settlement area subdivides. Segments too thin to carry
#   their label stay unlabelled - hovering names any of them.
pv_sunburst(
  pv_city_landuse[pv_city_landuse$city == "Luzern", ],
  levels = c("group", "category"), value = "hectares",
  title = "Lucerne's land, ring by ring",
  subtitle = "Click a segment to zoom in; click the centre to zoom out",
  source = "Source: Bundesamt für Statistik – Arealstatistik"
)
