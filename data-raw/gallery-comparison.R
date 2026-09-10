# Gallery snippets for the comparison family: slope, dumbbell, pyramid,
# waterfall, bullet. Each block: an id, an explanation for the demo page,
# and one runnable expression on the real bundled data.
# data-raw/build-gallery.R assembles these into docs/.

## slope
# explain: The slope chart is for exactly two moments and the change
#   between them: one line per group, and the slope IS the message - what
#   rose, what fell, and how steeply. Reach for it over a line chart when
#   the in-between years would only be noise, and over a bar pair when
#   the groups are many: names and starting values sit at the left ends,
#   final values at the right, and labels nudge apart instead of
#   colliding. Rising lines keep the full text ink while falling lines
#   recede into grey, so decline reads as receding ink - and highlight
#   picks out the line the chart is about in the accent colour. Between
#   the 1970 and 2024 censuses the big Swiss cities went separate ways:
#   Basel lost a sixth of its residents, Bern nearly as large a share,
#   and St. Gallen never regained its 1970 count, while Genève added a
#   fifth and Winterthur - the highlighted boom town - grew by nearly a
#   third.
#   Hover any line for both values and the change.
pv_slope(
  pv_city_population[pv_city_population$city %in%
                       c("Zürich", "Basel", "Genève", "Bern", "Lausanne",
                         "Winterthur", "Luzern", "St. Gallen") &
                       pv_city_population$year %in% c(1970, 2024), ],
  x = "year", y = "population", group = "city", highlight = "Winterthur",
  title = "Half a century between two censuses",
  subtitle = "Permanent resident population of the largest Swiss cities, 1970 vs 2024",
  source = "Source: Bundesamt für Statistik – Statistik der Schweizer Städte"
)

## dumbbell
# explain: The dumbbell also compares two values per group, but lays the
#   groups down the left like a horizontal bar chart - reach for it over
#   the slope when the list is long or the names are, because every name
#   stays upright and readable. The first value is a quiet grey dot, the
#   second wears the accent colour, a two-entry legend names them, and
#   the gap is written right on each connector where it has the room.
#   sort = "gap" (the default) ranks the rows by change, so the chart
#   opens with its biggest movers. Lucerne publishes each municipality's
#   resource index - its tax strength, 100 = the cantonal average -
#   years ahead; between the first published year and the latest, the
#   twelve biggest movers split cleanly: the city and its suburbs
#   Kriens and Horw pulled away (Kriens by almost 26 points),
#   while the other nine all fell, losing between 13 and 20 index
#   points. Hover a row for both values and the change.
local({
  f20 <- pv_fiscal[pv_fiscal$year == 2020,
                   c("municipality", "resource_index")]
  f27 <- pv_fiscal[pv_fiscal$year == 2027,
                   c("municipality", "resource_index")]
  both <- merge(f20, f27, by = "municipality",
                suffixes = c("_2020", "_2027"))
  movers <- head(both[order(-abs(both$resource_index_2027 -
                                   both$resource_index_2020)), ], 12)
  pv_dumbbell(movers, y = "municipality", x1 = "resource_index_2020",
              x2 = "resource_index_2027", labels = c("2020", "2027"),
              title = "Whose tax base moved most",
              subtitle = "Resource index of Lucerne municipalities, first vs latest published year (100 = cantonal average)",
              source = "Source: LUSTAT Statistik Luzern")
})

## pyramid
# explain: The pyramid is the mirrored form for opposing flows: one row
#   per category, two non-negative values drawn as bars growing left and
#   right from a shared centre spine - age bands split male/female,
#   commuters in against commuters out, imports against exports. Both
#   sides share one symmetric scale sized to the larger side, and the
#   tick labels read as absolute values on both, so a leftward bar of
#   12,000 says 12,000, never -12,000. Reach for it over grouped bars
#   whenever the two columns are two directions of one thing - the
#   mirroring makes every imbalance a visible asymmetry. Canton Zug is a
#   jobs magnet, and the pyramid says so at a glance: every one of the
#   five regions leans left. Zürich runs the biggest exchange, 14,910
#   commuters in against 10,189 out, but Lucerne's is the most lopsided
#   of the big ones - 12,376 in against 5,136 heading back - and Aargau
#   sends more than four commuters for every one it receives. Hover a
#   row for both exact counts.
local({
  latest <- pv_commuters[pv_commuters$period ==
                           max(pv_commuters$period), ]
  inbound <- latest[latest$direction == "to Zug",
                    c("region", "commuters")]
  outbound <- latest[latest$direction == "from Zug",
                     c("region", "commuters")]
  both <- merge(inbound, outbound, by = "region",
                suffixes = c("_in", "_out"))
  pv_pyramid(both, y = "region", left = "commuters_in",
             right = "commuters_out", labels = c("to Zug", "from Zug"),
             sort = "total",
             title = "Commuters in and out of Canton Zug",
             subtitle = "Both directions of the commuter exchange, 2022–2024 average",
             source = "Source: Bundesamt für Statistik – Pendlermobilität")
})

## waterfall
# explain: The waterfall answers one question: how did the total get from
#   here to there? Each signed contribution floats where the running
#   total left off - gains in blue, losses in red, thin connectors
#   carrying the level across the gaps - and distinct grey bars anchor
#   the start and the end, so the deltas never hang from an invisible
#   ledge. Reach for it whenever a change decomposes into named parts
#   that sum exactly; the running total after every bar is in the
#   tooltip. Swiss electricity production fell about 2,800 GWh between
#   2020 and 2025, and the decomposition says why: solar's build-out
#   added 5,256 GWh - by far the largest single contribution, and
#   nearly enough on its own to balance the books - but nuclear
#   delivered 4,611 GWh less than in 2020, and both hydro families
#   came in lower too.
local({
  e20 <- aggregate(gwh ~ source,
                   pv_electricity[pv_electricity$year == 2020, ], sum)
  e25 <- aggregate(gwh ~ source,
                   pv_electricity[pv_electricity$year == 2025, ], sum)
  chg <- merge(e20, e25, by = "source", suffixes = c("_2020", "_2025"))
  chg$change <- chg$gwh_2025 - chg$gwh_2020
  # Largest gain first, so the eye walks downhill from solar's rise to
  # nuclear's fall.
  chg <- chg[order(-chg$change), ]
  pv_waterfall(chg, x = "source", y = "change",
               start = sum(chg$gwh_2020), total = "2025",
               xlab = NA, ylab = "GWh",
               title = "What changed Swiss electricity production",
               subtitle = "Annual production by source: from the 2020 total to the 2025 total",
               source = "Source: Bundesamt für Energie")
})

## bullet
# explain: Stephen Few's bullet graph packs a measure, its target, and
#   qualitative context into one compact row: a thin accent bar for the
#   value, a near-black tick for the target, muted grey bands for the
#   ground it crossed - darkest lowest, so the context never competes
#   with the value. Rows share one scale by default, so the eye can run
#   straight down the column. Reach for it when several measures each
#   have a mark to hit; the honest work is picking that mark. No canton
#   publishes hotel-night targets, so this chart synthesises them from
#   the data itself and says so: each canton's target is its own 2019
#   nights - the last pre-pandemic year - with bands at 50% and 80% of
#   it. By 2024 every one of the eight biggest tourism cantons except
#   Vaud had passed its pre-pandemic mark; Geneva cleared its target by
#   18%, and Zurich and Bern by about 13% each. Hover a row for the
#   exact value and target.
local({
  n24 <- aggregate(nights ~ canton,
                   pv_tourism[pv_tourism$year == 2024, ], sum)
  n19 <- aggregate(nights ~ canton,
                   pv_tourism[pv_tourism$year == 2019, ], sum)
  rec <- merge(n24, n19, by = "canton", suffixes = c("", "_2019"))
  rec <- head(rec[order(-rec$nights), ], 8)
  # The synthesised context: bands at half and 80% of each canton's own
  # pre-pandemic level.
  rec$half <- 0.5 * rec$nights_2019
  rec$most <- 0.8 * rec$nights_2019
  pv_bullet(rec, label = "canton", value = "nights",
            target = "nights_2019", bands = c("half", "most"),
            title = "Tourism against its pre-pandemic mark",
            subtitle = "Hotel nights 2024; each canton's target is its own 2019 nights",
            source = "Source: Bundesamt für Statistik – HESTA")
})
