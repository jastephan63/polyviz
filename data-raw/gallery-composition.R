# Gallery snippets for the composition family: donut, treemap, lollipop.
# Each block: an id, an explanation for the demo page, and one runnable
# expression on the real bundled data. data-raw/build-gallery.R assembles
# these into docs/.

## donut
# explain: The donut shows how one whole divides into parts - each slice's
#   angle is its share, the hole in the middle carries the grand total,
#   and slices sweep in largest-first. Big slices are labelled directly
#   with their share; slivers move to a legend, and hovering any slice
#   pops it outward with its exact value and percentage. Here it splits
#   the 385 municipal council seats filled in Lucerne's 2024 elections:
#   Mitte took nearly half of them, the FDP about a quarter, and no other
#   party reached ten percent.
{
  # pv_donut caps a chart at 8 slices - more than that is unreadable - so
  # everything after the 7 largest parties is folded into an "Other" bin
  # (pv_donut sums slices that share a label).
  seats <- aggregate(elected ~ party,
                     pv_elections[pv_elections$year == 2024, ], sum)
  seats <- seats[order(-seats$elected), ]
  seats$party[-(1:7)] <- "Other"
  pv_donut(
    seats, category = "party", value = "elected",
    title = "Who holds Lucerne's municipal council seats",
    subtitle = "Candidates elected in the 2024 municipal elections, by party",
    source = "Source: LUSTAT Statistik Luzern"
  )
}

## treemap
# explain: The treemap packs a hierarchy into nested rectangles - each
#   cell's area is its value, its colour names its top-level branch, and
#   labels appear only where they honestly fit. Hovering a cell keeps its
#   branch lit, dims the rest, and reads out the full path and share of
#   the total. This one carves up the city of Lucerne's roughly 2,900
#   hectares: settlement and cultivated land split it almost exactly in
#   half (buildings and agriculture are the two biggest single
#   categories), while natural land - water and unproductive ground - is
#   a thin sliver of under three percent.
pv_treemap(
  pv_city_landuse[pv_city_landuse$city == "Luzern", ],
  levels = c("group", "category"), value = "hectares",
  title = "How the city of Lucerne uses its land",
  subtitle = "Hectares by land-use group and category, 2013–2025 survey",
  source = "Source: Bundesamt für Statistik – Arealstatistik"
)

## lollipop
# explain: The lollipop is a ranking chart - the bar chart's lighter
#   cousin, marking each value with a hairline stem and a dot so dozens
#   of categories stay readable without heavy ink. Categories run down
#   the left, stems grow out from zero on load, the value sits at each
#   head, and hovering a row highlights it with the exact figure. Ranked
#   here: Lucerne's 2025 fiscal equalization, where Emmen receives about
#   23 million francs - more than three times second-placed Kriens - and
#   the amounts flatten out quickly further down the field.
{
  f25 <- pv_fiscal[pv_fiscal$year == 2025, ]
  top <- head(f25[order(-f25$equalization_chf), ], 25)
  pv_lollipop(
    top, x = "municipality", y = "equalization_chf",
    title = "Where Lucerne's fiscal equalization flows",
    subtitle = "The 25 municipalities receiving the most, 2025 (CHF)",
    source = "Source: LUSTAT Statistik Luzern",
    height = 560
  )
}
