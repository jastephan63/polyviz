# Gallery snippets for the composition family: donut, waffle, treemap,
# lollipop. Each block: an id, an explanation for the demo page, and one
# runnable expression on the real bundled data. data-raw/build-gallery.R
# assembles these into docs/.

## donut
# explain: The donut shows how one whole divides into parts - each slice's
#   angle is its share, the hole in the middle carries the grand total,
#   and slices sweep in largest-first. Big slices are labelled directly
#   with their share; slivers move to a legend, and hovering any slice
#   pops it outward with its exact value and percentage. On charts
#   narrower than about 480px the outside labels would collide, so all
#   slices move to the legend automatically (the labels flag can force
#   either look). Here it splits
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

## waffle
# explain: The waffle shows the same parts-of-a-whole as the donut, but
#   as countable unit squares: a 10-by-10 grid where every square is one
#   percent, categories filling it column by column from the bottom
#   left. Reach for it when the shares should be read as numbers rather
#   than compared as angles - the eye counts squares far better than it
#   judges arcs - and when the chart must survive printing: piped
#   through pv_textures(), each party's squares also wear their own
#   hatch. The rounding is honest by largest remainder - no category is
#   ever more than one square from its exact share, and the legend and
#   tooltip always carry the exact seats and percentage. The same 385
#   Lucerne council seats as the donut above, and the waffle makes the
#   arithmetic visible: Mitte's 46 squares are nearly half the grid,
#   Mitte and FDP together hold 72 of the 100, and the third-largest
#   block is no party at all - 48 independents, an eighth of all seats,
#   well clear of the SVP's 38. Hovering any square lights up its whole
#   party.
local({
  seats <- aggregate(elected ~ party,
                     pv_elections[pv_elections$year == 2024, ], sum)
  # Largest first, so the grid fills in rank order and the palette's
  # strongest colours go to the biggest parties.
  seats <- seats[order(-seats$elected), ]
  pv_waffle(seats, category = "party", value = "elected",
            title = "The council seats, one square per percent",
            subtitle = "385 municipal council seats won in Lucerne's 2024 elections",
            source = "Source: LUSTAT Statistik Luzern")
})

## treemap
# explain: The treemap packs a hierarchy into nested rectangles - each
#   cell's area is its value, its colour names its top-level branch, and
#   labels appear only where they honestly fit (the labels flag can make
#   them eager, or turn them off entirely). Hovering a cell keeps its
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
#   head, and hovering a row highlights it with the exact figure. The
#   name column never takes more than 45% of the width - on narrow
#   charts long names are shortened with an ellipsis (hover for the
#   full name) and the head values step aside when the plot drops under
#   200px (the value_labels flag can force either look). Ranked
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
