# Gallery examples for the evolution & matrix family (pv_area, pv_heatmap,
# pv_calendar, pv_horizon).
# One block per chart; each block is a single self-contained expression that
# builds a widget from a bundled dataset.

## area
# explain: A stacked area chart piles series on top of each other, so you read
#   the total from the outer edge and each part's contribution from its band —
#   the form to reach for when the question is "how did the whole grow, and who
#   did the growing". Hover anywhere for a crosshair that reads out every
#   series (and the total) at that year, and hover a band to lift it out of the
#   stack; offset = "percent" or "stream" re-renders the same data as
#   composition-over-time or a streamgraph. The legend appears whenever there is
#   more than one series (force it either way with legend = TRUE/FALSE), and
#   xlab/ylab override the column-name axis titles — NA or "" gives their room
#   back to the plot, which helps on phone-width charts. Here the five biggest
#   municipalities
#   of the Lucerne agglomeration grew from about 72,000 people in 1930 to almost
#   180,000 in 2024 — but the city of Luzern itself peaked around 1970, so the
#   suburbs did nearly all the growing since: Emmen and Kriens quadrupled, and
#   Ebikon grew more than sixfold.
{
  cities <- c("Luzern", "Emmen", "Kriens", "Horw", "Ebikon")
  agglo <- subset(pv_city_population, city %in% cities)
  agglo <- agglo[order(match(agglo$city, cities)), ]
  pv_area(agglo, x = "year", y = "population", series = "city",
          title = "How the Lucerne agglomeration grew",
          subtitle = "Permanent residents at census years, 1930–2024",
          source = "Source: Bundesamt für Statistik")
}

## area-stream
# explain: offset = "stream" lets the stacked area chart give up its zero
#   baseline: the bands wiggle around a centre line placed to keep the whole
#   shape as calm as possible, trading an easily read total for the clearest
#   view of each band's own rhythm — the form to reach for when composition
#   and cadence matter more than the sum. Read thickness, not position, and
#   hover anywhere for the crosshair with every band's exact value (offset =
#   "percent" is the third variant, pinning the same bands to a 0–100% frame).
#   The data is pv_electricity, new to the package in this release:
#   Switzerland's monthly electricity production 2020–2025, split into the
#   six sources of the national balance, and it breathes in annual waves —
#   river and storage hydro swell every summer to 64% of the May–August
#   output, nuclear runs as the steady ribbon that carries the winter (38% of
#   December–February production), and beneath them the solar band widens
#   from 2,700 GWh in 2020 to 7,900 in 2025, nearly tripling, while wind
#   stays a hairline. A frozen snapshot ships with the package;
#   pv_fetch_opendata() delivers the live balance from opendata.swiss.
{
  pv_area(pv_electricity, x = "date", y = "gwh", series = "source",
          offset = "stream", xlab = NA,
          title = "How Switzerland makes its electricity",
          subtitle = "Monthly production by source in GWh, 2020–2025",
          source = "Source: Bundesamt für Energie")
}

## heatmap
# explain: A heatmap crosses two categories and colours each cell by a value,
#   which makes a 200-cell table readable at a glance — use it when the pattern
#   across the whole grid matters more than any single number. Darker cells mean
#   a bigger share; hover any cell for the exact value and the full sector name,
#   and the gradient bar under the title is the colour scale. Cells print their
#   value only when there is room (cell_values = TRUE forces it, shrinking the
#   font for tight cells), and row labels shorten past truncate_labels
#   characters — further still on narrow screens, where the label margin never
#   takes more than 40% of the width, so the cells stay readable on a phone.
#   Across the twelve
#   most populous Swiss cities, health and social work is the biggest employer
#   almost everywhere (24% of jobs in Lausanne), but each city keeps a
#   signature: Bern and Bellinzona light up in public administration,
#   Zürich, Genève and Lugano in finance, and Biel/Bienne is the lone
#   manufacturing stronghold at over 20%.
{
  pop24 <- subset(pv_city_population, year == 2024)
  top12 <- head(pop24[order(-pop24$population), "city"], 12)
  emp <- subset(pv_city_sectors, city %in% top12)
  emp <- emp[order(match(emp$city, top12)), ]
  pv_heatmap(emp, x = "city", y = "sector", value = "share",
             title = "Where Swiss city jobs are",
             subtitle = "Employees by economic sector, % of each city's total",
             source = "Source: Bundesamt für Statistik")
}

## calendar
# explain: The GitHub-style calendar heatmap lays every day of a year out as one
#   horizontal block — weeks as columns, weekdays as rows — and colours each day
#   by its value, which is the form for daily data with a rhythm: seasons,
#   weekday/weekend cycles, streaks and gaps all surface at a glance. Hover any
#   day for its full date and exact value, read the colour scale off the
#   gradient in the header, and note that days with no data stay faint grey
#   instead of being painted as zero; years = picks which year blocks to show
#   (at most six). Here three years of Lucerne daily maximums glow brightest in
#   high summer: 2023 was the fiercest of the three, with 18 days at or above
#   30 °C and the peak of 35.5 °C on 11 July, 2024 eased off to 13 such days,
#   and 2025 climbed back to 17 — its hottest stretch arriving only in
#   mid-August, later than either summer before it.
{
  pv_calendar(pv_weather, date = "date", value = "temp_max",
              years = 2023:2025,
              title = "Three summers, getting hotter",
              subtitle = "Daily maximum temperature in Lucerne, °C",
              source = "Source: MeteoSwiss")
}

## horizon
# explain: The horizon chart is the answer to too many series: each line is
#   folded into a compact ribbon and the ribbons stack in rows, so 26 cantons
#   over 21 years fit in the height a line chart would spend on five. The fold
#   cuts every value into bands of equal width — the header legend shows the
#   actual inks, here one band = 2.31 million nights — and layers the slices
#   over each other, so deeper ink means a higher value and the shape on top
#   still says exactly where a series peaks. The scale is shared, so a given
#   shade means the same amount in every row; order = "max" ranks the rows by
#   their peak, and hovering anywhere drops a crosshair down all the rows at
#   once and reads every canton's true value at that year from one tooltip
#   (negative values, when a series has them, fold upward in the opposite
#   colour — mirror = TRUE is the default). Read down the 2020 column for the
#   pandemic in one glance: the city cantons go pale — Geneva and Zurich each
#   lost two-thirds of their hotel nights — while Graubünden's resorts, kept
#   afloat by domestic guests, gave up just 9% and Appenzell Innerrhoden
#   actually gained. By 2025 the recovery has outrun the record books: 43.9
#   million nights nationwide, Zurich alone at a deepest-ink 6.9 million.
{
  nights <- aggregate(nights ~ canton + year, pv_tourism, sum)
  pv_horizon(nights, x = "year", y = "nights", series = "canton",
             order = "max", xlab = NA,
             title = "Twenty-one years of tourism, all 26 cantons at once",
             subtitle = "Hotel nights per canton and year, 2005–2025",
             source = "Source: Bundesamt für Statistik – HESTA")
}
