# Gallery examples for the evolution & matrix family (pv_area, pv_heatmap).
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
