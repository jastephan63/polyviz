# Gallery snippet for the small-multiples facet modifier. Same format as
# gallery-core.R; data-raw/build-gallery.R assembles it into docs/.

## facet
# explain: Small multiples are the honest alternative to spaghetti lines.
#   Six cities on one chart means six crossing lines fighting for the
#   same vertical space; six cities in six panels means each trajectory
#   reads cleanly on its own, and because every panel shares the same
#   axis ranges (pv_facet's default), your eye can still compare them at
#   a glance - Lucerne's post-1970 plateau against Zug's and Baar's
#   unbroken climb, and Emmen, Kriens, and Horw levelling off with their
#   big neighbour. Any bar, line, scatter, or area chart facets the same
#   way: build the chart, then pipe it through pv_facet with the original
#   grouping column. The chart keeps one title, one source line, and one
#   tooltip; each panel just carries its name.
local({
  cities <- c("Luzern", "Emmen", "Kriens", "Horw", "Zug", "Baar")
  pop <- subset(pv_city_population, city %in% cities)
  w <- pv_line(pop, x = "year", y = "population", series = "city",
               title = "Six cities, six panels",
               subtitle = "Permanent resident population at census years since 1930, shared y axis",
               source = "Source: Bundesamt für Statistik – Statistik der Schweizer Städte")
  pv_facet(w, pop$city, ncol = 3)
})
