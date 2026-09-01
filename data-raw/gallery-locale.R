# Demo gallery example for locale-aware formatting: one chart built with
# the de-CH locale set. Each block below is one gallery entry: an
# explanation for the demo page, then a single runnable expression on the
# real bundled data. data-raw/build-gallery.R assembles these into docs/.

## bar-locale
# explain: Charts for a Swiss readership should write numbers the Swiss
#   way, and pv_locale() makes that a one-liner. It is session-wide: set
#   it once and every chart built afterwards formats numbers and dates by
#   the chosen convention - "de-CH", "it-CH", and "en-CH" group thousands
#   with the apostrophe of Swiss print (10'000), "fr-CH" groups with a
#   space and writes the decimal comma, and each carries its language's
#   month and weekday names for date axes and tooltips. This chart was
#   built under "de-CH": the population axis reads 100'000 through
#   400'000 - grouped in full, exactly as the Federal Chancellery writes
#   them - and every tooltip figure follows suit, while the chart itself
#   is unchanged in every other way. Ticks below 10'000 stay ungrouped,
#   so a year axis would keep reading 2024, not 2'024. With no locale
#   set, everything renders in d3's stock US English, exactly as before.
local({
  # A locale is session-wide state, like a theme: set, build, reset.
  pv_locale("de-CH")
  on.exit(pv_locale(NULL))
  staedte <- c("Zürich", "Genève", "Basel", "Lausanne", "Bern",
               "Winterthur", "Luzern", "St. Gallen")
  pop <- pv_city_population[pv_city_population$year == 2024 &
                              pv_city_population$city %in% staedte, ]
  pv_bar(pop, x = "city", y = "population", sort = TRUE,
         xlab = NA, ylab = NA,
         title = "Wo die städtische Schweiz wohnt",
         subtitle = "Ständige Wohnbevölkerung der acht grössten Städte, 2024",
         source = "Quelle: Bundesamt für Statistik – Statistik der Schweizer Städte")
})
