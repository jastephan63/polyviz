# Demo gallery example for the design-system table. Each block below is
# one gallery entry: an explanation for the demo page, then a single
# runnable expression built on a bundled dataset.

## table
# explain: A table is what you ship when readers need the exact numbers -
#   an auditor, a fact-checker, anyone about to quote a figure. Charts
#   trade digits for shape; pv_table() keeps every digit and lets three
#   in-cell encodings put the shape back. bars scales a thin mark to the
#   column's maximum, so magnitude is visible without rounding anything
#   away; shade washes each cell's background along the sequential ramp,
#   turning a column into a one-column heatmap; and spark draws a
#   list-column - one numeric vector per row - as inline sparklines. The
#   result is a real HTML table in the package's chart chrome: tabular
#   numerals, right-aligned, locale-aware formatting, honest to screen
#   readers and copy-paste, and every header sorts on click. Read the
#   exact 2024 populations off the bar column, let the shading pick out
#   Winterthur as the runaway grower since 1990 - and Basel, alone up
#   here, in decline - and see each city's whole century in its
#   sparkline.
local({
  pop <- pv_city_population[order(pv_city_population$city,
                                  pv_city_population$year), ]
  cities <- merge(subset(pop, year == 2024)[c("city", "population")],
                  subset(pop, year == 1990)[c("city", "population")],
                  by = "city", suffixes = c("", "_1990"))
  tbl <- data.frame(
    City = cities$city,
    `Residents 2024` = cities$population,
    `Growth since 1990 (%)` =
      100 * (cities$population / cities$population_1990 - 1),
    check.names = FALSE)
  tbl$`Since 1930` <- I(split(pop$population, pop$city)[tbl$City])
  tbl <- head(tbl[order(-tbl$`Residents 2024`), ], 10)
  pv_table(tbl, bars = "Residents 2024", shade = "Growth since 1990 (%)",
           spark = "Since 1930", digits = c(`Growth since 1990 (%)` = 1),
           title = "The ten largest Swiss cities, to the last digit",
           subtitle = "Resident population 2024, growth since 1990, and the trajectory since 1930",
           source = "Source: Bundesamt für Statistik – Statistik der Schweizer Städte")
})
