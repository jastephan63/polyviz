# Demo gallery example for the print-ready pairing: the paper theme and
# texture fills, set for one chart and reset. Each block below is one
# gallery entry: an explanation for the demo page, then a single runnable
# expression on the real bundled data. data-raw/build-gallery.R assembles
# these into docs/.

## bar-textured
# explain: Reach for this pairing the moment a chart is headed for a
#   printed page you don't control - a journal that prints greyscale, a
#   photocopied handout, a thesis bound in black and white.
#   pv_set_theme(pv_theme_paper()) swaps the whole look for a print-first
#   one: pure white surface, neutral near-black inks, and a five-colour
#   palette whose slots climb a luminance ladder, so the series stay
#   tellable apart even after every hue is thrown away (serif = TRUE sets
#   the type to match journals that want serif figures). pv_textures()
#   then adds the belt to those braces: each series is hatched in its own
#   diagonal and spacing, an identity channel that needs no colour at
#   all and survives both toner and colour-vision deficiency. The theme
#   is session-wide - set it, build the figures, reset - while textures
#   attach per chart. Under all that insurance, six municipalities from
#   asphalt to alp: Genève is nine-tenths built over, Zermatt not even
#   a fiftieth.
local({
  # Theme state is session-wide: build this one chart under the paper
  # theme, then put the packaged look back for the rest of the gallery.
  pv_set_theme(pv_theme_paper(serif = TRUE))
  on.exit(pv_reset_theme())
  cities <- c("Zürich", "Genève", "Luzern", "Lugano", "Davos", "Zermatt")
  lu <- aggregate(hectares ~ city + group,
                  pv_city_landuse[pv_city_landuse$city %in% cities, ], sum)
  # Bars and segments follow first-appearance order: municipalities from
  # most to least built over, settlement on the baseline.
  built <- with(lu, ave(hectares * (group == "Settlement"), city,
                        FUN = sum) / ave(hectares, city, FUN = sum))
  lu <- lu[order(-built, match(lu$group,
                               c("Settlement", "Cultivated", "Natural"))), ]
  pv_bar(lu, x = "city", y = "hectares", series = "group",
         stack = "percent", xlab = NA,
         title = "Six municipalities, from asphalt to alp",
         subtitle = "Share of municipal area by land-use group — paper theme, textured fills",
         source = "Source: Bundesamt für Statistik – Arealstatistik") |>
    pv_textures()
})
