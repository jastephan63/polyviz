# Gallery snippets for the motion charts. Each block: an id, an
# explanation for the demo page, and one runnable expression on the real
# bundled data. data-raw/build-gallery.R assembles these into docs/.

## race
# explain: The bar-chart race replays a ranking through time: bars grow
#   with their values and swap places at every overtake, risers slide in
#   from below and fallers drop out, while the big year ticks along in
#   the corner. Use it when the drama is in who passes whom - and press
#   the circled arrow, top right, to watch it again; hovering any bar
#   reads out its exact value at that moment. A century of Swiss city
#   populations shows Zürich leading wire to wire while the places
#   behind it churn: Genève takes second from Basel around 2000,
#   Lausanne slips past Bern, and La Chaux-de-Fonds - 10th in 1930, in
#   its watchmaking heyday - drifts out of the top 12 altogether.
pv_race(pv_city_population, time = "year", id = "city",
        value = "population", top_n = 12,
        title = "Swiss cities racing through a century",
        subtitle = "Permanent residents of the 12 largest cities, 1930–2024",
        source = "Source: Bundesamt für Statistik")

## bump
# explain: The bump chart plots rank against time, one line per entity
#   with a dot at every measurement - crossing lines are the overtakes,
#   and names label both ends of each line at its starting and finishing
#   rank. Use it when position matters more than magnitude; hover a line
#   to raise it, dim the rest, and read the rank and value at the
#   nearest year. Since 1930 Switzerland's big-city podium barely moved,
#   but just below it the lines cross constantly: Winterthur overtakes
#   St. Gallen by 1970, Genève passes Basel around 2000, Lausanne passes
#   Bern after 2000, and Lugano - outside the top 10 until 1980, so its
#   line starts late - climbs to 9th past Biel/Bienne.
pv_bump(pv_city_population, time = "year", id = "city",
        value = "population", top_n = 10,
        title = "Who overtook whom since 1930",
        subtitle = "Rank among Switzerland's largest cities by population",
        source = "Source: Bundesamt für Statistik")
