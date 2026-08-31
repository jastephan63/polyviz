# Builds the Switzerland-wide boundary layers and the city coordinate
# table for the 0.7.0 maps.
#
# Sources (verified 2026-08-31):
#   * All four objects come from one BFS asset, the same "Generalisierte
#     Gemeindegrenzen" GG25 release the Lucerne map was cut from:
#     boundary status 1.1.2025, generalisation level G1, LV95 GeoJSON.
#     https://dam-api.bfs.admin.ch/hub/api/dam/assets/34367751/master
#     (a ZIP; the files below sit in "Historized boundaries G1 20250101/")
#     License: open use, source citation required ("© BFS, ThemaKart").
#       - Cantons_G1_20250101_2056.geojson    -> pv_swiss_cantons
#       - Districts_G1_20250101_2056.geojson  -> pv_swiss_districts
#       - Lacs_G1_20250101_2056.geojson       -> pv_swiss_lakes
#       - Communes_G1_20250101_2056.geojson   -> pv_city_coords (centroids)
#
# Run from the package root after placing the raw files in data-raw/raw/.

# ---- Boundary layers (GeoJSON, reprojected and slimmed) -------------------
# Same treatment as pv_lucerne_map: BFS ships G1 only in the Swiss LV95
# projection, so sf reprojects to the WGS84 lon/lat d3 expects, the result
# goes back out as GeoJSON, and the features are slimmed to the two
# properties charts use, with coordinates rounded to 4 decimals (~11 m).
# G1 country-wide stays small enough after that (see the size print at the
# bottom), so no further simplification is applied.
round_coords <- function(x) {
  if (is.numeric(x)) return(round(x, 4))
  if (is.list(x)) return(lapply(x, round_coords))
  x
}

# Reads one LV95 layer and returns it as the FeatureCollection list the
# charts take, keeping per feature only what `props` extracts - a
# list(id =, name =) for joinable layers, list(name =) for background ones.
slim_layer <- function(file, props) {
  x <- sf::st_transform(sf::st_read(file.path("data-raw/raw", file),
                                    quiet = TRUE), 4326)
  tmp <- tempfile(fileext = ".geojson")
  sf::st_write(x, tmp, quiet = TRUE)
  gj <- jsonlite::fromJSON(tmp, simplifyVector = FALSE)
  feats <- lapply(gj$features, function(f) {
    list(
      type = "Feature",
      properties = props(f$properties),
      geometry = list(type = f$geometry$type,
                      coordinates = round_coords(f$geometry$coordinates))
    )
  })
  list(type = "FeatureCollection", features = feats)
}

# The cantons file carries numbers and two-letter codes but no names, so
# the official short names come from here, indexed by canton number.
canton_names <- c(
  "Zürich", "Bern", "Luzern", "Uri", "Schwyz", "Obwalden", "Nidwalden",
  "Glarus", "Zug", "Fribourg", "Solothurn", "Basel-Stadt",
  "Basel-Landschaft", "Schaffhausen", "Appenzell Ausserrhoden",
  "Appenzell Innerrhoden", "St. Gallen", "Graubünden", "Aargau", "Thurgau",
  "Ticino", "Vaud", "Valais", "Neuchâtel", "Genève", "Jura")

pv_swiss_cantons <- slim_layer(
  "Cantons_G1_20250101_2056.geojson",
  function(p) list(id = p$KTNR, name = canton_names[p$KTNR]))

pv_swiss_districts <- slim_layer(
  "Districts_G1_20250101_2056.geojson",
  function(p) list(id = p$BEZNR, name = p$BEZNAME))

# Lakes are background geography: a name for the tooltip but no id, so
# they never take part in a choropleth's data join (see pv_choropleth).
pv_swiss_lakes <- slim_layer(
  "Lacs_G1_20250101_2056.geojson",
  function(p) list(name = p$SEENAME))

cat("cantons:", length(pv_swiss_cantons$features),
    "| districts:", length(pv_swiss_districts$features),
    "| lakes:", length(pv_swiss_lakes$features), "\n")

# ---- City coordinates (centroids of the municipality polygons) ------------
# One point per city of pv_city_population. Every city is a municipality
# and its name matches GDENAME in the communes file exactly, so the lookup
# is a plain match. A municipality can span several polygon rows
# (exclaves), so rows are unioned per municipality first; the centroid is
# computed in the metric LV95 plane, then reprojected and rounded like the
# boundary coordinates.
load("data/pv_city_population.rda")
cities <- sort(unique(pv_city_population$city))
communes <- sf::st_read(file.path("data-raw/raw",
                                  "Communes_G1_20250101_2056.geojson"),
                        quiet = TRUE)
stopifnot(all(cities %in% communes$GDENAME))

pv_city_coords <- do.call(rbind, lapply(cities, function(cty) {
  rows <- communes[communes$GDENAME == cty, ]
  ctr <- sf::st_transform(
    sf::st_centroid(sf::st_union(sf::st_geometry(rows))), 4326)
  xy <- round(sf::st_coordinates(ctr)[1, ], 4)
  data.frame(city = cty, id = rows$GDENR[1],
             lon = unname(xy["X"]), lat = unname(xy["Y"]))
}))
rownames(pv_city_coords) <- NULL
cat("city coords:", nrow(pv_city_coords), "rows | lon:",
    paste(range(pv_city_coords$lon), collapse = " to "), "| lat:",
    paste(range(pv_city_coords$lat), collapse = " to "), "\n")

save(pv_swiss_cantons, file = "data/pv_swiss_cantons.rda", compress = "xz")
save(pv_swiss_districts, file = "data/pv_swiss_districts.rda",
     compress = "xz")
save(pv_swiss_lakes, file = "data/pv_swiss_lakes.rda", compress = "xz")
save(pv_city_coords, file = "data/pv_city_coords.rda", compress = "xz")
for (f in c("pv_swiss_cantons", "pv_swiss_districts", "pv_swiss_lakes",
            "pv_city_coords")) {
  cat(f, "rda size:", file.size(file.path("data", paste0(f, ".rda"))),
      "bytes\n")
}
