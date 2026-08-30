# Builds the map and weather datasets for the 0.3.0 charts.
#
# Sources (verified 2026-08-30):
#   * Municipal boundaries: BFS "Generalisierte Gemeindegrenzen" GG25,
#     K4 generalisation, WGS84 GeoJSON, boundary status 1.1.2025.
#     https://dam-api.bfs.admin.ch/hub/api/dam/assets/34367751/master
#     License: open use, source citation required ("© BFS, ThemaKart").
#   * Daily weather: MeteoSwiss open data, station LUZ (Luzern),
#     daily aggregates since 1880.
#     https://data.geo.admin.ch/ch.meteoschweiz.ogd-smn/luz/ogd-smn_luz_d_historical.csv
#     License: open use, source citation required ("Source: MeteoSwiss").
#
# Run from the package root after placing the raw files in data-raw/raw/.

# ---- Lucerne municipal boundaries (GeoJSON, trimmed and slimmed) ----------
# The K4 generalisation (14 points per municipality) is too coarse at
# canton zoom, so we take the detailed G1 boundaries, which BFS ships only
# in the Swiss LV95 projection, and reproject to the WGS84 lon/lat that d3
# expects. sf handles the reading and reprojection; the result is written
# back out as GeoJSON and then slimmed: properties down to the two fields
# charts join on, coordinates rounded to 4 decimals (~11 m).
g1 <- sf::st_read(file.path("data-raw/raw",
                            "Communes_G1_20250101_2056.geojson"),
                  quiet = TRUE)
lu_sf <- sf::st_transform(g1[g1$KTKZ == "LU", ], 4326)
tmp_geo <- tempfile(fileext = ".geojson")
sf::st_write(lu_sf, tmp_geo, quiet = TRUE)
gj <- jsonlite::fromJSON(tmp_geo, simplifyVector = FALSE)

round_coords <- function(x) {
  if (is.numeric(x)) return(round(x, 4))
  if (is.list(x)) return(lapply(x, round_coords))
  x
}
lu <- lapply(gj$features, function(f) {
  list(
    type = "Feature",
    properties = list(id = f$properties$GDENR,
                      name = f$properties$GDENAME),
    geometry = list(type = f$geometry$type,
                    coordinates = round_coords(f$geometry$coordinates))
  )
})
pv_lucerne_map <- list(type = "FeatureCollection", features = lu)
cat("Lucerne features:", length(lu), "\n")

# ---- Daily weather for Luzern (recent years) ------------------------------
w <- utils::read.csv(file.path("data-raw/raw", "ogd-smn_luz_d_historical.csv"),
                     sep = ";")
w$date <- as.Date(w$reference_timestamp, format = "%d.%m.%Y")
keep <- w$date >= as.Date("2020-01-01")
pv_weather <- data.frame(
  date = w$date[keep],
  temp_mean = as.numeric(w$tre200d0[keep]),
  temp_max = as.numeric(w$tre200dx[keep]),
  temp_min = as.numeric(w$tre200dn[keep]),
  precip_mm = as.numeric(w$rre150d0[keep]),
  sunshine_min = as.numeric(w$sre000d0[keep])
)
pv_weather <- pv_weather[order(pv_weather$date), ]
rownames(pv_weather) <- NULL
cat("weather rows:", nrow(pv_weather), "| range:",
    format(min(pv_weather$date)), "to", format(max(pv_weather$date)), "\n")

save(pv_lucerne_map, file = "data/pv_lucerne_map.rda", compress = "xz")
save(pv_weather, file = "data/pv_weather.rda", compress = "bzip2")
cat("saved; map rda size:", file.size("data/pv_lucerne_map.rda"), "bytes\n")
