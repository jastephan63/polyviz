expect_pvchart <- function(w, type) {
  expect_s3_class(w, "htmlwidget")
  expect_equal(attr(w, "package"), "polyviz")
  expect_equal(w$x$type, type)
  invisible(w)
}

fiscal25 <- function() {
  f <- pv_fiscal[pv_fiscal$year == 2025, ]
  rownames(f) <- NULL
  f
}

# One data row per feature of a bundled layer, joining on its ids.
layer_data <- function(layer) {
  data.frame(nr = vapply(layer$features,
                         function(f) f$properties$id, numeric(1)),
             v = seq_along(layer$features))
}

# A bundled FeatureCollection as an sf object, via the GeoJSON writer -
# the reverse of the trip pv_map_from_sf() takes.
sf_from_layer <- function(gj, env = parent.frame()) {
  tmp <- withr::local_tempfile(fileext = ".geojson", .local_envir = env)
  jsonlite::write_json(gj, tmp, auto_unbox = TRUE, digits = NA)
  sf::st_read(tmp, quiet = TRUE)
}

# Cache-touching map tests get a throwaway cache directory, exactly as
# in test-fetch.R; tools::R_user_dir honours R_USER_CACHE_DIR.
local_map_cache <- function(env = parent.frame()) {
  withr::local_envvar(
    c(R_USER_CACHE_DIR = withr::local_tempdir(.local_envir = env)),
    .local_envir = env)
}

geo_skip_if_offline <- function() {
  skip_on_cran()
  online <- if (requireNamespace("curl", quietly = TRUE)) {
    curl::has_internet()
  } else {
    withr::local_options(timeout = 10)
    tryCatch({
      con <- url("https://www.bfs.admin.ch", open = "rb")
      close(con)
      TRUE
    }, error = function(e) FALSE, warning = function(w) FALSE)
  }
  skip_if(!isTRUE(online), "no internet connection")
}

cities24 <- function() {
  merge(pv_city_coords,
        pv_city_population[pv_city_population$year == 2024, ],
        by = "city")
}

test_that("choropleth passes the map through untouched and joins on id", {
  f <- fiscal25()
  w <- expect_pvchart(
    pv_choropleth(f, id = "municipality_id", value = "resource_index"),
    "choropleth")
  # The map travels in the payload exactly as it came in.
  expect_identical(w$x$map, pv_lucerne_map)
  # Ids go across as strings (the JS side compares them as strings too).
  expect_equal(w$x$data$id, as.character(f$municipality_id))
  expect_equal(w$x$data$value, f$resource_index)
  expect_equal(w$x$vlab, "resource_index")
  # Sequential is the default, its domain the plain data range.
  expect_equal(w$x$palette, "sequential")
  expect_equal(w$x$domain, range(f$resource_index))
  expect_null(w$x$center)
})

test_that("choropleth diverging needs a center and sends a symmetric domain", {
  f <- fiscal25()
  expect_error(
    pv_choropleth(f, id = "municipality_id", value = "resource_index",
                  palette = "diverging"),
    "center")
  expect_error(
    pv_choropleth(f, id = "municipality_id", value = "resource_index",
                  palette = "diverging", center = "hundred"),
    "center")
  w <- pv_choropleth(f, id = "municipality_id", value = "resource_index",
                     palette = "diverging", center = 100)
  expect_equal(w$x$center, 100)
  m <- max(abs(range(f$resource_index) - 100))
  expect_equal(w$x$domain, 100 + c(-m, m))
  # And a center without the diverging palette is refused, not ignored.
  expect_error(
    pv_choropleth(f, id = "municipality_id", value = "resource_index",
                  center = 100),
    "diverging")
})

test_that("choropleth validates its columns and the map object", {
  f <- fiscal25()
  expect_error(pv_choropleth(f, id = "nope", value = "resource_index"),
               "not in `data`")
  expect_error(pv_choropleth(f, id = "municipality_id", value = "nope"),
               "not in `data`")
  expect_error(
    pv_choropleth(f, map = list(type = "Polygon"),
                  id = "municipality_id", value = "resource_index"),
    "FeatureCollection")
  # A collection whose features have no properties$id cannot be joined.
  bare <- list(type = "FeatureCollection",
               features = list(list(type = "Feature",
                                    properties = list(name = "X"),
                                    geometry = NULL)))
  expect_error(
    pv_choropleth(f, map = bare, id = "municipality_id",
                  value = "resource_index"),
    "properties")
})

test_that("choropleth warns on partly-missing ids and aborts on none", {
  f <- fiscal25()
  # Seven ids off the map: the warning names the first five, counts the rest.
  f$municipality_id[1:7] <- 90001:90007
  expect_warning(
    w <- pv_choropleth(f, id = "municipality_id",
                       value = "resource_index"),
    "7 id\\(s\\).*90001, 90002, 90003, 90004, 90005 and 2 more")
  # The unmatched rows still travel; the map simply has no feature for them.
  expect_equal(nrow(w$x$data), nrow(f))
  # No overlap at all is an error, not an empty grey map.
  g <- fiscal25()
  g$municipality_id <- seq_len(nrow(g)) + 90000
  expect_error(
    pv_choropleth(g, id = "municipality_id", value = "resource_index"),
    "No value in `municipality_id` matches")
})

test_that("choropleth drops missing values with a warning and refuses duplicate ids", {
  f <- fiscal25()
  f$resource_index[3] <- NA
  expect_warning(
    w <- pv_choropleth(f, id = "municipality_id", value = "resource_index"),
    "Dropped 1 row\\(s\\) with missing `resource_index` values")
  expect_equal(nrow(w$x$data), nrow(f) - 1)
  expect_false(as.character(f$municipality_id[3]) %in% w$x$data$id)
  # The NA row's domain influence goes with it.
  expect_equal(w$x$domain, range(f$resource_index, na.rm = TRUE))

  dup <- rbind(fiscal25(), fiscal25()[1, ])
  expect_error(
    pv_choropleth(dup, id = "municipality_id", value = "resource_index"),
    "aggregate")

  allna <- fiscal25()
  allna$resource_index <- NA
  expect_error(
    pv_choropleth(allna, id = "municipality_id", value = "resource_index"),
    "non-missing")
})

test_that("choropleth widens an all-equal colour domain", {
  f <- fiscal25()
  f$resource_index <- 100
  w <- pv_choropleth(f, id = "municipality_id", value = "resource_index")
  expect_equal(w$x$domain, c(99, 101))
  wd <- pv_choropleth(f, id = "municipality_id", value = "resource_index",
                      palette = "diverging", center = 100)
  expect_equal(wd$x$domain, c(99, 101))
})

test_that("choropleth resolves bundled layer names and joins on their ids", {
  cant <- layer_data(pv_swiss_cantons)
  w <- pv_choropleth(cant, map = "cantons", id = "nr", value = "v")
  # The bundled layer travels untouched, and every canton id joins.
  expect_identical(w$x$map, pv_swiss_cantons)
  expect_equal(w$x$data$id, as.character(cant$nr))

  dist <- layer_data(pv_swiss_districts)
  wd <- pv_choropleth(dist, map = "districts", id = "nr", value = "v")
  expect_identical(wd$x$map, pv_swiss_districts)
  expect_equal(nrow(wd$x$data), 143)

  # The named default is the same layer the historical default object is.
  f <- fiscal25()
  wl <- pv_choropleth(f, map = "lucerne", id = "municipality_id",
                      value = "resource_index")
  expect_identical(wl$x$map, pv_lucerne_map)

  expect_error(
    pv_choropleth(cant, map = "krantons", id = "nr", value = "v"),
    "Unknown map layer")
  expect_error(
    pv_choropleth(cant, map = c("cantons", "districts"), id = "nr",
                  value = "v"),
    "Unknown map layer")
})

test_that("the lakes layer follows TRUE/FALSE/auto and the map's extent", {
  f <- fiscal25()
  cant <- layer_data(pv_swiss_cantons)

  # "auto": on for the country-wide layers, off for the regional default.
  w <- pv_choropleth(cant, map = "cantons", id = "nr", value = "v")
  expect_identical(w$x$lakes, pv_swiss_lakes)
  wl <- pv_choropleth(f, id = "municipality_id", value = "resource_index")
  expect_null(wl$x$lakes)

  # "auto" on an anonymous list map goes by the longitude span.
  ww <- pv_choropleth(cant, map = pv_swiss_cantons, id = "nr", value = "v")
  expect_identical(ww$x$lakes, pv_swiss_lakes)
  wn <- pv_choropleth(f, map = pv_lucerne_map, id = "municipality_id",
                      value = "resource_index")
  expect_null(wn$x$lakes)

  # Explicit words win in both directions.
  wt <- pv_choropleth(f, id = "municipality_id", value = "resource_index",
                      lakes = TRUE)
  expect_identical(wt$x$lakes, pv_swiss_lakes)
  wf <- pv_choropleth(cant, map = "cantons", id = "nr", value = "v",
                      lakes = FALSE)
  expect_null(wf$x$lakes)

  expect_error(
    pv_choropleth(f, id = "municipality_id", value = "resource_index",
                  lakes = "yes"),
    '`lakes` must be TRUE, FALSE, or "auto"')
})

test_that("join_id is refused on maps that are not sf objects", {
  f <- fiscal25()
  expect_error(
    pv_choropleth(f, id = "municipality_id", value = "resource_index",
                  join_id = "municipality_id"),
    "only applies to an sf")
  # And a plain data frame is not a map at all.
  expect_error(
    pv_choropleth(f, map = data.frame(id = 1, name = "X"),
                  id = "municipality_id", value = "resource_index"),
    "geometry column")
})

test_that("an sf map is reprojected, converted, and joined through join_id", {
  skip_if_not_installed("sf")
  x <- sf_from_layer(pv_swiss_cantons)
  d <- layer_data(pv_swiss_cantons)

  # A column named "id" is the default join key...
  w <- pv_choropleth(d, map = x, id = "nr", value = "v")
  ids <- vapply(w$x$map$features,
                function(f) as.character(f$properties$id), character(1))
  expect_setequal(ids, as.character(d$nr))
  # ...and the feature names survive the conversion for the tooltips.
  nms <- vapply(w$x$map$features, function(f) {
    if (is.null(f$properties$name)) NA_character_ else f$properties$name
  }, character(1))
  expect_true("Luzern" %in% nms)

  # join_id names a differently-labelled id column.
  names(x)[names(x) == "id"] <- "knr"
  w2 <- pv_choropleth(d, map = x, id = "nr", value = "v", join_id = "knr")
  expect_equal(length(w2$x$map$features), 26)
  expect_error(
    pv_choropleth(d, map = x, id = "nr", value = "v", join_id = "nope"),
    "not in the sf")
  expect_error(
    pv_choropleth(d, map = x, id = "nr", value = "v", join_id = c("a", "b")),
    "single column name")

  # Without any id column the map cannot be joined, and the error points
  # at join_id.
  bare <- x[, "name"]
  expect_error(
    pv_choropleth(d, map = bare, id = "nr", value = "v"),
    "join_id")

  # A projected CRS round-trips through st_transform: LV95 in, WGS84 out.
  lv95 <- sf::st_transform(x, 2056)
  w3 <- pv_choropleth(d, map = lv95, id = "nr", value = "v",
                      join_id = "knr")
  bb <- polyviz:::pv_map_bbox(w3$x$map)
  expect_true(bb[1] > 5.5 && bb[2] < 11 && bb[3] > 45 && bb[4] < 48.5)

  # A geometry-carrying plain data frame works like the sf it came from.
  df <- as.data.frame(x)
  w4 <- pv_choropleth(d, map = df, id = "nr", value = "v", join_id = "knr")
  expect_equal(length(w4$x$map$features), 26)
})

test_that("an sf map without a CRS or without polygons is refused", {
  skip_if_not_installed("sf")
  x <- sf_from_layer(pv_swiss_cantons)
  nocrs <- suppressWarnings(sf::st_set_crs(x, NA))
  expect_error(
    pv_choropleth(layer_data(pv_swiss_cantons), map = nocrs, id = "nr",
                  value = "v"),
    "coordinate reference system")

  pts <- sf::st_as_sf(pv_city_coords, coords = c("lon", "lat"), crs = 4326)
  expect_error(
    pv_choropleth(layer_data(pv_swiss_cantons), map = pts, id = "nr",
                  value = "v"),
    "must hold polygons")
})

test_that("pv_fetch_map validates its arguments", {
  expect_error(pv_fetch_map("cantons"), "bundled")
  expect_error(pv_fetch_map(1), "`layer` must be")
  expect_error(pv_fetch_map("municipalities", refresh = "yes"),
               "TRUE or FALSE")
})

test_that("pv_fetch_map serves a processed layer from the cache", {
  local_map_cache()
  # Seed the cache with a tiny already-processed layer, the way a real
  # first fetch leaves one behind - everything after that must work
  # offline and without sf.
  gj <- list(type = "FeatureCollection", features = list(
    list(type = "Feature", properties = list(id = 1L, name = "A"),
         geometry = list(type = "Polygon", coordinates = list(list(
           list(8, 47), list(8.1, 47), list(8.1, 47.1), list(8, 47))))),
    list(type = "Feature", properties = list(id = 2L, name = "B"),
         geometry = list(type = "Polygon", coordinates = list(list(
           list(8.1, 47), list(8.2, 47), list(8.2, 47.1), list(8.1, 47)))))))
  polyviz:::pv_cache_store(
    polyviz:::pv_map_cache_url("municipalities"),
    jsonlite::toJSON(gj, auto_unbox = TRUE, digits = NA))

  # The delivery states the source and the ThemaKart licence terms.
  expect_message(muni <- pv_fetch_map("municipalities"), "ThemaKart")
  expect_equal(length(muni$features), 2)
  expect_match(attr(muni, "pv_source"), "Generalisierte Gemeindegrenzen")
  expect_match(attr(muni, "pv_licence"), "ThemaKart")

  # And the layer name resolves end-to-end inside a chart.
  d <- data.frame(nr = c(1, 2), v = c(10, 20))
  w <- suppressMessages(
    pv_choropleth(d, map = "municipalities", id = "nr", value = "v"))
  expect_equal(length(w$x$map$features), 2)
  # The named country-wide layer gets the lakes regardless of its extent.
  expect_identical(w$x$lakes, pv_swiss_lakes)
})

test_that("pv_fetch_map downloads and processes the live GG25 asset", {
  geo_skip_if_offline()
  skip_if_not_installed("sf")
  local_map_cache()

  expect_message(muni <- pv_fetch_map("municipalities"), "ThemaKart")
  expect_gt(length(muni$features), 2000)
  p <- muni$features[[1]]$properties
  expect_true(is.numeric(p$id) || is.character(p$id))
  expect_true(nzchar(p$name))

  # The second call is served from the cache: same object, nothing
  # downloaded again.
  entries <- pv_cache_status()
  before <- file.mtime(entries$file)
  expect_message(muni2 <- pv_fetch_map("municipalities"), "ThemaKart")
  expect_equal(length(muni2$features), length(muni$features))
  expect_equal(file.mtime(entries$file), before)
})

# ---- bubble map ------------------------------------------------------------

test_that("bubble map builds its payload from coords, size, and options", {
  cities <- cities24()
  w <- expect_pvchart(
    pv_bubble_map(cities, lon = "lon", lat = "lat", size = "population",
                  label = "city"),
    "bubblemap")
  # "cantons" is the default base, with the lakes over it.
  expect_identical(w$x$map, pv_swiss_cantons)
  expect_identical(w$x$lakes, pv_swiss_lakes)
  expect_equal(w$x$data$lon, cities$lon)
  expect_equal(w$x$data$lat, cities$lat)
  expect_equal(w$x$data$size, cities$population)
  expect_equal(w$x$data$label, cities$city)
  expect_null(w$x$data$series)
  expect_equal(w$x$sizelab, "population")
  expect_false(w$x$showLegend)
  expect_equal(w$x$legend, "auto")

  # A colour mapping adds the series column and flips the legend on.
  cities$region <- rep_len(c("west", "centre", "east"), nrow(cities))
  w2 <- pv_bubble_map(cities, lon = "lon", lat = "lat",
                      size = "population", color = "region")
  expect_equal(w2$x$data$series, cities$region)
  expect_true(w2$x$showLegend)

  # More than three colour levels cannot stay distinguishable.
  cities$many <- rep_len(letters[1:4], nrow(cities))
  expect_error(
    pv_bubble_map(cities, lon = "lon", lat = "lat", size = "population",
                  color = "many"),
    "more than 3 levels")
})

test_that("bubble map validates its columns and flags", {
  cities <- cities24()
  expect_error(
    pv_bubble_map(cities, lon = "nope", lat = "lat", size = "population"),
    "not in `data`")
  expect_error(
    pv_bubble_map(cities, lon = "lon", lat = "lat", size = "city"),
    "not numeric")
  expect_error(
    pv_bubble_map(cities[0, ], lon = "lon", lat = "lat",
                  size = "population"),
    "no rows")
  expect_error(
    pv_bubble_map(cities, lon = "lon", lat = "lat", size = "population",
                  legend = "maybe"),
    '`legend` must be TRUE, FALSE, or "auto"')
})

test_that("bubble map refuses impossible coordinates and sizes", {
  df <- data.frame(city = c("Luzern", "Bern", "Lugano"),
                   lon = c(8.31, 7.44, 8.95),
                   lat = c(47.05, 46.95, 46.01),
                   n = c(82000, 134000, 63000))
  # Swapped columns put a longitude-sized number into latitude - say so.
  far <- data.frame(lon = c(103.8, 114.2), lat = c(1.35, 22.3), n = c(1, 2))
  expect_error(
    pv_bubble_map(far, lon = "lat", lat = "lon", size = "n"),
    "swapped")
  # Swapped Swiss coordinates stay inside the possible ranges but land in
  # the Indian Ocean - the off-map error catches those.
  expect_error(
    pv_bubble_map(df, lon = "lat", lat = "lon", size = "n"),
    "No point falls on the map")

  neg <- df
  neg$n[2] <- -5
  expect_error(pv_bubble_map(neg, lon = "lon", lat = "lat", size = "n"),
               "negative")
  zero <- df
  zero$n <- 0
  expect_error(pv_bubble_map(zero, lon = "lon", lat = "lat", size = "n"),
               "no positive values")
})

test_that("bubble map drops missing rows and off-map points with warnings", {
  df <- data.frame(city = c("Luzern", "Bern", "Paris"),
                   lon = c(8.31, 7.44, 2.35),
                   lat = c(47.05, 46.95, 48.86),
                   n = c(82000, 134000, 2100000))
  # Paris sits outside the Swiss base map; the warning names it.
  expect_warning(
    w <- pv_bubble_map(df, lon = "lon", lat = "lat", size = "n",
                       label = "city"),
    "outside the map.*Paris")
  expect_equal(w$x$data$label, c("Luzern", "Bern"))

  nas <- df[1:2, ]
  nas$lat[2] <- NA
  expect_warning(
    w2 <- pv_bubble_map(nas, lon = "lon", lat = "lat", size = "n"),
    "missing `lat`")
  expect_equal(nrow(w2$x$data), 1)

  # Nothing on the map at all is an error, not an empty sea.
  off <- data.frame(lon = c(2.35, -0.13), lat = c(48.86, 51.5), n = c(1, 2))
  expect_error(
    pv_bubble_map(off, lon = "lon", lat = "lat", size = "n"),
    "No point falls on the map")
})

# ---- headless render checks ------------------------------------------------
# The canonical Lucerne choropleth is covered in test-render.R; the new
# drawing paths - lakes over a country-wide layer, and the bubble map
# renderer - get the same real-Chrome treatment here.

test_that("a cantons choropleth with lakes renders without JavaScript errors", {
  render_skip_if_no_chrome()
  w <- pv_choropleth(layer_data(pv_swiss_cantons), map = "cantons",
                     id = "nr", value = "v",
                     title = "Cantons under the lakes")
  path <- file.path(render_out_dir(), "choropleth_cantons.png")
  expect_no_warning(pv_save(w, path, quiet = TRUE))
  expect_true(file.exists(path))
  expect_gt(file.size(path), 20000)
  render_publish(path)
})

test_that("the bubble map renders without JavaScript errors", {
  render_skip_if_no_chrome()
  cities <- cities24()
  cities$region <- rep_len(c("west", "centre", "east"), nrow(cities))
  w <- pv_bubble_map(cities, lon = "lon", lat = "lat", size = "population",
                     color = "region", label = "city",
                     title = "Cities as circles")
  path <- file.path(render_out_dir(), "bubblemap.png")
  expect_no_warning(pv_save(w, path, quiet = TRUE))
  expect_true(file.exists(path))
  expect_gt(file.size(path), 20000)
  render_publish(path)
})

# ---- flow map --------------------------------------------------------------

# The flagship commuter flows: every exchange between Zug and its named
# neighbours in the latest period, both directions, so bidirectional
# pairs are part of the canonical build.
zug_flows <- function() {
  latest <- pv_commuters[pv_commuters$period == "2022-2024" &
                           pv_commuters$region != "Restliche Schweiz", ]
  data.frame(
    from = ifelse(latest$direction == "to Zug", latest$region, "Zug"),
    to = ifelse(latest$direction == "to Zug", "Zug", latest$region),
    commuters = latest$commuters)
}

test_that("flow map builds its payload from flows and place centroids", {
  flows <- zug_flows()
  w <- expect_pvchart(
    pv_flow_map(flows, from = "from", to = "to", value = "commuters"),
    "flowmap")
  # Flows travel exactly as given - names, order, and values.
  expect_named(w$x$data, c("from", "to", "value"))
  expect_equal(w$x$data$from, flows$from)
  expect_equal(w$x$data$to, flows$to)
  expect_equal(w$x$data$value, flows$commuters)
  expect_equal(w$x$vlab, "commuters")
  # "cantons" is the default base, with the lakes over it.
  expect_identical(w$x$map, pv_swiss_cantons)
  expect_identical(w$x$lakes, pv_swiss_lakes)
  # One place row per endpoint, each on its feature's centroid.
  expect_setequal(w$x$places$place,
                  c("Zug", "Aargau", "Luzern", "Schwyz", "Z\u00fcrich"))
  cent <- polyviz:::pv_layer_centroids(pv_swiss_cantons)
  zug <- w$x$places[w$x$places$place == "Zug", ]
  expect_equal(zug$lon, cent$lon[cent$name == "Zug"])
  expect_equal(zug$lat, cent$lat[cent$name == "Zug"])
  # Total throughput is everything in plus everything out: every flow
  # touches Zug, and Luzern carries just its own two directions.
  expect_equal(zug$total, sum(flows$commuters))
  lu <- w$x$places[w$x$places$place == "Luzern", ]
  expect_equal(lu$total, sum(flows$commuters[flows$from == "Luzern" |
                                               flows$to == "Luzern"]))
  # Every endpoint is labelled by default.
  expect_true(all(w$x$places$labelled))
})

test_that("layer centroids mirror the pv_city_coords construction", {
  # pv_city_coords was built with sf in the metric LV95 plane; the
  # runtime centroids use the same area-weighted idea on the raw rings,
  # so on the same municipality polygons the two must agree to within
  # the published rounding.
  cent <- polyviz:::pv_layer_centroids(pv_lucerne_map)
  for (city in c("Luzern", "Kriens", "Emmen", "Willisau")) {
    mine <- cent[cent$name == city, ]
    ref <- pv_city_coords[pv_city_coords$city == city, ]
    expect_lt(abs(mine$lon - ref$lon), 2e-4)
    expect_lt(abs(mine$lat - ref$lat), 2e-4)
  }
  # Background features without keys (the Lucerne lakes) stay out.
  expect_false(anyNA(cent$lon))
  expect_equal(nrow(cent), 79)
})

test_that("flow endpoints match feature names first and ids second", {
  # Canton numbers name the same places the canton names do.
  by_id <- data.frame(a = c("3", "19"), b = c("9", "9"), n = c(10, 20))
  w <- pv_flow_map(by_id, from = "a", to = "b", value = "n")
  cent <- polyviz:::pv_layer_centroids(pv_swiss_cantons)
  p3 <- w$x$places[w$x$places$place == "3", ]
  expect_equal(p3$lon, cent$lon[cent$name == "Luzern"])
  # And numeric id columns work like their string form.
  by_num <- data.frame(a = c(3, 19), b = c(9, 9), n = c(10, 20))
  wn <- pv_flow_map(by_num, from = "a", to = "b", value = "n")
  expect_equal(wn$x$places$lon, w$x$places$lon)
})

test_that("flow map aborts loudly on unmatched endpoints", {
  bad <- data.frame(from = c("Luzern", "Atlantis", "Narnia"),
                    to = c("Zug", "Zug", "Mordor"),
                    n = c(1, 2, 3))
  expect_error(
    pv_flow_map(bad, from = "from", to = "to", value = "n"),
    "3 endpoint\\(s\\) match no feature.*Atlantis, Narnia, Mordor")
  # No overlap at all points at the map itself, not the names.
  none <- data.frame(from = c("Atlantis", "Lemuria"),
                     to = c("Mu", "Avalon"), n = c(1, 2))
  expect_error(
    pv_flow_map(none, from = "from", to = "to", value = "n"),
    "Wrong columns, or wrong map\\?")
  # More than five unmatched names are counted, not all spelled out.
  many <- data.frame(from = paste0("Nowhere", 1:7), to = "Zug",
                     n = 1:7)
  expect_error(
    pv_flow_map(many, from = "from", to = "to", value = "n"),
    "7 endpoint\\(s\\).*and 2 more")
})

test_that("flow map refuses duplicate pairs but keeps opposite directions", {
  dup <- data.frame(from = c("Luzern", "Luzern"), to = c("Zug", "Zug"),
                    n = c(1, 2))
  expect_error(
    pv_flow_map(dup, from = "from", to = "to", value = "n"),
    "aggregate it first")
  # A to B and B to A are two legitimate flows, not duplicates.
  both <- data.frame(from = c("Luzern", "Zug"), to = c("Zug", "Luzern"),
                     n = c(1, 2))
  expect_equal(nrow(pv_flow_map(both, from = "from", to = "to",
                                value = "n")$x$data), 2)
  # A place flowing to itself has no chord to draw.
  loop <- data.frame(from = c("Luzern", "Zug"), to = c("Zug", "Zug"),
                     n = c(1, 2))
  expect_error(
    pv_flow_map(loop, from = "from", to = "to", value = "n"),
    "from a place to itself")
})

test_that("flow map validates its columns and values", {
  flows <- zug_flows()
  expect_error(
    pv_flow_map(flows, from = "nope", to = "to", value = "commuters"),
    "not in `data`")
  expect_error(
    pv_flow_map(flows, from = "from", to = "to", value = "from"),
    "not numeric")
  expect_error(
    pv_flow_map(flows[0, ], from = "from", to = "to", value = "commuters"),
    "no rows")
  neg <- flows
  neg$commuters[2] <- -5
  expect_error(
    pv_flow_map(neg, from = "from", to = "to", value = "commuters"),
    "negative")
  zero <- flows
  zero$commuters <- 0
  expect_error(
    pv_flow_map(zero, from = "from", to = "to", value = "commuters"),
    "no positive values")
  nas <- flows
  nas$commuters[3] <- NA
  expect_warning(
    w <- pv_flow_map(nas, from = "from", to = "to", value = "commuters"),
    "Dropped 1 row\\(s\\) with missing `commuters`")
  expect_equal(nrow(w$x$data), nrow(flows) - 1)
  naf <- flows
  naf$from[1] <- NA
  expect_warning(
    pv_flow_map(naf, from = "from", to = "to", value = "commuters"),
    "missing `from`")
})

test_that("the labels argument picks which endpoints get named", {
  flows <- zug_flows()
  w <- pv_flow_map(flows, from = "from", to = "to", value = "commuters",
                   labels = c("Zug", "Luzern"))
  expect_equal(sort(w$x$places$place[w$x$places$labelled]),
               c("Luzern", "Zug"))
  # An empty selection labels nothing but still draws the dots.
  w0 <- pv_flow_map(flows, from = "from", to = "to", value = "commuters",
                    labels = character(0))
  expect_false(any(w0$x$places$labelled))
  expect_equal(nrow(w0$x$places), 5)
  expect_error(
    pv_flow_map(flows, from = "from", to = "to", value = "commuters",
                labels = c("Zug", "Bermuda")),
    "carry no flow: Bermuda")
  expect_error(
    pv_flow_map(flows, from = "from", to = "to", value = "commuters",
                labels = 1:2),
    "`labels` must be NULL or a character vector")
})

test_that("explicit endpoint coordinates override the centroid lookup", {
  df <- data.frame(from = c("HQ", "Plant"), to = c("Plant", "Depot"),
                   n = c(10, 4),
                   flon = c(8.31, 7.44), flat = c(47.05, 46.95),
                   tlon = c(7.44, 8.95), tlat = c(46.95, 46.01))
  # All four columns or none - a half-given override is a mistake.
  expect_error(
    pv_flow_map(df, from = "from", to = "to", value = "n",
                from_lon = "flon"),
    "all four columns")
  w <- pv_flow_map(df, from = "from", to = "to", value = "n",
                   from_lon = "flon", from_lat = "flat",
                   to_lon = "tlon", to_lat = "tlat")
  # The names never touch the map; the coordinates are the columns' own.
  hq <- w$x$places[w$x$places$place == "HQ", ]
  expect_equal(hq$lon, 8.31)
  expect_equal(hq$lat, 47.05)
  expect_equal(nrow(w$x$places), 3)
  # One name at two different spots would tear its endpoint dot apart.
  torn <- df
  torn$tlon[1] <- 7.5
  torn$from[2] <- "Plant"
  torn$flon[2] <- 7.44
  torn$flat[2] <- 46.95
  expect_error(
    pv_flow_map(torn, from = "from", to = "to", value = "n",
                from_lon = "flon", from_lat = "flat",
                to_lon = "tlon", to_lat = "tlat"),
    "more than one set of coordinates: Plant")
})

test_that("explicit coordinates get the bubble map's sanity checks", {
  # Swapped columns put a longitude-sized number into latitude.
  far <- data.frame(from = "A", to = "B", n = 1,
                    flon = 1.35, flat = 103.8, tlon = 22.3, tlat = 114.2)
  expect_error(
    pv_flow_map(far, from = "from", to = "to", value = "n",
                from_lon = "flon", from_lat = "flat",
                to_lon = "tlon", to_lat = "tlat"),
    "swapped")
  # A flow with an endpoint beyond the map is dropped, loudly.
  mixed <- data.frame(from = c("Luzern", "Paris"), to = c("Bern", "Bern"),
                      n = c(1, 2),
                      flon = c(8.31, 2.35), flat = c(47.05, 48.86),
                      tlon = c(7.44, 7.44), tlat = c(46.95, 46.95))
  expect_warning(
    w <- pv_flow_map(mixed, from = "from", to = "to", value = "n",
                     from_lon = "flon", from_lat = "flat",
                     to_lon = "tlon", to_lat = "tlat"),
    "outside the map.*Paris -> Bern")
  expect_equal(w$x$data$from, "Luzern")
  # Nothing on the map at all is an error, not an empty sea.
  off <- data.frame(from = "Paris", to = "London", n = 1,
                    flon = 2.35, flat = 48.86, tlon = -0.13, tlat = 51.5)
  expect_error(
    pv_flow_map(off, from = "from", to = "to", value = "n",
                from_lon = "flon", from_lat = "flat",
                to_lon = "tlon", to_lat = "tlat"),
    "No flow falls on the map")
})

test_that("flow map lakes and layers behave like the geo siblings", {
  flows <- zug_flows()
  # Explicit words win; "auto" leaves regional maps dry.
  wf <- pv_flow_map(flows, from = "from", to = "to", value = "commuters",
                    lakes = FALSE)
  expect_null(wf$x$lakes)
  lu <- data.frame(from = c("Emmen", "Kriens"), to = "Luzern", n = c(2, 1))
  wl <- pv_flow_map(lu, from = "from", to = "to", value = "n",
                    map = "lucerne")
  expect_identical(wl$x$map, pv_lucerne_map)
  expect_null(wl$x$lakes)
  expect_error(
    pv_flow_map(flows, from = "from", to = "to", value = "commuters",
                lakes = "yes"),
    '`lakes` must be TRUE, FALSE, or "auto"')
  expect_error(
    pv_flow_map(flows, from = "from", to = "to", value = "commuters",
                map = "krantons"),
    "Unknown map layer")
})

test_that("flow map works on an sf layer through the usual conversion", {
  skip_if_not_installed("sf")
  x <- sf_from_layer(pv_swiss_cantons)
  flows <- zug_flows()
  w <- pv_flow_map(flows, from = "from", to = "to", value = "commuters",
                   map = x)
  expect_equal(nrow(w$x$places), 5)
  # The converted features carry the same polygons, so the centroids
  # land where the bundled layer puts them.
  cent <- polyviz:::pv_layer_centroids(pv_swiss_cantons)
  zug <- w$x$places[w$x$places$place == "Zug", ]
  expect_lt(abs(zug$lon - cent$lon[cent$name == "Zug"]), 1e-6)
})

test_that("the flow map renders without JavaScript errors", {
  render_skip_if_no_chrome()
  w <- pv_flow_map(zug_flows(), from = "from", to = "to",
                   value = "commuters",
                   title = "Commuter exchange with Canton Zug")
  path <- file.path(render_out_dir(), "flowmap.png")
  expect_no_warning(pv_save(w, path, quiet = TRUE))
  expect_true(file.exists(path))
  expect_gt(file.size(path), 20000)
  render_publish(path)

  # A lone bidirectional pair and a regional layer go through the same
  # real-Chrome treatment - the offset arcs and the no-lakes path.
  bi <- data.frame(a = c("Bern", "Ticino"), b = c("Ticino", "Bern"),
                   n = c(9000, 3000))
  wb <- pv_flow_map(bi, from = "a", to = "b", value = "n",
                    title = "Two directions, two sides of the chord")
  pb <- file.path(render_out_dir(), "flowmap_bidirectional.png")
  expect_no_warning(pv_save(wb, pb, quiet = TRUE))
  expect_gt(file.size(pb), 20000)
  render_publish(pb)

  lu <- data.frame(from = c("Emmen", "Kriens", "Horw", "Ebikon"),
                   to = "Luzern", n = c(5200, 4100, 2300, 2900))
  wu <- pv_flow_map(lu, from = "from", to = "to", value = "n",
                    map = "lucerne",
                    title = "Commuting into the city of Lucerne")
  pu <- file.path(render_out_dir(), "flowmap_lucerne.png")
  expect_no_warning(pv_save(wu, pu, quiet = TRUE))
  expect_gt(file.size(pu), 20000)
  render_publish(pu)
})

test_that("the flow map exports a standalone SVG", {
  render_skip_if_no_chrome()
  w <- pv_flow_map(zug_flows(), from = "from", to = "to",
                   value = "commuters",
                   title = "Commuter exchange with Canton Zug")
  f <- withr::local_tempfile(fileext = ".svg")
  expect_no_warning(pv_save(w, f, quiet = TRUE))
  svg <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"),
               collapse = "\n")
  expect_match(svg, "^<\\?xml")
  expect_match(svg, "Commuter exchange with Canton Zug", fixed = TRUE)
  # All eight flow bands made it into the standalone document.
  expect_equal(
    lengths(regmatches(svg, gregexpr('class="band"', svg))), 8)
})

# Stages a widget the way pv_save() does (light mode, no entrance
# animation, filling a page opened at a fixed size) and hands back the
# live Chrome session - the comparison charts' hover-check pattern.
geo_page_session <- function(w, width = 700, height = 460) {
  w$x$mode <- "light"
  w$x$duration <- 0
  w$width <- NULL
  w$height <- NULL
  w$sizingPolicy$browser$fill <- TRUE
  w$sizingPolicy$browser$padding <- 0
  stage <- tempfile("pv-geo-page-")
  dir.create(stage)
  page <- file.path(stage, "chart.html")
  htmlwidgets::saveWidget(w, page, selfcontained = FALSE, libdir = "lib")
  b <- chromote::ChromoteSession$new(width = width, height = height)
  errors <- polyviz:::export_watch_errors(b)
  loaded <- b$Page$loadEventFired(wait_ = FALSE)
  b$Page$navigate(utils::URLencode(paste0("file://", normalizePath(page))),
                  wait_ = FALSE)
  b$wait_for(loaded)
  polyviz:::export_wait_settled(b, errors, 0,
                                polyviz:::export_settle_count_js(w))
  list(b = b, errors = errors)
}

test_that("hovering a flow lights it, dims the rest, and names both ends", {
  render_skip_if_no_chrome()
  s <- geo_page_session(
    pv_flow_map(zug_flows(), from = "from", to = "to", value = "commuters",
                title = "Commuter exchange with Canton Zug"))
  withr::defer(try(s$b$close(), silent = TRUE))
  expect_identical(s$errors$msgs, character())
  res <- s$b$Runtime$evaluate("
    (function () {
      /* Each flow group's second path is its invisible fat hover twin
         along the centerline; poke the pointer at the biggest flow's
         twin and read the tooltip. */
      var gs = document.querySelectorAll('g.flow');
      if (!gs.length) return 'no flows';
      var over = gs[0].querySelector('path.hover');
      var r = over.getBoundingClientRect();
      var cx = r.left + r.width / 2, cy = r.top + r.height / 2;
      over.dispatchEvent(new PointerEvent('pointerenter',
        { clientX: cx, clientY: cy, bubbles: true }));
      over.dispatchEvent(new PointerEvent('pointermove',
        { clientX: cx, clientY: cy, bubbles: true }));
      var tip = document.querySelector('.pv-tooltip');
      var ops = Array.prototype.map.call(
        document.querySelectorAll('path.band'),
        function (p) { return +p.getAttribute('fill-opacity'); });
      return JSON.stringify({ flows: gs.length, opacity: tip.style.opacity,
        html: tip.innerHTML, lit: ops.filter(function (o) {
          return o > 0.5; }).length,
        dimmed: ops.filter(function (o) { return o < 0.1; }).length });
    })()", returnByValue = TRUE)$result$value
  got <- jsonlite::fromJSON(res)
  expect_equal(got$flows, 8)
  expect_equal(got$opacity, "1")
  # The biggest flow leads the drawing order: Zurich into Zug.
  expect_match(got$html, "Z\u00fcrich \u2192 Zug")
  expect_match(got$html, "commuters: <b>")
  # Exactly one band lights up; every other flow steps back.
  expect_equal(got$lit, 1)
  expect_equal(got$dimmed, 7)
})
