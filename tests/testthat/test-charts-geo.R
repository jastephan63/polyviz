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
