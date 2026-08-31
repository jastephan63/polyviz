# Structural checks on the bundled Swiss boundary layers and city
# coordinates - the shapes the geo charts depend on, frozen at the GG25
# release they were cut from (boundary status 1.1.2025).

# Collects every coordinate of a FeatureCollection into one lon/lat
# matrix, so bounds and NA checks can run over a whole layer at once.
# The nested coordinate lists hold plain [lon, lat] pairs, so unlisting
# in document order and folding by two recovers the points.
all_coords <- function(map) {
  v <- unlist(lapply(map$features, function(f) f$geometry$coordinates))
  expect_identical(length(v) %% 2L, 0L)
  matrix(v, ncol = 2, byrow = TRUE)
}

expect_boundary_layer <- function(map, n_features) {
  expect_type(map, "list")
  expect_identical(map$type, "FeatureCollection")
  expect_length(map$features, n_features)
  for (f in map$features) {
    expect_identical(f$type, "Feature")
    expect_true(f$geometry$type %in% c("Polygon", "MultiPolygon"))
    expect_gt(length(f$geometry$coordinates), 0)
  }
  # Every point must be a real lon/lat pair inside the Swiss bounding
  # box - a projection slip would land far outside it.
  xy <- all_coords(map)
  expect_false(anyNA(xy))
  expect_true(all(xy[, 1] >= 5.9 & xy[, 1] <= 10.6))
  expect_true(all(xy[, 2] >= 45.7 & xy[, 2] <= 47.9))
  invisible(map)
}

test_that("pv_swiss_cantons holds the 26 cantons, joinable by number", {
  expect_boundary_layer(pv_swiss_cantons, 26)
  ids <- vapply(pv_swiss_cantons$features,
                function(f) f$properties$id, integer(1))
  expect_setequal(ids, 1:26)
  nms <- vapply(pv_swiss_cantons$features,
                function(f) f$properties$name, character(1))
  expect_true(all(nzchar(nms)))
  expect_true(all(c("Zürich", "Ticino", "Genève") %in% nms))
})

test_that("pv_swiss_districts covers the country with unique ids", {
  expect_boundary_layer(pv_swiss_districts, 143)
  ids <- vapply(pv_swiss_districts$features,
                function(f) f$properties$id, integer(1))
  expect_false(anyDuplicated(ids) > 0)
  # BFS district numbers are canton number * 100 plus a serial.
  expect_true(all(ids >= 101 & ids <= 2699))
  nms <- vapply(pv_swiss_districts$features,
                function(f) f$properties$name, character(1))
  expect_true(all(nzchar(nms)))
})

test_that("pv_swiss_lakes is a named background layer without ids", {
  expect_boundary_layer(pv_swiss_lakes, 22)
  nms <- vapply(pv_swiss_lakes$features,
                function(f) f$properties$name, character(1))
  expect_false(anyDuplicated(nms) > 0)
  expect_true(all(c("Lac Léman", "Bodensee") %in% nms))
  # No id: the lakes must stay out of every choropleth join.
  for (f in pv_swiss_lakes$features) expect_null(f$properties$id)
})

test_that("pv_city_coords puts every population city on the map", {
  expect_s3_class(pv_city_coords, "data.frame")
  expect_named(pv_city_coords, c("city", "id", "lon", "lat"))
  expect_setequal(pv_city_coords$city, unique(pv_city_population$city))
  expect_false(anyDuplicated(pv_city_coords$id) > 0)
  expect_false(anyNA(pv_city_coords))
  expect_true(all(pv_city_coords$lon >= 5.9 & pv_city_coords$lon <= 10.6))
  expect_true(all(pv_city_coords$lat >= 45.7 & pv_city_coords$lat <= 47.9))
  # Spot checks against well-known coordinates, at map precision.
  bern <- pv_city_coords[pv_city_coords$city == "Bern", ]
  expect_equal(bern$id, 351)
  expect_equal(bern$lon, 7.45, tolerance = 0.05)
  expect_equal(bern$lat, 46.95, tolerance = 0.05)
})

test_that("the Swiss layers feed pv_choropleth unchanged", {
  w <- pv_choropleth(data.frame(id = 1:26, v = 1:26),
                     map = pv_swiss_cantons, id = "id", value = "v")
  expect_identical(w$x$map, pv_swiss_cantons)
  # Cantons plus lakes: the merged collection still joins on the canton
  # ids while the id-less lakes ride along as background.
  both <- list(type = "FeatureCollection",
               features = c(pv_swiss_cantons$features,
                            pv_swiss_lakes$features))
  w <- pv_choropleth(data.frame(id = 1:26, v = 1:26),
                     map = both, id = "id", value = "v")
  expect_length(w$x$map$features, 26 + 22)
})
