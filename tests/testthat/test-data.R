# Structural checks on the bundled Swiss boundary layers and city
# coordinates - the shapes the geo charts depend on, frozen at the GG25
# release they were cut from (boundary status 1.1.2025) - and on the
# curated open-data frames that ride along for examples and tests.

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

# The curated open-data frames: frozen snapshots of the BFE electricity
# balance and the BFS accommodation statistics, bundled so examples and
# tests never touch the network.

test_that("pv_electricity covers 2020-2025 with six sources per month", {
  expect_s3_class(pv_electricity, "data.frame")
  expect_named(pv_electricity, c("date", "year", "month", "source", "gwh"))
  expect_identical(nrow(pv_electricity), 432L)
  expect_s3_class(pv_electricity$date, "Date")
  expect_setequal(unique(pv_electricity$year), 2020:2025)
  expect_setequal(unique(pv_electricity$source),
                  c("River hydro", "Storage hydro", "Nuclear", "Thermal",
                    "Wind", "Solar"))
  expect_false(anyNA(pv_electricity))
  expect_true(all(pv_electricity$gwh >= 0))
  # every month carries each source exactly once
  expect_true(all(table(pv_electricity$date) == 6L))
  # annual totals must land in Switzerland's familiar production band -
  # a unit slip (MWh, TWh) would fly far out of it
  twh <- tapply(pv_electricity$gwh, pv_electricity$year, sum) / 1000
  expect_true(all(twh > 55 & twh < 90))
})

test_that("pv_tourism joins the canton layer and sums to real totals", {
  expect_s3_class(pv_tourism, "data.frame")
  expect_named(pv_tourism, c("year", "canton_id", "canton", "origin",
                             "arrivals", "nights"))
  expect_identical(nrow(pv_tourism), 6006L)
  expect_setequal(unique(pv_tourism$year), 2005:2025)
  expect_false(anyNA(pv_tourism))
  # the canton numbers are the same join key the boundary layer carries
  map_ids <- vapply(pv_swiss_cantons$features,
                    function(f) f$properties$id, integer(1))
  expect_setequal(unique(pv_tourism$canton_id), map_ids)
  # ten named markets plus the folded remainder, complete everywhere
  expect_length(unique(pv_tourism$origin), 11)
  expect_true(all(table(pv_tourism$year, pv_tourism$canton_id) == 11L))
  expect_true(all(c("Switzerland", "Germany", "China",
                    "Other countries") %in% pv_tourism$origin))
  # a guest who arrives stays at least one night
  expect_true(all(pv_tourism$arrivals >= 0))
  expect_true(all(pv_tourism$nights >= pv_tourism$arrivals))
  # the 2020 collapse is in the data: national nights fell by ~40%
  nights <- tapply(pv_tourism$nights, pv_tourism$year, sum)
  expect_lt(nights[["2020"]], 0.7 * nights[["2019"]])
})

test_that("the curated frames feed the charts their docs promise", {
  expect_pvchart <- function(w, type) {
    expect_s3_class(w, "htmlwidget")
    expect_equal(attr(w, "package"), "polyviz")
    expect_equal(w$x$type, type)
    invisible(w)
  }
  expect_pvchart(pv_area(pv_electricity, "date", "gwh", series = "source",
                         offset = "stream"), "area")
  t24 <- pv_tourism[pv_tourism$year == 2024, ]
  expect_pvchart(pv_heatmap(t24, "origin", "canton", "nights"), "heatmap")
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
