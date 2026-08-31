# Fetcher for the boundary layers too big to bundle. The Switzerland-wide
# canton, district, and lake layers ship in data/; the ~2100 municipality
# polygons would blow the package size, so they are downloaded from the BFS
# on first use, processed into the same FeatureCollection layout, and kept
# in the shared download cache (R/fetch.R) - later calls are instant and
# work offline.

# The one BFS asset every boundary layer comes from: the "Generalisierte
# Gemeindegrenzen" GG25 release (boundary status 1.1.2025, generalisation
# level G1, LV95), the same ZIP data-raw/process-boundaries.R cut the
# bundled layers from.
pv_map_zip_url <-
  "https://dam-api.bfs.admin.ch/hub/api/dam/assets/34367751/master"

pv_licence_themakart <-
  'Open use, source citation required ("\u00a9 BFS, ThemaKart").'

# Rounds every coordinate in a parsed GeoJSON geometry to 4 decimals
# (~11 m) - the same slimming the bundled layers got, and well inside
# what a generalised country-wide map can honestly claim anyway.
pv_round_coords <- function(x) {
  if (is.numeric(x)) {
    return(round(x, 4))
  }
  if (is.list(x)) {
    return(lapply(x, pv_round_coords))
  }
  x
}

# Writes already-processed text into the download cache under a synthetic
# URL, with the same .dat/.json file pair pv_download() leaves behind - so
# pv_cache_status() lists it and pv_cache_clear() removes it. The staging
# rename keeps a crash mid-write from leaving a half-written entry.
pv_cache_store <- function(url, text) {
  key <- pv_cache_key(url)
  dat <- file.path(pv_cache_dir(), paste0(key, ".dat"))
  tmp <- paste0(dat, ".tmp")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(text, tmp, useBytes = TRUE)
  file.rename(tmp, dat)
  jsonlite::write_json(
    list(url = url, fetched = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
         size = file.size(dat)),
    file.path(pv_cache_dir(), paste0(key, ".json")),
    auto_unbox = TRUE)
  dat
}

# Where the processed layer sits in the cache, if a previous call already
# built it. The synthetic key URL names the source asset plus the
# processing, so a future format change can bump it without colliding.
pv_map_cache_url <- function(layer) {
  paste0(pv_map_zip_url, "#", layer, "-g1-wgs84-v1")
}

#' Fetch a Switzerland-wide boundary layer from the BFS
#'
#' Downloads the map layers too large to bundle with the package.
#' `pv_fetch_map("municipalities")` returns the boundary polygons of all
#' Swiss municipalities (status 1 January 2025) as a GeoJSON
#' FeatureCollection stored as a plain R list — the same layout as
#' [pv_swiss_cantons] — with each feature carrying `id` (the official BFS
#' municipality number) and `name`. [pv_choropleth()] and
#' [pv_bubble_map()] call this for you when given
#' `map = "municipalities"`.
#'
#' The first call downloads the BFS "Generalisierte Gemeindegrenzen"
#' (GG25) asset and needs the sf package to reproject it from the Swiss
#' LV95 grid to the WGS84 longitude/latitude D3 expects; the processed
#' layer is then kept in the polyviz download cache (see
#' [pv_cache_status()]), so later calls are instant, work offline, and no
#' longer need sf.
#'
#' The Bundesamt für Statistik publishes these boundaries for open use
#' with source citation required ("© BFS, ThemaKart"); every fetch prints
#' these terms and attaches them to the result (`pv_source`,
#' `pv_licence`, `pv_url`).
#'
#' @param layer The layer to fetch. Currently `"municipalities"` — the
#'   canton, district, and lake layers are bundled as [pv_swiss_cantons],
#'   [pv_swiss_districts], and [pv_swiss_lakes] and need no fetching.
#' @param refresh Set to TRUE to bypass the cache, download the asset
#'   again, and rebuild the processed layer.
#' @return A GeoJSON FeatureCollection stored as a plain R list, ready
#'   for the `map` argument of [pv_choropleth()] and [pv_bubble_map()];
#'   source and licence are attached as attributes and printed on every
#'   fetch.
#' @seealso [pv_swiss_cantons], [pv_cache_status()]
#' @examples
#' \dontrun{
#' muni <- pv_fetch_map("municipalities")
#' length(muni$features)
#' }
#' @export
pv_fetch_map <- function(layer = "municipalities", refresh = FALSE) {
  known <- c("municipalities")
  if (!is.character(layer) || length(layer) != 1 || is.na(layer) ||
      !layer %in% known) {
    rlang::abort(sprintf(paste0(
      "`layer` must be one of: %s. The canton, district, and lake layers ",
      "are bundled (`pv_swiss_cantons`, `pv_swiss_districts`, ",
      "`pv_swiss_lakes`) and need no fetching."),
      paste0('"', known, '"', collapse = ", ")))
  }
  if (!isTRUE(refresh) && !isFALSE(refresh)) {
    rlang::abort("`refresh` must be TRUE or FALSE.")
  }

  cache_url <- pv_map_cache_url(layer)
  dat <- file.path(pv_cache_dir(create = FALSE),
                   paste0(pv_cache_key(cache_url), ".dat"))
  if (refresh || !file.exists(dat)) {
    dat <- pv_map_build(layer, cache_url, refresh)
  }
  # Always deliver by re-reading the cached file, so the first call and
  # every later one hand back the exact same object.
  gj <- jsonlite::fromJSON(dat, simplifyVector = FALSE)
  pv_deliver(
    gj,
    source = paste("Bundesamt f\u00fcr Statistik, Generalisierte",
                   "Gemeindegrenzen (GG25, level G1, status 1 January 2025)"),
    licence = pv_licence_themakart,
    url = pv_map_zip_url)
}

# Downloads the GG25 ZIP (through the shared cache), pulls out the
# municipality GeoJSON, reprojects it with sf, slims each feature to the
# two properties the charts use, and stores the result in the cache.
# Returns the cached file's path.
pv_map_build <- function(layer, cache_url, refresh) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    rlang::abort(paste(
      "Processing the downloaded boundaries needs the sf package;",
      'install it with `install.packages("sf")`.',
      "(Only the first fetch needs it - the processed layer is cached.)"))
  }

  zip <- pv_download(pv_map_zip_url, refresh = refresh)$path
  entries <- utils::unzip(zip, list = TRUE)$Name
  # The file sits in a "Historized boundaries G1 .../" folder; match on the
  # basename so a reshuffled folder name doesn't break the fetch.
  wanted <- grep("Communes_G1_.*_2056\\.geojson$", entries, value = TRUE)
  if (!length(wanted)) {
    rlang::abort(sprintf(paste(
      "The downloaded BFS asset has no municipality GeoJSON",
      "(no Communes_G1_*_2056.geojson entry). Its layout may have changed;",
      "please report this at %s."),
      "https://github.com/jastephan63/polyviz/issues"))
  }
  exdir <- tempfile("pv-map-")
  dir.create(exdir)
  on.exit(unlink(exdir, recursive = TRUE), add = TRUE)
  utils::unzip(zip, files = wanted[[1]], exdir = exdir, junkpaths = TRUE)

  x <- sf::st_read(file.path(exdir, basename(wanted[[1]])), quiet = TRUE)
  if (!all(c("GDENR", "GDENAME") %in% names(x))) {
    rlang::abort(paste(
      "The BFS municipality file no longer carries the GDENR/GDENAME",
      "columns this fetcher expects; its layout may have changed."))
  }
  # Same treatment as the bundled layers (data-raw/process-boundaries.R):
  # reproject LV95 to the WGS84 lon/lat d3 expects, round the way there
  # too, and keep only the id and name of each feature.
  ids <- x$GDENR
  nms <- x$GDENAME
  x <- sf::st_transform(x[, character(0)], 4326)
  tmp <- tempfile(fileext = ".geojson")
  on.exit(unlink(tmp), add = TRUE)
  sf::st_write(x, tmp, quiet = TRUE)
  raw <- jsonlite::fromJSON(tmp, simplifyVector = FALSE)
  feats <- lapply(seq_along(raw$features), function(i) {
    list(
      type = "Feature",
      properties = list(id = ids[[i]], name = nms[[i]]),
      geometry = list(
        type = raw$features[[i]]$geometry$type,
        coordinates = pv_round_coords(raw$features[[i]]$geometry$coordinates))
    )
  })
  gj <- list(type = "FeatureCollection", features = feats)
  pv_cache_store(
    cache_url,
    jsonlite::toJSON(gj, auto_unbox = TRUE, digits = NA))
}
