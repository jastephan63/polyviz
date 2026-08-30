#' Simulated monthly sales
#'
#' Two years of simulated monthly sales for five products across four
#' regions, with seasonality, growth, and noise. The demo dataset behind
#' most polyviz examples; the same table ships as
#' `inst/extdata/demo.sqlite` (table `sales`) and, labelled and truncated,
#' as `inst/extdata/demo_sales.xpt`.
#'
#' @format A data frame with 480 rows:
#' \describe{
#'   \item{date}{First day of the month (`Date`).}
#'   \item{month}{Month as `"YYYY-MM"`.}
#'   \item{region}{North, South, East, or West.}
#'   \item{product}{One of five product lines.}
#'   \item{units}{Units sold.}
#'   \item{revenue}{Net revenue.}
#' }
"pv_sales"

#' Simulated collaboration network
#'
#' A 16-person, 4-team network for [pv_force()]: people collaborate mostly
#' within their team, occasionally across.
#'
#' @format A list of two data frames: `nodes` (`id`, `group`) and `links`
#'   (`source`, `target`, `value`).
"pv_network"

#' Simulated inter-warehouse shipment flows
#'
#' A 5x5 matrix of shipment volumes between warehouses for [pv_chord()];
#' `pv_flows[i, j]` is the flow from warehouse `i` to warehouse `j`.
#'
#' @format A numeric matrix with warehouse names as dimnames.
"pv_flows"

#' Swiss city populations since 1930
#'
#' Permanent resident population of 181 Swiss cities at census reference
#' years from 1930 to today. Nearly a century of urban growth — good for
#' long time-series charts.
#'
#' @format A data frame: `city`, `size_class` (population size band),
#'   `year`, `population`.
#' @source Bundesamt für Statistik (BFS), Statistik der Schweizer Städte,
#'   via stats.swiss. Open use, source citation required
#'   ("Quelle: Bundesamt für Statistik").
"pv_city_population"

#' Employment shares by economic sector in Swiss cities
#'
#' Share of employees (percent) in each NOGA 2008 economic section, per
#' Swiss city. Naturally hierarchical (city → sector) — good for treemaps,
#' sunbursts, and heatmaps.
#'
#' @format A data frame: `city`, `sector_code` (NOGA section letter A–S),
#'   `sector`, `share` (percent of the city's employees).
#' @source Bundesamt für Statistik (BFS), Statistik der Unternehmensstruktur
#'   STATENT, via stats.swiss. Open use, source citation required.
"pv_city_sectors"

#' Land use in Swiss cities
#'
#' Area in hectares by land-use category for 181 Swiss cities (survey
#' period 2013–2025), grouped into Settlement, Cultivated, and Natural.
#' Two hierarchy levels — good for treemaps and part-of-whole charts.
#'
#' @format A data frame: `city`, `group`, `category`, `hectares`.
#' @source Bundesamt für Statistik (BFS), Arealstatistik der Schweiz,
#'   via stats.swiss. Open use, source citation required.
"pv_city_landuse"

#' Commuter flows between Canton Zug and its neighbours
#'
#' People commuting to and from Canton Zug by partner region (Aargau,
#' Lucerne, Schwyz, Zurich, rest of Switzerland) in pooled three-year
#' periods. Directional flows — good for chord and Sankey diagrams.
#'
#' @format A data frame: `period`, `region`, `direction`
#'   (`"to Zug"`/`"from Zug"`), `commuters`.
#' @source Kanton Zug, Fachstelle Statistik, via opendata.swiss. Open use,
#'   source citation required.
"pv_commuters"

#' Fiscal equalization of Lucerne municipalities
#'
#' Fiscal capacity and equalization payments for every municipality of
#' Canton Lucerne since 2020: tax resource potential per resident, the
#' resource index (cantonal average = 100), and the equalization amount
#' received. Distributions across ~80 municipalities — good for boxplots,
#' histograms, and lollipop rankings.
#'
#' @format A data frame: `year`, `municipality_id` (official BFS
#'   municipality number — joins [pv_lucerne_map]), `municipality`,
#'   `resource_per_capita` (CHF), `resource_index`, `equalization_chf`.
#' @source LUSTAT Statistik Luzern, "Finanzausgleich Kanton Luzern", via
#'   opendata.swiss. Open use with source citation
#'   ("Quelle: LUSTAT Statistik Luzern"); commercial use requires the data
#'   owner's permission.
"pv_fiscal"

#' Lucerne municipal council elections
#'
#' Every candidacy in Lucerne municipal council elections since 2020:
#' party, sex, incumbent or new, and whether the candidate was elected.
#' Categorical hierarchies — good for sunbursts, donuts, and grouped bars.
#'
#' @format A data frame: `year`, `municipality_id` (BFS municipality
#'   number), `municipality`, `party`, `sex`, `status`, `elected`.
#' @source LUSTAT Statistik Luzern, "Gemeinderatswahlen Kanton Luzern seit
#'   2020", via opendata.swiss. Open use with source citation
#'   ("Quelle: LUSTAT Statistik Luzern"); commercial use requires the data
#'   owner's permission.
"pv_elections"

#' Municipal boundaries of Canton Lucerne
#'
#' Boundary polygons of the 79 Lucerne municipalities (status 1 January
#' 2025) as a GeoJSON FeatureCollection stored as a plain R list, ready
#' for [pv_choropleth()]. Each feature carries `id` (the official BFS
#' municipality number, which joins [pv_fiscal] and [pv_elections]) and
#' `name`.
#'
#' @format A list mirroring GeoJSON: `type`, and `features` — one per
#'   boundary polygon, with `properties` (`id`, `name`) and `geometry`
#'   in WGS84 longitude/latitude.
#' @source Bundesamt für Statistik, "Generalisierte Gemeindegrenzen"
#'   (GG25, generalisation level G1, reprojected to WGS84). Open use,
#'   source citation required ("© BFS, ThemaKart").
"pv_lucerne_map"

#' Daily weather in Lucerne
#'
#' Daily measurements from the MeteoSwiss station LUZ (Luzern, 454 m),
#' 2020 through 2025 — temperature, precipitation, and sunshine. Good for
#' calendar heatmaps and seasonal charts.
#'
#' @format A data frame: `date`, `temp_mean`, `temp_max`, `temp_min`
#'   (°C), `precip_mm`, `sunshine_min` (minutes).
#' @source MeteoSwiss open government data, SwissMetNet station LUZ.
#'   Open use, source citation required ("Source: MeteoSwiss").
"pv_weather"
