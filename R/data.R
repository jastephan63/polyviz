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
