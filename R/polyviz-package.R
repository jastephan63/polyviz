#' polyviz: polyglot data analysis with a pure R interface
#'
#' polyviz is an exploratory data analysis toolkit whose backend spans four
#' languages while the interface stays entirely in R:
#'
#' * **SQL** — connect to, query, and script SQLite databases
#'   ([pv_db_connect()], [pv_query()], [pv_run_sql_file()]).
#' * **SAS** — read and write `sas7bdat`/`xpt` datasets with variable-label
#'   support ([pv_read_sas()], [pv_write_sas()], [pv_labels()]).
#' * **Python** — numeric profiling and outlier detection delegated to a
#'   bundled Python module when available, with an identical pure-R fallback
#'   ([pv_profile()], [pv_outliers()]).
#' * **JavaScript / D3** — interactive visualisations rendered by a bundled
#'   D3.js v7, driven from R data frames ([pv_bar()], [pv_line()],
#'   [pv_scatter()], [pv_force()], [pv_chord()], [pv_sunburst()]).
#'
#' @keywords internal
#' @importFrom stats sd median cor quantile complete.cases setNames
#' @importFrom utils read.csv head modifyList
#' @importFrom rlang %||% abort .data
"_PACKAGE"

# pv_lucerne_map is a lazy-loaded dataset used as a default argument in
# pv_choropleth(); declare it so the checker knows the binding is real.
utils::globalVariables("pv_lucerne_map")
