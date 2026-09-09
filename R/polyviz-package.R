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
#' * **JavaScript / D3** — 34 kinds of interactive, animated chart
#'   rendered by a bundled D3.js v7 and driven entirely from R data
#'   frames: from [pv_bar()], [pv_line()], and [pv_scatter()] through
#'   distributions ([pv_histogram()], [pv_violin()]), maps
#'   ([pv_choropleth()], [pv_bubble_map()]), networks ([pv_force()],
#'   [pv_arc()], [pv_sankey()]), hierarchies ([pv_treemap()],
#'   [pv_sunburst()]), and motion ([pv_race()], [pv_bump()]) to the
#'   design-system table ([pv_table()]).
#'
#' Around the charts sit the pieces that carry them into real work:
#' exports to PNG, SVG, PDF, GIF, self-contained HTML, and PowerPoint
#' ([pv_save()], [pv_deck()]); automatic alt text on every chart
#' ([pv_alt_text()]); themes gated by an accessibility check
#' ([pv_set_theme()], [pv_check_palette()]); Swiss number and date
#' locales ([pv_locale()]); live fetchers for the Swiss open-data
#' portals ([pv_fetch_bfs()], [pv_fetch_opendata()]); and bundled
#' Swiss datasets to learn on ([pv_city_population], [pv_fiscal],
#' [pv_weather]).
#'
#' @keywords internal
#' @importFrom stats sd median cor quantile complete.cases setNames
#' @importFrom utils read.csv head modifyList
#' @importFrom rlang %||% abort .data
"_PACKAGE"

# pv_lucerne_map is a lazy-loaded dataset used as a default argument in
# pv_choropleth(); declare it so the checker knows the binding is real.
utils::globalVariables(c("pv_lucerne_map", "pv_swiss_cantons",
                         "pv_swiss_districts", "pv_swiss_lakes"))
