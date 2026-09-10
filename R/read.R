#' Read data from any supported source
#'
#' One entry point that dispatches on file extension:
#'
#' | Extension | Backend |
#' |---|---|
#' | `.csv`, `.tsv` | base R |
#' | `.rds` | base R |
#' | `.sas7bdat`, `.xpt` | SAS via `haven` |
#' | `.sqlite`, `.db`, `.s3db` | SQL via `DBI`/`RSQLite` |
#' | `.duckdb`, `.ddb` | SQL via `DBI`/`duckdb` |
#' | `.parquet` | DuckDB via `duckdb` |
#'
#' Parquet files are read in place by DuckDB, which needs the `duckdb`
#' package (a Suggests dependency) installed.
#'
#' @param path Path to a data file.
#' @param table For database files: the table to read. Defaults to the
#'   only table in the database; an error lists the options when there are
#'   several.
#' @param ... Passed to the underlying reader (e.g. `stringsAsFactors` for
#'   CSV).
#' @return A data frame.
#' @examples
#' db <- system.file("extdata", "demo.sqlite", package = "polyviz")
#' sales <- pv_read(db, table = "sales")
#' head(sales)
#' @export
pv_read <- function(path, table = NULL, ...) {
  if (!file.exists(path)) {
    rlang::abort(sprintf("File not found: %s", path))
  }
  ext <- tolower(tools::file_ext(path))
  switch(ext,
    "csv" = utils::read.csv(path, ...),
    "tsv" = utils::read.delim(path, ...),
    "rds" = readRDS(path),
    "sas7bdat" = ,
    "xpt" = pv_read_sas(path),
    "sqlite" = ,
    "db" = ,
    "s3db" = ,
    "duckdb" = ,
    "ddb" = read_db_table(path, table),
    "parquet" = read_parquet_file(path),
    rlang::abort(sprintf("Unsupported file extension '.%s'.", ext))
  )
}

# Opens the database just long enough to pull one table out. The
# connection is always closed on the way out, even if something fails.
# pv_db_connect() picks the engine from the file extension, so the same
# path serves SQLite and DuckDB files alike.
read_db_table <- function(path, table) {
  con <- pv_db_connect(path)
  on.exit(pv_db_disconnect(con), add = TRUE)
  tables <- pv_db_tables(con)
  if (is.null(table)) {
    if (length(tables) != 1) {
      rlang::abort(sprintf(
        "Database has %d tables; pick one with `table = `: %s",
        length(tables), paste(tables, collapse = ", ")))
    }
    table <- tables[[1]]
  }
  if (!table %in% tables) {
    rlang::abort(sprintf(
      "No table '%s'. Available: %s", table, paste(tables, collapse = ", ")))
  }
  DBI::dbReadTable(con, table)
}

# DuckDB reads the parquet file where it sits - nothing is imported into
# a database first. A throwaway in-memory connection does the work and is
# closed on the way out.
read_parquet_file <- function(path) {
  sql_need_duckdb("Reading .parquet files")
  con <- pv_db_connect(driver = "duckdb")
  on.exit(pv_db_disconnect(con), add = TRUE)
  sql <- sprintf("SELECT * FROM read_parquet(%s)",
                 DBI::dbQuoteString(con, path))
  pv_query(con, sql)
}
