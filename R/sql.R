# Thin wrapper so tests can pretend duckdb is not installed.
sql_has_duckdb <- function() {
  requireNamespace("duckdb", quietly = TRUE)
}

# DuckDB is optional; fail before any work with a message that says what
# to install. `what` names the task so the error reads naturally from
# every caller.
sql_need_duckdb <- function(what) {
  if (!sql_has_duckdb()) {
    rlang::abort(sprintf(paste(
      "%s needs the duckdb package.",
      'Install it with install.packages("duckdb").'), what))
  }
}

#' Connect to a SQLite or DuckDB database
#'
#' @param path Path to a database file, or `":memory:"` for an in-memory
#'   database.
#' @param driver Database engine. The default `"auto"` picks DuckDB for
#'   `.duckdb` and `.ddb` files and SQLite for everything else,
#'   `":memory:"` included, so existing code keeps its SQLite behaviour.
#'   Pass `"duckdb"` explicitly for an in-memory DuckDB database or a
#'   DuckDB file with an unusual extension.
#' @return A `DBIConnection`. Close it with [pv_db_disconnect()].
#' @details
#' Both engines drive the same functions — [pv_query()], [pv_db_write()],
#' [pv_db_tables()], [pv_run_sql_file()] — through DBI. DuckDB (a
#' Suggests dependency) earns its place on analytics that outgrow memory:
#' its queries can read Parquet and CSV files straight from disk, so
#' nothing is imported first. See [pv_query()] for an example.
#' @examples
#' con <- pv_db_connect()
#' pv_db_write(con, "cars", mtcars)
#' pv_query(con, "SELECT COUNT(*) AS n FROM cars")
#' pv_db_disconnect(con)
#' @export
pv_db_connect <- function(path = ":memory:",
                          driver = c("auto", "sqlite", "duckdb")) {
  driver <- match.arg(driver)
  if (driver == "auto") {
    # The file extension decides: DuckDB's own extensions go to DuckDB,
    # everything else - ":memory:" included - stays SQLite, which keeps
    # code written against earlier versions of the package working.
    ext <- tolower(tools::file_ext(path))
    driver <- if (ext %in% c("duckdb", "ddb")) "duckdb" else "sqlite"
  }
  if (driver == "duckdb") {
    sql_need_duckdb("Connecting to DuckDB databases")
    DBI::dbConnect(duckdb::duckdb(), dbdir = path)
  } else {
    DBI::dbConnect(RSQLite::SQLite(), path)
  }
}

#' Disconnect from a database
#'
#' @param con A connection from [pv_db_connect()].
#' @return `TRUE`, invisibly.
#' @export
pv_db_disconnect <- function(con) {
  if (inherits(con, "duckdb_connection")) {
    # Shutting the driver down releases the database file immediately
    # instead of waiting for the garbage collector to get round to it.
    invisible(DBI::dbDisconnect(con, shutdown = TRUE))
  } else {
    invisible(DBI::dbDisconnect(con))
  }
}

#' Write a data frame to a database table
#'
#' @param con A connection from [pv_db_connect()].
#' @param name Table name.
#' @param data A data frame.
#' @param overwrite Replace the table if it already exists?
#' @return `name`, invisibly, so calls can be piped.
#' @export
pv_db_write <- function(con, name, data, overwrite = TRUE) {
  DBI::dbWriteTable(con, name, as.data.frame(data), overwrite = overwrite)
  invisible(name)
}

#' List tables in a database
#'
#' @param con A connection from [pv_db_connect()].
#' @return Character vector of table names.
#' @export
pv_db_tables <- function(con) {
  DBI::dbListTables(con)
}

#' Run a SQL query and return the result
#'
#' @param con A connection from [pv_db_connect()].
#' @param sql A single SQL statement. Use `?` placeholders with `params`
#'   for values — never paste user input into the SQL string.
#' @param params Optional list of values bound to `?` placeholders.
#' @return A data frame.
#' @details
#' On a DuckDB connection the `FROM` clause can name a Parquet or CSV
#' file directly — `SELECT ... FROM 'sales.parquet'` — and DuckDB streams
#' the file through the query instead of loading it, so aggregations over
#' files far larger than memory return a small data frame without the
#' data ever passing through R.
#' @examples
#' con <- pv_db_connect()
#' pv_db_write(con, "cars", mtcars)
#' pv_query(con, "SELECT cyl, AVG(mpg) AS mpg FROM cars GROUP BY cyl")
#' pv_query(con, "SELECT * FROM cars WHERE cyl = ?", params = list(6))
#' pv_db_disconnect(con)
#'
#' @examplesIf requireNamespace("duckdb", quietly = TRUE)
#' # DuckDB queries Parquet files in place - no import step, and files
#' # larger than memory stream through the aggregation just fine.
#' con <- pv_db_connect(driver = "duckdb")
#' parquet <- tempfile(fileext = ".parquet")
#' pv_db_write(con, "cars", mtcars)
#' DBI::dbExecute(con, sprintf("COPY cars TO '%s' (FORMAT PARQUET)", parquet))
#' pv_query(con, sprintf(
#'   "SELECT cyl, AVG(mpg) AS mpg FROM '%s' GROUP BY cyl", parquet))
#' pv_db_disconnect(con)
#' @export
pv_query <- function(con, sql, params = NULL) {
  if (is.null(params)) {
    DBI::dbGetQuery(con, sql)
  } else {
    DBI::dbGetQuery(con, sql, params = params)
  }
}

#' Execute a SQL script file
#'
#' Runs every statement in a `.sql` file against a connection. Statements
#' are separated by semicolons; `--` line comments are stripped. Useful for
#' schema definitions and seed data kept as plain SQL alongside R code.
#'
#' @param con A connection from [pv_db_connect()].
#' @param path Path to a `.sql` file.
#' @return The number of statements executed, invisibly.
#' @examples
#' con <- pv_db_connect()
#' script <- system.file("sql", "demo.sql", package = "polyviz")
#' pv_run_sql_file(con, script)
#' pv_db_tables(con)
#' pv_db_disconnect(con)
#' @export
pv_run_sql_file <- function(con, path) {
  if (!file.exists(path)) {
    rlang::abort(sprintf("SQL file not found: %s", path))
  }
  # Read the whole file, throw away "--" comments, then split what is left
  # into individual statements at each semicolon. Blank chunks (for example
  # after the final semicolon) are dropped before running.
  lines <- readLines(path, warn = FALSE)
  lines <- sub("--.*$", "", lines)
  statements <- strsplit(paste(lines, collapse = "\n"), ";", fixed = TRUE)[[1]]
  statements <- trimws(statements)
  statements <- statements[nzchar(statements)]

  # Statements run one at a time, in file order, so later ones can rely on
  # tables that earlier ones created.
  for (s in statements) {
    DBI::dbExecute(con, s)
  }
  invisible(length(statements))
}
