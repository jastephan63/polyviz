#' Connect to a SQLite database
#'
#' @param path Path to a SQLite file, or `":memory:"` for an in-memory
#'   database.
#' @return A `DBIConnection`. Close it with [pv_db_disconnect()].
#' @examples
#' con <- pv_db_connect()
#' pv_db_write(con, "cars", mtcars)
#' pv_query(con, "SELECT COUNT(*) AS n FROM cars")
#' pv_db_disconnect(con)
#' @export
pv_db_connect <- function(path = ":memory:") {
  DBI::dbConnect(RSQLite::SQLite(), path)
}

#' Disconnect from a database
#'
#' @param con A connection from [pv_db_connect()].
#' @return `TRUE`, invisibly.
#' @export
pv_db_disconnect <- function(con) {
  invisible(DBI::dbDisconnect(con))
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
#' @examples
#' con <- pv_db_connect()
#' pv_db_write(con, "cars", mtcars)
#' pv_query(con, "SELECT cyl, AVG(mpg) AS mpg FROM cars GROUP BY cyl")
#' pv_query(con, "SELECT * FROM cars WHERE cyl = ?", params = list(6))
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
