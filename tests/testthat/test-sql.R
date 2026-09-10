test_that("round trip through SQLite works", {
  con <- pv_db_connect()
  on.exit(pv_db_disconnect(con))
  pv_db_write(con, "cars", mtcars)
  expect_identical(pv_db_tables(con), "cars")
  out <- pv_query(con, "SELECT COUNT(*) AS n FROM cars")
  expect_equal(out$n, nrow(mtcars))
})

test_that("parameterised queries bind values", {
  con <- pv_db_connect()
  on.exit(pv_db_disconnect(con))
  pv_db_write(con, "cars", mtcars)
  out <- pv_query(con, "SELECT * FROM cars WHERE cyl = ?", params = list(6))
  expect_equal(nrow(out), sum(mtcars$cyl == 6))
})

test_that("SQL script files execute", {
  con <- pv_db_connect()
  on.exit(pv_db_disconnect(con))
  script <- system.file("sql", "demo.sql", package = "polyviz")
  n <- pv_run_sql_file(con, script)
  expect_gte(n, 3)
  dim_tbl <- pv_query(con, "SELECT * FROM product_dim ORDER BY product")
  expect_equal(nrow(dim_tbl), 5)
  expect_equal(dim_tbl$product[1], "Aster")
})

test_that("missing script file errors cleanly", {
  con <- pv_db_connect()
  on.exit(pv_db_disconnect(con))
  expect_error(pv_run_sql_file(con, "nope.sql"), "not found")
})

# The DuckDB half mirrors the SQLite tests above: every function is
# meant to behave identically on either engine.

test_that("round trip through DuckDB works", {
  skip_if_not_installed("duckdb")
  con <- pv_db_connect(driver = "duckdb")
  on.exit(pv_db_disconnect(con))
  pv_db_write(con, "cars", mtcars)
  expect_identical(pv_db_tables(con), "cars")
  out <- pv_query(con, "SELECT COUNT(*) AS n FROM cars")
  expect_equal(out$n, nrow(mtcars))
})

test_that("parameterised queries bind values on DuckDB", {
  skip_if_not_installed("duckdb")
  con <- pv_db_connect(driver = "duckdb")
  on.exit(pv_db_disconnect(con))
  pv_db_write(con, "cars", mtcars)
  out <- pv_query(con, "SELECT * FROM cars WHERE cyl = ?", params = list(6))
  expect_equal(nrow(out), sum(mtcars$cyl == 6))
})

test_that("SQL script files execute on DuckDB", {
  skip_if_not_installed("duckdb")
  con <- pv_db_connect(driver = "duckdb")
  on.exit(pv_db_disconnect(con))
  script <- system.file("sql", "demo.sql", package = "polyviz")
  n <- pv_run_sql_file(con, script)
  expect_gte(n, 3)
  dim_tbl <- pv_query(con, "SELECT * FROM product_dim ORDER BY product")
  expect_equal(nrow(dim_tbl), 5)
  expect_equal(dim_tbl$product[1], "Aster")
})

test_that("driver auto-detection picks the engine from the extension", {
  skip_if_not_installed("duckdb")
  ddb <- withr::local_tempfile(fileext = ".duckdb")
  con <- pv_db_connect(ddb)
  expect_s4_class(con, "duckdb_connection")
  pv_db_disconnect(con)

  sqlite <- withr::local_tempfile(fileext = ".sqlite")
  con <- pv_db_connect(sqlite)
  expect_s4_class(con, "SQLiteConnection")
  pv_db_disconnect(con)
})

test_that(":memory: stays SQLite unless DuckDB is asked for", {
  con <- pv_db_connect()
  expect_s4_class(con, "SQLiteConnection")
  pv_db_disconnect(con)
})

test_that("an explicit driver = \"duckdb\" works for any path", {
  skip_if_not_installed("duckdb")
  con <- pv_db_connect(":memory:", driver = "duckdb")
  expect_s4_class(con, "duckdb_connection")
  pv_db_disconnect(con)
})

test_that("DuckDB queries parquet files straight from disk", {
  skip_if_not_installed("duckdb")
  parquet <- withr::local_tempfile(fileext = ".parquet")
  con <- pv_db_connect(driver = "duckdb")
  on.exit(pv_db_disconnect(con))
  pv_db_write(con, "cars", mtcars)
  DBI::dbExecute(con, sprintf("COPY cars TO '%s' (FORMAT PARQUET)", parquet))
  out <- pv_query(con, sprintf(
    "SELECT cyl, AVG(mpg) AS mpg FROM '%s' GROUP BY cyl ORDER BY cyl",
    parquet))
  expect_equal(out$cyl, c(4, 6, 8))
  expect_equal(out$mpg[1], mean(mtcars$mpg[mtcars$cyl == 4]))
})

test_that("asking for DuckDB without the package errors clearly", {
  local_mocked_bindings(sql_has_duckdb = function() FALSE)
  expect_error(pv_db_connect(driver = "duckdb"), "duckdb package")
  expect_error(pv_db_connect("x.duckdb"), 'install.packages\\("duckdb"\\)')
})
