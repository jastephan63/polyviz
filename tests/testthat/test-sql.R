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
