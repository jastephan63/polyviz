test_that("pv_read dispatches on extension", {
  csv <- withr::local_tempfile(fileext = ".csv")
  write.csv(mtcars, csv, row.names = FALSE)
  expect_equal(nrow(pv_read(csv)), nrow(mtcars))

  rds <- withr::local_tempfile(fileext = ".rds")
  saveRDS(iris, rds)
  expect_equal(pv_read(rds), iris)
})

test_that("pv_read reads SQLite databases", {
  db <- withr::local_tempfile(fileext = ".sqlite")
  con <- pv_db_connect(db)
  pv_db_write(con, "one", mtcars)
  pv_db_disconnect(con)
  expect_equal(nrow(pv_read(db)), nrow(mtcars))

  con <- pv_db_connect(db)
  pv_db_write(con, "two", iris)
  pv_db_disconnect(con)
  expect_error(pv_read(db), "pick one")
  expect_equal(nrow(pv_read(db, table = "two")), nrow(iris))
  expect_error(pv_read(db, table = "three"), "Available")
})

test_that("pv_read errors are clear", {
  expect_error(pv_read("missing.csv"), "not found")
  bad <- withr::local_tempfile(fileext = ".xyz")
  writeLines("x", bad)
  expect_error(pv_read(bad), "Unsupported")
})

test_that("pv_read reads DuckDB databases", {
  skip_if_not_installed("duckdb")
  db <- withr::local_tempfile(fileext = ".duckdb")
  con <- pv_db_connect(db)
  pv_db_write(con, "one", mtcars)
  pv_db_disconnect(con)
  expect_equal(nrow(pv_read(db)), nrow(mtcars))

  con <- pv_db_connect(db)
  pv_db_write(con, "two", iris)
  pv_db_disconnect(con)
  expect_error(pv_read(db), "pick one")
  expect_equal(nrow(pv_read(db, table = "two")), nrow(iris))
  expect_error(pv_read(db, table = "three"), "Available")
})

test_that("pv_read round-trips parquet files through DuckDB", {
  skip_if_not_installed("duckdb")
  parquet <- withr::local_tempfile(fileext = ".parquet")
  con <- pv_db_connect(driver = "duckdb")
  pv_db_write(con, "cars", mtcars)
  DBI::dbExecute(con, sprintf("COPY cars TO '%s' (FORMAT PARQUET)", parquet))
  pv_db_disconnect(con)

  out <- pv_read(parquet)
  expect_equal(nrow(out), nrow(mtcars))
  expect_equal(names(out), names(mtcars))
  expect_equal(out$mpg, mtcars$mpg)
})

test_that("reading parquet without duckdb names the dependency", {
  local_mocked_bindings(sql_has_duckdb = function() FALSE)
  parquet <- withr::local_tempfile(fileext = ".parquet")
  writeLines("x", parquet)
  expect_error(pv_read(parquet), 'install.packages\\("duckdb"\\)')
})
