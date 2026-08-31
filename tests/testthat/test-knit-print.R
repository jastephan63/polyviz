# The generated NAMESPACE registers knit_print.pvchart with knitr at
# install time; these tests run from load_all, so register it by hand.
if (requireNamespace("knitr", quietly = TRUE)) {
  registerS3method("knit_print", "pvchart", polyviz:::knit_print.pvchart,
                   envir = asNamespace("knitr"))
}

knit_skip_if_no_chrome <- function() {
  skip_on_cran()
  skip_if_not_installed("chromote")
  chrome <- tryCatch(chromote::find_chrome(), error = function(e) NULL)
  skip_if(is.null(chrome) || !nzchar(chrome), "no Chrome-based browser found")
}

knit_png_size <- function(path) {
  bytes <- readBin(path, "raw", n = 24)
  c(width = sum(as.integer(bytes[17:20]) * 256^(3:0)),
    height = sum(as.integer(bytes[21:24]) * 256^(3:0)))
}

test_that("charts knit to PNG figures in markdown output", {
  skip_on_cran()
  skip_if_not_installed("knitr")
  knit_skip_if_no_chrome()
  dir <- withr::local_tempdir()
  writeLines(c(
    "---", "title: Sales note", "---", "",
    "```{r setup, include=FALSE}",
    "knitr::opts_chunk$set(screenshot.force = FALSE)",
    "```", "",
    "Some prose before the chart.", "",
    "```{r chart, fig.width=6, fig.height=4, dpi=96}",
    "agg <- aggregate(revenue ~ region, pv_sales, sum)",
    "pv_bar(agg, \"region\", \"revenue\", title = \"Revenue by region\")",
    "pv_bar(agg, \"region\", \"revenue\", title = \"Second widget\")",
    "```", "",
    "```{r big, fig.width=6, fig.height=4, dpi=192}",
    "pv_bar(agg, \"region\", \"revenue\", title = \"Retina widget\")",
    "```", ""), file.path(dir, "doc.Rmd"))
  withr::local_dir(dir)
  knitr::knit("doc.Rmd", "doc.md", quiet = TRUE,
              envir = new.env(parent = globalenv()))
  md <- paste(readLines("doc.md", warn = FALSE), collapse = "\n")
  # two widgets in one chunk get their own files; the next chunk restarts
  expect_match(md, "figure/chart-pv-1.png", fixed = TRUE)
  expect_match(md, "figure/chart-pv-2.png", fixed = TRUE)
  expect_match(md, "figure/big-pv-1.png", fixed = TRUE)
  expect_false(grepl("chart-pv-3", md, fixed = TRUE))
  expect_true(file.exists("figure/chart-pv-1.png"))
  expect_gt(file.size("figure/chart-pv-1.png"), 5000)
  # fig.width * dpi pixels: 6in x 4in at 96 and at 192 dpi
  expect_identical(knit_png_size("figure/chart-pv-1.png"),
                   c(width = 576, height = 384))
  expect_identical(knit_png_size("figure/big-pv-1.png"),
                   c(width = 1152, height = 768))
  # a second knit of the same document restarts the numbering, so the
  # references (and files) stay stable instead of accumulating
  knitr::knit("doc.Rmd", "doc2.md", quiet = TRUE,
              envir = new.env(parent = globalenv()))
  md2 <- paste(readLines("doc2.md", warn = FALSE), collapse = "\n")
  expect_match(md2, "figure/chart-pv-1.png", fixed = TRUE)
  expect_false(grepl("chart-pv-3", md2, fixed = TRUE))
})

test_that("html output gets the live widget, not a PNG", {
  skip_if_not_installed("knitr")
  w <- pv_bar(aggregate(revenue ~ region, pv_sales, sum),
              "region", "revenue")
  withr::local_options(list(knitr.in.progress = TRUE))
  knitr::opts_knit$set("rmarkdown.pandoc.to" = "html")
  withr::defer(knitr::opts_knit$set("rmarkdown.pandoc.to" = NULL))
  res <- knitr::knit_print(w, options = list(screenshot.force = FALSE))
  expect_s3_class(res, "knit_asis")
})

test_that("printing outside a knit falls through to the widget path", {
  skip_if_not_installed("knitr")
  w <- pv_bar(aggregate(revenue ~ region, pv_sales, sum),
              "region", "revenue")
  res <- knitr::knit_print(w, options = list(screenshot.force = FALSE))
  expect_s3_class(res, "knit_asis")
})

test_that("chunk figure options translate to pixels, with a fallback", {
  fig_size <- polyviz:::knit_fig_size
  expect_identical(fig_size(list()),
                   list(width = 900, height = NULL, scale = 2))
  expect_identical(
    fig_size(list(fig.width = 6, fig.height = 4, dpi = 96)),
    list(width = 576, height = 384, scale = 1))
  expect_identical(
    fig_size(list(fig.width = 6, fig.height = 4, dpi = 192)),
    list(width = 576, height = 384, scale = 2))
  expect_identical(fig_size(list(fig.width = NA_real_)),
                   list(width = 900, height = NULL, scale = 2))
})

test_that("figure numbering counts within a chunk and resets by label", {
  skip_if_not_installed("knitr")
  fig_number <- polyviz:::knit_fig_number
  knitr::opts_knit$set(polyviz.fig.count = NULL)
  withr::defer(knitr::opts_knit$set(polyviz.fig.count = NULL))
  a <- list(fig.path = "figure/", label = "a")
  expect_identical(fig_number(a), 1L)
  expect_identical(fig_number(a), 2L)
  b <- list(fig.path = "figure/", label = "b")
  expect_identical(fig_number(b), 1L)
  expect_identical(fig_number(a), 1L)
})
