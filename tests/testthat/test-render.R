# The end-to-end render safety net. The rest of the suite checks the R
# side of every chart - the payload handed to JavaScript - but never runs
# the JavaScript. These tests do: the canonical chart of each type (built
# in helper-render.R) is saved as a real PNG, which makes headless Chrome
# execute the full d3 pipeline, and pv_save() turns any JavaScript error
# raised on the page into an R warning. So "no warning" means "the
# renderer actually drew this chart". One test per type keeps the failure
# message naming the broken renderer.

test_that("every chart type has a render check", {
  # The names in helper-render.R must stay in lockstep with the widget
  # type strings the JavaScript dispatches on. A new chart type has to
  # show up here, or it ships with no render coverage.
  expect_setequal(names(render_charts), c(
    "bar", "line", "scatter", "force", "chord", "sunburst",
    "histogram", "boxplot", "violin", "ridgeline",
    "donut", "treemap", "lollipop",
    "area", "heatmap", "calendar",
    "sankey", "parallel",
    "pack", "dendrogram",
    "choropleth", "race", "bump", "beeswarm"))
  for (id in names(render_charts)) {
    w <- render_charts[[id]]()
    expect_s3_class(w, "pvchart")
    expect_identical(w$x$type, id)
  }
})

test_that("bar renders without JavaScript errors", {
  expect_chart_renders("bar")
})

test_that("line renders without JavaScript errors", {
  expect_chart_renders("line")
})

test_that("scatter renders without JavaScript errors", {
  expect_chart_renders("scatter")
})

test_that("force renders without JavaScript errors", {
  expect_chart_renders("force")
})

test_that("chord renders without JavaScript errors", {
  expect_chart_renders("chord")
})

test_that("sunburst renders without JavaScript errors", {
  expect_chart_renders("sunburst")
})

test_that("histogram renders without JavaScript errors", {
  expect_chart_renders("histogram")
})

test_that("boxplot renders without JavaScript errors", {
  expect_chart_renders("boxplot")
})

test_that("violin renders without JavaScript errors", {
  expect_chart_renders("violin")
})

test_that("ridgeline renders without JavaScript errors", {
  expect_chart_renders("ridgeline")
})

test_that("donut renders without JavaScript errors", {
  expect_chart_renders("donut")
})

test_that("treemap renders without JavaScript errors", {
  expect_chart_renders("treemap")
})

test_that("lollipop renders without JavaScript errors", {
  expect_chart_renders("lollipop")
})

test_that("area renders without JavaScript errors", {
  expect_chart_renders("area")
})

test_that("heatmap renders without JavaScript errors", {
  expect_chart_renders("heatmap")
})

test_that("calendar renders without JavaScript errors", {
  expect_chart_renders("calendar")
})

test_that("sankey renders without JavaScript errors", {
  expect_chart_renders("sankey")
})

test_that("parallel renders without JavaScript errors", {
  expect_chart_renders("parallel")
})

test_that("pack renders without JavaScript errors", {
  expect_chart_renders("pack")
})

test_that("dendrogram renders without JavaScript errors", {
  expect_chart_renders("dendrogram")
})

test_that("choropleth renders without JavaScript errors", {
  expect_chart_renders("choropleth")
})

test_that("race renders without JavaScript errors", {
  expect_chart_renders("race")
})

test_that("bump renders without JavaScript errors", {
  expect_chart_renders("bump")
})

test_that("beeswarm renders without JavaScript errors", {
  expect_chart_renders("beeswarm")
})
