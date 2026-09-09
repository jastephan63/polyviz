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
    "bar", "line", "scatter", "force", "chord", "arc", "sunburst",
    "histogram", "boxplot", "violin", "ridgeline",
    "donut", "waffle", "treemap", "icicle", "lollipop",
    "slope", "dumbbell", "waterfall", "bullet",
    "area", "heatmap", "calendar", "horizon",
    "sankey", "parallel",
    "pack", "dendrogram",
    "choropleth", "bubblemap", "flowmap", "race", "bump", "beeswarm",
    "pairs", "table"))
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

test_that("arc renders without JavaScript errors", {
  expect_chart_renders("arc")
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

test_that("waffle renders without JavaScript errors", {
  expect_chart_renders("waffle")
})

test_that("treemap renders without JavaScript errors", {
  expect_chart_renders("treemap")
})

test_that("icicle renders without JavaScript errors", {
  expect_chart_renders("icicle")
})

test_that("lollipop renders without JavaScript errors", {
  expect_chart_renders("lollipop")
})

test_that("slope renders without JavaScript errors", {
  expect_chart_renders("slope")
})

test_that("dumbbell renders without JavaScript errors", {
  expect_chart_renders("dumbbell")
})

test_that("waterfall renders without JavaScript errors", {
  expect_chart_renders("waterfall")
})

test_that("bullet renders without JavaScript errors", {
  expect_chart_renders("bullet")
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

test_that("horizon renders without JavaScript errors", {
  expect_chart_renders("horizon")
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

test_that("bubblemap renders without JavaScript errors", {
  expect_chart_renders("bubblemap")
})

test_that("flowmap renders without JavaScript errors", {
  expect_chart_renders("flowmap")
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

test_that("pairs renders without JavaScript errors", {
  expect_chart_renders("pairs")
})

test_that("table renders without JavaScript errors", {
  expect_chart_renders("table")
})

# The opt-in chart options ship extra drawing code the canonical charts
# above never reach - stacked layouts, marker dots, curve interpolation,
# the zoom strip, violin overlays, texture patterns, locale formatting,
# the scatter's density contours, its hexagon bins, and its canvas mark
# layer. One variant per option keeps those paths under the same render
# contract.

test_that("every option variant builds its base chart type", {
  expect_setequal(names(render_variants), c(
    "bar_stacked", "bar_percent", "bar_textured", "bar_locale",
    "violin_overlays", "line_markers", "line_zoom",
    "scatter_density", "scatter_canvas", "scatter_hex",
    "choropleth_cantons"))
  for (id in names(render_variants)) {
    w <- render_variants[[id]]()
    expect_s3_class(w, "pvchart")
    expect_identical(w$x$type, sub("_.*$", "", id))
  }
})

test_that("stacked bars render without JavaScript errors", {
  expect_chart_renders("bar_stacked")
})

test_that("percent-stacked bars render without JavaScript errors", {
  expect_chart_renders("bar_percent")
})

test_that("textured bars under the paper theme render without JavaScript errors", {
  expect_chart_renders("bar_textured")
})

test_that("de-CH locale bars render without JavaScript errors", {
  expect_chart_renders("bar_locale")
})

test_that("violin with box and points renders without JavaScript errors", {
  expect_chart_renders("violin_overlays")
})

test_that("line with markers and curve renders without JavaScript errors", {
  expect_chart_renders("line_markers")
})

test_that("zoomed line renders without JavaScript errors", {
  expect_chart_renders("line_zoom")
})

test_that("scatter density contours render without JavaScript errors", {
  expect_chart_renders("scatter_density")
})

test_that("scatter canvas marks render without JavaScript errors", {
  expect_chart_renders("scatter_canvas")
})

test_that("scatter hexagon bins render without JavaScript errors", {
  expect_chart_renders("scatter_hex")
})

# The country-wide geo paths: the cantons layer with its lakes overlay
# under the choropleth, and the bubble map renderer end to end. Both run
# on bundled layers only - no test may touch the network.

test_that("cantons choropleth renders without JavaScript errors", {
  expect_chart_renders("choropleth_cantons")
})

