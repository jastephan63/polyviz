expect_pvchart <- function(w, type) {
  expect_s3_class(w, "htmlwidget")
  expect_equal(attr(w, "package"), "polyviz")
  expect_equal(w$x$type, type)
  invisible(w)
}

# The renderer loads through the widget's shared dependency manifest -
# make sure hierarchy.js is actually registered there.
expect_hierarchy_dep <- function(w) {
  yaml <- readLines(system.file("htmlwidgets", "pvchart.yaml",
                                package = "polyviz"))
  expect_true(any(grepl("hierarchy\\.js", yaml)))
  invisible(w)
}

# Walk a nested {name, children/value} payload tree and collect leaves.
collect_leaves <- function(node) {
  if (is.null(node$children)) return(list(node))
  do.call(c, lapply(node$children, collect_leaves))
}

test_that("pack builds the nested tree with summed leaf values", {
  luzern <- pv_city_landuse[pv_city_landuse$city == "Luzern", ]
  w <- expect_pvchart(
    pv_pack(luzern, levels = c("group", "category"), value = "hectares"),
    "pack")
  expect_hierarchy_dep(w)
  expect_equal(w$x$root$name, "root")
  # split() orders the branches alphabetically by group name.
  expect_equal(
    vapply(w$x$root$children, function(n) n$name, character(1)),
    sort(unique(luzern$group)))
  # Every hectare ends up in exactly one leaf.
  leaves <- collect_leaves(w$x$root)
  expect_equal(sum(vapply(leaves, function(l) l$value, numeric(1))),
               sum(luzern$hectares))
  expect_equal(w$x$vlab, "hectares")
  expect_equal(w$x$labels, "auto")
})

test_that("pack aggregates duplicate leaves across the dropped columns", {
  # Summed over all cities: one leaf per group/category pair, not per row.
  w <- pv_pack(pv_city_landuse, levels = c("group", "category"),
               value = "hectares")
  leaves <- collect_leaves(w$x$root)
  pairs <- unique(pv_city_landuse[c("group", "category")])
  expect_equal(length(leaves), nrow(pairs))
  expect_equal(sum(vapply(leaves, function(l) l$value, numeric(1))),
               sum(pv_city_landuse$hectares))
})

test_that("pack validates columns, levels, groups, and the labels flag", {
  luzern <- pv_city_landuse[pv_city_landuse$city == "Luzern", ]
  expect_error(pv_pack(luzern, levels = "nope", value = "hectares"),
               "not in `data`")
  expect_error(pv_pack(luzern, levels = character(0), value = "hectares"),
               "at least one")
  # 181 cities as the top level blows the 8-colour palette.
  expect_error(pv_pack(pv_city_landuse, levels = c("city", "category"),
                       value = "hectares"), "8")
  expect_error(pv_pack(luzern, levels = "group", value = "hectares",
                       labels = "sometimes"), 'TRUE, FALSE, or "auto"')
  wt <- pv_pack(luzern, levels = "group", value = "hectares", labels = TRUE)
  expect_true(wt$x$labels)
})

# A tiny deterministic tree: heights 1, 4, and 8 apart.
small_hclust <- function() {
  d <- stats::dist(c(a = 0, b = 1, c = 5, d = 13))
  stats::hclust(d, method = "single")
}

test_that("dendrogram unfolds merge/height into a nested tree", {
  hc <- small_hclust()
  w <- expect_pvchart(pv_dendrogram(hc), "dendrogram")
  expect_hierarchy_dep(w)
  # The root is the last merge, at the largest height.
  expect_equal(w$x$tree$height, max(hc$height))
  expect_equal(length(w$x$tree$children), 2)
  # Every observation appears exactly once as a leaf, at height 0,
  # named from hc$labels.
  leaves <- collect_leaves(w$x$tree)
  expect_equal(sort(vapply(leaves, function(l) l$name, character(1))),
               c("a", "b", "c", "d"))
  expect_equal(vapply(leaves, function(l) l$height, numeric(1)),
               rep(0, 4))
  expect_null(w$x$k)
})

test_that("dendrogram leaf labels fall back and can be overridden", {
  hc <- small_hclust()
  hc$labels <- NULL
  # No labels anywhere: leaves take the observation index.
  w <- pv_dendrogram(hc)
  expect_equal(sort(vapply(collect_leaves(w$x$tree), function(l) l$name,
                           character(1))),
               c("1", "2", "3", "4"))
  # An explicit labels vector wins.
  w2 <- pv_dendrogram(hc, labels = c("w", "x", "y", "z"))
  expect_equal(sort(vapply(collect_leaves(w2$x$tree), function(l) l$name,
                           character(1))),
               c("w", "x", "y", "z"))
  expect_error(pv_dendrogram(hc, labels = c("w", "x")), "one entry per leaf")
})

test_that("dendrogram k cuts the tree and tags every leaf", {
  hc <- small_hclust()
  w <- pv_dendrogram(hc, k = 2)
  expect_equal(w$x$k, 2L)
  leaves <- collect_leaves(w$x$tree)
  got <- vapply(leaves, function(l) l$cluster, integer(1))
  names(got) <- vapply(leaves, function(l) l$name, character(1))
  want <- stats::cutree(hc, k = 2)
  expect_equal(got[names(want)], want)
})

test_that("dendrogram validates hc, k, and labels", {
  expect_error(pv_dendrogram(list(merge = 1)), "hclust")
  hc <- small_hclust()
  expect_error(pv_dendrogram(hc, k = 1), "at least 2")
  expect_error(pv_dendrogram(hc, k = 2.5), "at least 2")
  # More clusters than leaves (and, on big trees, than palette slots).
  expect_error(pv_dendrogram(hc, k = 5), "at most 4")
  many <- stats::hclust(stats::dist(seq_len(20)), method = "single")
  expect_error(pv_dendrogram(many, k = 9), "at most 8")
})

test_that("icicle builds the nested tree with summed leaf values", {
  luzern <- pv_city_landuse[pv_city_landuse$city == "Luzern", ]
  w <- expect_pvchart(
    pv_icicle(luzern, levels = c("group", "category"), value = "hectares"),
    "icicle")
  expect_hierarchy_dep(w)
  expect_equal(w$x$root$name, "root")
  # split() orders the branches alphabetically by group name.
  expect_equal(
    vapply(w$x$root$children, function(n) n$name, character(1)),
    sort(unique(luzern$group)))
  # Every hectare ends up in exactly one leaf.
  leaves <- collect_leaves(w$x$root)
  expect_equal(sum(vapply(leaves, function(l) l$value, numeric(1))),
               sum(luzern$hectares))
  expect_equal(w$x$vlab, "hectares")
  expect_equal(w$x$labels, "auto")
})

test_that("icicle shares the sunburst's data contract exactly", {
  # Same levels/value arguments, same hierarchy building: the payload
  # tree must be identical to what pv_sunburst ships for the same data.
  luzern <- pv_city_landuse[pv_city_landuse$city == "Luzern", ]
  ici <- pv_icicle(luzern, levels = c("group", "category"),
                   value = "hectares")
  sun <- pv_sunburst(luzern, levels = c("group", "category"),
                     value = "hectares")
  expect_identical(ici$x$root, sun$x$root)
})

test_that("icicle validates columns, levels, values, groups, and labels", {
  luzern <- pv_city_landuse[pv_city_landuse$city == "Luzern", ]
  expect_error(pv_icicle(luzern, levels = "nope", value = "hectares"),
               "not in `data`")
  expect_error(pv_icicle(luzern, levels = character(0), value = "hectares"),
               "at least one")
  neg <- data.frame(g = c("a", "b"), v = c(5, -1))
  expect_error(pv_icicle(neg, levels = "g", value = "v"), "non-negative")
  # 181 cities as the top level blows the 8-colour palette.
  expect_error(pv_icicle(pv_city_landuse, levels = c("city", "category"),
                         value = "hectares"), "8")
  expect_error(pv_icicle(luzern, levels = "group", value = "hectares",
                         labels = "sometimes"), 'TRUE, FALSE, or "auto"')
  wt <- pv_icicle(luzern, levels = "group", value = "hectares",
                  labels = TRUE)
  expect_true(wt$x$labels)
  wf <- pv_icicle(luzern, levels = "group", value = "hectares",
                  labels = FALSE)
  expect_false(wf$x$labels)
})

test_that("icicle generates alt text without a dedicated describer", {
  luzern <- pv_city_landuse[pv_city_landuse$city == "Luzern", ]
  alt <- pv_alt_text(pv_icicle(luzern, levels = c("group", "category"),
                               value = "hectares", title = "Land use"))
  expect_true(is.character(alt) && length(alt) == 1 && nzchar(alt))
})

test_that("icicle renders without JavaScript errors", {
  render_skip_if_no_chrome()
  luzern <- pv_city_landuse[pv_city_landuse$city == "Luzern", ]
  w <- pv_icicle(luzern, levels = c("group", "category"),
                 value = "hectares",
                 title = "Land use in the city of Lucerne")
  path <- tempfile("icicle-", fileext = ".png")
  on.exit(unlink(path), add = TRUE)
  expect_no_warning(pv_save(w, path, quiet = TRUE))
  expect_true(file.exists(path))
  expect_gt(file.size(path), 20000)
})

test_that("gallery dendrogram data holds up: 25 cities, ward.D2, k = 4", {
  latest <- pv_city_population[
    pv_city_population$year == max(pv_city_population$year), ]
  top <- head(latest$city[order(-latest$population)], 25)
  sect <- pv_city_sectors[pv_city_sectors$city %in% top, ]
  wide <- stats::xtabs(share ~ city + sector_code, data = sect)
  hc <- stats::hclust(stats::dist(scale(wide)), method = "ward.D2")
  w <- pv_dendrogram(hc, k = 4)
  leaves <- collect_leaves(w$x$tree)
  expect_equal(length(leaves), 25)
  expect_equal(sort(unique(vapply(leaves, function(l) l$cluster,
                                  integer(1)))), 1:4)
  expect_equal(sort(vapply(leaves, function(l) l$name, character(1))),
               sort(rownames(wide)))
})
