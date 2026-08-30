expect_pvchart <- function(w, type) {
  expect_s3_class(w, "htmlwidget")
  expect_equal(attr(w, "package"), "polyviz")
  expect_equal(w$x$type, type)
  invisible(w)
}

commuter_links <- function() {
  latest <- pv_commuters[pv_commuters$period == max(pv_commuters$period), ]
  data.frame(
    source = ifelse(latest$direction == "to Zug", latest$region, "Zug"),
    target = ifelse(latest$direction == "to Zug", "Zug",
                    paste0(latest$region, " ")),
    value = latest$commuters
  )
}

test_that("sankey builds nodes in first-appearance order with index links", {
  links <- commuter_links()
  w <- expect_pvchart(pv_sankey(links), "sankey")
  # 5 regions in, Zug, 5 regions out (trailing space) = 11 nodes.
  expect_equal(nrow(w$x$nodes), 11)
  expect_equal(w$x$nodes$name[1], links$source[1])
  # Links carry 0-based indices back into the node list.
  expect_true(all(w$x$links$source %in% 0:(nrow(w$x$nodes) - 1)))
  expect_true(all(w$x$links$target %in% 0:(nrow(w$x$nodes) - 1)))
  expect_equal(w$x$nodes$name[w$x$links$source + 1], links$source)
  expect_equal(w$x$nodes$name[w$x$links$target + 1], links$target)
  expect_equal(w$x$links$value, links$value)
  expect_equal(w$x$align, "justify")
})

test_that("sankey validates columns, align, and values", {
  links <- commuter_links()
  expect_error(pv_sankey(links, source = "nope"), "not in `data`")
  expect_error(pv_sankey(links, align = "diagonal"))
  bad <- links
  bad$value[1] <- NA
  expect_error(pv_sankey(bad), "non-negative")
  bad$value[1] <- -5
  expect_error(pv_sankey(bad), "non-negative")
})

test_that("sankey names one offending cycle", {
  loop <- data.frame(source = c("A", "B", "C", "C"),
                     target = c("B", "C", "A", "D"),
                     value = c(1, 2, 3, 4))
  expect_error(pv_sankey(loop), "A -> B -> C -> A")
  self <- data.frame(source = "A", target = "A", value = 1)
  expect_error(pv_sankey(self), "A -> A")
  # An acyclic diamond must pass: two paths to one node is not a cycle.
  diamond <- data.frame(source = c("A", "A", "B", "C"),
                        target = c("B", "C", "D", "D"),
                        value = 1:4)
  expect_pvchart(pv_sankey(diamond), "sankey")
})

landuse_wide <- function() {
  lu <- pv_city_landuse
  lu$share <- 100 * lu$hectares / ave(lu$hectares, lu$city, FUN = sum)
  keep <- c("Buildings", "Agriculture", "Forest")
  wide <- reshape(lu[lu$category %in% keep, c("city", "category", "share")],
                  direction = "wide", idvar = "city", timevar = "category")
  names(wide) <- sub("^share\\.", "", names(wide))
  wide
}

test_that("parallel packs axis columns as v1..vk plus display names", {
  wide <- landuse_wide()
  cols <- c("Buildings", "Agriculture", "Forest")
  w <- expect_pvchart(pv_parallel(wide, cols, label = "city"), "parallel")
  expect_equal(w$x$columns, cols)
  expect_true(all(c("v1", "v2", "v3", "label") %in% names(w$x$data)))
  expect_equal(w$x$data$v1, wide$Buildings)
  expect_equal(w$x$data$label, wide$city)
  expect_false(w$x$showLegend)
})

test_that("parallel enforces 2-8 numeric columns", {
  wide <- landuse_wide()
  expect_error(pv_parallel(wide, "Buildings"), "between 2 and 8")
  expect_error(pv_parallel(wide, rep(c("Buildings", "Forest"), 5)),
               "between 2 and 8")
  expect_error(pv_parallel(wide, c("city", "Buildings")), "not numeric")
  expect_error(pv_parallel(wide, c("Buildings", "nope")), "not in `data`")
})

test_that("parallel caps colour levels at 3 like scatter", {
  wide <- landuse_wide()
  wide$g <- rep_len(letters[1:3], nrow(wide))
  w <- pv_parallel(wide, c("Buildings", "Forest"), color = "g")
  expect_true(w$x$showLegend)
  expect_equal(sort(unique(w$x$data$series)), c("a", "b", "c"))
  wide$g4 <- rep_len(letters[1:4], nrow(wide))
  expect_error(pv_parallel(wide, c("Buildings", "Forest"), color = "g4"), "3")
})

test_that("parallel drops rows with a missing axis value", {
  wide <- landuse_wide()
  wide$Forest[2] <- NA
  w <- pv_parallel(wide, c("Buildings", "Forest"), label = "city")
  expect_equal(nrow(w$x$data), nrow(wide) - 1)
  expect_false(wide$city[2] %in% w$x$data$label)
})
