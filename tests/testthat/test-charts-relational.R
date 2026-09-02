expect_pvchart <- function(w, type) {
  expect_s3_class(w, "htmlwidget")
  expect_equal(attr(w, "package"), "polyviz")
  expect_equal(w$x$type, type)
  invisible(w)
}

# The renderer loads through the widget's shared dependency manifest -
# make sure relational.js is actually registered there.
expect_relational_dep <- function(w) {
  yaml <- readLines(system.file("htmlwidgets", "pvchart.yaml",
                                package = "polyviz"))
  expect_true(any(grepl("relational\\.js", yaml)))
  invisible(w)
}

# An independent brute-force crossing counter for a shipped payload: two
# arcs cross exactly when their endpoint slots strictly interleave. Kept
# deliberately different from the package's own counter, so the ordering
# tests don't check a function against itself.
payload_crossings <- function(w) {
  pos <- stats::setNames(seq_len(nrow(w$x$nodes)), w$x$nodes$id)
  lk <- w$x$links
  a <- pmin(pos[lk$source], pos[lk$target])
  b <- pmax(pos[lk$source], pos[lk$target])
  if (length(a) < 2) {
    return(0L)
  }
  sum(utils::combn(seq_along(a), 2, function(p) {
    i <- p[1]
    j <- p[2]
    (a[i] < a[j] && a[j] < b[i] && b[i] < b[j]) ||
      (a[j] < a[i] && a[i] < b[j] && b[j] < b[i])
  }))
}

test_that("arc ships nodes, links, and group levels", {
  w <- expect_pvchart(
    pv_arc(pv_network$nodes, pv_network$links, group = "group"), "arc")
  expect_relational_dep(w)
  expect_setequal(w$x$nodes$id, pv_network$nodes$id)
  # Labels default to the id column.
  expect_equal(w$x$nodes$label, w$x$nodes$id)
  expect_equal(w$x$links$source, pv_network$links$source)
  expect_equal(w$x$links$target, pv_network$links$target)
  expect_equal(w$x$links$value, as.numeric(pv_network$links$value))
  # Group levels travel in first-appearance order over the caller's
  # nodes - the colour order - not in display order.
  expect_equal(as.character(w$x$groups), unique(pv_network$nodes$group))
})

test_that("arc defaults link values to 1 and omits groups without a mapping", {
  nodes <- data.frame(id = c("A", "B", "C"))
  links <- data.frame(source = c("A", "B"), target = c("B", "C"))
  w <- expect_pvchart(pv_arc(nodes, links), "arc")
  expect_equal(w$x$links$value, c(1, 1))
  expect_null(w$x$groups)
  expect_null(w$x$nodes$group)
})

test_that("arc validates nodes and links like its relational siblings", {
  nodes <- data.frame(id = c("A", "B", "C"))
  links <- data.frame(source = "A", target = "B", value = 2)
  expect_error(pv_arc(nodes, links, id = "nope"), "not in `data`")
  expect_error(pv_arc(data.frame(id = character(0)), links),
               "no rows")
  expect_error(pv_arc(nodes, data.frame(source = "A")), "not in `data`")
  bad <- data.frame(source = "A", target = "D", value = 1)
  expect_error(pv_arc(nodes, bad), "unknown node ids: D")
  # Duplicated node ids would stack two dots on one slot.
  expect_error(
    pv_arc(data.frame(id = c("A", "A", "B")), links),
    "duplicated ids: A")
})

test_that("arc refuses self-links and duplicate pairs", {
  nodes <- data.frame(id = c("A", "B"))
  expect_error(
    pv_arc(nodes, data.frame(source = "A", target = "A", value = 1)),
    "connect a node to itself \\(A\\)")
  dup <- data.frame(source = c("A", "A"), target = c("B", "B"),
                    value = c(1, 2))
  expect_error(pv_arc(nodes, dup),
               "more than one row for the same source/target pair")
  # Opposite directions are two different flows, not a duplicate.
  both <- data.frame(source = c("A", "B"), target = c("B", "A"),
                     value = c(1, 2))
  expect_pvchart(pv_arc(nodes, both), "arc")
})

test_that("arc validates link values", {
  nodes <- data.frame(id = c("A", "B", "C"))
  expect_error(
    pv_arc(nodes, data.frame(source = "A", target = "B", value = NA_real_)),
    "non-negative")
  expect_error(
    pv_arc(nodes, data.frame(source = "A", target = "B", value = -1)),
    "non-negative")
  expect_error(
    pv_arc(nodes, data.frame(source = "A", target = "B", value = "big")),
    "not numeric")
})

test_that("arc caps group levels at the active theme's palette", {
  nodes <- data.frame(id = letters[1:9], grp = paste0("g", 1:9))
  links <- data.frame(source = "a", target = "b", value = 1)
  expect_error(pv_arc(nodes, links, group = "grp"),
               "9 levels but the active theme's palette has 8")
  ok <- data.frame(id = letters[1:8], grp = paste0("g", 1:8))
  expect_pvchart(pv_arc(ok, links, group = "grp"), "arc")
})

test_that('order = "none" keeps arrival order, explicit order is obeyed', {
  w <- pv_arc(pv_network$nodes, pv_network$links, order = "none")
  expect_equal(w$x$nodes$id, pv_network$nodes$id)
  nodes <- data.frame(id = c("A", "B", "C"))
  links <- data.frame(source = c("A", "B"), target = c("B", "C"))
  w2 <- pv_arc(nodes, links, order = c("C", "A", "B"))
  expect_equal(w2$x$nodes$id, c("C", "A", "B"))
})

test_that("an explicit order must be a permutation of the node ids", {
  nodes <- data.frame(id = c("A", "B", "C"))
  links <- data.frame(source = "A", target = "B")
  expect_error(pv_arc(nodes, links, order = c("A", "B")),
               "permutation of the node ids \\(missing: C\\)")
  expect_error(pv_arc(nodes, links, order = c("A", "B", "C", "D")),
               "not nodes: D")
  expect_error(pv_arc(nodes, links, order = c("A", "B", "B")),
               "missing: C; duplicated: B")
  expect_error(pv_arc(nodes, links, order = 1:3),
               '`order` must be "auto", "none", or a character vector')
})

test_that('order = "auto" reduces crossings on the collaboration network', {
  auto <- pv_arc(pv_network$nodes, pv_network$links, order = "auto")
  none <- pv_arc(pv_network$nodes, pv_network$links, order = "none")
  # The same nodes and links either way - only the arrangement moves.
  expect_setequal(auto$x$nodes$id, none$x$nodes$id)
  expect_equal(auto$x$links, none$x$links)
  # The heuristic's contract: never worse than arrival order, and on
  # this graph strictly better.
  expect_lt(payload_crossings(auto), payload_crossings(none))
})

test_that('order = "auto" never has more crossings than arrival order', {
  # A shuffled path graph, where the ideal line order is known and the
  # arrival order is a deliberate tangle.
  set.seed(42)
  n <- 12
  perm <- sample(n)
  inv <- match(seq_len(n), perm)
  nodes <- data.frame(id = paste0("n", seq_len(n)))
  links <- data.frame(source = paste0("n", inv[seq_len(n - 1)]),
                      target = paste0("n", inv[2:n]))
  auto <- pv_arc(nodes, links, order = "auto")
  none <- pv_arc(nodes, links, order = "none")
  expect_lte(payload_crossings(auto), payload_crossings(none))
})

test_that("group colours stay put whichever order is chosen", {
  a <- pv_arc(pv_network$nodes, pv_network$links, group = "group",
              order = "auto")
  b <- pv_arc(pv_network$nodes, pv_network$links, group = "group",
              order = "none")
  expect_equal(a$x$groups, b$x$groups)
})

test_that("arc handles a lone node and an empty link table", {
  nodes <- data.frame(id = "solo")
  links <- data.frame(source = character(0), target = character(0),
                      value = numeric(0))
  w <- expect_pvchart(pv_arc(nodes, links), "arc")
  expect_equal(nrow(w$x$links), 0)
})

test_that("arc inherits the shared chart-option validation", {
  nodes <- data.frame(id = c("A", "B"))
  links <- data.frame(source = "A", target = "B")
  expect_error(pv_arc(nodes, links, mode = "sepia"), "must be \"auto\"")
  expect_error(pv_arc(nodes, links, duration = -1), "non-negative")
})

test_that("arc gets sane generic alt text", {
  w <- pv_arc(pv_network$nodes, pv_network$links, group = "group",
              title = "Who works with whom")
  txt <- pv_alt_text(w)
  expect_match(txt, "^An interactive arc chart")
  expect_match(txt, "Who works with whom")
  expect_identical(txt, w$x$alt)
})

test_that("arc renders without JavaScript errors", {
  render_skip_if_no_chrome()
  w <- pv_arc(pv_network$nodes, pv_network$links, group = "group",
              title = "Collaboration, name by name")
  path <- file.path(render_out_dir(), "arc.png")
  expect_no_warning(pv_save(w, path, quiet = TRUE))
  expect_true(file.exists(path))
  expect_gt(file.size(path), 20000)
  render_publish(path)
})

test_that("an ungrouped arc with explicit order renders without JavaScript errors", {
  render_skip_if_no_chrome()
  latest <- pv_commuters[pv_commuters$period == max(pv_commuters$period), ]
  flows <- aggregate(commuters ~ region, latest, sum)
  nodes <- data.frame(id = c(flows$region, "Zug"))
  links <- data.frame(source = flows$region, target = "Zug",
                      value = flows$commuters)
  w <- pv_arc(nodes, links, order = c("Zug", sort(flows$region)),
              title = "Commuting ties of Canton Zug",
              source = "Source: Kanton Zug, Fachstelle Statistik")
  path <- file.path(render_out_dir(), "arc_explicit.png")
  expect_no_warning(pv_save(w, path, quiet = TRUE))
  expect_true(file.exists(path))
  expect_gt(file.size(path), 20000)
  render_publish(path)
})
