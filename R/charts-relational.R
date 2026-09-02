# Relational charts that lay a network out on a line: the arc diagram.
# As everywhere in polyviz, this file only shapes the payload - R decides
# the node order, JavaScript (lib/pv-renderers/relational.js) draws the
# dots and arcs.

# Counts how many pairs of arcs would cross for a given node arrangement.
# `pos` maps each node index to its display slot; two arcs cross exactly
# when their endpoint intervals interleave strictly - arcs that share a
# node meet at the dot, they don't cross. Quadratic in the link count,
# which is fine at the sizes a readable arc diagram can hold.
arc_crossings <- function(pos, si, ti) {
  if (length(si) < 2) {
    return(0L)
  }
  a <- pmin(pos[si], pos[ti])
  b <- pmax(pos[si], pos[ti])
  total <- 0L
  for (k in seq_along(a)[-1]) {
    prev <- seq_len(k - 1)
    total <- total + sum(
      (a[prev] < a[k] & a[k] < b[prev] & b[prev] < b[k]) |
        (a[k] < a[prev] & a[prev] < b[k] & b[k] < b[prev]))
  }
  as.integer(total)
}

# The polishing half of the "auto" order: walk the line and swap two
# neighbouring nodes whenever that strictly reduces crossings, repeating
# until a full pass changes nothing. Swapping adjacent slots only affects
# link pairs where one link touches each of the two nodes, so each
# candidate swap is judged on those pairs alone rather than a full
# recount. Never increases crossings, by construction.
arc_swap_refine <- function(ord, si, ti) {
  n <- length(ord)
  pos <- integer(n)
  pos[ord] <- seq_len(n)
  inc <- rep(list(integer(0)), n)
  for (k in seq_along(si)) {
    inc[[si[k]]] <- c(inc[[si[k]]], k)
    inc[[ti[k]]] <- c(inc[[ti[k]]], k)
  }
  # Do the endpoint intervals (a1, b1) and (a2, b2) strictly interleave?
  crossing <- function(a1, b1, a2, b2) {
    (a1 < a2 & a2 < b1 & b1 < b2) | (a2 < a1 & a1 < b2 & b2 < b1)
  }
  for (pass in seq_len(40L)) {
    changed <- FALSE
    for (p in seq_len(n - 1)) {
      u <- ord[p]
      v <- ord[p + 1]
      # Links on one node but not the other; a link joining u and v
      # itself spans the two slots either way and can be ignored.
      lu <- setdiff(inc[[u]], inc[[v]])
      lv <- setdiff(inc[[v]], inc[[u]])
      if (!length(lu) || !length(lv)) next
      # The far ends of those links, as display slots.
      far_u <- pos[ifelse(si[lu] == u, ti[lu], si[lu])]
      far_v <- pos[ifelse(si[lv] == v, ti[lv], si[lv])]
      before <- 0L
      after <- 0L
      for (x in far_u) {
        before <- before + sum(crossing(
          min(p, x), max(p, x), pmin(p + 1, far_v), pmax(p + 1, far_v)))
        after <- after + sum(crossing(
          min(p + 1, x), max(p + 1, x), pmin(p, far_v), pmax(p, far_v)))
      }
      if (after < before) {
        ord[p] <- v
        ord[p + 1] <- u
        pos[u] <- p + 1
        pos[v] <- p
        changed <- TRUE
      }
    }
    if (!changed) break
  }
  ord
}

# The crossing-reduction heuristic behind `order = "auto"`, in two
# stages. First, repeated barycenter sweeps: each sweep moves every node
# toward the average display position of its neighbours, then re-sorts
# the line (current position breaks ties, so the pass is stable and
# deterministic); the best arrangement seen - the arrival order included
# - is kept. Second, that arrangement is polished with greedy neighbour
# swaps (arc_swap_refine above). The sweeps untangle globally, the swaps
# fix what they leave locally, and since neither stage can end worse
# than it started, "auto" never draws more crossings than "none" would.
# Returns the node indices in display order.
arc_auto_order <- function(n, si, ti) {
  nbr <- rep(list(integer(0)), n)
  for (k in seq_along(si)) {
    nbr[[si[k]]] <- c(nbr[[si[k]]], ti[k])
    nbr[[ti[k]]] <- c(nbr[[ti[k]]], si[k])
  }
  ord <- seq_len(n)
  pos <- integer(n)
  pos[ord] <- seq_len(n)
  best <- ord
  best_x <- arc_crossings(pos, si, ti)
  for (sweep in seq_len(8L)) {
    bary <- vapply(seq_len(n), function(i) {
      if (length(nbr[[i]])) mean(pos[nbr[[i]]]) else as.numeric(pos[i])
    }, numeric(1))
    new_ord <- order(bary, pos)
    if (identical(new_ord, ord)) {
      break
    }
    ord <- new_ord
    pos[ord] <- seq_len(n)
    x <- arc_crossings(pos, si, ti)
    if (x < best_x) {
      best <- ord
      best_x <- x
    }
  }
  arc_swap_refine(best, si, ti)
}

#' Interactive D3 arc diagram
#'
#' A network laid out for reading: every node sits on one horizontal
#' line, labelled plainly below it, and each link bows over the line as
#' an arc. Where the force layout ([pv_force()]) shows a network's shape,
#' the arc diagram shows its members - the line keeps every label
#' horizontal and legible, which makes it the right network view when
#' the names matter as much as the wiring.
#'
#' Dots are sized gently by their number of connections (on a square-root
#' scale), arcs are as wide as the square root of their `value`, and arc
#' colour follows the source node's `group`. Hovering a node lights up
#' its arcs and fades the rest; hovering an arc reads out source, target,
#' and value. When labels get tight they stagger into two rows and then
#' shorten with an ellipsis - the tooltip always carries the full name.
#'
#' Node order is the whole layout, so `order` controls it explicitly.
#' `"auto"` (the default) runs a small crossing-reduction heuristic:
#' repeated barycenter sweeps (each node pulled toward the average
#' position of its neighbours), polished by greedy swaps of neighbouring
#' nodes wherever a swap removes crossings. Neither stage can end worse
#' than it started, so `"auto"` never draws more arc crossings than the
#' arrival order; on the bundled [pv_network] collaboration graph it
#' removes about a quarter of them. Hub-and-spoke graphs, where every
#' link touches one centre, have no crossings under any order - there
#' `"auto"` simply pulls each hub next to its spokes.
#'
#' In Shiny, clicking a node reports
#' `list(part = "node", id, label, group, connections)` and clicking an
#' arc `list(part = "link", source, target, value)` as
#' `input$<outputId>_click`.
#'
#' @param nodes Data frame of nodes, one row each.
#' @param links Data frame of links with `source`/`target` columns
#'   holding node ids, and optionally `value` for flow size (arc width;
#'   non-negative, no missing values; 1 when the column is absent).
#'   Repeating the same source/target pair is an error - aggregate those
#'   rows first. A link from a node to itself has no arc to draw and is
#'   refused too. Opposite directions (`A` to `B` and `B` to `A`) are
#'   allowed but draw on the same curve, one arc over the other; sum the
#'   directions first if one arc per pair reads better.
#' @param id Name of the node id column (default `"id"`). Ids must be
#'   unique - each node is one dot on the line.
#' @param label Name of the node label column (defaults to the id
#'   column).
#' @param group Optional name of a grouping column mapped to colour.
#'   Colours are assigned to group levels in first-appearance order over
#'   the `nodes` you passed, so they stay put whichever `order` is
#'   chosen. At most as many levels as the active theme's palette has
#'   colours (8 in the packaged theme).
#' @param order How to arrange the nodes along the line. `"auto"`
#'   (default) applies the crossing-reduction heuristic described above;
#'   `"none"` keeps the `nodes` arrival order; a character vector of
#'   node ids (a permutation of them - anything else is an error) places
#'   the nodes exactly in that order, left to right.
#' @inheritParams pv_bar
#' @return An htmlwidget.
#' @examples
#' pv_arc(pv_network$nodes, pv_network$links, group = "group",
#'        title = "The collaboration network, name by name")
#' # The same graph in arrival order, for comparison:
#' pv_arc(pv_network$nodes, pv_network$links, group = "group",
#'        order = "none")
#' @export
pv_arc <- function(nodes, links, id = "id", label = id, group = NULL,
                   order = "auto",
                   title = NULL, subtitle = NULL, mode = "auto",
                   duration = 600, source = NULL, width = NULL,
                   height = NULL, elementId = NULL) {
  check_columns(nodes, list(id, label, group))
  check_nonempty(nodes, "nodes")
  check_columns(links, list("source", "target"))

  nd <- data.frame(id = as.character(nodes[[id]]),
                   label = as.character(nodes[[label]]))
  if (!is.null(group)) nd$group <- as.character(nodes[[group]])
  # One dot per node: a duplicated id would put two nodes on the same
  # slot and make every id lookup ambiguous.
  if (anyDuplicated(nd$id)) {
    dups <- unique(nd$id[duplicated(nd$id)])
    rlang::abort(sprintf("`nodes` has duplicated ids: %s.",
                         paste(dups, collapse = ", ")))
  }
  if (!is.null(group)) {
    check_theme_palette_fit(unique(nd$group), group)
  }

  src <- as.character(links$source)
  tgt <- as.character(links$target)
  if ("value" %in% names(links)) {
    check_value_column(links, "value")
    val <- as.numeric(links$value)
    # A negative or missing value has no arc width to draw.
    if (anyNA(val) || any(val < 0)) {
      rlang::abort(
        "`value` must be non-negative numbers with no missing values.")
    }
  } else {
    val <- rep(1, length(src))
  }
  # A link pointing at a node that doesn't exist has no place on the
  # line; catch it here with a clear message, as pv_force does.
  unknown <- setdiff(c(src, tgt), nd$id)
  if (length(unknown)) {
    rlang::abort(sprintf("Links reference unknown node ids: %s",
                         paste(unique(unknown), collapse = ", ")))
  }
  # A self-link starts and ends on the same dot - there is no arc there.
  if (any(src == tgt)) {
    rlang::abort(sprintf(
      "`links` connect a node to itself (%s); an arc needs two distinct endpoints.",
      paste(unique(src[src == tgt]), collapse = ", ")))
  }
  # The same flow twice would draw as two arcs stacked on the identical
  # curve, reading as one wrong width - refused here like pv_sankey.
  if (anyDuplicated(paste(src, tgt, sep = "\r"))) {
    rlang::abort(
      "`links` has more than one row for the same source/target pair; aggregate it first.")
  }

  # Colours are assigned to group levels in the order the caller's nodes
  # first show them - captured now, before any reordering, so the same
  # data wears the same colours under every `order`.
  grp_levels <- if (is.null(group)) NULL else unique(nd$group)

  # Resolve the display order. The node data frame is reordered here so
  # the JavaScript side simply draws the nodes left to right as shipped.
  si <- match(src, nd$id)
  ti <- match(tgt, nd$id)
  is_mode <- is.character(order) && length(order) == 1 && !is.na(order) &&
    order %in% c("auto", "none")
  if (is_mode) {
    if (order == "auto") {
      nd <- nd[arc_auto_order(nrow(nd), si, ti), , drop = FALSE]
      rownames(nd) <- NULL
    }
  } else {
    if (!is.character(order)) {
      rlang::abort(
        '`order` must be "auto", "none", or a character vector of node ids.')
    }
    missing_ids <- setdiff(nd$id, order)
    extra <- setdiff(order, nd$id)
    if (length(missing_ids) || length(extra) || anyDuplicated(order)) {
      detail <- c(
        if (length(missing_ids)) {
          sprintf("missing: %s", paste(missing_ids, collapse = ", "))
        },
        if (length(extra)) {
          sprintf("not nodes: %s", paste(extra, collapse = ", "))
        },
        if (anyDuplicated(order)) {
          sprintf("duplicated: %s",
                  paste(unique(order[duplicated(order)]), collapse = ", "))
        })
      rlang::abort(sprintf(
        "`order` must be a permutation of the node ids (%s).",
        paste(detail, collapse = "; ")))
    }
    nd <- nd[match(order, nd$id), , drop = FALSE]
    rownames(nd) <- NULL
  }

  payload <- list(
    nodes = nd,
    links = data.frame(source = src, target = tgt, value = val)
  )
  # I() keeps a single level travelling as a JSON array, not a bare
  # string, so the JavaScript side always sees a list of levels.
  if (!is.null(group)) payload$groups <- I(grp_levels)
  pv_widget("arc", c(payload,
                     chart_opts(title, subtitle, mode, duration, source)),
            width, height, elementId)
}
