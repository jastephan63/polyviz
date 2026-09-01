# Automatic alt text. Every chart carries a written description of
# itself, assembled here from the same payload the JavaScript renderer
# reads: pv_widget() attaches it at build time, knit-print.R hands it to
# knitr as the figure's alt text, and pv_alt() swaps in the author's own
# words. The rule for the generated text is honesty: say only what the
# payload states - the chart form, the title, what is measured, how much
# data there is, and where its computed extremes sit - and never an
# interpretation the numbers don't contain.

# A single usable string: one element, not NA, not empty.
alt_str <- function(v) {
  is.character(v) && length(v) == 1 && !is.na(v) && nzchar(v)
}

# A payload label (an axis title, a value-column name) with a fallback
# for suppressed or absent ones.
alt_lab <- function(v, fallback = "values") {
  if (alt_str(v)) v else fallback
}

# A measured quantity, formatted compactly: thousands get separators and
# no decimals, smaller magnitudes keep three significant digits.
alt_num <- function(v) {
  v <- suppressWarnings(as.numeric(v[1]))
  if (!length(v) || is.na(v) || !is.finite(v)) {
    return("(no value)")
  }
  if (abs(v) >= 1000) {
    format(round(v), big.mark = ",", scientific = FALSE, trim = TRUE)
  } else {
    format(signif(v, 3), scientific = FALSE, trim = TRUE)
  }
}

# An axis position - a year, an index, a date already shaped as an ISO
# string. Positions are names, not quantities, so no thousands
# separators: "1930", never "1,930".
alt_pos <- function(v) {
  v <- v[1]
  if (is.numeric(v)) {
    format(signif(v, 6), scientific = FALSE, trim = TRUE)
  } else {
    as.character(v)
  }
}

# A share of a whole as a compact percentage; below 1% one decimal
# survives so a small slice doesn't read as nothing.
alt_pct <- function(p) {
  r <- 100 * as.numeric(p)
  if (round(r) >= 1) paste0(round(r), "%") else sprintf("%.1f%%", r)
}

# A count with its noun: "1 category", "4 categories", "2,192 points".
alt_count <- function(n, one, many) {
  sprintf("%s %s", format(as.integer(n), big.mark = ","),
          if (n == 1) one else many)
}

# Joins names the way prose does: "A", "A and B", "A, B, and C".
alt_join <- function(v) {
  v <- as.character(v)
  n <- length(v)
  if (n <= 1) {
    return(paste(v, collapse = ""))
  }
  if (n == 2) {
    return(paste(v, collapse = " and "))
  }
  paste0(paste(v[-n], collapse = ", "), ", and ", v[n])
}

# A parenthetical listing kept short: every name up to six, otherwise
# the first three and a count of the rest. Returns "" for no names, and
# carries its own leading space so callers can drop it in anywhere.
alt_names <- function(v, max = 6) {
  v <- as.character(v)
  if (!length(v)) {
    return("")
  }
  listed <- if (length(v) <= max) {
    alt_join(v)
  } else {
    sprintf("%s, and %d more", paste(v[1:3], collapse = ", "),
            length(v) - 3)
  }
  sprintf(" (%s)", listed)
}

# Every description opens the same way: the chart form, the title when
# there is one, and the handler's participial clause ("showing revenue
# across 4 categories of region"). The title wears typographic quotes -
# the right look in a caption, and safe inside an HTML alt attribute,
# where a straight double quote would end the attribute early.
alt_lead <- function(x, opener, clause = NULL) {
  title <- if (alt_str(x$title)) {
    sprintf(" titled \u201c%s\u201d", x$title)
  } else {
    ""
  }
  if (is.null(clause)) {
    return(sprintf("%s%s.", opener, title))
  }
  sep <- if (nzchar(title)) ", " else " "
  sprintf("%s%s%s%s.", opener, title, sep, clause)
}

# Walks the nested {name, children/value} trees the hierarchy charts
# ship: leaves under a node, and the summed value of a node's leaves.
alt_tree_leaves <- function(node) {
  if (is.null(node$children)) {
    return(1L)
  }
  sum(vapply(node$children, alt_tree_leaves, integer(1)))
}

alt_tree_value <- function(node) {
  if (is.null(node$children)) {
    return(as.numeric(node$value %||% 0))
  }
  sum(vapply(node$children, alt_tree_value, numeric(1)))
}

# The top level of a hierarchy payload: one name and summed value per
# top-level group.
alt_tree_groups <- function(root) {
  kids <- root$children %||% list()
  list(name = vapply(kids, function(k) as.character(k$name), character(1)),
       value = vapply(kids, alt_tree_value, numeric(1)))
}

# ---- one describer per chart type ------------------------------------------

alt_bar <- function(x) {
  df <- x$data
  n <- length(unique(df$x))
  xl <- alt_lab(x$xlab, "category")
  yl <- alt_lab(x$ylab)
  cats <- alt_count(n, "category", "categories")
  if (is.null(df$series)) {
    hi <- which.max(df$y)
    lo <- which.min(df$y)
    rest <- if (df$y[hi] == df$y[lo]) {
      sprintf("Every bar holds %s.", alt_num(df$y[hi]))
    } else {
      sprintf("Values run from %s (%s) up to %s (%s).",
              alt_num(df$y[lo]), df$x[lo], alt_num(df$y[hi]), df$x[hi])
    }
    return(paste(alt_lead(x, "A bar chart",
                          sprintf("showing %s across %s of %s", yl, cats, xl)),
                 rest))
  }
  sers <- unique(df$series)
  s <- alt_count(length(sers), "series", "series")
  if (identical(x$stack, "stack")) {
    tot <- tapply(df$total, df$x, max)
    hi <- which.max(tot)
    lo <- which.min(tot)
    return(paste(
      alt_lead(x, "A stacked bar chart",
               sprintf("stacking %s%s into one bar for each of %s of %s",
                       s, alt_names(sers), cats, xl)),
      sprintf("Category totals run from %s (%s) up to %s (%s).",
              alt_num(tot[lo]), names(tot)[lo],
              alt_num(tot[hi]), names(tot)[hi])))
  }
  if (identical(x$stack, "percent")) {
    hi <- which.max(df$share)
    return(paste(
      alt_lead(x, "A percent-stacked bar chart",
               sprintf("showing the percentage split across %s%s for each of %s of %s",
                       s, alt_names(sers), cats, xl)),
      sprintf("The largest single share is %s, %s of the %s bar.",
              df$series[hi], alt_pct(df$share[hi]), df$x[hi])))
  }
  hi <- which.max(df$y)
  paste(
    alt_lead(x, "A grouped bar chart",
             sprintf("comparing %s across %s%s in %s of %s",
                     yl, s, alt_names(sers), cats, xl)),
    sprintf("The tallest bar is %s in %s, at %s.",
            df$series[hi], df$x[hi], alt_num(df$y[hi])))
}

alt_line <- function(x) {
  df <- x$data
  xl <- alt_lab(x$xlab, "x")
  yl <- alt_lab(x$ylab)
  xs <- unique(df$x)
  span <- if (identical(x$xtype, "category")) {
    c(xs[[1]], xs[[length(xs)]])
  } else {
    range(df$x)
  }
  clause <- if (isTRUE(x$showLegend)) {
    sers <- unique(df$series)
    sprintf("plotting %s against %s with %s%s", yl, xl,
            alt_count(length(sers), "line", "lines"), alt_names(sers))
  } else {
    sprintf("plotting %s against %s as a single line of %s", yl, xl,
            alt_count(nrow(df), "point", "points"))
  }
  out <- paste(
    alt_lead(x, "A line chart", clause),
    sprintf("The %s axis runs from %s to %s, with %s from %s to %s.",
            xl, alt_pos(span[1]), alt_pos(span[2]), yl,
            alt_num(min(df$y)), alt_num(max(df$y))))
  if (isTRUE(x$showLegend)) {
    fx <- span[2]
    fin <- df[df$x == fx, , drop = FALSE]
    if (nrow(fin)) {
      hi <- which.max(fin$y)
      out <- paste(out, sprintf("At the last point (%s), %s is highest at %s.",
                                alt_pos(fx), fin$series[hi],
                                alt_num(fin$y[hi])))
    }
  }
  out
}

alt_scatter <- function(x) {
  df <- x$data
  xl <- alt_lab(x$xlab, "x")
  yl <- alt_lab(x$ylab, "y")
  sized <- if (alt_str(x$sizelab)) sprintf(", sized by %s", x$sizelab) else ""
  out <- paste(
    alt_lead(x, "A scatter plot",
             sprintf("plotting %s against %s with %s%s", yl, xl,
                     alt_count(nrow(df), "point", "points"), sized)),
    sprintf("The x axis covers %s from %s to %s, and the y axis %s from %s to %s.",
            xl, alt_num(min(df$x)), alt_num(max(df$x)),
            yl, alt_num(min(df$y)), alt_num(max(df$y))))
  if (!is.null(df$label)) {
    hi <- which.max(df$y)
    out <- paste(out, sprintf("%s has the highest %s, at %s.",
                              df$label[hi], yl, alt_num(df$y[hi])))
  }
  out
}

alt_force <- function(x) {
  nd <- x$nodes
  lk <- x$links
  clause <- sprintf("connecting %s with %s",
                    alt_count(nrow(nd), "node", "nodes"),
                    alt_count(nrow(lk), "link", "links"))
  if (!is.null(nd$group)) {
    groups <- unique(nd$group)
    clause <- paste0(clause, sprintf(", coloured by %s%s",
                                     alt_count(length(groups), "group", "groups"),
                                     alt_names(groups)))
  }
  out <- alt_lead(x, "A network diagram", clause)
  if (nrow(lk)) {
    deg <- table(c(lk$source, lk$target))
    top <- names(deg)[which.max(deg)]
    lab <- nd$label[match(top, nd$id)]
    out <- paste(out, sprintf("%s has the most connections, with %s.",
                              lab, alt_count(max(deg), "link", "links")))
  }
  out
}

alt_chord <- function(x) {
  m <- do.call(rbind, lapply(x$matrix, as.numeric))
  labs <- x$labels
  out <- alt_lead(x, "A chord diagram",
                  sprintf("showing flows among %s%s",
                          alt_count(length(labs), "group", "groups"),
                          alt_names(labs)))
  if (max(m) > 0) {
    idx <- which(m == max(m), arr.ind = TRUE)[1, ]
    out <- paste(out, sprintf("The largest flow runs from %s to %s, at %s.",
                              labs[idx[1]], labs[idx[2]], alt_num(max(m))))
  }
  out
}

# Sunburst, treemap, and circle packing share one hierarchy payload, so
# they share one largest-group sentence too.
alt_top_group <- function(g) {
  total <- sum(g$value)
  if (!length(g$value) || total <= 0) {
    return(NULL)
  }
  hi <- which.max(g$value)
  sprintf("The largest group, %s, holds %s (%s of the total).",
          g$name[hi], alt_num(g$value[hi]), alt_pct(g$value[hi] / total))
}

alt_sunburst <- function(x) {
  g <- alt_tree_groups(x$root)
  leaves <- alt_tree_leaves(x$root)
  clause <- if (leaves > length(g$name)) {
    sprintf("showing a hierarchy of %s in %s as concentric rings",
            alt_count(leaves, "leaf segment", "leaf segments"),
            alt_count(length(g$name), "top-level group", "top-level groups"))
  } else {
    sprintf("showing %s as a ring",
            alt_count(length(g$name), "segment", "segments"))
  }
  paste(c(alt_lead(x, "A sunburst chart", clause), alt_top_group(g)),
        collapse = " ")
}

alt_treemap <- function(x) {
  g <- alt_tree_groups(x$root)
  leaves <- alt_tree_leaves(x$root)
  clause <- sprintf("sizing nested rectangles by %s, with %s%s split into %s",
                    alt_lab(x$vlab),
                    alt_count(length(g$name), "top-level group",
                              "top-level groups"),
                    alt_names(g$name), alt_count(leaves, "cell", "cells"))
  paste(c(alt_lead(x, "A treemap", clause), alt_top_group(g)), collapse = " ")
}

alt_pack <- function(x) {
  g <- alt_tree_groups(x$root)
  leaves <- alt_tree_leaves(x$root)
  clause <- sprintf("nesting %s inside %s%s, sized by %s",
                    alt_count(leaves, "circle", "circles"),
                    alt_count(length(g$name), "top-level group",
                              "top-level groups"),
                    alt_names(g$name), alt_lab(x$vlab))
  paste(c(alt_lead(x, "A circle-packing chart", clause), alt_top_group(g)),
        collapse = " ")
}

alt_histogram <- function(x) {
  b <- x$data
  clause <- sprintf("showing the distribution of %s across %s%s",
                    alt_lab(x$xlab, "the values"),
                    alt_count(nrow(b), "bin", "bins"),
                    if (!is.null(x$density)) ", with a density curve overlaid"
                    else "")
  hi <- which.max(b$count)
  paste(
    alt_lead(x, "A histogram", clause),
    sprintf("It covers %s from %s to %s.",
            alt_count(sum(b$count), "value", "values"),
            alt_num(min(b$x0)), alt_num(max(b$x1))),
    sprintf("The fullest bin, %s to %s, holds %s.",
            alt_num(b$x0[hi]), alt_num(b$x1[hi]),
            alt_count(b$count[hi], "value", "values")))
}

alt_boxplot <- function(x) {
  boxes <- x$boxes
  yl <- alt_lab(x$ylab)
  if (length(boxes) == 1) {
    b <- boxes[[1]]
    out <- paste(
      alt_lead(x, "A box plot",
               sprintf("summarising %s of %s",
                       alt_count(b$n, "value", "values"), yl)),
      sprintf("The median is %s, the middle half spans %s to %s, and the whiskers reach from %s to %s.",
              alt_num(b$median), alt_num(b$q1), alt_num(b$q3),
              alt_num(b$lo), alt_num(b$hi)))
    n_out <- length(b$outliers)
    if (n_out > 0) {
      out <- paste(out, sprintf("%s %s beyond the whiskers.",
                                alt_count(n_out, "outlier", "outliers"),
                                if (n_out == 1) "lies" else "lie"))
    }
    return(out)
  }
  groups <- vapply(boxes, function(b) as.character(b$group), character(1))
  meds <- vapply(boxes, function(b) as.numeric(b$median), numeric(1))
  ns <- vapply(boxes, function(b) as.numeric(b$n), numeric(1))
  lo <- which.min(meds)
  hi <- which.max(meds)
  paste(
    alt_lead(x, "A box plot",
             sprintf("summarising %s across %s%s", yl,
                     alt_count(length(boxes), "group", "groups"),
                     alt_names(groups))),
    sprintf("Medians run from %s (%s) to %s (%s), across %s in all.",
            alt_num(meds[lo]), groups[lo], alt_num(meds[hi]), groups[hi],
            alt_count(sum(ns), "value", "values")))
}

alt_violin <- function(x) {
  vs <- x$violins
  groups <- vapply(vs, function(v) as.character(v$group), character(1))
  meds <- vapply(vs, function(v) as.numeric(v$median), numeric(1))
  lo <- which.min(meds)
  hi <- which.max(meds)
  paste(
    alt_lead(x, "A violin plot",
             sprintf("comparing the distribution of %s across %s%s",
                     alt_lab(x$ylab),
                     alt_count(length(vs), "group", "groups"),
                     alt_names(groups))),
    sprintf("Medians run from %s (%s) to %s (%s).",
            alt_num(meds[lo]), groups[lo], alt_num(meds[hi]), groups[hi]))
}

alt_ridgeline <- function(x) {
  rs <- x$ridges
  groups <- vapply(rs, function(r) as.character(r$group), character(1))
  meds <- vapply(rs, function(r) as.numeric(r$median), numeric(1))
  out <- alt_lead(x, "A ridgeline plot",
                  sprintf("layering the distribution of %s for %s%s, ordered by median",
                          alt_lab(x$xlab),
                          alt_count(length(rs), "group", "groups"),
                          alt_names(groups)))
  if (length(rs) > 1) {
    # Ridges ship sorted by median, largest on top.
    out <- paste(out, sprintf(
      "Medians run from %s (%s, at the top) down to %s (%s).",
      alt_num(meds[1]), groups[1],
      alt_num(meds[length(rs)]), groups[length(rs)]))
  }
  out
}

alt_donut <- function(x) {
  df <- x$data
  total <- sum(df$value)
  opener <- if (isTRUE(x$innerRadius == 0)) "A pie chart" else "A donut chart"
  out <- paste(
    alt_lead(x, opener,
             sprintf("dividing %s across %s%s", alt_lab(x$vlab),
                     alt_count(nrow(df), "slice", "slices"),
                     alt_names(df$category))),
    sprintf("The total is %s.", alt_num(total)))
  if (total > 0) {
    hi <- which.max(df$value)
    out <- paste(out, sprintf("The largest slice, %s, holds %s (%s of the total).",
                              df$category[hi], alt_num(df$value[hi]),
                              alt_pct(df$value[hi] / total)))
  }
  out
}

alt_lollipop <- function(x) {
  df <- x$data
  hi <- which.max(df$y)
  lo <- which.min(df$y)
  paste(
    alt_lead(x, "A lollipop chart",
             sprintf("ranking %s of %s by %s",
                     alt_count(nrow(df), "category", "categories"),
                     alt_lab(x$xlab, "category"), alt_lab(x$ylab))),
    sprintf("%s is highest at %s, and %s lowest at %s.",
            df$x[hi], alt_num(df$y[hi]), df$x[lo], alt_num(df$y[lo])))
}

alt_area <- function(x) {
  df <- x$data
  xl <- alt_lab(x$xlab, "x")
  yl <- alt_lab(x$ylab)
  sers <- x$series
  single <- identical(sers, "value")
  xs <- unique(df$x)
  span <- if (identical(x$xtype, "category")) {
    c(xs[[1]], xs[[length(xs)]])
  } else {
    range(df$x)
  }
  opener <- switch(x$offset,
    percent = "A percent-stacked area chart",
    stream = "A streamgraph",
    if (single) "An area chart" else "A stacked area chart")
  clause <- if (single) {
    sprintf("showing %s over %s as a single filled area", yl, xl)
  } else if (identical(x$offset, "percent")) {
    sprintf("showing the percentage mix of %s%s over %s",
            alt_count(length(sers), "series", "series"), alt_names(sers), xl)
  } else {
    sprintf("showing %s over %s as %s%s",
            if (identical(x$offset, "stream")) alt_lab(x$ylab, "values")
            else yl,
            xl, alt_count(length(sers), "band", "bands"), alt_names(sers))
  }
  rest <- if (identical(x$offset, "percent")) {
    sprintf("The %s axis runs from %s to %s.", xl,
            alt_pos(span[1]), alt_pos(span[2]))
  } else {
    tot <- tapply(df$y, df$x, sum)
    sprintf("From %s to %s, the %s ranges from %s to %s.",
            alt_pos(span[1]), alt_pos(span[2]),
            if (single) "area's height" else "stacked total",
            alt_num(min(tot)), alt_num(max(tot)))
  }
  paste(alt_lead(x, opener, clause), rest)
}

alt_heatmap <- function(x) {
  df <- x$data
  nx <- length(unique(df$x))
  ny <- length(unique(df$y))
  hi <- which.max(df$value)
  out <- paste(
    alt_lead(x, "A heatmap",
             sprintf("colouring a %d-by-%d grid (%s by %s) by %s", nx, ny,
                     alt_lab(x$xlab, "column"), alt_lab(x$ylab, "row"),
                     alt_lab(x$vlab))),
    sprintf("Cell values range from %s to %s, peaking in the %s / %s cell.",
            alt_num(min(df$value)), alt_num(max(df$value)),
            df$x[hi], df$y[hi]))
  if (identical(x$palette, "diverging")) {
    out <- paste(out, "The colour scale diverges around zero.")
  }
  out
}

alt_calendar <- function(x) {
  df <- x$data
  hi <- which.max(df$value)
  paste(
    alt_lead(x, "A calendar heatmap",
             sprintf("showing daily %s for %s", alt_lab(x$vlab),
                     alt_join(x$years))),
    sprintf("It covers %s, with values from %s to %s.",
            alt_count(nrow(df), "day", "days"),
            alt_num(min(df$value)), alt_num(max(df$value))),
    sprintf("The highest day is %s, at %s.",
            df$date[hi], alt_num(df$value[hi])))
}

alt_sankey <- function(x) {
  lk <- x$links
  out <- alt_lead(x, "A sankey diagram",
                  sprintf("tracing flows among %s through %s",
                          alt_count(nrow(x$nodes), "node", "nodes"),
                          alt_count(nrow(lk), "link", "links")))
  if (nrow(lk) && max(lk$value) > 0) {
    i <- which.max(lk$value)
    # Link endpoints travel as 0-based indices into the node list.
    out <- paste(out, sprintf("The largest flow runs from %s to %s, at %s.",
                              x$nodes$name[lk$source[i] + 1],
                              x$nodes$name[lk$target[i] + 1],
                              alt_num(lk$value[i])))
  }
  out
}

alt_parallel <- function(x) {
  cols <- x$columns
  out <- alt_lead(x, "A parallel-coordinates plot",
                  sprintf("drawing %s as lines across %s (%s)",
                          alt_count(nrow(x$data), "row", "rows"),
                          alt_count(length(cols), "axis", "axes"),
                          alt_join(cols)))
  if (isTRUE(x$showLegend) && !is.null(x$data$series)) {
    g <- unique(x$data$series)
    out <- paste(out, sprintf("Lines are coloured by %s%s.",
                              alt_count(length(g), "group", "groups"),
                              alt_names(g)))
  }
  out
}

alt_dendrogram <- function(x) {
  clause <- sprintf("arranging %s in a clustering tree",
                    alt_count(alt_tree_leaves(x$tree), "leaf", "leaves"))
  if (!is.null(x$k)) {
    clause <- paste0(clause, sprintf(", cut into %s",
                                     alt_count(x$k, "coloured cluster",
                                               "coloured clusters")))
  }
  alt_lead(x, "A dendrogram", clause)
}

alt_choropleth <- function(x) {
  df <- x$data
  feats <- x$map$features
  ids <- vapply(feats, function(f) {
    v <- f$properties$id
    if (is.null(v)) NA_character_ else as.character(v)
  }, character(1))
  nms <- vapply(feats, function(f) {
    v <- f$properties$name
    if (is.null(v)) NA_character_ else as.character(v)
  }, character(1))
  region <- function(i) {
    nm <- nms[match(df$id[i], ids)]
    if (!is.na(nm)) nm else paste("region", df$id[i])
  }
  lo <- which.min(df$value)
  hi <- which.max(df$value)
  scale_note <- if (identical(x$palette, "diverging")) {
    sprintf(", on a colour scale diverging around %s", alt_pos(x$center))
  } else {
    ""
  }
  out <- paste(
    alt_lead(x, "A choropleth map",
             sprintf("shading %s by %s",
                     alt_count(nrow(df), "region", "regions"),
                     alt_lab(x$vlab))),
    sprintf("Values range from %s (%s) to %s (%s)%s.",
            alt_num(df$value[lo]), region(lo),
            alt_num(df$value[hi]), region(hi), scale_note))
  blank <- sum(!is.na(ids) & !(ids %in% df$id))
  if (blank > 0) {
    out <- paste(out, sprintf("%s on the map %s no data.",
                              alt_count(blank, "region", "regions"),
                              if (blank == 1) "has" else "have"))
  }
  out
}

alt_bubblemap <- function(x) {
  df <- x$data
  clause <- sprintf("placing %s sized by %s",
                    alt_count(nrow(df), "circle", "circles"),
                    alt_lab(x$sizelab))
  if (isTRUE(x$showLegend) && !is.null(df$series)) {
    g <- unique(df$series)
    clause <- paste0(clause, sprintf(", coloured by %s%s",
                                     alt_count(length(g), "group", "groups"),
                                     alt_names(g)))
  }
  hi <- which.max(df$size)
  who <- if (!is.null(df$label)) sprintf(" (%s)", df$label[hi]) else ""
  paste(
    alt_lead(x, "A bubble map", clause),
    sprintf("Sizes run from %s up to %s%s.",
            alt_num(min(df$size)), alt_num(max(df$size)), who))
}

alt_race <- function(x) {
  times <- x$times
  leader <- x$entities[[1]]
  fv <- x$data$value[x$data$id == leader &
                       x$data$t == times[length(times)]]
  paste(
    alt_lead(x, "An animated bar-chart race",
             sprintf("ranking %s by %s over %s from %s to %s",
                     alt_count(length(x$entities), "entity", "entities"),
                     alt_lab(x$vlab),
                     alt_count(length(times), "time point", "time points"),
                     alt_pos(times[1]), alt_pos(times[length(times)]))),
    sprintf("%s leads the final standings, at %s.", leader, alt_num(fv[1])))
}

alt_bump <- function(x) {
  times <- x$times
  paste(
    alt_lead(x, "A bump chart",
             sprintf("tracking how %s rank by %s across %s from %s to %s",
                     alt_count(length(x$entities), "entity", "entities"),
                     alt_lab(x$vlab),
                     alt_count(length(times), "time point", "time points"),
                     alt_pos(times[1]), alt_pos(times[length(times)]))),
    sprintf("%s holds first place at the end.", x$entities[[1]]))
}

alt_beeswarm <- function(x) {
  df <- x$data
  clause <- sprintf("spreading %s along %s",
                    alt_count(nrow(df), "dot", "dots"), alt_lab(x$xlab))
  if (!is.null(df$group)) {
    g <- unique(df$group)
    clause <- paste0(clause, sprintf(" in %s%s",
                                     alt_count(length(g), "lane", "lanes"),
                                     alt_names(g)))
  }
  hi <- which.max(df$value)
  who <- if (!is.null(df$label)) sprintf(" (%s)", df$label[hi]) else ""
  paste(
    alt_lead(x, "A beeswarm chart", clause),
    sprintf("Values run from %s to %s%s.",
            alt_num(min(df$value)), alt_num(max(df$value)), who))
}

# The dispatcher pv_widget() and pv_alt_text() call. Any type without a
# dedicated describer (a facet payload, say) still gets an honest
# generic sentence rather than nothing.
alt_describe <- function(x) {
  type <- if (alt_str(x$type)) x$type else "data"
  switch(type,
    bar = alt_bar(x),
    line = alt_line(x),
    scatter = alt_scatter(x),
    force = alt_force(x),
    chord = alt_chord(x),
    sunburst = alt_sunburst(x),
    histogram = alt_histogram(x),
    boxplot = alt_boxplot(x),
    violin = alt_violin(x),
    ridgeline = alt_ridgeline(x),
    donut = alt_donut(x),
    treemap = alt_treemap(x),
    lollipop = alt_lollipop(x),
    area = alt_area(x),
    heatmap = alt_heatmap(x),
    calendar = alt_calendar(x),
    sankey = alt_sankey(x),
    parallel = alt_parallel(x),
    pack = alt_pack(x),
    dendrogram = alt_dendrogram(x),
    choropleth = alt_choropleth(x),
    bubblemap = alt_bubblemap(x),
    race = alt_race(x),
    bump = alt_bump(x),
    beeswarm = alt_beeswarm(x),
    alt_lead(x, sprintf("An interactive %s chart", type))
  )
}

#' The generated description of a chart
#'
#' Every polyviz chart carries a short written description of itself -
#' its alt text - built automatically when the chart is constructed:
#' the chart form in plain words, the title, what is measured, and the
#' data's actual shape (how many categories, series, or points; the
#' value range; the largest category or final leader where the data
#' makes that computable). The text states only what the data says and
#' never adds interpretation. Screen readers get it through the widget
#' (`role="img"` with the description as the accessible name), knitted
#' Word/HTML documents carry it on the included figure, and this
#' function returns it so it can be reused - say, as the start of a
#' figure caption.
#'
#' `pv_alt_text()` always builds the text fresh from the chart's data,
#' so it shows what the automatic description would say even after
#' [pv_alt()] has replaced the attached text with the author's own
#' words. The text actually attached to the chart is the one `pv_alt()`
#' set, when it was called.
#'
#' @param w A polyviz chart.
#' @return A single string of one to three sentences.
#' @examples
#' sales <- aggregate(revenue ~ region, pv_sales, sum)
#' pv_alt_text(pv_bar(sales, x = "region", y = "revenue",
#'                    title = "Revenue by region"))
#' @seealso [pv_alt()] to replace the attached text with your own.
#' @export
pv_alt_text <- function(w) {
  check_pv_widget(w, "pv_alt_text")
  alt_describe(w$x)
}

#' Set a chart's alt text by hand
#'
#' Replaces the automatically generated description (see
#' [pv_alt_text()]) with the author's own words. The text travels
#' everywhere the generated one would: to screen readers through the
#' rendered widget, and onto the figure knitr includes in static
#' documents.
#'
#' @param w A polyviz chart.
#' @param text A single non-empty string describing the chart. One to
#'   three plain sentences work best - say what the chart shows, not
#'   what it looks like.
#' @return The chart, with the description attached - ready for more
#'   pipe steps.
#' @examples
#' sales <- aggregate(revenue ~ region, pv_sales, sum)
#' pv_bar(sales, x = "region", y = "revenue") |>
#'   pv_alt("Revenue by region: North leads, the other three are close.")
#' @export
pv_alt <- function(w, text) {
  check_pv_widget(w, "pv_alt")
  if (!alt_str(text)) {
    rlang::abort("`text` must be a single non-empty string.")
  }
  w$x$alt <- text
  w
}
