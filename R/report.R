# One-call HTML report: pv_report() profiles a data frame and writes a
# standalone page of real polyviz widgets - the same live d3 charts the
# individual pv_* functions produce, arranged into sections with a line of
# generated context each. The page chrome (fonts, surface, hairlines,
# light/dark) reuses the design tokens from palette.R, so the report looks
# like the package gallery rather than a printout.

# Sets the sizes htmlwidgets uses when the widget is embedded in a page:
# full container width, fixed pixel height.
report_size <- function(w, height) {
  w$width <- "100%"
  w$height <- height
  w
}

# The page stylesheet, generated from the palette tokens so the report and
# the widgets always agree on colours. With mode "auto" the light tokens
# are the default and a prefers-color-scheme block swaps in the dark set,
# mirroring what the widgets themselves do; a forced mode bakes in one set
# and skips the media query.
report_css <- function(mode) {
  ink <- pv_colors$ink
  vars <- function(i) {
    paste0("--surface:", i$surface, ";--primary:", i$primary,
           ";--secondary:", i$secondary, ";--muted:", i$muted,
           ";--grid:", i$grid, ";--baseline:", i$baseline, ";")
  }
  base <- if (mode == "dark") ink$dark else ink$light
  scheme <- switch(mode, auto = "light dark", light = "light", dark = "dark")
  dark_block <- if (mode == "auto") {
    paste0("@media (prefers-color-scheme: dark) { :root { ",
           vars(ink$dark), " } }\n")
  } else ""
  paste0(
    ":root { color-scheme: ", scheme, "; ", vars(base), " }\n",
    dark_block,
    "body { margin: 0; background: var(--surface); color: var(--primary);\n",
    "       font-family: ", pv_font_stack(), "; }\n",
    "main { max-width: 960px; margin: 0 auto; padding: 0 20px 48px; }\n",
    "header { padding: 42px 0 4px; }\n",
    "header h1 { font-size: 32px; letter-spacing: -0.02em; margin: 0; }\n",
    "header p { color: var(--secondary); line-height: 1.55;\n",
    "           max-width: 640px; }\n",
    "section { margin: 38px 0 0; }\n",
    "section h2 { font-size: 20px; letter-spacing: -0.01em;\n",
    "             margin: 0 0 4px; }\n",
    ".context { color: var(--secondary); line-height: 1.6; max-width: 720px;\n",
    "           margin: 0 0 14px; }\n",
    ".note { color: var(--muted); font-size: 12.5px; margin: 10px 0 0; }\n",
    ".two-up { display: grid; grid-template-columns: 1fr 1fr;\n",
    "          gap: 18px; }\n",
    "@media (max-width: 720px) { .two-up { grid-template-columns: 1fr; } }\n",
    ".table-wrap { overflow-x: auto; border: 1px solid var(--grid);\n",
    "              border-radius: 8px; }\n",
    # min-width keeps the rows one line tall on narrow screens - the wrap
    # scrolls sideways instead of squeezing the value sketches into tall
    # multi-line cells
    "table { border-collapse: collapse; width: 100%; min-width: 640px;\n",
    "        font-size: 13px; line-height: 1.45; }\n",
    "th { text-align: left; font-weight: 600; color: var(--secondary);\n",
    "     border-bottom: 1px solid var(--baseline); padding: 9px 12px;\n",
    "     white-space: nowrap; }\n",
    "td { border-bottom: 1px solid var(--grid); padding: 8px 12px;\n",
    "     vertical-align: top; }\n",
    "tr:last-child td { border-bottom: none; }\n",
    "th.num, td.num { text-align: right;\n",
    "                 font-variant-numeric: tabular-nums; }\n",
    "footer { color: var(--muted); font-size: 12.5px; line-height: 1.6;\n",
    "         border-top: 1px solid var(--grid); padding-top: 16px;\n",
    "         margin-top: 44px; }\n",
    "footer a { color: inherit; }\n")
}

# The pv_summary table rendered as plain HTML. Numeric columns are
# right-aligned with tabular figures; the label column disappears when no
# column carries a SAS-style label, same as the print method.
report_summary_table <- function(s) {
  tg <- htmltools::tags
  cols <- s$columns
  if (all(is.na(cols$label))) cols$label <- NULL
  headers <- c(variable = "column", label = "label", class = "type",
               n_missing = "missing", pct_missing = "missing %",
               n_distinct = "distinct", sketch = "values")
  numeric_cols <- c("n_missing", "pct_missing", "n_distinct")
  cls <- function(nm) if (nm %in% numeric_cols) "num"
  head_row <- tg$tr(lapply(names(cols), function(nm) {
    tg$th(class = cls(nm), headers[[nm]])
  }))
  body_rows <- lapply(seq_len(nrow(cols)), function(i) {
    tg$tr(lapply(names(cols), function(nm) {
      v <- cols[[nm]][i]
      tg$td(class = cls(nm), if (is.na(v)) "" else format(v))
    }))
  })
  tg$div(class = "table-wrap",
         tg$table(tg$thead(head_row), tg$tbody(body_rows)))
}

# A section is a heading, one sentence of context, and whatever content
# follows. Kept as a helper so every section composes the same way.
report_section <- function(id, heading, context, ...) {
  tg <- htmltools::tags
  tg$section(id = id, tg$h2(heading), tg$p(class = "context", context), ...)
}

#' One-call HTML report of a data frame
#'
#' Profiles a data frame and writes a standalone, themed HTML report built
#' from real polyviz widgets — every chart on the page is the same live d3
#' graphic the individual chart functions produce. The report has a header
#' with the dimensions and completeness, the [pv_summary()] column table
#' as styled HTML, a [pv_histogram()] for each numeric column (first 8,
#' two per row), a horizontal [pv_bar()] of the most common levels of each
#' categorical column with at most 30 distinct values (first 4), and —
#' when the frame has three or more usable numeric columns — a
#' [pv_heatmap()] of their pairwise Pearson correlations on the diverging
#' palette centred at zero. Sections with nothing to show are skipped.
#'
#' @param data A data frame.
#' @param file Path of the HTML file to write. The widget assets (d3, the
#'   renderers, the bundled Inter font) are copied into a `lib/` folder
#'   next to the file, so move or publish the two together.
#' @param title Report heading. Defaults to the expression `data` was
#'   called with, so `pv_report(airquality, f)` is titled "airquality".
#' @param mode `"auto"` (default: the page and every widget follow the
#'   viewer's light/dark setting), `"light"`, or `"dark"`.
#' @param open Also open the finished report in the default browser?
#' @return The `file` path, invisibly.
#' @examples
#' report <- file.path(tempdir(), "sales-report.html")
#' pv_report(pv_sales, report, title = "Simulated monthly sales")
#' @export
pv_report <- function(data, file, title = deparse(substitute(data)),
                      mode = "auto", open = FALSE) {
  # deparse() can split a long expression over lines; fold it back.
  title <- paste(as.character(title), collapse = " ")
  if (!is.data.frame(data)) {
    rlang::abort("`data` must be a data frame.")
  }
  if (!is.character(file) || length(file) != 1 || is.na(file) ||
      !nzchar(file)) {
    rlang::abort("`file` must be a single file path.")
  }
  if (!is.character(mode) || length(mode) != 1 || is.na(mode) ||
      !mode %in% c("auto", "light", "dark")) {
    rlang::abort('`mode` must be "auto", "light", or "dark".')
  }
  tg <- htmltools::tags
  s <- pv_summary(data)

  # Sort the columns into the roles the sections need. Numerics must have
  # at least 2 real values to bin; categoricals (character, factor, or
  # logical) qualify with 1-30 distinct levels - beyond that they are
  # id-like and a bar chart of them would say nothing.
  num_cols <- names(data)[vapply(data, is.numeric, logical(1))]
  num_cols <- num_cols[vapply(num_cols, function(nm) {
    sum(!is.na(data[[nm]])) >= 2
  }, logical(1))]
  cat_cols <- names(data)[vapply(data, function(x) {
    is.character(x) || is.factor(x) || is.logical(x)
  }, logical(1))]
  cat_cols <- cat_cols[vapply(cat_cols, function(nm) {
    k <- length(unique(data[[nm]][!is.na(data[[nm]])]))
    k >= 1 && k <= 30
  }, logical(1))]

  # --- header ------------------------------------------------------------
  header <- tg$header(
    tg$h1(title),
    tg$p(sprintf(
      "%s rows and %s columns; %.1f%% of rows are complete.",
      format(s$n_rows, big.mark = ","), s$n_cols, 100 * s$complete_rate))
  )

  sections <- list()

  # --- column summary ----------------------------------------------------
  sections[[length(sections) + 1]] <- report_section(
    "summary", "Column summary",
    paste("Every column at a glance: type, missingness, distinct values,",
          "and a compact sketch of the values."),
    report_summary_table(s))

  # --- distributions -----------------------------------------------------
  if (length(num_cols)) {
    shown <- utils::head(num_cols, 8)
    charts <- lapply(shown, function(nm) {
      n_miss <- sum(is.na(data[[nm]]))
      sub <- if (n_miss > 0) {
        sprintf("%d missing value%s", n_miss, if (n_miss == 1) "" else "s")
      }
      # the chart title already names the column, so the x-axis title
      # would only repeat it
      report_size(pv_histogram(data, nm, title = nm, subtitle = sub,
                               xlab = NA, mode = mode), 330)
    })
    note <- if (length(num_cols) > length(shown)) {
      tg$p(class = "note", sprintf(
        "Showing the first %d of %d numeric columns.",
        length(shown), length(num_cols)))
    }
    sections[[length(sections) + 1]] <- report_section(
      "distributions", "Distributions",
      sprintf(paste(
        "The shape of %s, binned with R's own histogram rules;",
        "hover any bar for the bin's exact range and count."),
        if (length(num_cols) == 1) "the one numeric column"
        else sprintf("each of the %d numeric columns", length(num_cols))),
      tg$div(class = "two-up", charts), note)
  }

  # --- categories --------------------------------------------------------
  if (length(cat_cols)) {
    shown <- utils::head(cat_cols, 4)
    charts <- lapply(shown, function(nm) {
      tab <- sort(table(as.character(data[[nm]])), decreasing = TRUE)
      top <- utils::head(tab, 12)
      df <- data.frame(level = names(top), count = as.numeric(top))
      sub <- if (length(tab) > nrow(df)) {
        sprintf("top %d of %d levels", nrow(df), length(tab))
      }
      report_size(
        pv_bar(df, x = "level", y = "count", horizontal = TRUE, sort = TRUE,
               xlab = NA, ylab = NA, title = nm, subtitle = sub,
               mode = mode),
        max(240, 110 + 26 * nrow(df)))
    })
    note <- if (length(cat_cols) > length(shown)) {
      tg$p(class = "note", sprintf(
        "Showing the first %d of %d categorical columns.",
        length(shown), length(cat_cols)))
    }
    sections[[length(sections) + 1]] <- report_section(
      "categories", "Categories",
      "The most common levels of each categorical column, largest first.",
      tg$div(class = "two-up", charts), note)
  }

  # --- relationships -----------------------------------------------------
  # Correlations need columns that actually vary; a constant column has no
  # correlation with anything and would only put NA warnings in the run.
  cor_cols <- num_cols[vapply(num_cols, function(nm) {
    isTRUE(stats::sd(data[[nm]], na.rm = TRUE) > 0)
  }, logical(1))]
  if (length(cor_cols) >= 3) {
    m <- stats::cor(data[cor_cols], use = "pairwise.complete.obs")
    # Long form for the heatmap: one row per cell, column-major, so both
    # axes keep the columns in data-frame order.
    long <- data.frame(x = colnames(m)[col(m)],
                       y = rownames(m)[row(m)],
                       r = as.numeric(m))
    long <- long[!is.na(long$r), ]
    heat <- report_size(
      pv_heatmap(long, x = "x", y = "y", value = "r",
                 palette = "diverging", mode = mode),
      min(560, max(280, 110 + 34 * length(cor_cols))))
    sections[[length(sections) + 1]] <- report_section(
      "relationships", "Relationships",
      sprintf(paste(
        "Pairwise Pearson correlations across the %d numeric columns:",
        "blue is negative, red is positive, and the pale midpoint is no",
        "correlation."), length(cor_cols)),
      heat)
  }

  # --- footer ------------------------------------------------------------
  footer <- tg$footer(
    "Report generated by ",
    tg$a(href = "https://github.com/jastephan63/polyviz", "polyviz"),
    sprintf(" on %s. Interactive d3.js charts driven entirely from R.",
            format(Sys.Date(), "%Y-%m-%d")))

  # save_html builds the <html>/<head>/<body> shell itself and hoists
  # tags$head content into the document head, so the page is handed over
  # as head material plus the main element - no nested html tags. The
  # background is the surface token, so nothing flashes white while the
  # stylesheet loads.
  page <- htmltools::tagList(
    tg$head(
      tg$meta(name = "viewport",
              content = "width=device-width, initial-scale=1"),
      tg$title(title),
      tg$style(htmltools::HTML(report_css(mode)))
    ),
    tg$main(header, sections, footer)
  )
  surface <- if (mode == "dark") pv_colors$ink$dark$surface
             else pv_colors$ink$light$surface
  htmltools::save_html(page, file = file, background = surface,
                       libdir = "lib")
  if (isTRUE(open)) {
    utils::browseURL(file)
  }
  invisible(file)
}
