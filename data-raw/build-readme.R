# Regenerates the chart-gallery section of README.md from the same snippet
# files that feed the demo page and the figure captures - one source of
# truth for examples, explanations, and images. Everything between the
# gallery markers is replaced; the rest of the README is hand-written.
#
# Run from the package root: LANG=en_US.UTF-8 Rscript data-raw/build-readme.R

display_names <- c(
  bar = "Bar chart", line = "Line chart", scatter = "Scatter plot",
  force = "Force-directed network", chord = "Chord diagram",
  sunburst = "Zoomable sunburst", histogram = "Histogram",
  boxplot = "Boxplot", ridgeline = "Ridgeline", donut = "Donut chart",
  treemap = "Treemap", lollipop = "Lollipop chart", area = "Stacked area",
  heatmap = "Heatmap", sankey = "Sankey diagram",
  parallel = "Parallel coordinates"
)

parse_snippets <- function(path) {
  lines <- readLines(path, warn = FALSE)
  starts <- grep("^## ", lines)
  lapply(seq_along(starts), function(i) {
    from <- starts[i]
    to <- if (i < length(starts)) starts[i + 1] - 1 else length(lines)
    block <- lines[from:to]
    comment <- grepl("^#", block[-1])
    explain <- block[-1][comment]
    explain <- paste(trimws(sub("^#( +explain:)?", "", explain)),
                     collapse = " ")
    list(id = sub("^## ", "", block[1]), explain = trimws(explain),
         code = paste(block[-1][!comment], collapse = "\n"))
  })
}

files <- unique(c("data-raw/gallery-core.R", Sys.glob("data-raw/gallery-*.R")))
out <- character()
for (f in files[file.exists(files)]) {
  for (s in parse_snippets(f)) {
    nm <- if (s$id %in% names(display_names)) display_names[[s$id]] else s$id
    out <- c(out,
      paste0("### ", nm),
      "",
      s$explain,
      "",
      paste0('<picture><source media="(prefers-color-scheme: dark)" ',
             'srcset="man/figures/', s$id, '-dark.png">',
             '<img alt="', nm, '" src="man/figures/', s$id,
             '-light.png"></picture>'),
      "",
      "```r",
      s$code,
      "```",
      "")
  }
}

readme <- readLines("README.md", warn = FALSE)
a <- grep("<!-- gallery:start -->", readme, fixed = TRUE)
b <- grep("<!-- gallery:end -->", readme, fixed = TRUE)
stopifnot(length(a) == 1, length(b) == 1, a < b)
writeLines(c(readme[1:a], "", out, readme[b:length(readme)]), "README.md")
cat("README gallery updated with", sum(grepl("^### ", out)), "charts\n")
