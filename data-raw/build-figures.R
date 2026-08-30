# Captures a PNG of every gallery chart for the README, in both light and
# dark mode, using headless Chrome via webshot2. GitHub's README renderer
# picks the right one through a <picture> element.
#
# Run from the package root: LANG=en_US.UTF-8 Rscript data-raw/build-figures.R

suppressMessages(pkgload::load_all(".", quiet = TRUE))

parse_snippets <- function(path) {
  lines <- readLines(path, warn = FALSE)
  starts <- grep("^## ", lines)
  lapply(seq_along(starts), function(i) {
    from <- starts[i]
    to <- if (i < length(starts)) starts[i + 1] - 1 else length(lines)
    block <- lines[from:to]
    comment <- grepl("^#", block[-1])
    list(id = sub("^## ", "", block[1]),
         code = paste(block[-1][!comment], collapse = "\n"))
  })
}

dir.create("man/figures", recursive = TRUE, showWarnings = FALSE)
tmp <- tempfile("figs")
dir.create(tmp)

files <- unique(c("data-raw/gallery-core.R", Sys.glob("data-raw/gallery-*.R")))
for (f in files[file.exists(files)]) {
  for (s in parse_snippets(f)) {
    for (mode in c("light", "dark")) {
      w <- eval(parse(text = s$code), envir = new.env(parent = globalenv()))
      # Force the mode so the capture doesn't depend on the machine's theme.
      w$x$mode <- mode
      html <- file.path(tmp, paste0(s$id, "-", mode, ".html"))
      htmlwidgets::saveWidget(w, html, selfcontained = FALSE,
                              libdir = "lib")
      png <- file.path("man/figures", paste0(s$id, "-", mode, ".png"))
      webshot2::webshot(html, png, vwidth = 840, vheight = 470,
                        delay = 1.6)
      cat("captured:", png, "\n")
    }
  }
}
