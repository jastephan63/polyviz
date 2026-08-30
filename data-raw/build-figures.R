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
      # Force the mode so the capture doesn't depend on the machine's theme,
      # and switch entrance animations off: the screenshot machinery can
      # resize the viewport mid-capture, which re-renders the widget and
      # would otherwise freeze the picture mid-animation.
      w$x$mode <- mode
      w$x$duration <- 0
      html <- file.path(tmp, paste0(s$id, "-", mode, ".html"))
      htmlwidgets::saveWidget(w, html, selfcontained = FALSE,
                              libdir = "lib")
      png <- file.path("man/figures", paste0(s$id, "-", mode, ".png"))
      # Plain CDP screenshot instead of webshot2: the higher-level helpers
      # resize the viewport to compute their clip, which triggers the
      # widget's ResizeObserver re-render and freezes the picture
      # mid-animation. A session opened at the right size and captured
      # as-is never re-renders.
      wanted_h <- suppressWarnings(as.integer(w$height))
      if (!length(wanted_h) || is.na(wanted_h)) wanted_h <- 430
      b <- chromote::ChromoteSession$new(width = 840, height = wanted_h + 40)
      b$Page$navigate(paste0("file://", normalizePath(html)))
      Sys.sleep(2.2)
      shot <- b$Page$captureScreenshot(format = "png")
      writeBin(jsonlite::base64_dec(shot$data), png)
      b$close()
      cat("captured:", png, "\n")
    }
  }
}
