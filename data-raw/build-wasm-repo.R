# Builds the WebAssembly package repository behind the gallery's "Run in
# your browser" buttons (docs/wasm-repo/, served next to the gallery by
# GitHub Pages). In the page, webR installs polyviz from here and pulls
# the CRAN dependencies from repo.r-wasm.org.
#
# polyviz has no compiled code, so a webR "binary" needs no emscripten
# toolchain: it is simply the package installed into a plain library and
# tarred back up, laid out the way webr::install() expects
# (bin/emscripten/contrib/<R major.minor>/). Two things make the tarball
# portable into webR's R:
#
#   * --no-byte-compile keeps version-tagged bytecode out of the
#     lazy-load databases, so a package installed under this machine's R
#     loads in the (newer) R inside webR - plain serialized closures are
#     readable by any R >= 3.5.
#   * The DESCRIPTION shipped to webR moves the heavy back-end Imports
#     (ggplot2, haven, DBI, RSQLite, base64enc, crosstalk) to Suggests.
#     NAMESPACE only imports from rlang/stats/utils, and the gallery
#     snippets never touch those back ends, so the trimmed build loads
#     and charts identically while sparing the browser tens of MB of
#     wasm downloads. Calling pv_db_connect() or friends inside webR
#     fails with R's usual "there is no package" message, which is
#     honest: those back ends are not part of the browser demo.
#
# Run from the package root: LANG=en_US.UTF-8 Rscript data-raw/build-wasm-repo.R
# Rebuild whenever the package version bumps, and bump `contrib_version`
# together with the webR release pinned in data-raw/build-gallery.R.

# The R major.minor inside the pinned webR release (v0.6.0 carries
# R 4.6.0); webr::install() looks the repo up under this path.
contrib_version <- "4.6"

# Imports the wasm build keeps (base packages plus what the chart path
# actually loads); everything else in Imports moves to Suggests.
imports_keep <- c("graphics", "htmltools", "htmlwidgets", "jsonlite",
                  "rlang", "stats", "tools", "utils")

pkg_root <- normalizePath(".")
desc <- read.dcf(file.path(pkg_root, "DESCRIPTION"))
version <- desc[1, "Version"]

# ---- a trimmed copy of the source ------------------------------------
# Only what an installed package is made from; tests, vignettes, and the
# website sources stay behind. man/ stays behind too because the install
# below skips the docs anyway.
src <- file.path(tempdir(), "polyviz")
unlink(src, recursive = TRUE)
dir.create(src)
for (f in c("DESCRIPTION", "NAMESPACE", "LICENSE", "NEWS.md",
            "R", "data", "inst")) {
  file.copy(file.path(pkg_root, f), src, recursive = TRUE)
}

split_field <- function(dcf, field) {
  if (!field %in% colnames(dcf) || is.na(dcf[1, field])) return(character())
  trimws(strsplit(dcf[1, field], ",")[[1]])
}
imports <- split_field(desc, "Imports")
suggests <- split_field(desc, "Suggests")
# Version constraints ride along with the names ("R (>= 4.1)" style), so
# match on the bare package name in front of any constraint.
bare <- sub("\\s*\\(.*$", "", imports)
moved <- imports[!bare %in% imports_keep]
desc[1, "Imports"] <- paste(imports[bare %in% imports_keep],
                            collapse = ",\n    ")
desc[1, "Suggests"] <- paste(c(moved, suggests), collapse = ",\n    ")
write.dcf(desc, file.path(src, "DESCRIPTION"), keep.white = colnames(desc))

# ---- install, then tar the installed tree ----------------------------
lib <- file.path(tempdir(), "wasm-lib")
unlink(lib, recursive = TRUE)
dir.create(lib)
status <- system2(
  file.path(R.home("bin"), "R"),
  c("CMD", "INSTALL", "--no-byte-compile", "--no-docs",
    "-l", shQuote(lib), shQuote(src)),
  stdout = TRUE, stderr = TRUE
)
if (!is.null(attr(status, "status"))) {
  cat(status, sep = "\n")
  stop("R CMD INSTALL of the trimmed package failed")
}

repo_dir <- file.path(pkg_root, "docs", "wasm-repo", "bin", "emscripten",
                      "contrib", contrib_version)
unlink(file.path(pkg_root, "docs", "wasm-repo"), recursive = TRUE)
dir.create(repo_dir, recursive = TRUE)
tgz <- file.path(repo_dir, paste0("polyviz_", version, ".tgz"))
# Finder droppings copied along with inst/ have no place in the archive.
unlink(list.files(lib, pattern = "^\\.DS_Store$", recursive = TRUE,
                  all.files = TRUE, full.names = TRUE))
# R's internal tar keeps macOS resource-fork noise out of the archive.
old <- setwd(lib)
utils::tar(tgz, files = "polyviz", compression = "gzip", tar = "internal")
setwd(old)

# The PACKAGES index webR resolves polyviz (and its trimmed dependency
# list) from; mac.binary just means "read each DESCRIPTION out of a
# .tgz", which is exactly what this repo holds.
tools::write_PACKAGES(repo_dir, type = "mac.binary")

cat(sprintf("wasm repo written to docs/wasm-repo (polyviz %s, contrib %s, %.1f MB)\n",
            version, contrib_version, file.size(tgz) / 2^20))
