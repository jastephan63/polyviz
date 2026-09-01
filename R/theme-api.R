# The theming API: check a palette for accessibility (pv_check_palette),
# swap the packaged colour tokens for your own (pv_set_theme), and put
# everything back (pv_reset_theme). pv_widget() in widgets.R reads the
# result out of `the$theme` / `the$font` when it builds a payload, so a
# theme set here restyles every chart for the rest of the session.
#
# The colour math below is the same pipeline the packaged palette was
# built with: sRGB -> linear RGB -> OKLab (a perceptual space where
# Euclidean distance roughly matches how different two colours look),
# with colour-vision deficiency simulated in linear RGB using the
# Machado et al. (2009) severity-1 matrices.

# ---- colour space conversions -----------------------------------------

# Hex strings to sRGB rows in [0, 1]. col2rgb also understands R colour
# names, so "tomato" works anywhere "#ff6347" does.
hex_to_srgb <- function(colors) {
  m <- tryCatch(grDevices::col2rgb(colors), error = function(e) {
    rlang::abort(sprintf("Invalid colour in palette: %s",
                         conditionMessage(e)))
  })
  t(m) / 255
}

# Undo / apply the sRGB gamma so we can do arithmetic on light, not on
# encoded pixel values.
srgb_to_linear <- function(v) {
  ifelse(v <= 0.04045, v / 12.92, ((v + 0.055) / 1.055)^2.4)
}
linear_to_srgb <- function(v) {
  ifelse(v <= 0.0031308, 12.92 * v, 1.055 * v^(1 / 2.4) - 0.055)
}

# Cube root that survives the tiny negative values out-of-gamut colours
# can produce (x^(1/3) in R returns NaN for negative x).
safe_cbrt <- function(x) sign(x) * abs(x)^(1 / 3)

# Björn Ottosson's OKLab reference matrices: linear RGB to cone-ish LMS,
# cube-rooted LMS to Lab. Rows in, rows out.
OKLAB_M1 <- rbind(c(0.4122214708, 0.5363325363, 0.0514459929),
                  c(0.2119034982, 0.6806995451, 0.1073969566),
                  c(0.0883024619, 0.2817188376, 0.6299787005))
OKLAB_M2 <- rbind(c(0.2104542553, 0.7936177850, -0.0040720468),
                  c(1.9779984951, -2.4285922050, 0.4505937099),
                  c(0.0259040371, 0.7827717662, -0.8086757660))

linear_to_oklab <- function(rgb) {
  safe_cbrt(rgb %*% t(OKLAB_M1)) %*% t(OKLAB_M2)
}

hex_to_oklab <- function(colors) {
  linear_to_oklab(srgb_to_linear(hex_to_srgb(colors)))
}

# The way back: OKLab rows to linear RGB rows. Out-of-gamut colours come
# back with channels outside [0, 1]; callers decide whether to clamp or
# to desaturate and retry.
oklab_to_linear <- function(lab) {
  l <- (lab[, 1] + 0.3963377774 * lab[, 2] + 0.2158037573 * lab[, 3])^3
  m <- (lab[, 1] - 0.1055613458 * lab[, 2] - 0.0638541728 * lab[, 3])^3
  s <- (lab[, 1] - 0.0894841775 * lab[, 2] - 1.2914855480 * lab[, 3])^3
  cbind(4.0767416621 * l - 3.3077115913 * m + 0.2309699292 * s,
        -1.2684380046 * l + 2.6097574011 * m - 0.3413193965 * s,
        -0.0041960863 * l - 0.7034186147 * m + 1.7076147010 * s)
}

# ---- colour-vision deficiency and distances ---------------------------

# Machado et al. (2009) severity-1 matrices, applied in LINEAR RGB.
# Protanopia and deuteranopia cover the overwhelming majority of colour
# vision deficiency; tritan is rare enough that palettes are not gated
# on it.
CVD_PROTAN <- rbind(c(0.152286, 1.052583, -0.204868),
                    c(0.114503, 0.786281, 0.099216),
                    c(-0.003882, -0.048116, 1.051998))
CVD_DEUTAN <- rbind(c(0.367322, 0.860646, -0.227968),
                    c(0.280085, 0.672501, 0.047413),
                    c(-0.011820, 0.042940, 0.968881))

simulate_cvd <- function(linear_rgb, matrix) {
  pmin(pmax(linear_rgb %*% t(matrix), 0), 1)
}

# Pairwise perceptual distance: 100 times the Euclidean distance in
# OKLab, so the thresholds read on a familiar 0-100 Delta E scale.
delta_e_matrix <- function(lab) {
  d <- as.matrix(stats::dist(lab))
  100 * d
}

# Pairwise distance as a colour-deficient viewer sees it: simulate both
# deficiencies, measure in OKLab, and keep the smaller of the two - a
# pair must survive whichever deficiency confuses it more.
cvd_delta_matrix <- function(colors) {
  lin <- srgb_to_linear(hex_to_srgb(colors))
  pmin(delta_e_matrix(linear_to_oklab(simulate_cvd(lin, CVD_PROTAN))),
       delta_e_matrix(linear_to_oklab(simulate_cvd(lin, CVD_DEUTAN))))
}

# WCAG relative luminance (linear RGB with Rec. 709 weights) and the
# WCAG contrast ratio between two colours.
relative_luminance <- function(colors) {
  drop(srgb_to_linear(hex_to_srgb(colors)) %*% c(0.2126, 0.7152, 0.0722))
}
contrast_ratio <- function(colors, surface) {
  lc <- relative_luminance(colors)
  ls <- relative_luminance(surface)
  (pmax(lc, ls) + 0.05) / (pmin(lc, ls) + 0.05)
}

# ---- ordering search --------------------------------------------------

# Every permutation of 1..n as rows, in lexicographic order. n is capped
# at 8 by the callers (8! = 40320 rows - instant; 9! would not be).
all_perms <- function(n) {
  if (n == 1) return(matrix(1L, 1, 1))
  sub <- all_perms(n - 1)
  do.call(rbind, lapply(seq_len(n), function(k) {
    rest <- setdiff(seq_len(n), k)
    cbind(k, matrix(rest[sub], nrow(sub)))
  }))
}

# The worst adjacent-pair value each ordering achieves, given a pairwise
# distance matrix. Vectorised over all orderings at once.
worst_adjacent <- function(orders, pair_matrix) {
  worst <- rep(Inf, nrow(orders))
  for (i in seq_len(ncol(orders) - 1)) {
    worst <- pmin(worst, pair_matrix[cbind(orders[, i], orders[, i + 1])])
  }
  worst
}

# Exhaustively find the ordering whose WORST adjacent pair is as
# distinguishable as possible under colour-vision deficiency. Ties are
# broken in favour of keeping the caller's first colour first (their
# lead series keeps its colour), then by enumeration order, which keeps
# the result deterministic.
best_adjacent_order <- function(cvd_matrix) {
  n <- nrow(cvd_matrix)
  if (n == 1) return(1L)
  # Beyond 8 colours the factorial search is off the table - and polyviz
  # charts cap at 8 series anyway - so just keep the input order.
  if (n > 8) return(seq_len(n))
  orders <- all_perms(n)
  worst <- worst_adjacent(orders, cvd_matrix)
  best <- max(worst)
  tied <- which(worst >= best - 1e-9)
  first_kept <- tied[orders[tied, 1] == 1L]
  as.integer(orders[if (length(first_kept)) first_kept[1] else tied[1], ])
}

# ---- the validator ----------------------------------------------------

# The lightness band a categorical colour must sit in, per surface. On
# paper-white, colours outside it read washed-out or inky; on the dark
# surface the band is tighter because both glare and mud arrive sooner.
lightness_band <- function(mode) {
  if (mode == "light") c(0.43, 0.77) else c(0.48, 0.67)
}

# A tidy little table of every adjacent pair and its two distances -
# both the checks and the print method read from it.
adjacent_pairs <- function(colors, cvd, normal) {
  n <- length(colors)
  if (n < 2) {
    return(data.frame(i = integer(), j = integer(),
                      color_i = character(), color_j = character(),
                      cvd = numeric(), normal = numeric()))
  }
  i <- seq_len(n - 1)
  data.frame(i = i, j = i + 1,
             color_i = colors[i], color_j = colors[i + 1],
             cvd = cvd[cbind(i, i + 1)],
             normal = normal[cbind(i, i + 1)],
             stringsAsFactors = FALSE)
}

#' Check a categorical palette for accessibility
#'
#' Runs the accessibility gauntlet the packaged polyviz palette was built
#' against, in OKLab/OKLCH space:
#'
#' * **lightness** — every colour's OKLab L must sit inside the band for
#'   its surface (`0.43`–`0.77` on light, `0.48`–`0.67` on dark), so
#'   nothing reads washed-out or inky.
#' * **chroma** — OKLCH chroma of at least `0.10`, so no colour collapses
#'   into grey.
#' * **cvd** — adjacent pairs must stay apart for colour-deficient
#'   viewers: the smaller of the protanopia/deuteranopia distances
#'   (Machado et al. 2009, severity 1, simulated in linear RGB; Delta E =
#'   100 × OKLab distance) must reach `8`. Pairs at `6`–`8` pass with a
#'   warning — usable only when a secondary encoding (direct labels,
#'   position, pattern) carries the series identity too.
#' * **separation** — adjacent pairs need normal-vision Delta E of at
#'   least `15`.
#' * **contrast** — colours should reach WCAG contrast `3:1` against the
#'   chart surface; ones that fall short get a note (give their marks a
#'   thin surface-coloured outline), not a failure.
#'
#' Only *adjacent* pairs are gated because polyviz assigns colours in slot
#' order — the order is part of the accessibility contract. The result
#' therefore also carries `suggested_order`: the ordering of your input
#' (exhaustive search up to 8 colours) that maximises the worst adjacent
#' pair under colour-vision deficiency, keeping your first colour first
#' when possible.
#'
#' @param colors Character vector of colours (hex strings or R colour
#'   names), in the order series will receive them.
#' @param mode `"light"` or `"dark"` — the surface the palette will
#'   render on, which picks the lightness band and default surface.
#' @param surface Optional surface colour to test contrast against.
#'   Defaults to the packaged chart surface for `mode` (`"#fbf9f5"`
#'   light, `"#1b1a18"` dark).
#' @param x,... A `pv_palette_check` object and further arguments
#'   (ignored), for the print method.
#' @return An object of class `"pv_palette_check"`: a list with `colors`,
#'   `mode`, `surface`, `ok` (`TRUE` when no check failed hard), `checks`
#'   (per-check `status` — `"pass"`, `"warn"`, `"fail"`, or `"note"` —
#'   plus the offending colours or pairs), `suggested_order` (integer
#'   permutation of the input), and the worst adjacent CVD Delta E before
#'   (`input_worst`) and after (`suggested_worst`) reordering. Printing
#'   it gives a readable report.
#' @examples
#' # The packaged palette passes its own gauntlet
#' pv_check_palette(pv_palette(8))
#'
#' # Two near-identical greens side by side do not
#' chk <- pv_check_palette(c("#006ba2", "#4f9d3f", "#529f42", "#db444b"))
#' chk$ok
#' chk$suggested_order
#' @export
pv_check_palette <- function(colors, mode = c("light", "dark"),
                             surface = NULL) {
  mode <- match.arg(mode)
  if (!is.character(colors) || length(colors) < 1 || anyNA(colors)) {
    rlang::abort("`colors` must be a character vector of colours, no NAs.")
  }
  surface <- surface %||% pv_colors$ink[[mode]]$surface
  n <- length(colors)

  lab <- hex_to_oklab(colors)
  L <- lab[, 1]
  C <- sqrt(lab[, 2]^2 + lab[, 3]^2)
  cvd <- cvd_delta_matrix(colors)
  normal <- delta_e_matrix(lab)
  pairs <- adjacent_pairs(colors, cvd, normal)
  ratio <- contrast_ratio(colors, surface)
  band <- lightness_band(mode)

  status <- function(bad, level) if (any(bad)) level else "pass"

  checks <- list(
    lightness = list(
      status = status(L < band[1] | L > band[2], "fail"),
      band = band, L = stats::setNames(L, colors),
      offending = colors[L < band[1] | L > band[2]]),
    chroma = list(
      status = status(C < 0.10, "fail"),
      floor = 0.10, C = stats::setNames(C, colors),
      offending = colors[C < 0.10]),
    cvd = list(
      # Below 6 the pair is simply confusable: hard failure. Between 6
      # and 8 colour alone is not enough, but colour plus a secondary
      # encoding still works: warn.
      status = if (any(pairs$cvd < 6)) "fail"
               else if (any(pairs$cvd < 8)) "warn" else "pass",
      threshold = 8, warn_floor = 6, pairs = pairs,
      offending = pairs[pairs$cvd < 8, , drop = FALSE]),
    separation = list(
      status = status(pairs$normal < 15, "fail"),
      threshold = 15, pairs = pairs,
      offending = pairs[pairs$normal < 15, , drop = FALSE]),
    contrast = list(
      status = if (any(ratio < 3)) "note" else "pass",
      threshold = 3, surface = surface,
      ratio = stats::setNames(ratio, colors),
      offending = colors[ratio < 3])
  )

  suggested <- best_adjacent_order(cvd)
  structure(list(
    colors = colors, mode = mode, surface = surface,
    checks = checks,
    ok = !any(vapply(checks, `[[`, character(1), "status") == "fail"),
    suggested_order = suggested,
    input_worst = if (n > 1) min(pairs$cvd) else Inf,
    suggested_worst = if (n > 1) {
      min(cvd[cbind(suggested[-n], suggested[-1])])
    } else Inf
  ), class = "pv_palette_check")
}

#' @rdname pv_check_palette
#' @export
print.pv_palette_check <- function(x, ...) {
  n <- length(x$colors)
  cat(sprintf("polyviz palette check - %s mode, %d colour%s vs surface %s\n",
              x$mode, n, if (n == 1) "" else "s", x$surface))

  # One line per check: a status tag, the check's name, and either an
  # all-clear or the specific offenders with their numbers.
  tag <- c(pass = "  ok ", warn = " WARN", fail = " FAIL", note = " note")
  line <- function(name, st, detail) {
    cat(sprintf("%s  %-10s %s\n", tag[[st]], name, detail))
  }
  fmt_pair <- function(p, value, target) {
    paste(sprintf("%s/%s Delta E %.1f (need >= %g)",
                  p$color_i, p$color_j, value, target), collapse = "; ")
  }

  ck <- x$checks
  line("lightness", ck$lightness$status, if (length(ck$lightness$offending))
    paste("outside L", sprintf("[%.2f, %.2f]:", ck$lightness$band[1],
      ck$lightness$band[2]), paste(ck$lightness$offending, collapse = ", "))
    else sprintf("all L within [%.2f, %.2f]", ck$lightness$band[1],
                 ck$lightness$band[2]))
  line("chroma", ck$chroma$status, if (length(ck$chroma$offending))
    paste("below C 0.10:", paste(ck$chroma$offending, collapse = ", "))
    else "all C >= 0.10")
  off <- ck$cvd$offending
  line("cvd", ck$cvd$status, if (nrow(off))
    fmt_pair(off, off$cvd, 8) else "adjacent pairs survive protan/deutan")
  off <- ck$separation$offending
  line("separation", ck$separation$status, if (nrow(off))
    fmt_pair(off, off$normal, 15) else "adjacent pairs >= 15 apart")
  line("contrast", ck$contrast$status, if (length(ck$contrast$offending))
    paste0("below 3:1 vs surface (give marks a thin surface-coloured ",
           "outline): ", paste(ck$contrast$offending, collapse = ", "))
    else "all colours reach 3:1 vs surface")

  cat("overall:", if (x$ok) "ok" else "FAIL", "\n")
  if (ck$cvd$status == "warn") {
    cat("  pairs at Delta E 6-8 need a secondary encoding",
        "(direct labels, position, pattern)\n")
  }
  # Only advertise the reordering when it actually buys something.
  if (n > 1 && x$suggested_worst > x$input_worst + 0.05) {
    cat(sprintf(
      "suggested order: %s  (worst adjacent CVD Delta E %.1f -> %.1f)\n",
      paste(x$suggested_order, collapse = ", "),
      x$input_worst, x$suggested_worst))
  }
  invisible(x)
}

# Register the print method at load time as well. The roxygen @export
# above puts an S3method() entry in NAMESPACE when documentation is next
# regenerated; this keeps dispatch working in the meantime (and under
# pkgload), and registering twice is harmless.
.onLoad <- function(libname, pkgname) {
  registerS3method("print", "pv_palette_check", print.pv_palette_check)
}

# ---- deriving a dark palette ------------------------------------------

# One OKLCH colour to hex, desaturating in small steps until it fits the
# sRGB gamut. Lightness and hue are kept; only chroma gives way, which
# is the least noticeable thing to lose.
oklch_to_hex <- function(L, C, h) {
  repeat {
    lin <- oklab_to_linear(matrix(c(L, C * cos(h), C * sin(h)), nrow = 1))
    if (all(lin >= -1e-6 & lin <= 1 + 1e-6) || C <= 0) {
      srgb <- linear_to_srgb(pmin(pmax(lin, 0), 1))
      return(tolower(grDevices::rgb(srgb[1], srgb[2], srgb[3])))
    }
    C <- max(0, C - 0.005)
  }
}

# Build the dark-mode twins of a light palette: same hue and chroma per
# slot, with each colour's position in the light lightness band mapped
# to the same position in the (tighter) dark band. That keeps the
# palette's internal light/dark rhythm while landing every colour where
# it reads well on the dark surface.
derive_dark_palette <- function(colors) {
  lab <- hex_to_oklab(colors)
  L <- lab[, 1]
  C <- sqrt(lab[, 2]^2 + lab[, 3]^2)
  h <- atan2(lab[, 3], lab[, 2])
  light <- lightness_band("light")
  dark <- lightness_band("dark")
  t <- pmin(1, pmax(0, (L - light[1]) / (light[2] - light[1])))
  L_dark <- dark[1] + t * (dark[2] - dark[1])
  vapply(seq_along(colors),
         function(i) oklch_to_hex(L_dark[i], C[i], h[i]), character(1))
}

# ---- setting and resetting the theme ----------------------------------

# Shared validation for the palette arguments: a character vector of
# parseable colours or nothing at all.
check_color_vector <- function(value, name, min_len = 1) {
  if (is.null(value)) return(NULL)
  if (!is.character(value) || length(value) < min_len || anyNA(value)) {
    rlang::abort(sprintf(
      "`%s` must be a character vector of at least %d colour%s.",
      name, min_len, if (min_len == 1) "" else "s"))
  }
  hex_to_srgb(value)  # errors readably on anything unparseable
  value
}

#' Set a session-wide polyviz theme
#'
#' Replaces the packaged colour tokens for every chart built afterwards
#' in this session. Pieces you don't supply keep the packaged values, so
#' `pv_set_theme(colors = my_brand)` swaps only the categorical palette
#' and leaves the sequential ramps, diverging scale, and chart chrome
#' alone.
#'
#' When `colors` is given without `colors_dark`, the dark palette is
#' derived automatically: each colour keeps its hue and chroma and is
#' re-stepped into the dark-surface lightness band in OKLCH (desaturated
#' just enough to stay inside the sRGB gamut).
#'
#' By default the categorical palettes for both modes must pass
#' [pv_check_palette()] — a hard failure aborts with the full report
#' printed, and pairs in the warning band (Delta E 6–8 under
#' colour-vision deficiency) come with a warning to pair colour with a
#' secondary encoding. `check = FALSE` skips the gate entirely; polyviz
#' then makes no accessibility promises about your charts.
#'
#' @param colors Character vector of categorical colours for light mode,
#'   in the order series will receive them (slot order is part of the
#'   accessibility contract — see [pv_check_palette()]). Alternatively a
#'   complete theme object such as [pv_theme_paper()]: then every other
#'   palette argument must stay `NULL`, and the object's tokens —
#'   including its diverging scale and ink chrome, which have no
#'   piecemeal arguments — are applied wholesale.
#' @param colors_dark Categorical colours for dark mode, same length as
#'   `colors`. Omit to derive them from `colors` automatically.
#' @param sequential Light-mode sequential ramp (low to high, at least 3
#'   colours).
#' @param sequential_dark Dark-mode sequential ramp.
#' @param font CSS `font-family` stack for widget text, e.g.
#'   `"Georgia, serif"`.
#' @param check Validate the categorical palettes with
#'   [pv_check_palette()]? `TRUE` (default) aborts on hard failure;
#'   `FALSE` skips validation with a warning that accessibility is now
#'   your responsibility.
#' @return The full token list now in effect (shaped like [pv_colors]),
#'   invisibly.
#' @examples
#' # A four-colour brand palette; the dark twins are derived
#' pv_set_theme(colors = c("#e07a3f", "#6a4c93", "#7f9032", "#0090ad"))
#' sales <- aggregate(revenue ~ region, pv_sales, sum)
#' pv_bar(sales, x = "region", y = "revenue", title = "Now in brand colours")
#' pv_reset_theme()
#' @export
pv_set_theme <- function(colors = NULL, colors_dark = NULL,
                         sequential = NULL, sequential_dark = NULL,
                         font = NULL, check = TRUE) {
  # A complete theme object (class "pv_theme", e.g. pv_theme_paper())
  # carries every token at once. Unpack it into the piecemeal arguments
  # so it goes through exactly the same validation below; the pieces
  # with no argument of their own (diverging, ink) are held aside and
  # overlaid after the standard ones.
  bundle <- NULL
  if (inherits(colors, "pv_theme")) {
    if (!is.null(colors_dark) || !is.null(sequential) ||
        !is.null(sequential_dark) || !is.null(font)) {
      rlang::abort(paste(
        "When `colors` is a complete theme object, leave the other",
        "palette arguments NULL - the object already carries its own",
        "tokens."))
    }
    bundle <- colors
    colors <- bundle$categorical$light
    colors_dark <- bundle$categorical$dark
    sequential <- bundle$sequential$light
    sequential_dark <- bundle$sequential$dark
    font <- bundle$font
    # Fail before anything is applied if the bundled diverging poles or
    # ink chrome hold something col2rgb cannot parse.
    for (extra in list(bundle$diverging, bundle$ink)) {
      if (!is.null(extra)) hex_to_srgb(unlist(extra))
    }
  }
  check_color_vector(colors, "colors")
  check_color_vector(colors_dark, "colors_dark")
  check_color_vector(sequential, "sequential", min_len = 3)
  check_color_vector(sequential_dark, "sequential_dark", min_len = 3)
  if (!is.null(font) &&
      (!is.character(font) || length(font) != 1 || is.na(font))) {
    rlang::abort("`font` must be a single CSS font-family string.")
  }
  if (!is.null(colors) && !is.null(colors_dark) &&
      length(colors) != length(colors_dark)) {
    rlang::abort(paste(
      "`colors` and `colors_dark` must be the same length -",
      "each slot is the same series in both modes."))
  }

  # Start from the packaged tokens and overlay what was supplied, so the
  # stored theme is always a complete, pv_colors-shaped list.
  tokens <- pv_colors
  if (!is.null(colors)) {
    tokens$categorical$light <- colors
    tokens$categorical$dark <- colors_dark %||% derive_dark_palette(colors)
  } else if (!is.null(colors_dark)) {
    tokens$categorical$dark <- colors_dark
  }
  if (!is.null(sequential)) tokens$sequential$light <- sequential
  if (!is.null(sequential_dark)) tokens$sequential$dark <- sequential_dark
  if (!is.null(bundle$diverging)) tokens$diverging <- bundle$diverging
  if (!is.null(bundle$ink)) tokens$ink <- bundle$ink

  if (isTRUE(check)) {
    for (m in c("light", "dark")) {
      # Contrast is judged against the surface the charts will actually
      # use - the theme's own when it swaps the ink chrome.
      chk <- pv_check_palette(tokens$categorical[[m]], mode = m,
                              surface = tokens$ink[[m]]$surface)
      if (!chk$ok) {
        print(chk)
        rlang::abort(sprintf(paste(
          "The %s-mode palette fails accessibility checks (report above).",
          "Fix the colours, try `suggested_order`,%s or pass",
          "`check = FALSE` to take responsibility yourself."), m,
          if (m == "dark" && is.null(colors_dark))
            " supply `colors_dark` explicitly," else ""))
      }
      if (chk$checks$cvd$status == "warn") {
        rlang::warn(sprintf(paste(
          "Some adjacent pairs in the %s-mode palette sit at Delta E 6-8",
          "under colour-vision deficiency: keep a secondary encoding",
          "(direct labels, position, pattern) on those series."), m))
      }
    }
  } else {
    rlang::warn(paste(
      "Palette checks skipped (`check = FALSE`):",
      "the accessibility of these colours is now your responsibility."))
  }

  the$theme <- tokens
  if (!is.null(font)) the$font <- font
  invisible(tokens)
}

#' Reset the polyviz theme
#'
#' Clears any theme set with [pv_set_theme()]; charts built afterwards
#' use the packaged [pv_colors] tokens and font again.
#'
#' @return `NULL`, invisibly.
#' @examples
#' pv_reset_theme()
#' @export
pv_reset_theme <- function() {
  the$theme <- NULL
  the$font <- NULL
  invisible(NULL)
}
