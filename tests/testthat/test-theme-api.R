# A brand palette that clears every check in both modes (found by
# running the validator itself); several tests below lean on it.
brand <- c("#e07a3f", "#6a4c93", "#7f9032", "#0090ad")

# The worst adjacent-pair CVD Delta E a palette achieves in a given
# order - recomputed from scratch so the tests don't just trust the
# numbers the check object reports about itself.
worst_adjacent_cvd <- function(colors) {
  m <- cvd_delta_matrix(colors)
  n <- length(colors)
  min(m[cbind(seq_len(n - 1), 2:n)])
}

test_that("the packaged palettes pass their own validator", {
  chk <- pv_check_palette(pv_colors$categorical$light, mode = "light")
  expect_s3_class(chk, "pv_palette_check")
  expect_true(chk$ok)
  for (name in c("lightness", "chroma", "cvd", "separation")) {
    expect_equal(chk$checks[[name]]$status, "pass")
  }
  # The documented accessibility claim: worst adjacent pair 11.6 light,
  # 10.2 dark. If the maths here drifts from the palette builder's,
  # these two numbers are the tripwire.
  expect_equal(chk$input_worst, 11.6, tolerance = 0.05)
  chk_dark <- pv_check_palette(pv_colors$categorical$dark, mode = "dark")
  expect_true(chk_dark$ok)
  expect_equal(chk_dark$input_worst, 10.2, tolerance = 0.05)
})

test_that("near-identical adjacent greens fail, naming the CVD check", {
  bad <- c("#006ba2", "#4f9d3f", "#529f42", "#db444b")
  chk <- pv_check_palette(bad)
  expect_false(chk$ok)
  expect_equal(chk$checks$cvd$status, "fail")
  # the green/green pair is named - and so is green-next-to-red, the
  # classic protan confusion, while the sound blue/green pair is not
  expect_equal(chk$checks$cvd$offending$i, c(2, 3))
  expect_equal(chk$checks$cvd$offending$j, c(3, 4))
  expect_lt(chk$checks$cvd$offending$cvd[1], 6)
  # colours this close fail plain-vision separation too, but pass the
  # per-colour checks - the failure is about the pair, not the colours
  expect_equal(chk$checks$separation$status, "fail")
  expect_equal(chk$checks$lightness$status, "pass")
  expect_equal(chk$checks$chroma$status, "pass")
  out <- paste(capture.output(print(chk)), collapse = "\n")
  expect_match(out, "FAIL\\s+cvd")
  expect_match(out, "#4f9d3f/#529f42")
  expect_match(out, "overall: FAIL")
})

test_that("washed-out and grey colours fail their per-colour checks", {
  chk <- pv_check_palette(c("#f5f0e6", "#006ba2"))  # near-white on light
  expect_equal(chk$checks$lightness$status, "fail")
  expect_equal(chk$checks$lightness$offending, "#f5f0e6")
  chk2 <- pv_check_palette(c("#808080", "#006ba2"))  # plain grey
  expect_equal(chk2$checks$chroma$status, "fail")
  expect_equal(chk2$checks$chroma$offending, "#808080")
})

test_that("pairs in the 6-8 band warn instead of failing", {
  chk <- pv_check_palette(c("#2d9c7a", "#b2477e"))
  expect_true(chk$ok)
  expect_equal(chk$checks$cvd$status, "warn")
  out <- paste(capture.output(print(chk)), collapse = "\n")
  expect_match(out, "secondary encoding")
})

test_that("low contrast against the surface is a note, not a failure", {
  chk <- pv_check_palette(pv_colors$categorical$light, mode = "light")
  expect_equal(chk$checks$contrast$status, "note")
  expect_true("#dca61c" %in% chk$checks$contrast$offending)
  expect_true(chk$ok)
  # a darker surface flips the same colours to passing
  chk2 <- pv_check_palette(pv_colors$categorical$light, surface = "#000000")
  expect_equal(chk2$checks$contrast$status, "pass")
})

test_that("suggested_order improves the worst pair of a scramble", {
  scrambled <- pv_colors$categorical$light[c(6, 4, 2, 8, 5, 1, 7, 3)]
  chk <- pv_check_palette(scrambled)
  expect_setequal(chk$suggested_order, 1:8)
  expect_equal(chk$input_worst, worst_adjacent_cvd(scrambled))
  expect_equal(chk$suggested_worst,
               worst_adjacent_cvd(scrambled[chk$suggested_order]))
  expect_gt(chk$suggested_worst, chk$input_worst)
  # the exhaustive search can never do worse than the packaged order,
  # which is one of the orderings it visits
  expect_gte(chk$suggested_worst, 11.6 - 0.05)
})

test_that("suggested_order keeps the first colour first on ties", {
  # with two colours both orders score identically; the tie-break must
  # keep the caller's lead colour in front
  chk <- pv_check_palette(c("#006ba2", "#db444b"))
  expect_equal(chk$suggested_order, c(1L, 2L))
})

test_that("the validator refuses non-colours", {
  expect_error(pv_check_palette(c("#006ba2", "not a colour")), "[Ii]nvalid")
  expect_error(pv_check_palette(character(0)), "character vector")
  expect_error(pv_check_palette(c("#006ba2", NA)), "character vector")
})

test_that("pv_set_theme round-trips through the widget payload", {
  on.exit(pv_reset_theme())
  agg <- aggregate(revenue ~ region, pv_sales, sum)
  pv_set_theme(colors = brand, font = "Georgia, serif")
  w <- pv_bar(agg, "region", "revenue")
  expect_equal(w$x$theme$categorical$light, brand)
  expect_equal(w$x$theme$font, "Georgia, serif")
  # unsupplied pieces keep the packaged values
  expect_equal(w$x$theme$sequential, pv_colors$sequential)
  expect_equal(w$x$theme$ink, pv_colors$ink)
  # the derived darks are one per light colour, and are real hex
  expect_length(w$x$theme$categorical$dark, length(brand))
  expect_true(all(grepl("^#[0-9a-f]{6}$", w$x$theme$categorical$dark)))
  pv_reset_theme()
  w2 <- pv_bar(agg, "region", "revenue")
  expect_equal(w2$x$theme$categorical, pv_colors$categorical)
  expect_equal(w2$x$theme$font, pv_font_stack())
})

test_that("sequential ramps can be swapped independently", {
  on.exit(pv_reset_theme())
  ramp <- c("#f7fbff", "#6baed6", "#08306b")
  pv_set_theme(sequential = ramp)
  agg <- aggregate(revenue ~ region, pv_sales, sum)
  w <- pv_bar(agg, "region", "revenue")
  expect_equal(w$x$theme$sequential$light, ramp)
  expect_equal(w$x$theme$sequential$dark, pv_colors$sequential$dark)
  expect_equal(w$x$theme$categorical, pv_colors$categorical)
})

test_that("derived darks keep hue but land inside the dark band", {
  darks <- derive_dark_palette(pv_colors$categorical$light)
  expect_length(darks, 8)
  expect_true(all(grepl("^#[0-9a-f]{6}$", darks)))
  lab <- hex_to_oklab(darks)
  band <- lightness_band("dark")
  # 0.01 of slack absorbs the 8-bit hex quantisation
  expect_true(all(lab[, 1] >= band[1] - 0.01 & lab[, 1] <= band[2] + 0.01))
  # hue preserved: compare hue angles before and after, modulo wrap
  lab_l <- hex_to_oklab(pv_colors$categorical$light)
  hue_diff <- atan2(lab[, 3], lab[, 2]) - atan2(lab_l[, 3], lab_l[, 2])
  hue_diff <- atan2(sin(hue_diff), cos(hue_diff))
  expect_true(all(abs(hue_diff) < 0.05))
  # and the brand palette's derived darks pass the full dark-mode check
  expect_true(pv_check_palette(derive_dark_palette(brand),
                               mode = "dark")$ok)
})

test_that("pv_set_theme aborts on a hard failure, printing the report", {
  on.exit(pv_reset_theme())
  bad <- c("#006ba2", "#4f9d3f", "#529f42", "#db444b")
  out <- capture.output(
    expect_error(pv_set_theme(colors = bad), "fails accessibility"))
  expect_match(paste(out, collapse = "\n"), "FAIL\\s+cvd")
  # nothing half-applied: the packaged theme is still in force
  agg <- aggregate(revenue ~ region, pv_sales, sum)
  w <- pv_bar(agg, "region", "revenue")
  expect_equal(w$x$theme$categorical$light, pv_colors$categorical$light)
})

test_that("warn-band palettes go through with a warning", {
  on.exit(pv_reset_theme())
  expect_warning(
    pv_set_theme(colors = c("#2d9c7a", "#b2477e"),
                 colors_dark = c("#006ba2", "#db444b")),
    "secondary encoding")
  agg <- aggregate(revenue ~ region, pv_sales, sum)
  w <- pv_bar(agg, "region", "revenue")
  expect_equal(w$x$theme$categorical$light, c("#2d9c7a", "#b2477e"))
})

test_that("check = FALSE warns that accessibility is on the caller", {
  on.exit(pv_reset_theme())
  bad <- c("#4f9d3f", "#529f42")
  expect_warning(pv_set_theme(colors = bad, check = FALSE),
                 "your responsibility")
  agg <- aggregate(revenue ~ region, pv_sales, sum)
  w <- pv_bar(agg, "region", "revenue")
  expect_equal(w$x$theme$categorical$light, bad)
})

test_that("pv_set_theme validates its arguments", {
  expect_error(pv_set_theme(colors = brand, colors_dark = brand[1:2]),
               "same length")
  expect_error(pv_set_theme(colors = c("#006ba2", "nope")), "[Ii]nvalid")
  expect_error(pv_set_theme(sequential = c("#fff", "#000")), "at least 3")
  expect_error(pv_set_theme(colors = brand, font = c("a", "b")),
               "font-family")
})
