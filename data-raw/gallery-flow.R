# Gallery snippets for the flow and multivariate charts. Each block: an
# id, an explanation for the demo page, and one runnable expression on
# the real bundled data. data-raw/build-gallery.R assembles these into
# docs/.

## sankey
# explain: The sankey diagram traces flows through stages - node height
#   is the volume passing through, ribbon width each individual flow, and
#   the totals are conserved from column to column. Hovering a ribbon
#   reads out its exact count; hovering a node fades every flow that
#   doesn't touch it. Here every 2024 candidacy for a Lucerne municipal
#   council flows from party through gender to outcome: Mitte and FDP
#   field most of the candidates, women are just 37% of the field - but
#   the two gender streams split into "elected" at virtually the same
#   rate (87% vs 86%), so the imbalance sits in who stands for election,
#   not in who wins one.
local({
  e <- pv_elections[pv_elections$year == max(pv_elections$year), ]
  outcome <- ifelse(e$elected, "elected", "not elected")
  # Two aggregated stages that share the gender nodes in the middle;
  # totals are conserved, so both columns fill the same height.
  stage1 <- aggregate(list(value = rep(1, nrow(e))),
                      list(source = e$party, target = e$sex), sum)
  stage2 <- aggregate(list(value = rep(1, nrow(e))),
                      list(source = e$sex, target = outcome), sum)
  pv_sankey(rbind(stage1, stage2),
            title = "The path to a council seat",
            subtitle = "Lucerne municipal council candidacies 2024: party \u2192 gender \u2192 outcome",
            note = "Source: LUSTAT Statistik Luzern")
})

## parallel
# explain: Parallel coordinates draw each row as one line threading
#   across several vertical axes, one per variable - the line's shape is
#   the row's profile, and bundles of similar shapes are the clusters.
#   Drag along any axis to keep only a value range (brushes on several
#   axes combine), double-click an axis to clear it, and hover a line to
#   read the full row. Each line here is one of 181 Swiss cities profiled
#   by what covers its ground: brush the top of the Buildings axis and
#   the surviving lines dive on Agriculture - the dense Geneva suburbs
#   around 70% built-up - while rural towns like Appenzell run the
#   opposite diagonal.
local({
  lu <- pv_city_landuse
  lu$share <- 100 * lu$hectares / ave(lu$hectares, lu$city, FUN = sum)
  keep <- c("Buildings", "Transport", "Agriculture", "Forest", "Urban green")
  wide <- reshape(lu[lu$category %in% keep, c("city", "category", "share")],
                  direction = "wide", idvar = "city", timevar = "category")
  names(wide) <- sub("^share\\.", "", names(wide))
  pv_parallel(wide, columns = keep, label = "city",
              title = "What covers the ground in Swiss cities",
              subtitle = "Share of each city's area in percent - drag along an axis to filter",
              source = "Source: Bundesamt für Statistik – Arealstatistik")
})
