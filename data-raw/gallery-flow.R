# Gallery snippets for the flow and multivariate charts. Each block: an
# id, an explanation for the demo page, and one runnable expression on
# the real bundled data. data-raw/build-gallery.R assembles these into
# docs/.

## sankey
# explain: The sankey diagram traces flows between stages - node height
#   is the total volume passing through, ribbon width the size of each
#   individual flow. Hovering a ribbon reads out its exact count, and
#   hovering a node fades every flow that doesn't touch it. Commuting in
#   and out of Canton Zug shows a one-sided bargain: in 2022-2024 about
#   43,000 people streamed in each day (Zurich and Lucerne sending the
#   biggest contingents) while only 20,000 left - and half of those
#   headed for Zurich.
local({
  latest <- pv_commuters[pv_commuters$period == max(pv_commuters$period), ]
  # Inbound rows flow region -> Zug, outbound rows Zug -> region. The
  # trailing space keeps each outbound destination distinct from its
  # inbound namesake, so every region shows up on both sides of Zug.
  links <- data.frame(
    source = ifelse(latest$direction == "to Zug", latest$region, "Zug"),
    target = ifelse(latest$direction == "to Zug", "Zug",
                    paste0(latest$region, " ")),
    value = latest$commuters
  )
  pv_sankey(links,
            title = "Commuter flows in and out of Canton Zug",
            subtitle = paste("Daily commuters by neighbouring region,",
                             max(latest$period)),
            note = "Source: Fachstelle Statistik Kanton Zug")
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
