# Machinery for the end-to-end render tests in test-render.R: one small
# canonical widget per chart type, and an expectation that pushes it
# through pv_save()'s real headless-Chrome pipeline.

# The render tests drive a real headless Chrome; skip cleanly wherever
# chromote or a browser is missing (CRAN included).
render_skip_if_no_chrome <- function() {
  skip_on_cran()
  skip_if_not_installed("chromote")
  chrome <- tryCatch(chromote::find_chrome(), error = function(e) NULL)
  skip_if(is.null(chrome) || !nzchar(chrome), "no Chrome-based browser found")
}

# One builder per chart type, each a trimmed-down cousin of the demo
# gallery snippet for that chart, on the bundled datasets. The list names
# are exactly the widget type strings the JavaScript dispatches on, so
# every renderer gets exercised.
render_charts <- list(
  bar = function() {
    pv_bar(aggregate(revenue ~ region, pv_sales, sum),
           x = "region", y = "revenue", title = "Revenue by region")
  },
  line = function() {
    pv_line(pv_city_population[pv_city_population$city %in%
                                 c("Luzern", "Emmen", "Kriens", "Zug"), ],
            x = "year", y = "population", series = "city",
            title = "A century of urban growth")
  },
  scatter = function() {
    pv_scatter(pv_fiscal[pv_fiscal$year == 2025, ],
               x = "resource_index", y = "equalization_chf",
               label = "municipality", title = "Fiscal equalization")
  },
  force = function() {
    pv_force(pv_network$nodes, pv_network$links, group = "group",
             title = "Collaboration network")
  },
  chord = function() {
    pv_chord(pv_flows, title = "Inter-warehouse shipments")
  },
  sunburst = function() {
    pv_sunburst(pv_city_landuse[pv_city_landuse$city == "Luzern", ],
                levels = c("group", "category"), value = "hectares",
                title = "Lucerne's land, ring by ring")
  },
  histogram = function() {
    pv_histogram(pv_fiscal[pv_fiscal$year == 2025, ],
                 x = "resource_per_capita", bins = 20, density = TRUE,
                 title = "Municipal tax bases")
  },
  boxplot = function() {
    pv_boxplot(pv_fiscal, value = "resource_per_capita", group = "year",
               title = "Tax resources by year")
  },
  violin = function() {
    pv_violin(pv_fiscal, value = "resource_per_capita", group = "year",
              title = "The same skewed shape, year after year")
  },
  ridgeline = function() {
    pv_ridgeline(pv_fiscal, value = "resource_index", group = "year",
                 title = "Resource index across municipalities")
  },
  donut = function() {
    seats <- aggregate(elected ~ party,
                       pv_elections[pv_elections$year == 2024, ], sum)
    seats <- seats[order(-seats$elected), ]
    seats$party[-(1:7)] <- "Other"
    pv_donut(seats, category = "party", value = "elected",
             title = "Council seats by party")
  },
  treemap = function() {
    pv_treemap(pv_city_landuse[pv_city_landuse$city == "Luzern", ],
               levels = c("group", "category"), value = "hectares",
               title = "How Lucerne uses its land")
  },
  lollipop = function() {
    f25 <- pv_fiscal[pv_fiscal$year == 2025, ]
    top <- head(f25[order(-f25$equalization_chf), ], 25)
    pv_lollipop(top, x = "municipality", y = "equalization_chf",
                title = "Where fiscal equalization flows")
  },
  area = function() {
    cities <- c("Luzern", "Emmen", "Kriens", "Horw", "Ebikon")
    agglo <- pv_city_population[pv_city_population$city %in% cities, ]
    pv_area(agglo, x = "year", y = "population", series = "city",
            title = "How the Lucerne agglomeration grew")
  },
  heatmap = function() {
    pop24 <- pv_city_population[pv_city_population$year == 2024, ]
    top12 <- head(pop24$city[order(-pop24$population)], 12)
    pv_heatmap(pv_city_sectors[pv_city_sectors$city %in% top12, ],
               x = "city", y = "sector", value = "share",
               title = "Where Swiss city jobs are")
  },
  calendar = function() {
    pv_calendar(pv_weather, date = "date", value = "temp_max",
                years = 2024:2025, title = "Two summers of daily maximums")
  },
  sankey = function() {
    e <- pv_elections[pv_elections$year == 2024, ]
    outcome <- ifelse(e$elected, "elected", "not elected")
    stage1 <- aggregate(list(value = rep(1, nrow(e))),
                        list(source = e$party, target = e$sex), sum)
    stage2 <- aggregate(list(value = rep(1, nrow(e))),
                        list(source = e$sex, target = outcome), sum)
    pv_sankey(rbind(stage1, stage2), title = "The path to a council seat")
  },
  parallel = function() {
    lu <- pv_city_landuse
    lu$share <- 100 * lu$hectares / ave(lu$hectares, lu$city, FUN = sum)
    keep <- c("Buildings", "Transport", "Agriculture", "Forest",
              "Urban green")
    wide <- reshape(lu[lu$category %in% keep,
                       c("city", "category", "share")],
                    direction = "wide", idvar = "city",
                    timevar = "category")
    names(wide) <- sub("^share\\.", "", names(wide))
    pv_parallel(wide, columns = keep, label = "city",
                title = "What covers the ground in Swiss cities")
  },
  pack = function() {
    pv_pack(pv_city_landuse, levels = c("group", "category"),
            value = "hectares", title = "What Swiss urban ground is made of")
  },
  dendrogram = function() {
    latest <- pv_city_population[pv_city_population$year ==
                                   max(pv_city_population$year), ]
    top <- head(latest$city[order(-latest$population)], 25)
    sect <- pv_city_sectors[pv_city_sectors$city %in% top, ]
    wide <- stats::xtabs(share ~ city + sector_code, data = sect)
    hc <- stats::hclust(stats::dist(scale(wide)), method = "ward.D2")
    pv_dendrogram(hc, k = 4, title = "Families of city economies")
  },
  choropleth = function() {
    pv_choropleth(pv_fiscal[pv_fiscal$year == 2025, ],
                  id = "municipality_id", value = "resource_index",
                  palette = "diverging", center = 100,
                  title = "Lucerne's wealth wears a lakeside ring")
  },
  race = function() {
    pv_race(pv_city_population, time = "year", id = "city",
            value = "population", top_n = 12,
            title = "Swiss cities racing through a century")
  },
  bump = function() {
    pv_bump(pv_city_population, time = "year", id = "city",
            value = "population", top_n = 10,
            title = "Who overtook whom since 1930")
  },
  beeswarm = function() {
    f25 <- pv_fiscal[pv_fiscal$year == 2025, ]
    f25$side <- ifelse(f25$equalization_chf > 0, "receives", "contributes")
    pv_beeswarm(f25, value = "resource_index", group = "side",
                label = "municipality",
                title = "Every municipality is a dot")
  }
)

# All PNGs from one test run land in the same directory, so a debugging
# session can look at every capture side by side.
render_out_dir <- local({
  dir <- NULL
  function() {
    if (is.null(dir)) {
      dir <<- tempfile("polyviz-render-")
      dir.create(dir)
    }
    dir
  }
})

# When CI (or a curious human) sets POLYVIZ_RENDER_DIR, every capture is
# also copied there under its chart-type name, for eyeballing later.
render_publish <- function(path) {
  out <- Sys.getenv("POLYVIZ_RENDER_DIR")
  if (!nzchar(out)) {
    return(invisible())
  }
  if (!dir.exists(out)) {
    dir.create(out, recursive = TRUE, showWarnings = FALSE)
  }
  file.copy(path, file.path(out, basename(path)), overwrite = TRUE)
  invisible()
}

# The render contract for one chart type: pv_save() must complete with no
# warning (it warns when the browser hit a JavaScript error - the
# regression signal these tests exist for), and the PNG must exist and be
# too big to be a blank capture. No pixel comparison on purpose: font
# rendering differs across machines, so exact baselines would flake.
expect_chart_renders <- function(id) {
  render_skip_if_no_chrome()
  w <- render_charts[[id]]()
  path <- file.path(render_out_dir(), paste0(id, ".png"))
  expect_no_warning(pv_save(w, path, quiet = TRUE))
  expect_true(file.exists(path))
  expect_gt(file.size(path), 20000)
  render_publish(path)
}
