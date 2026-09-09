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
  arc = function() {
    latest <- pv_commuters[pv_commuters$period ==
                             max(pv_commuters$period), ]
    both <- aggregate(commuters ~ region, latest, sum)
    both <- both[order(-both$commuters), ]
    pv_arc(data.frame(id = c(both$region, "Zug")),
           data.frame(source = both$region, target = "Zug",
                      value = both$commuters),
           title = "Commuter exchange with Canton Zug")
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
  waffle = function() {
    seats <- aggregate(elected ~ party,
                       pv_elections[pv_elections$year == 2024, ], sum)
    seats <- seats[order(-seats$elected), ]
    pv_waffle(seats, category = "party", value = "elected",
              title = "Council seats, one square per percent")
  },
  treemap = function() {
    pv_treemap(pv_city_landuse[pv_city_landuse$city == "Luzern", ],
               levels = c("group", "category"), value = "hectares",
               title = "How Lucerne uses its land")
  },
  icicle = function() {
    pv_icicle(pv_city_landuse[pv_city_landuse$city == "Luzern", ],
              levels = c("group", "category"), value = "hectares",
              title = "Lucerne's land, column by column")
  },
  lollipop = function() {
    f25 <- pv_fiscal[pv_fiscal$year == 2025, ]
    top <- head(f25[order(-f25$equalization_chf), ], 25)
    pv_lollipop(top, x = "municipality", y = "equalization_chf",
                title = "Where fiscal equalization flows")
  },
  slope = function() {
    big <- c("Z\u00fcrich", "Basel", "Gen\u00e8ve", "Bern", "Winterthur",
             "Luzern")
    pv_slope(pv_city_population[pv_city_population$city %in% big &
                                  pv_city_population$year %in%
                                    c(1970, 2024), ],
             x = "year", y = "population", group = "city",
             highlight = "Winterthur",
             title = "Half a century between two censuses")
  },
  dumbbell = function() {
    f20 <- pv_fiscal[pv_fiscal$year == 2020,
                     c("municipality", "resource_index")]
    f27 <- pv_fiscal[pv_fiscal$year == 2027,
                     c("municipality", "resource_index")]
    both <- merge(f20, f27, by = "municipality",
                  suffixes = c("_2020", "_2027"))
    movers <- head(both[order(-abs(both$resource_index_2027 -
                                     both$resource_index_2020)), ], 12)
    pv_dumbbell(movers, y = "municipality", x1 = "resource_index_2020",
                x2 = "resource_index_2027", labels = c("2020", "2027"),
                title = "Whose tax base moved most")
  },
  waterfall = function() {
    e20 <- aggregate(gwh ~ source,
                     pv_electricity[pv_electricity$year == 2020, ], sum)
    e25 <- aggregate(gwh ~ source,
                     pv_electricity[pv_electricity$year == 2025, ], sum)
    chg <- merge(e20, e25, by = "source", suffixes = c("_2020", "_2025"))
    chg$change <- chg$gwh_2025 - chg$gwh_2020
    chg <- chg[order(-chg$change), ]
    pv_waterfall(chg, x = "source", y = "change",
                 start = sum(chg$gwh_2020), total = "2025",
                 title = "What changed Swiss electricity production")
  },
  bullet = function() {
    n24 <- aggregate(nights ~ canton,
                     pv_tourism[pv_tourism$year == 2024, ], sum)
    n19 <- aggregate(nights ~ canton,
                     pv_tourism[pv_tourism$year == 2019, ], sum)
    rec <- merge(n24, n19, by = "canton", suffixes = c("", "_2019"))
    rec <- head(rec[order(-rec$nights), ], 8)
    # Honest synthetic context: each canton's own pre-pandemic level is
    # the target, with bands at half and 80% of it.
    rec$half <- 0.5 * rec$nights_2019
    rec$most <- 0.8 * rec$nights_2019
    pv_bullet(rec, label = "canton", value = "nights",
              target = "nights_2019", bands = c("half", "most"),
              title = "Tourism against its pre-pandemic mark")
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
  horizon = function() {
    nights <- aggregate(nights ~ canton + year, pv_tourism, sum)
    pv_horizon(nights, x = "year", y = "nights", series = "canton",
               title = "Where Switzerland's guests sleep")
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
  bubblemap = function() {
    cities <- merge(pv_city_coords,
                    pv_city_population[pv_city_population$year == 2024, ],
                    by = "city")
    pv_bubble_map(cities, lon = "lon", lat = "lat", size = "population",
                  label = "city", title = "Where urban Switzerland lives")
  },
  flowmap = function() {
    latest <- pv_commuters[pv_commuters$period ==
                             max(pv_commuters$period) &
                             pv_commuters$region != "Restliche Schweiz", ]
    flows <- data.frame(
      from = ifelse(latest$direction == "to Zug", latest$region, "Zug"),
      to = ifelse(latest$direction == "to Zug", "Zug", latest$region),
      commuters = latest$commuters)
    pv_flow_map(flows, from = "from", to = "to", value = "commuters",
                title = "Commuter exchange with Canton Zug")
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
  },
  pairs = function() {
    f25 <- pv_fiscal[pv_fiscal$year == 2025, ]
    f25$side <- ifelse(f25$equalization_chf > 0, "receives", "contributes")
    pv_pairs(f25, columns = c("resource_per_capita", "resource_index",
                              "equalization_chf"),
             color = "side", label = "municipality",
             title = "Every pair of fiscal measures at once")
  },
  # The one chart drawn as HTML rather than SVG. All three in-cell
  # encodings ride along, so the bar, shade, and sparkline paths are all
  # under the render contract.
  table = function() {
    pop <- pv_city_population[order(pv_city_population$city,
                                    pv_city_population$year), ]
    now <- pop[pop$year == 2024, c("city", "population")]
    then <- pop[pop$year == 1990, c("city", "population")]
    tab <- merge(now, then, by = "city", suffixes = c("", "_1990"))
    tab$growth <- 100 * (tab$population / tab$population_1990 - 1)
    tab$trend <- I(split(pop$population, pop$city)[tab$city])
    tab <- head(tab[order(-tab$population), ], 10)
    pv_table(tab, columns = c("city", "population", "growth", "trend"),
             bars = "population", shade = "growth", spark = "trend",
             title = "The largest Swiss cities in numbers")
  }
)

# Option variants: the same renderers again, but with the opt-in chart
# options switched on, so the headless run also executes those drawing
# paths. Each name is "<type>_<variant>"; the part before the underscore
# is the widget type the builder must produce.
render_variants <- list(
  bar_stacked = function() {
    mix <- aggregate(revenue ~ region + product, pv_sales, sum)
    pv_bar(mix, x = "region", y = "revenue", series = "product",
           stack = "stack", title = "Revenue by region and product")
  },
  bar_percent = function() {
    mix <- aggregate(revenue ~ region + product, pv_sales, sum)
    pv_bar(mix, x = "region", y = "revenue", series = "product",
           stack = "percent", title = "Product mix by region")
  },
  # The full print-ready pairing in one build: the paper theme's tokens
  # in the payload and the texture patterns drawn over every segment.
  # The theme is session state, so it is set for this one build and put
  # back before the builder returns - same discipline as the theme tests.
  bar_textured = function() {
    pv_set_theme(pv_theme_paper())
    on.exit(pv_reset_theme())
    mix <- aggregate(revenue ~ region + product, pv_sales, sum)
    pv_bar(mix, x = "region", y = "revenue", series = "product",
           stack = "stack", title = "Revenue by region and product") |>
      pv_textures()
  },
  # The de-CH locale exercises the JavaScript d3.formatLocale path: axis
  # ticks of 10'000 and up print apostrophe-grouped. Locale state is
  # session-wide like the theme, so the same set/reset discipline.
  bar_locale = function() {
    pv_locale("de-CH")
    on.exit(pv_locale(NULL))
    staedte <- c("Z\u00fcrich", "Gen\u00e8ve", "Basel", "Lausanne")
    pop <- pv_city_population[pv_city_population$year == 2024 &
                                pv_city_population$city %in% staedte, ]
    pv_bar(pop, x = "city", y = "population",
           title = "Bev\u00f6lkerung der gr\u00f6ssten St\u00e4dte")
  },
  violin_overlays = function() {
    pv_violin(pv_fiscal, value = "resource_per_capita", group = "year",
              box = TRUE, points = TRUE,
              title = "Tax resources with every municipality shown")
  },
  line_markers = function() {
    pv_line(pv_city_population[pv_city_population$city %in%
                                 c("Luzern", "Zug"), ],
            x = "year", y = "population", series = "city",
            show_points = TRUE, curve = "monotone",
            title = "Census years as dots")
  },
  line_zoom = function() {
    pv_line(pv_weather, x = "date", y = "temp_mean", zoom = TRUE,
            title = "Six years of daily means")
  },
  # The scatter's three big-cloud treatments. Contours replace the marks
  # with the d3-contour density pipeline; canvas = TRUE forces the
  # canvas mark layer on, so that drawing path runs regardless of the
  # 8,000-point threshold "auto" would apply; density = "hex" runs the
  # d3-hexbin counting path.
  scatter_density = function() {
    pv_scatter(pv_weather, x = "temp_min", y = "temp_max",
               density = TRUE, title = "Six years of days, as contours")
  },
  scatter_canvas = function() {
    pv_scatter(pv_weather, x = "temp_min", y = "temp_max",
               canvas = TRUE, title = "Six years of days, on canvas")
  },
  scatter_hex = function() {
    pv_scatter(pv_weather, x = "temp_min", y = "temp_max",
               density = "hex", title = "Six years of days, in hexagons")
  },
  # The three time-series layers of v1.3.0. Each computes in R and draws
  # through existing machinery, but each also exercises drawing code the
  # canonical line chart never reaches: the forecast fan (nested bands,
  # the dashed continuation, the end-of-data rule), the changepoint
  # marks piped through pv_annotate, and the decomposition's four-panel
  # facet path (shared x, per-panel y, the zero hline in every panel).
  line_forecast = function() {
    nights <- aggregate(nights ~ year, pv_tourism, sum)
    pv_line(nights, x = "year", y = "nights",
            title = "Hotel nights with a forecast fan") |>
      pv_forecast(horizon = 4)
  },
  line_changepoints = function() {
    nuclear <- pv_electricity[pv_electricity$source == "Nuclear", ]
    pv_line(nuclear, x = "date", y = "gwh",
            title = "Nuclear output and its level shift") |>
      pv_changepoints(levels = TRUE)
  },
  facet_decompose = function() {
    monthly <- aggregate(gwh ~ date, pv_electricity, sum)
    pv_decompose(monthly, x = "date", y = "gwh",
                 title = "Electricity production, decomposed")
  },
  # The two country-wide geo paths, on bundled layers only - the
  # municipality layer would need a network fetch, which tests never do.
  choropleth_cantons = function() {
    # The BFS issues municipality numbers in cantonal blocks, so each
    # city's canton number is a findInterval() lookup away.
    blocks <- c(1, 301, 1001, 1201, 1301, 1401, 1501, 1601, 1701, 2001,
                2401, 2701, 2761, 2901, 3001, 3101, 3201, 3501, 4001,
                4401, 5001, 5401, 6001, 6401, 6601, 6701)
    cities <- merge(pv_city_population[pv_city_population$year == 2024, ],
                    pv_city_coords[, c("city", "id")], by = "city")
    urban <- aggregate(
      list(population = cities$population),
      list(canton = findInterval(cities$id, blocks)), sum)
    pv_choropleth(urban, map = "cantons", id = "canton",
                  value = "population",
                  title = "Where urban Switzerland lives")
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
  w <- c(render_charts, render_variants)[[id]]()
  path <- file.path(render_out_dir(), paste0(id, ".png"))
  expect_no_warning(pv_save(w, path, quiet = TRUE))
  expect_true(file.exists(path))
  expect_gt(file.size(path), 20000)
  render_publish(path)
}
