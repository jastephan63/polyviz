# Builds the data story page (docs/story.html): "A Swiss canton in charts",
# a guided read through Canton Lucerne on live LUSTAT open data, with the
# package's bundled national datasets stepping in where the whole country
# sharpens a point. The page keeps one house rule throughout: every number
# in the prose is computed here from the fetched data - nothing is typed in
# from memory - so re-running the script refreshes the words along with the
# charts.
#
# The first run downloads the LUSTAT files (the population-scenario CSVs
# are large - allow a few minutes); every later run is served from the
# package's download cache (see pv_cache_status()) and works offline.
#
# Run from the package root: LANG=en_US.UTF-8 Rscript data-raw/build-story.R

suppressMessages(pkgload::load_all(".", quiet = TRUE))
library(htmltools)

# ---- small helpers ---------------------------------------------------------

# Whole numbers with thousands separators; percentages at a chosen
# precision; and the embed sizing every widget on the page shares.
fmt_n <- function(x) {
  format(round(as.numeric(x)), big.mark = ",", scientific = FALSE,
         trim = TRUE)
}
fmt_pct <- function(x, digits = 0) {
  paste0(formatC(x, format = "f", digits = digits), "%")
}
sized <- function(w, height) {
  w$width <- "100%"
  w$height <- height
  w
}

# The ids the bundled Lucerne map joins on (lakes carry no id and drop out).
lucerne_map_ids <- unlist(lapply(pv_lucerne_map$features, function(f) {
  v <- f$properties$id
  if (is.null(v)) NULL else as.character(v)
}))

quelle_lustat <- "Quelle: LUSTAT Statistik Luzern"
quelle_lustat_map <-
  "Quelle: LUSTAT Statistik Luzern; Grenzen: © BFS, ThemaKart"
quelle_bfs <- "Quelle: Bundesamt für Statistik"

# ---- fetch the LUSTAT data -------------------------------------------------

cat("Fetching LUSTAT datasets (cached after the first run)...\n")
szbv <- list(
  reference = pv_fetch_lustat("szbv-lu-2025-2055-referenz"),
  high      = pv_fetch_lustat("szbv-lu-2025-2055-hoch"),
  low       = pv_fetch_lustat("szbv-lu-2025-2055-tief")
)
gefis <- pv_fetch_lustat("gefis-lu-jr")
fa <- pv_fetch_lustat("fa-lu-ra")

ref <- szbv$reference

# ============================================================================
# Bevoelkerung - the people
# ============================================================================

# ---- chart 1: the three population scenarios -------------------------------

# One row per year and scenario: the canton total of the permanent resident
# population (swb = staendige Wohnbevoelkerung at year end). pv_line hands
# out the palette in alphabetical series order, so the variants are named
# "stronger" and "weaker" - that sorts "reference" first and lands the lead
# colour on the main line.
scenario_totals <- do.call(rbind, Map(function(d, nm) {
  tot <- tapply(d$swb, d$jahr, sum)
  data.frame(year = as.integer(names(tot)), scenario = nm,
             population = round(as.numeric(tot)))
}, szbv, c("reference", "stronger", "weaker")))
scenario_totals <- scenario_totals[
  order(match(scenario_totals$scenario,
              c("reference", "stronger", "weaker")),
        scenario_totals$year), ]

yr_first <- min(scenario_totals$year)
yr_last <- max(scenario_totals$year)
pop_now <- scenario_totals$population[
  scenario_totals$scenario == "reference" &
    scenario_totals$year == yr_first]
pop_ref <- scenario_totals$population[
  scenario_totals$scenario == "reference" & scenario_totals$year == yr_last]
pop_high <- scenario_totals$population[
  scenario_totals$scenario == "stronger" & scenario_totals$year == yr_last]
pop_low <- scenario_totals$population[
  scenario_totals$scenario == "weaker" & scenario_totals$year == yr_last]
growth_ref_pct <- 100 * (pop_ref / pop_now - 1)
cross_rows <- scenario_totals[
  scenario_totals$scenario == "reference" &
    scenario_totals$population >= 500000, ]
yr_500k <- if (nrow(cross_rows)) min(cross_rows$year) else NA

w_scenarios <- sized(pv_line(
  scenario_totals, x = "year", y = "population", series = "scenario",
  xlab = NA, ylab = "permanent residents",
  title = "Thirty years of growth, three ways it could go",
  subtitle = sprintf(
    "Permanent residents of Canton Lucerne at year end, %d–%d — LUSTAT scenarios",
    yr_first, yr_last),
  source = quelle_lustat), 430)

analysis_scenarios <- sprintf(paste(
  "The canton counts %s permanent residents at the end of %d.",
  "LUSTAT's reference scenario carries that to %s by %d — %s more",
  "people — and the weaker and stronger variants bracket the future",
  "between %s and %s. Even the cautious path adds %s residents%s."),
  fmt_n(pop_now), yr_first, fmt_n(pop_ref), yr_last,
  fmt_pct(growth_ref_pct, 1), fmt_n(pop_low), fmt_n(pop_high),
  fmt_n(pop_low - pop_now),
  if (!is.na(yr_500k)) {
    sprintf(paste("; on the reference path the canton passes the",
                  "half-million mark around %d"), yr_500k)
  } else "")

# ---- chart 2: the ageing wave ----------------------------------------------

# Five-year age bands, oldest at the top so the darkening upper rows read
# as the canton ageing. Rows arrive in the order the heatmap should draw
# them - it keeps first-appearance order on both axes.
band_start <- pmin(ref$alter %/% 5L * 5L, 100L)
band_label <- ifelse(band_start == 100L, "100+",
                     paste0(band_start, "–", band_start + 4L))
age_grid <- tapply(ref$swb, list(band = band_label, year = ref$jahr), sum)
age_order <- order(-vapply(
  rownames(age_grid),
  function(b) as.integer(sub("^(\\d+).*$", "\\1", b)), integer(1)))
age_grid <- age_grid[age_order, , drop = FALSE]
age_long <- data.frame(
  age = rep(rownames(age_grid), times = ncol(age_grid)),
  year = rep(colnames(age_grid), each = nrow(age_grid)),
  residents = round(as.numeric(age_grid)))

share65 <- function(yr) {
  d <- ref[ref$jahr == yr, ]
  100 * sum(d$swb[d$alter >= 65]) / sum(d$swb)
}
n80 <- function(yr) {
  d <- ref[ref$jahr == yr, ]
  sum(d$swb[d$alter >= 80])
}
share65_now <- share65(yr_first)
share65_end <- share65(yr_last)
n80_now <- n80(yr_first)
n80_end <- n80(yr_last)
# The largest single-year age in the base year names the big cohorts the
# eye follows up the chart.
base_ages <- tapply(ref$swb[ref$jahr == yr_first],
                    ref$alter[ref$jahr == yr_first], sum)
peak_age <- as.integer(names(base_ages)[which.max(base_ages)])
peak_cohort_born <- yr_first - peak_age

w_ageing <- sized(pv_heatmap(
  age_long, x = "year", y = "age", value = "residents",
  truncate_labels = 8,
  title = "The ageing wave rolls up the chart",
  subtitle = sprintf(
    "Residents by five-year age band, reference scenario %d–%d",
    yr_first, yr_last),
  source = quelle_lustat), 540)

analysis_ageing <- sprintf(paste(
  "Growth is not spread evenly across the ages. In %d, %s of residents",
  "are 65 or older; by %d it is %s. The 80-plus group grows fastest of",
  "all, from %s to %s people — %.1f times as many — which is the",
  "dark band thickening at the top of the chart. The brightest diagonal",
  "is the canton's largest cohort, the roughly %d-year-olds of %d (born",
  "around %d), climbing one row every five years."),
  yr_first, fmt_pct(share65_now, 1), yr_last, fmt_pct(share65_end, 1),
  fmt_n(n80_now), fmt_n(n80_end), n80_end / n80_now,
  peak_age, yr_first, peak_cohort_born)

# ---- chart 3: the city among the Swiss cities (bundled BFS data) -----------

city_rank <- function(yr, city) {
  d <- pv_city_population[pv_city_population$year == yr, ]
  match(city, d$city[order(-d$population)])
}
luz <- pv_city_population[pv_city_population$city == "Luzern", ]
luz <- luz[order(luz$year), ]
luz_peak_i <- which.max(luz$population[luz$year < max(luz$year)])
luz_peak_yr <- luz$year[luz_peak_i]
luz_peak <- luz$population[luz_peak_i]
after_peak <- luz[luz$year > luz_peak_yr, ]
luz_trough_i <- which.min(after_peak$population)
luz_trough_yr <- after_peak$year[luz_trough_i]
luz_trough <- after_peak$population[luz_trough_i]
recovered <- after_peak[after_peak$population >= luz_peak, ]
luz_recover_yr <- if (nrow(recovered)) min(recovered$year) else NA
luz_latest_yr <- max(luz$year)
luz_latest <- luz$population[luz$year == luz_latest_yr]
rank_first <- city_rank(min(pv_city_population$year), "Luzern")
rank_latest <- city_rank(luz_latest_yr, "Luzern")
yr_city_first <- min(pv_city_population$year)

w_cities <- sized(pv_bump(
  pv_city_population, time = "year", id = "city", value = "population",
  top_n = 8,
  title = "Where Lucerne sits on the national ladder",
  subtitle = sprintf(
    "Switzerland's eight largest statistical cities by residents, ranked %d–%d",
    yr_city_first, luz_latest_yr),
  source = quelle_bfs), 450)

analysis_cities <- sprintf(paste(
  "Zoom out to the whole country and the cantonal capital holds a",
  "remarkably steady place: rank %d among Switzerland's statistical",
  "cities in %d, rank %d in %d. The path between was anything but",
  "steady — the city grew to %s residents by %d, lent %s of them to",
  "its suburbs by %d, and only passed its %d size again in %d, at %s",
  "residents. That dip-and-recover arc is the story of most large Swiss",
  "cities; Lucerne just drew it more sharply than its rank ever showed."),
  rank_first, yr_city_first, rank_latest, luz_latest_yr,
  fmt_n(luz_peak), luz_peak_yr, fmt_n(luz_peak - luz_trough),
  luz_trough_yr, luz_peak_yr, luz_recover_yr, fmt_n(luz_latest))

# ============================================================================
# Wirtschaft - the economy
# ============================================================================

# ---- chart 4: the capital's job mix (bundled BFS data) ---------------------

sector_short <- c(
  "Human Health And Social Work Activities" = "Health & social work",
  "Wholesale And Retail Trade; Repair Of Motor Vehicles And Motorcycles" =
    "Trade & repair",
  "Education" = "Education",
  "Professional, Scientific And Technical Activities" =
    "Professional & technical",
  "Financial And Insurance Activities" = "Finance & insurance",
  "Accommodation And Food Service Activities" = "Hotels & restaurants",
  "Administrative And Support Service Activities" = "Admin & support",
  "Public Administration And Defence; Compulsory Social Security" =
    "Public administration",
  "Construction" = "Construction",
  "Manufacturing" = "Manufacturing",
  "Information And Communication" = "Information & communication",
  "Transportation And Storage" = "Transport & storage",
  "Arts, Entertainment And Recreation" = "Arts & recreation",
  "Other Service Activities" = "Other services",
  "Real Estate Activities" = "Real estate"
)
luz_sec <- pv_city_sectors[pv_city_sectors$city == "Luzern", ]
luz_sec <- luz_sec[order(-luz_sec$share), ]
top_sec <- utils::head(luz_sec, 10)
top_sec$sector_label <- ifelse(is.na(sector_short[top_sec$sector]),
                               top_sec$sector,
                               unname(sector_short[top_sec$sector]))

health_share <- luz_sec$share[luz_sec$sector_code == "Q"]
second_share <- luz_sec$share[2]
second_name <- sector_short[luz_sec$sector[2]]
if (is.na(second_name)) second_name <- luz_sec$sector[2]
health_all <- pv_city_sectors[pv_city_sectors$sector_code == "Q", ]
n_cities_sec <- length(unique(pv_city_sectors$city))
n_health_higher <- sum(health_all$share > health_share)

w_sectors <- sized(pv_bar(
  top_sec, x = "sector_label", y = "share", horizontal = TRUE, sort = TRUE,
  xlab = NA, ylab = "share of the city's employees (%)",
  title = "A city that cares for a living",
  subtitle = "Ten largest sectors of the city of Luzern by share of employees",
  source = quelle_bfs), 420)

analysis_sectors <- sprintf(paste(
  "LUSTAT's open catalogue has no jobs file, so the federal enterprise",
  "statistics stand in for the capital. They show a service town with a",
  "twist: %s of the city of Luzern's employees work in health and social",
  "work, %.1f times the share of the next sector, %s (%s). Just %d of",
  "the %d Swiss statistical cities lean harder on health — the",
  "cantonal hospital, clinics, and care homes are the capital's",
  "quiet economic anchor."),
  fmt_pct(health_share, 1), health_share / second_share,
  tolower(second_name), fmt_pct(second_share, 1),
  n_health_higher, n_cities_sec)

# ---- chart 5: where the municipal franc goes (LUSTAT) ----------------------

# Operating expense from the harmonised municipal accounts: account group 3,
# with the internal cost charging (39) netted out - those francs appear
# once as expense and once as revenue in the same accounts, so keeping
# them would double-count. Function groups follow the first digit of the
# HRM2 function number.
fn_names <- c(
  "0" = "Administration", "1" = "Order & safety", "2" = "Education",
  "3" = "Culture & sport", "4" = "Health", "5" = "Social security",
  "6" = "Transport", "7" = "Environment & space", "8" = "Economy",
  "9" = "Finances & taxes")
er <- gefis[gefis$erhebungsteil == "Erfolgsrechnung" & !is.na(gefis$art_nr), ]
expense <- er[startsWith(er$art_nr, "3") &
                substr(er$art_nr, 1, 2) != "39" & !is.na(er$fkt_nr), ]
expense$fn <- unname(fn_names[substr(expense$fkt_nr, 1, 1)])
spend <- aggregate(saldo ~ jahr + fn, expense, sum)
spend$chf_m <- spend$saldo / 1e6

gefis_last <- max(spend$jahr)
gefis_first <- min(spend$jahr)
last_by_fn <- spend[spend$jahr == gefis_last, ]
last_by_fn <- last_by_fn[order(-last_by_fn$chf_m), ]
keep_fn <- utils::head(last_by_fn$fn, 7)
spend$fn_group <- ifelse(spend$fn %in% keep_fn, spend$fn, "Everything else")
spend_g <- aggregate(chf_m ~ jahr + fn_group, spend, sum)
# Largest series first, so the stack and the legend read big to small.
fn_order <- c(keep_fn, "Everything else")
spend_g <- spend_g[order(match(spend_g$fn_group, fn_order), spend_g$jahr), ]
names(spend_g)[names(spend_g) == "fn_group"] <- "function_group"

tot_last <- sum(last_by_fn$chf_m)
tot_first <- sum(spend$chf_m[spend$jahr == gefis_first])
top_fn <- last_by_fn$fn[1]
top_fn_share <- 100 * last_by_fn$chf_m[1] / tot_last
first_by_fn <- spend[spend$jahr == gefis_first, ]
growth_by_fn <- merge(first_by_fn[, c("fn", "chf_m")],
                      last_by_fn[, c("fn", "chf_m")],
                      by = "fn", suffixes = c("_first", "_last"))
growth_by_fn$pct <- 100 * (growth_by_fn$chf_m_last /
                             growth_by_fn$chf_m_first - 1)
big_growers <- growth_by_fn[growth_by_fn$chf_m_last >= 100, ]
fastest <- big_growers[which.max(big_growers$pct), ]

w_spending <- sized(pv_area(
  spend_g, x = "jahr", y = "chf_m", series = "function_group",
  xlab = NA, ylab = "CHF million",
  title = "Where the municipal franc goes",
  subtitle = sprintf(
    "Operating expense of all Lucerne municipalities in CHF million, %d–%d",
    gefis_first, gefis_last),
  source = quelle_lustat), 440)

top_fn_growth <- growth_by_fn$pct[growth_by_fn$fn == top_fn]
analysis_spending <- sprintf(paste(
  "Add up the operating accounts of every municipality and the canton's",
  "communal level spent CHF %s million in %d — %s more than in %d,",
  "with internal charges between departments netted out. %s is the",
  "biggest item, %s of the total, and grew %s over the period; the",
  "fastest-growing major item is %s, up %s in %d years."),
  fmt_n(tot_last), gefis_last, fmt_pct(100 * (tot_last / tot_first - 1)),
  gefis_first, top_fn, fmt_pct(top_fn_share), fmt_pct(top_fn_growth),
  tolower(fastest$fn), fmt_pct(fastest$pct), gefis_last - gefis_first)

# ---- chart 6: the fiscal map (LUSTAT) --------------------------------------

# The newest equalisation year whose municipalities all sit on the bundled
# map - LUSTAT publishes a year or two ahead, and a merger can give a new
# municipality a number the map vintage does not know yet.
fa_years <- sort(unique(fa$fa_jahr), decreasing = TRUE)
fa_year <- fa_years[vapply(fa_years, function(y) {
  all(fa$gnr[fa$fa_jahr == y] %in% lucerne_map_ids)
}, logical(1))][1]
fa_now <- fa[fa$fa_jahr == fa_year, ]
ri_top <- fa_now[which.max(fa_now$ri), ]
ri_bottom <- fa_now[which.min(fa_now$ri), ]
n_below <- sum(fa_now$ri < 100)
n_muni_fa <- nrow(fa_now)
ra_total_m <- sum(fa_now$ra[fa_now$ra > 0]) / 1e6
n_receiving <- sum(fa_now$ra > 0)

w_fiscal <- sized(pv_choropleth(
  fa_now, id = "gnr", value = "ri", palette = "diverging", center = 100,
  title = "The lakeside ring pays for the hinterland",
  subtitle = sprintf(
    "Municipal resource index, %d equalisation year — cantonal average = 100",
    fa_year),
  source = quelle_lustat_map), 530)

analysis_fiscal <- sprintf(paste(
  "The same economy looks very different across the canton's map. In the",
  "%d equalisation year, %s tops the resource index at %s — %.1f",
  "times the cantonal average of 100 — while %s sits at %s. %d of",
  "the %d municipalities fall below the average, and the resource",
  "equalisation moves CHF %s million to %d of them: the lakeside ring",
  "around the capital underwrites the rural west."),
  fa_year, ri_top$gname, formatC(ri_top$ri, format = "f", digits = 1),
  ri_top$ri / 100, ri_bottom$gname,
  formatC(ri_bottom$ri, format = "f", digits = 1),
  n_below, n_muni_fa, fmt_n(ra_total_m), n_receiving)

# ============================================================================
# Raum - the territory
# ============================================================================

# ---- chart 7: where the growth lands (LUSTAT) ------------------------------

muni_now <- tapply(ref$swb[ref$jahr == yr_first], ref$gnr[ref$jahr == yr_first], sum)
muni_end <- tapply(ref$swb[ref$jahr == yr_last], ref$gnr[ref$jahr == yr_last], sum)
muni_names <- ref$gemeinde[!duplicated(ref$gnr)]
names(muni_names) <- ref$gnr[!duplicated(ref$gnr)]
growth <- data.frame(
  gnr = as.integer(names(muni_end)),
  municipality = unname(muni_names[names(muni_end)]),
  now = round(as.numeric(muni_now[names(muni_end)])),
  growth_pct = round(100 * (as.numeric(muni_end[names(muni_end)]) /
                              as.numeric(muni_now[names(muni_end)]) - 1), 1))
n_muni <- nrow(growth)
added_ref <- pop_ref - pop_now
fast <- growth[which.max(growth$growth_pct), ]
slow <- growth[which.min(growth$growth_pct), ]
n_shrinking <- sum(growth$growth_pct < 0)
n_above_avg <- sum(growth$growth_pct > growth_ref_pct)
any_shrink <- n_shrinking > 0

w_growth <- sized(pv_choropleth(
  growth, id = "gnr", value = "growth_pct",
  palette = if (any_shrink) "diverging" else "sequential",
  center = if (any_shrink) 0 else NULL,
  title = sprintf("Where the next %s residents will land", fmt_n(added_ref)),
  subtitle = sprintf(
    "Projected population change %d–%d in percent, reference scenario",
    yr_first, yr_last),
  source = quelle_lustat_map), 530)

top3 <- utils::head(growth[order(-growth$growth_pct), ], 3)
analysis_growth <- sprintf(paste(
  "The reference scenario adds %s residents to the canton — %s —",
  "but drops them very unevenly on the map. %s grows fastest, by %s,",
  "with %s (%s) and %s (%s) behind it; at the other end %s %s. Only",
  "%d of the %d municipalities out-grow the cantonal average%s — the",
  "coming growth is concentrated, not sprinkled."),
  fmt_n(added_ref), fmt_pct(growth_ref_pct, 1), top3$municipality[1],
  fmt_pct(top3$growth_pct[1], 1),
  top3$municipality[2], fmt_pct(top3$growth_pct[2], 1),
  top3$municipality[3], fmt_pct(top3$growth_pct[3], 1),
  slow$municipality,
  if (slow$growth_pct < 0) {
    sprintf("is projected to shrink by %s",
            fmt_pct(abs(slow$growth_pct), 1))
  } else {
    sprintf("barely moves at %s", fmt_pct(slow$growth_pct, 1))
  },
  n_above_avg, n_muni,
  if (n_shrinking > 0) {
    sprintf(", and %d are projected to shrink", n_shrinking)
  } else "")

# ---- chart 8: growth sorted by municipal size (LUSTAT) ---------------------

growth$size <- cut(growth$now, c(0, 2000, 10000, Inf),
                   labels = c("under 2,000 residents",
                              "2,000–10,000",
                              "over 10,000"))
growth <- growth[order(growth$size), ]
med_by_size <- tapply(growth$growth_pct, growth$size, stats::median)
size_counts <- table(growth$size)
biggest <- growth[which.max(growth$now), ]

w_swarm <- sized(pv_beeswarm(
  growth, value = "growth_pct", group = "size", label = "municipality",
  xlab = sprintf("projected growth %d–%d (%%)", yr_first, yr_last),
  title = sprintf("%d municipal futures, sorted by size", n_muni),
  subtitle = "Each dot is a municipality — hover for its name and growth",
  source = quelle_lustat), 430)

analysis_swarm <- sprintf(paste(
  "Sorting the same %d futures by today's size shows who carries the",
  "growth. The median municipality under 2,000 residents grows %s, the",
  "mid-sized ones %s, and the %d municipalities over 10,000 grow %s at",
  "the median%s. The capital itself (%s residents in %d) sits at %s,",
  "%s the canton-wide %s."),
  n_muni, fmt_pct(med_by_size[[1]], 1), fmt_pct(med_by_size[[2]], 1),
  size_counts[[3]], fmt_pct(med_by_size[[3]], 1),
  if (med_by_size[[3]] > med_by_size[[2]] &&
        med_by_size[[2]] > med_by_size[[1]]) {
    paste(" — the canton's future is being built where people",
          "already are")
  } else "",
  fmt_n(biggest$now), yr_first, fmt_pct(biggest$growth_pct, 1),
  if (biggest$growth_pct < growth_ref_pct) "below" else "above",
  fmt_pct(growth_ref_pct, 1))

# ---- chart 9: the ground it lands on (bundled BFS data) --------------------

luz_land <- pv_city_landuse[pv_city_landuse$city == "Luzern", ]
tot_ha <- sum(luz_land$hectares)
settle_ha <- sum(luz_land$hectares[luz_land$group == "Settlement"])
settle_share <- 100 * settle_ha / tot_ha
buildings_ha <- luz_land$hectares[luz_land$category == "Buildings"]
green_ha <- sum(luz_land$hectares[luz_land$category %in%
                                    c("Agriculture", "Forest")])
all_cities_settle <- 100 *
  sum(pv_city_landuse$hectares[pv_city_landuse$group == "Settlement"]) /
  sum(pv_city_landuse$hectares)

w_landuse <- sized(pv_treemap(
  luz_land, levels = c("group", "category"), value = "hectares",
  title = "What the capital's ground is used for",
  subtitle = "Land use of the city of Luzern in hectares, Arealstatistik",
  source = quelle_bfs), 470)

analysis_landuse <- sprintf(paste(
  "LUSTAT's open catalogue carries no land-use file yet, so the federal",
  "Arealstatistik describes the ground all that growth lands on. Of the",
  "city of Luzern's %s hectares, %s is settlement — buildings alone",
  "cover %s hectares — and yet farmland and forest still hold %s",
  "hectares inside the city limits. Across all %d Swiss statistical",
  "cities together, settlement covers %s of the ground: the capital is",
  "dense even by urban standards, which is exactly why the beeswarm",
  "above shows its growth flowing to the municipalities around it."),
  fmt_n(tot_ha), fmt_pct(settle_share), fmt_n(buildings_ha),
  fmt_n(green_ha), length(unique(pv_city_landuse$city)),
  fmt_pct(all_cities_settle))

# ---- assemble the page -----------------------------------------------------

# The page chrome mirrors the report and gallery: the same ink tokens from
# palette.R as CSS variables, light tokens by default and the dark set
# behind prefers-color-scheme, so the prose always agrees with the widgets.
story_css <- local({
  ink <- pv_colors$ink
  vars <- function(i) {
    paste0("--surface:", i$surface, ";--primary:", i$primary,
           ";--secondary:", i$secondary, ";--muted:", i$muted,
           ";--grid:", i$grid, ";--baseline:", i$baseline, ";")
  }
  paste0(
    ":root { color-scheme: light dark; ", vars(ink$light), " }\n",
    "@media (prefers-color-scheme: dark) { :root { ", vars(ink$dark),
    " } }\n",
    "body { margin: 0; background: var(--surface); color: var(--primary);\n",
    "       font-family: ", pv_font_stack(), "; }\n",
    "main { max-width: 880px; margin: 0 auto; padding: 0 20px 60px; }\n",
    "header { padding: 52px 0 6px; }\n",
    ".eyebrow { color: var(--muted); font-size: 12.5px;\n",
    "           letter-spacing: 0.14em; text-transform: uppercase;\n",
    "           margin: 0 0 10px; }\n",
    "header h1 { font-size: 36px; letter-spacing: -0.02em; margin: 0; }\n",
    ".lede { color: var(--secondary); max-width: 700px; line-height: 1.6;\n",
    "        font-size: 16px; }\n",
    "nav { display: flex; flex-wrap: wrap; gap: 6px 18px;\n",
    "      padding: 8px 0 10px; }\n",
    "nav a { color: var(--secondary); font-size: 13.5px;\n",
    "        text-decoration: none;\n",
    "        border-bottom: 1px solid transparent; }\n",
    "nav a:hover { border-bottom-color: #006ba2; color: #006ba2; }\n",
    "section.theme { margin-top: 46px; border-top: 1px solid var(--grid);\n",
    "                padding-top: 30px; }\n",
    "section.theme > h2 { font-size: 25px; letter-spacing: -0.01em;\n",
    "                     margin: 0; }\n",
    "section.theme > h2 span { color: var(--muted); font-weight: 400; }\n",
    ".theme-intro { color: var(--secondary); line-height: 1.6;\n",
    "               max-width: 700px; margin: 8px 0 0; }\n",
    ".chart-block { margin: 38px 0 0; }\n",
    ".chart-block h3 { font-size: 19px; letter-spacing: -0.01em;\n",
    "                  margin: 0 0 6px; }\n",
    ".analysis { color: var(--secondary); line-height: 1.65;\n",
    "            max-width: 720px; margin: 0 0 16px; }\n",
    "footer { color: var(--muted); font-size: 12.5px; line-height: 1.6;\n",
    "         border-top: 1px solid var(--grid); padding-top: 18px;\n",
    "         margin-top: 52px; }\n",
    "footer a { color: inherit; }\n")
})

chart_block <- function(id, heading, analysis, widget) {
  tags$div(id = id, class = "chart-block",
           tags$h3(heading), tags$p(class = "analysis", analysis), widget)
}

theme_section <- function(id, german, english, intro, ...) {
  tags$section(id = id, class = "theme",
               tags$h2(german, tags$span(paste0(" — ", english))),
               tags$p(class = "theme-intro", intro), ...)
}

page <- tags$html(lang = "en", tags$head(
  tags$meta(charset = "utf-8"),
  tags$meta(name = "viewport",
            content = "width=device-width, initial-scale=1"),
  tags$title("A Swiss canton in charts"),
  tags$style(HTML(story_css))
), tags$body(tags$main(
  tags$header(
    tags$p(class = "eyebrow", "A polyviz data story"),
    tags$h1("A Swiss canton in charts"),
    tags$p(class = "lede", sprintf(paste(
      "LUSTAT Statistik Luzern, the statistical office of Canton",
      "Lucerne, opens its data one canton at a time — so this is not",
      "Switzerland in charts but one Swiss canton in charts, Lucerne as",
      "the lens. The portrait follows LUSTAT's own themes —",
      "Bevölkerung, Wirtschaft, Raum — through the portal's",
      "open datasets, and reaches for the package's bundled national",
      "data where the whole country sharpens the point. Every chart is",
      "live — hover, and take any of them along via the download",
      "button in the top-right corner — and every number in the text",
      "is computed from the data when the page is built. The canton",
      "counted %s people at the end of %d; here is where they live, work,",
      "and are headed."),
      fmt_n(pop_now), yr_first))
  ),
  tags$nav(
    tags$a(href = "#bevoelkerung", "Bevölkerung — the people"),
    tags$a(href = "#wirtschaft", "Wirtschaft — the economy"),
    tags$a(href = "#raum", "Raum — the territory"),
    tags$a(href = "index.html", "← polyviz gallery")
  ),
  theme_section(
    "bevoelkerung", "Bevölkerung", "the people",
    paste("How many people the canton holds, how old they are, and how",
          "its capital measures up against the other Swiss cities."),
    chart_block("scenarios", "Three roads out of the present",
                analysis_scenarios, w_scenarios),
    chart_block("ageing", "The ageing wave", analysis_ageing, w_ageing),
    chart_block("cities", "The capital on the national ladder",
                analysis_cities, w_cities)
  ),
  theme_section(
    "wirtschaft", "Wirtschaft", "the economy",
    paste("What the canton's people do for a living, what its",
          "municipalities spend, and how the tax base is shared out",
          "across the map."),
    chart_block("sectors", "What the capital does for a living",
                analysis_sectors, w_sectors),
    chart_block("spending", "Where the municipal franc goes",
                analysis_spending, w_spending),
    chart_block("fiscal", "The fiscal map", analysis_fiscal, w_fiscal)
  ),
  theme_section(
    "raum", "Raum", "the territory",
    paste("Where the coming growth will land, which places carry it,",
          "and what the ground it lands on looks like."),
    chart_block("growth-map", "Where the growth lands",
                analysis_growth, w_growth),
    chart_block("growth-size", "Who carries the growth",
                analysis_swarm, w_swarm),
    chart_block("landuse", "The ground it lands on",
                analysis_landuse, w_landuse)
  ),
  tags$footer(HTML(paste0(
    "Data: LUSTAT Statistik Luzern via data.lustat.ch, published under ",
    "the opendata.swiss “OPEN BY ASK” terms — free use ",
    "with source citation (“Quelle: LUSTAT Statistik Luzern”); ",
    "commercial use requires the data owner's permission. National ",
    "context: Bundesamt für Statistik via stats.swiss (“OPEN ",
    "BY” — free use, source citation required). Boundaries ",
    "© BFS, ThemaKart. Charts rendered by ",
    "<a href='https://d3js.org'>d3.js</a> v7, type set in ",
    "<a href='https://rsms.me/inter/'>Inter</a>. Story built with the ",
    "MIT-licensed R package ",
    "<a href='https://github.com/jastephan63/polyviz'>jastephan63/",
    "polyviz</a> on ", format(Sys.Date(), "%Y-%m-%d"),
    "; every figure is recomputed from the source data on each build. ",
    "<a href='index.html'>Back to the gallery</a>.")))
)))

dir.create("docs", showWarnings = FALSE)
save_html(page, "docs/story.html", libdir = "lib")
cat("story written to docs/story.html\n")
