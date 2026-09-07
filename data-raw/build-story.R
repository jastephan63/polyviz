# Builds the data story page (docs/story.html): "Anatomy of a Swiss canton",
# an investigation of Canton Lucerne on live open data - LUSTAT's municipal
# scenarios and equalisation accounts, joined by federal statistics from
# stats.swiss where the analysis needs them. The piece asks four questions:
# how many people the canton will hold and how old they will be; where the
# growth lands and what arrives with it; whether poor municipalities catch
# up with rich ones; and how much today's facts actually predict.
#
# The page keeps one house rule throughout: every number in the prose is
# computed here from the fetched data - nothing is typed in from memory -
# so re-running the script refreshes the words along with the charts. The
# quantitative methods (convergence regressions, Gini coefficients, Ward
# clustering, leave-one-out cross-validation) all run in this file, in
# plain base R, and are described in the appendix at the end of the page.
#
# The first run downloads the LUSTAT files (the population-scenario CSVs
# are large - allow a few minutes); every later run is served from the
# package's download cache (see pv_cache_status()) and works offline.
#
# Run from the package root: LANG=en_US.UTF-8 Rscript data-raw/build-story.R

suppressMessages(pkgload::load_all(".", quiet = TRUE))
library(htmltools)

# ---- small helpers ---------------------------------------------------------

fmt_n <- function(x) {
  format(round(as.numeric(x)), big.mark = ",", scientific = FALSE,
         trim = TRUE)
}
fmt_pct <- function(x, digits = 0) {
  paste0(formatC(x, format = "f", digits = digits), "%")
}
fmt_1 <- function(x) formatC(x, format = "f", digits = 1)
fmt_2 <- function(x) formatC(x, format = "f", digits = 2)
sized <- function(w, height) {
  w$width <- "100%"
  w$height <- height
  w
}

# Population-weighted Gini of a per-capita quantity: sort by the value,
# walk the Lorenz curve (cumulative population share vs cumulative share
# of the total), and take one minus twice the area under it (trapezoids).
gini_w <- function(x, w) {
  o <- order(x)
  x <- x[o]
  w <- w[o] / sum(w)
  cw <- cumsum(w)
  cx <- cumsum(x * w) / sum(x * w)
  b <- sum(diff(c(0, cw)) * (c(0, utils::head(cx, -1)) + cx) / 2)
  1 - 2 * b
}

# The Lorenz curve itself, as plot-ready points (percent on both axes).
lorenz_pts <- function(x, w) {
  o <- order(x)
  x <- x[o]
  w <- w[o] / sum(w)
  data.frame(pop = 100 * c(0, cumsum(w)),
             res = 100 * c(0, cumsum(x * w) / sum(x * w)))
}

rmse <- function(a, b) sqrt(mean((a - b)^2))

# The ids the bundled Lucerne map joins on (lakes carry no id and drop out).
lucerne_map_ids <- unlist(lapply(pv_lucerne_map$features, function(f) {
  v <- f$properties$id
  if (is.null(v)) NULL else as.character(v)
}))

quelle_lustat <- "Quelle: LUSTAT Statistik Luzern"
quelle_lustat_map <-
  "Quelle: LUSTAT Statistik Luzern; Grenzen: © BFS, ThemaKart"
quelle_bfs <- "Quelle: Bundesamt für Statistik"
quelle_mixed <- "Quelle: LUSTAT Statistik Luzern; Bundesamt für Statistik"

# ---- fetch the data --------------------------------------------------------

cat("Fetching open datasets (cached after the first run)...\n")
szbv <- list(
  reference = pv_fetch_lustat("szbv-lu-2025-2055-referenz"),
  stronger  = pv_fetch_lustat("szbv-lu-2025-2055-hoch"),
  weaker    = pv_fetch_lustat("szbv-lu-2025-2055-tief")
)
fa <- pv_fetch_lustat("fa-lu-ra")
phh <- pv_fetch_bfs("DF_STATPOP_PHH")
bil <- pv_fetch_bfs("DF_SSV_POP_BIL")

ref <- szbv$reference

# ============================================================================
# Chapter 1 - Drei Zukuenfte: the people and their futures
# ============================================================================

# ---- chart 1: the three population scenarios -------------------------------

scenario_totals <- do.call(rbind, Map(function(d, nm) {
  tot <- tapply(d$swb, d$jahr, sum)
  data.frame(year = as.integer(names(tot)), scenario = nm,
             population = round(as.numeric(tot)))
}, szbv, names(szbv)))
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
if (!is.na(yr_500k)) {
  w_scenarios <- pv_annotate(
    w_scenarios,
    pv_vline(yr_500k, label = sprintf("half a million (%d)", yr_500k)))
}

analysis_scenarios <- sprintf(paste(
  "The canton counts %s permanent residents at the end of %d. LUSTAT's",
  "reference scenario carries that to %s by %d — %s more people — and",
  "the weaker and stronger variants bracket the endpoint between %s and",
  "%s, a spread of %s residents. Note what the fan does not contain: a",
  "future in which the canton stops growing. Even the cautious path adds",
  "%s people%s. The real uncertainty, the next three charts argue, is",
  "not whether Lucerne grows but what the growth is made of — and what",
  "it cannot change."),
  fmt_n(pop_now), yr_first, fmt_n(pop_ref), yr_last,
  fmt_pct(growth_ref_pct, 1), fmt_n(pop_low), fmt_n(pop_high),
  fmt_n(pop_high - pop_low), fmt_n(pop_low - pop_now),
  if (!is.na(yr_500k)) {
    sprintf(paste(", and on the reference path the canton passes the",
                  "half-million mark around %d"), yr_500k)
  } else "")

# ---- chart 2: what the growth is made of -----------------------------------

# Every scenario row carries the year's flows: geb (births), tod (deaths,
# stored negative), ws (net migration). They add up exactly: start-of-year
# population + births + deaths + net migration = year-end population, which
# the script asserts rather than assumes.
comp_by <- lapply(szbv, function(d) {
  cp <- aggregate(cbind(geb, tod, ws, swb0, swb) ~ jahr, d, sum)
  stopifnot(max(abs(cp$swb0 + cp$geb + cp$tod + cp$ws - cp$swb)) < 1e-3)
  cp
})
comp <- comp_by$reference
comp_long <- rbind(
  data.frame(year = comp$jahr, flow = "births", persons = round(comp$geb)),
  data.frame(year = comp$jahr, flow = "deaths", persons = round(-comp$tod)),
  data.frame(year = comp$jahr, flow = "net migration",
             persons = round(comp$ws)))

# The year deaths first outnumber births, per scenario (NA = never).
cross_year <- vapply(comp_by, function(cp) {
  hit <- cp$jahr[-cp$tod > cp$geb]
  if (length(hit)) min(hit) else NA_integer_
}, integer(1))
cum_births <- sum(comp$geb)
cum_deaths <- -sum(comp$tod)
cum_mig <- sum(comp$ws)
cum_natural <- cum_births - cum_deaths
mig_share <- 100 * cum_mig / (cum_natural + cum_mig)

w_components <- sized(pv_line(
  comp_long, x = "year", y = "persons", series = "flow",
  xlab = NA, ylab = "persons per year",
  title = "Growth is imported: the scissors close on the birth surplus",
  subtitle = sprintf(
    "Projected births, deaths, and net migration per year, reference scenario %d–%d",
    yr_first, yr_last),
  source = quelle_lustat), 430)
# Only mark the crossing when it happens inside the horizon proper - at
# the very edge the vline label collides with the series labels, and the
# closing scissors are visible anyway.
if (!is.na(cross_year[["reference"]]) &&
    cross_year[["reference"]] <= yr_last - 3) {
  w_components <- pv_annotate(
    w_components,
    pv_vline(cross_year[["reference"]],
             label = sprintf("deaths overtake births (%d)",
                             cross_year[["reference"]])))
}

analysis_components <- sprintf(paste(
  "Decompose the reference path into its three flows and the growth",
  "changes character. Over the thirty years the canton gains %s people",
  "from net migration and only %s from natural change — %s of the net",
  "gain walks in from outside. And the natural surplus is a closing",
  "scissors: births rise from %s to %s a year, but deaths rise faster,",
  "from %s to %s, until they cross%s. The weaker variant crosses",
  "%s; the stronger one %s. Whichever line the canton follows, its",
  "growth engine is migration — a fact that matters for everything the",
  "later chapters find about where growth lands."),
  fmt_n(cum_mig), fmt_n(cum_natural), fmt_pct(mig_share),
  fmt_n(comp$geb[1]), fmt_n(comp$geb[nrow(comp)]),
  fmt_n(-comp$tod[1]), fmt_n(-comp$tod[nrow(comp)]),
  if (!is.na(cross_year[["reference"]])) {
    sprintf(" in %d", cross_year[["reference"]])
  } else " just beyond the horizon",
  if (!is.na(cross_year[["weaker"]])) {
    sprintf("already in %d", cross_year[["weaker"]])
  } else "never inside the horizon",
  if (!is.na(cross_year[["stronger"]])) {
    sprintf("in %d", cross_year[["stronger"]])
  } else "never inside the horizon")

# ---- chart 3: the dependency ratio is scenario-proof -----------------------

dep_rows <- do.call(rbind, lapply(names(szbv), function(nm) {
  d <- szbv[[nm]]
  old <- tapply(d$swb[d$alter >= 65], d$jahr[d$alter >= 65], sum)
  work <- tapply(d$swb[d$alter >= 20 & d$alter < 65],
                 d$jahr[d$alter >= 20 & d$alter < 65], sum)
  data.frame(year = as.integer(names(old)), scenario = nm,
             oadr = round(100 * as.numeric(old) /
                            as.numeric(work[names(old)]), 2))
}))
dep_rows <- dep_rows[
  order(match(dep_rows$scenario, c("reference", "stronger", "weaker")),
        dep_rows$year), ]
oadr_now <- dep_rows$oadr[dep_rows$scenario == "reference" &
                            dep_rows$year == yr_first]
oadr_end <- dep_rows$oadr[dep_rows$scenario == "reference" &
                            dep_rows$year == yr_last]
oadr_end_all <- dep_rows$oadr[dep_rows$year == yr_last]
oadr_spread <- max(oadr_end_all) - min(oadr_end_all)
pop_spread_pct <- 100 * (pop_high - pop_low) / pop_ref
ydr <- function(d, yr) {
  dd <- d[d$jahr == yr, ]
  100 * sum(dd$swb[dd$alter < 20]) /
    sum(dd$swb[dd$alter >= 20 & dd$alter < 65])
}
ydr_now <- ydr(ref, yr_first)
ydr_end <- ydr(ref, yr_last)

w_dependency <- sized(pv_line(
  dep_rows, x = "year", y = "oadr", series = "scenario",
  xlab = NA, ylab = "residents 65+ per 100 aged 20–64",
  title = "Migration buys size, not youth",
  subtitle = sprintf(
    "Old-age dependency ratio under all three scenarios, %d–%d",
    yr_first, yr_last),
  source = quelle_lustat), 420)

analysis_dependency <- sprintf(paste(
  "Here is the sharpest finding in the scenario data. The old-age",
  "dependency ratio — residents 65 and older per 100 of working age —",
  "climbs from %s to %s on the reference path, a rise of a third. Now",
  "compare the scenarios: in %d they end within %s points of each other,",
  "while the population totals they belong to differ by %s. Thirty years",
  "of higher or lower migration moves the size of the canton a great",
  "deal and its age structure almost not at all — the ageing wave is",
  "baked into cohorts already born. The youth ratio barely moves either",
  "(%s to %s young people per 100 of working age), so the whole shift",
  "lands on the old-age side of the ledger."),
  fmt_1(oadr_now), fmt_1(oadr_end), yr_last, fmt_1(oadr_spread),
  fmt_pct(pop_spread_pct, 1), fmt_1(ydr_now), fmt_1(ydr_end))

# ---- chart 4: the ageing wave, cohort by cohort ----------------------------

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
  "The wave the dependency ratio measures is visible cohort by cohort.",
  "In %d, %s of residents are 65 or older; by %d it is %s. The 80-plus",
  "group grows fastest of all, from %s to %s people — %.1f times as",
  "many — the band thickening at the top of the chart. The",
  "strongest diagonal is the canton's largest cohort, the roughly",
  "%d-year-olds of %d (born around %d), climbing one row every five",
  "years; follow it and it enters retirement age in the %ss. Nothing",
  "migration can plausibly do dissolves that diagonal — it can only",
  "pour younger rows in beneath it."),
  yr_first, fmt_pct(share65_now, 1), yr_last, fmt_pct(share65_end, 1),
  fmt_n(n80_now), fmt_n(n80_end), n80_end / n80_now,
  peak_age, yr_first, peak_cohort_born,
  floor((peak_cohort_born + 65) / 10) * 10)

# ============================================================================
# Chapter 2 - Wo das Wachstum landet: growth on the map
# ============================================================================

# ---- chart 5: where the growth lands ---------------------------------------

muni_now <- tapply(ref$swb[ref$jahr == yr_first],
                   ref$gnr[ref$jahr == yr_first], sum)
muni_end <- tapply(ref$swb[ref$jahr == yr_last],
                   ref$gnr[ref$jahr == yr_last], sum)
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
  "The reference scenario adds %s residents to the canton — %s — but",
  "drops them very unevenly on the map. %s grows fastest, by %s, with",
  "%s (%s) and %s (%s) behind it; at the other end %s %s. Only %d of",
  "the %d municipalities out-grow the cantonal average%s. The growth is",
  "concentrated, not sprinkled — and the concentration has a shape the",
  "clustering in chapter four will make precise: a corridor around the",
  "lake, a belt that keeps pace, and a hinterland that empties."),
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

# ---- chart 6: growth sorted by municipal size ------------------------------

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
  "%s the canton-wide %s — the city region grows through the ring",
  "around the city, not the city alone."),
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

# ---- chart 7: cradles or moving vans ---------------------------------------

# Cumulative 2025-2055 flows per municipality, as a share of the 2025
# population: natural change (births + deaths) against net migration.
cum_muni <- aggregate(cbind(geb, tod, ws) ~ gnr, ref, sum)
cum_muni$natural_rate <- 100 * (cum_muni$geb + cum_muni$tod) /
  as.numeric(muni_now[as.character(cum_muni$gnr)])
cum_muni$mig_rate <- 100 * cum_muni$ws /
  as.numeric(muni_now[as.character(cum_muni$gnr)])
cum_muni$municipality <- unname(muni_names[as.character(cum_muni$gnr)])

n_mig_only <- sum(cum_muni$natural_rate < 0 & cum_muni$mig_rate > 0)
n_both <- sum(cum_muni$natural_rate > 0 & cum_muni$mig_rate > 0)
n_neither <- sum(cum_muni$natural_rate < 0 & cum_muni$mig_rate < 0)
top_mig <- cum_muni[which.max(cum_muni$mig_rate), ]
top_nat <- cum_muni[which.max(cum_muni$natural_rate), ]
low_nat <- cum_muni[which.min(cum_muni$natural_rate), ]

w_cradles <- sized(pv_annotate(
  pv_scatter(
    cum_muni, x = "natural_rate", y = "mig_rate", label = "municipality",
    xlab = "natural change 2025–2055 (% of 2025 residents)",
    ylab = "net migration 2025–2055 (% of 2025 residents)",
    title = "Cradles or moving vans",
    subtitle = sprintf(
      "Cumulative projected flows per municipality, %d–%d, reference scenario",
      yr_first, yr_last),
    source = quelle_lustat),
  pv_hline(0), pv_vline(0),
  pv_note(top_mig$natural_rate, top_mig$mig_rate, top_mig$municipality,
          dx = -30, dy = 12),
  pv_note(low_nat$natural_rate, low_nat$mig_rate, low_nat$municipality,
          dx = 14, dy = -10)), 440)

analysis_cradles <- sprintf(paste(
  "Chapter one showed the canton's growth is imported; this chart shows",
  "the same is true municipality by municipality. Each dot places a",
  "municipality by its two cumulative flows over the thirty years, as a",
  "share of its %d population. %d of the %d municipalities sit in the",
  "upper-left quadrant — more deaths than births, growth only if the",
  "moving vans keep coming — and %d more grow on both counts. Just %d",
  "lose people on both. The extremes tell the story: %s tops the",
  "migration axis at %s of its base population, while %s runs the",
  "deepest natural deficit at %s; the largest natural surplus, %s,",
  "belongs to %s — the young families are already where the growth",
  "corridor is."),
  yr_first, n_mig_only, nrow(cum_muni), n_both, n_neither,
  top_mig$municipality, fmt_pct(top_mig$mig_rate, 0),
  low_nat$municipality, fmt_pct(low_nat$natural_rate, 0),
  fmt_pct(top_nat$natural_rate, 0), top_nat$municipality)

# ---- chart 8: the households the growth arrives in -------------------------

# BFS STATPOP household counts for Canton Lucerne, by household size.
phh_lu <- phh[phh$geo_unit_code == "LU" & !is.na(phh$obs_value), ]
size_order <- c("1", "2", "3", "4", "5", "60")
size_labels <- c("1 person", "2 persons", "3 persons", "4 persons",
                 "5 persons", "6+ persons")
hh <- phh_lu[phh_lu$hh_size_code %in% size_order, ]
hh$size <- size_labels[match(hh$hh_size_code, size_order)]
hh <- hh[order(match(hh$hh_size_code, size_order), hh$time_period), ]
hh_years <- sort(unique(hh$time_period))
hh_y0 <- min(hh_years)
hh_y1 <- max(hh_years)

hh_tot <- tapply(hh$obs_value, hh$time_period, sum)
# Average household size, counting "6+" as 6 - a slight undercount, so the
# true averages sit a shade above these.
hh_persons <- tapply(hh$obs_value * c(1, 2, 3, 4, 5, 6)[
  match(hh$hh_size_code, size_order)], hh$time_period, sum)
avg_size <- hh_persons / hh_tot
one_two <- tapply(hh$obs_value[hh$hh_size_code %in% c("1", "2")],
                  hh$time_period[hh$hh_size_code %in% c("1", "2")], sum)
one_two_share <- 100 * one_two / hh_tot
hh_growth_pct <- 100 * (hh_tot[[as.character(hh_y1)]] /
                          hh_tot[[as.character(hh_y0)]] - 1)
pp_growth_pct <- 100 * (hh_persons[[as.character(hh_y1)]] /
                          hh_persons[[as.character(hh_y0)]] - 1)

w_households <- sized(pv_area(
  hh, x = "time_period", y = "obs_value", series = "size",
  offset = "percent", xlab = NA,
  title = "The growth arrives in ever-smaller households",
  subtitle = sprintf(
    "Private households in Canton Lucerne by size, %d–%d — share of all households",
    hh_y0, hh_y1),
  source = quelle_bfs), 420)

analysis_households <- sprintf(paste(
  "Growth is usually counted in people, but it lands in households —",
  "and those are a different series. Between %d and %d the canton's",
  "household count grew %s while the people living in them grew",
  "roughly %s: the average household thinned from about %s to %s",
  "persons (counting six-plus households as six, so the true figures",
  "sit a shade higher). One- and two-person households now make up %s",
  "of all households, up from %s. Every percentage point of population",
  "growth therefore demands more than a point of new dwellings — the",
  "quiet multiplier underneath every housing debate the coming %s",
  "residents will trigger."),
  hh_y0, hh_y1, fmt_pct(hh_growth_pct, 1), fmt_pct(pp_growth_pct, 1),
  fmt_2(avg_size[[as.character(hh_y0)]]),
  fmt_2(avg_size[[as.character(hh_y1)]]),
  fmt_pct(one_two_share[[as.character(hh_y1)]], 1),
  fmt_pct(one_two_share[[as.character(hh_y0)]], 1),
  fmt_n(added_ref))

# ============================================================================
# Chapter 3 - Ungleiche Kassen: convergence, or not
# ============================================================================

fa_years <- sort(unique(fa$fa_jahr))
fa_y0 <- min(fa_years)
fa_y1 <- max(fa_years)

# ---- chart 9: the fiscal map -----------------------------------------------

# The newest equalisation year whose municipalities all sit on the bundled
# map - a merger can give a new municipality a number the map vintage does
# not know yet.
fa_map_year <- rev(fa_years)[vapply(rev(fa_years), function(y) {
  all(fa$gnr[fa$fa_jahr == y] %in% lucerne_map_ids)
}, logical(1))][1]
fa_now <- fa[fa$fa_jahr == fa_map_year, ]
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
    fa_map_year),
  source = quelle_lustat_map), 530)

analysis_fiscal <- sprintf(paste(
  "The tax base is as unevenly spread as the growth. In the %d",
  "equalisation year, %s tops the resource index at %s — %.1f times",
  "the cantonal average of 100 — while %s sits at %s. %d of the %d",
  "municipalities fall below the average, and the resource equalisation",
  "moves CHF %s million to %d of them. The interesting question is not",
  "the snapshot, though: it is whether the gap closes over time. The",
  "textbook says capital and people flowing to cheap land should pull",
  "the laggards up. The next four charts test that story on eight years",
  "of equalisation accounts."),
  fa_map_year, ri_top$gname, fmt_1(ri_top$ri), ri_top$ri / 100,
  ri_bottom$gname, fmt_1(ri_bottom$ri),
  n_below, n_muni_fa, fmt_n(ra_total_m), n_receiving)

# ---- chart 10: beta-convergence, tested ------------------------------------

beta_a <- fa[fa$fa_jahr == fa_y0, c("gnr", "gname", "rp_pEinw", "ri")]
beta_b <- fa[fa$fa_jahr == fa_y1, c("gnr", "rp_pEinw")]
beta <- merge(beta_a, beta_b, by = "gnr", suffixes = c("0", "1"))
beta$g <- 100 * ((beta$rp_pEinw1 / beta$rp_pEinw0)^(1 / (fa_y1 - fa_y0)) - 1)
n_beta <- nrow(beta)
n_dropped_beta <- length(unique(fa$gnr[fa$fa_jahr == fa_y0])) - n_beta

beta_fit <- stats::lm(g ~ ri0, data = transform(beta, ri0 = ri))
beta_slope100 <- 100 * stats::coef(beta_fit)[[2]]
beta_ci100 <- 100 * stats::confint(beta_fit)[2, ]
beta_r2 <- summary(beta_fit)$r.squared

w_beta <- sized(pv_annotate(
  pv_trend(pv_scatter(
    transform(beta, ri0 = ri), x = "ri0", y = "g", label = "gname",
    xlab = sprintf("resource index, %d equalisation year", fa_y0),
    ylab = sprintf("growth of per-capita resources %d–%d (%%/yr)",
                   fa_y0, fa_y1),
    title = "The catch-up that isn't happening",
    subtitle = sprintf(
      "Initial fiscal strength vs subsequent growth, %d municipalities — flat line, no convergence",
      n_beta),
    source = quelle_lustat), method = "lm"),
  pv_vline(100, label = "cantonal average")), 460)

analysis_beta <- sprintf(paste(
  "The classic convergence test regresses growth on the starting level:",
  "if poor municipalities catch up, the line slopes down. It does not.",
  "Across the %d municipalities with a full record, a hundred extra",
  "points of initial resource index go with %s percentage points of",
  "annual growth (95%% CI %s to %s) — a confidence band that straddles",
  "zero comfortably — and the starting level explains %s of the",
  "variation in growth (R² = %s). With n = %d and %d years this is an",
  "association test, not a verdict on causes, and it cannot rule out",
  "slow convergence hiding under noise. But the textbook's strong",
  "claim — poor places growing systematically faster — simply is not",
  "in these accounts."),
  n_beta, fmt_2(beta_slope100), fmt_2(beta_ci100[[1]]),
  fmt_2(beta_ci100[[2]]), fmt_pct(100 * beta_r2, 1),
  formatC(beta_r2, format = "f", digits = 3), n_beta, fa_y1 - fa_y0)

# ---- chart 11: sigma-divergence --------------------------------------------

# Short series names so the direct labels at the lines' right ends fit
# inside the chart; the prose spells both measures out in full.
sigma <- do.call(rbind, lapply(fa_years, function(y) {
  d <- fa[fa$fa_jahr == y, ]
  rbind(
    data.frame(year = y, measure = "CV (s.d./mean)",
               value = round(stats::sd(d$rp_pEinw) / mean(d$rp_pEinw), 4)),
    data.frame(year = y, measure = "s.d. of logs",
               value = round(stats::sd(log(d$rp_pEinw)), 4)))
}))
cv_first <- sigma$value[sigma$year == fa_y0 &
                          sigma$measure == "CV (s.d./mean)"]
cv_last <- sigma$value[sigma$year == fa_y1 &
                         sigma$measure == "CV (s.d./mean)"]
sdlog_first <- sigma$value[sigma$year == fa_y0 &
                             sigma$measure == "s.d. of logs"]
sdlog_last <- sigma$value[sigma$year == fa_y1 &
                            sigma$measure == "s.d. of logs"]

# The weakest half's share of the resource potential, first and last year -
# the same walk up the Lorenz curve as the next chart, taken at both ends
# of the record.
bottom50_of <- function(y) {
  d <- fa[fa$fa_jahr == y, ]
  o <- order(-d$rp_pEinw)
  cw <- cumsum(d$mwb_bj3_bj5[o]) / sum(d$mwb_bj3_bj5)
  cr <- cumsum(d$rp_pEinw[o] * d$mwb_bj3_bj5[o]) /
    sum(d$rp_pEinw * d$mwb_bj3_bj5)
  100 * (1 - stats::approx(cw, cr, xout = 0.50)$y)
}
b50_first <- bottom50_of(fa_y0)
b50_last <- bottom50_of(fa_y1)

w_sigma <- sized(pv_line(
  sigma, x = "year", y = "value", series = "measure", show_points = TRUE,
  xlab = NA, ylab = "dispersion across municipalities",
  title = "The spread is widening, not closing",
  subtitle = sprintf(
    "Dispersion of per-capita resource potential across municipalities, %d–%d",
    fa_y0, fa_y1),
  source = quelle_lustat), 420)

analysis_sigma <- sprintf(paste(
  "Sigma-convergence asks the same question without a regression: is",
  "the dispersion of per-capita resources shrinking? Both measures say",
  "no. The coefficient of variation rises from %s to %s over the eight",
  "equalisation years (+%s), the standard deviation of log resources",
  "from %s to %s. Neither line moves dramatically, and neither moves in",
  "a straight line — both dip and wobble on the way — but both end",
  "higher than they began: the municipalities are drifting apart, not",
  "together. One movement underneath is steady, though: the share of",
  "the resource potential held by the weakest half of the canton's",
  "residents slips from %s to %s across the same years. The next two",
  "charts measure that concentration properly — and what the transfers",
  "achieve against it."),
  fmt_2(cv_first), fmt_2(cv_last),
  fmt_pct(100 * (cv_last / cv_first - 1), 0),
  fmt_2(sdlog_first), fmt_2(sdlog_last),
  fmt_pct(b50_first, 1), fmt_pct(b50_last, 1))

# ---- chart 12: the Lorenz curve --------------------------------------------

fa_last <- fa[fa$fa_jahr == fa_y1, ]
lz_pre <- lorenz_pts(fa_last$rp_pEinw, fa_last$mwb_bj3_bj5)
lz_post <- lorenz_pts(fa_last$rp_pEinw + fa_last$ra / fa_last$mwb_bj3_bj5,
                      fa_last$mwb_bj3_bj5)
# "before/after transfers" rather than "equalisation" keeps the direct
# labels at the curves' ends inside the chart.
lorenz_df <- rbind(
  data.frame(pop = lz_pre$pop, share = lz_pre$res,
             curve = "before transfers"),
  data.frame(pop = lz_post$pop, share = lz_post$res,
             curve = "after transfers"),
  data.frame(pop = c(0, 100), share = c(0, 100), curve = "perfect equality"))
lorenz_df <- lorenz_df[order(match(lorenz_df$curve,
                                   c("before transfers",
                                     "after transfers",
                                     "perfect equality")),
                             lorenz_df$pop), ]
gini_pre_last <- gini_w(fa_last$rp_pEinw, fa_last$mwb_bj3_bj5)
gini_post_last <- gini_w(fa_last$rp_pEinw + fa_last$ra / fa_last$mwb_bj3_bj5,
                         fa_last$mwb_bj3_bj5)
# Share of the resource potential held by the strongest tenth of residents.
o <- order(-fa_last$rp_pEinw)
cw_top <- cumsum(fa_last$mwb_bj3_bj5[o]) / sum(fa_last$mwb_bj3_bj5)
cr_top <- cumsum(fa_last$rp_pEinw[o] * fa_last$mwb_bj3_bj5[o]) /
  sum(fa_last$rp_pEinw * fa_last$mwb_bj3_bj5)
top10_share <- 100 * stats::approx(cw_top, cr_top, xout = 0.10)$y
bottom50_share <- 100 * (1 - stats::approx(cw_top, cr_top, xout = 0.50)$y)

w_lorenz <- sized(pv_line(
  lorenz_df, x = "pop", y = "share", series = "curve",
  xlab = "cumulative share of residents (%), poorest municipalities first",
  ylab = "cumulative share of resources (%)",
  title = "How far the curve bends",
  subtitle = sprintf(
    "Lorenz curves of per-capita resource potential, %d equalisation year",
    fa_y1),
  source = quelle_lustat), 460)

analysis_lorenz <- sprintf(paste(
  "The Lorenz curve makes the concentration measurable. Sort residents",
  "by their municipality's per-capita resources and walk up the",
  "distribution: in the %d equalisation year, the half of the canton's",
  "residents in the weakest municipalities command %s of the resource",
  "potential, while the strongest tenth holds %s. The bend of the curve",
  "is the Gini coefficient — %s before equalisation. The transfers",
  "flatten it to %s: real compression, achieved with about CHF %s",
  "million, though the curve never comes close to the diagonal. This is",
  "a floor under the weak, not a ceiling on the strong."),
  fa_y1, fmt_pct(bottom50_share, 1), fmt_pct(top10_share, 1),
  formatC(gini_pre_last, format = "f", digits = 3),
  formatC(gini_post_last, format = "f", digits = 3),
  fmt_n(sum(fa_last$ra[fa_last$ra > 0]) / 1e6))

# ---- chart 13: the Gini path, before and after the transfers ---------------

gini_rows <- do.call(rbind, lapply(fa_years, function(y) {
  d <- fa[fa$fa_jahr == y, ]
  rbind(
    data.frame(year = y, state = "before transfers",
               gini = round(gini_w(d$rp_pEinw, d$mwb_bj3_bj5), 4)),
    data.frame(year = y, state = "after transfers",
               gini = round(gini_w(d$rp_pEinw + d$ra / d$mwb_bj3_bj5,
                                   d$mwb_bj3_bj5), 4)))
}))
gini_rows <- gini_rows[order(match(gini_rows$state,
                                   c("before transfers",
                                     "after transfers")),
                             gini_rows$year), ]
gini_pre0 <- gini_rows$gini[gini_rows$year == fa_y0 &
                              gini_rows$state == "before transfers"]
gini_pre1 <- gini_rows$gini[gini_rows$year == fa_y1 &
                              gini_rows$state == "before transfers"]
gini_post0 <- gini_rows$gini[gini_rows$year == fa_y0 &
                               gini_rows$state == "after transfers"]
gini_post1 <- gini_rows$gini[gini_rows$year == fa_y1 &
                               gini_rows$state == "after transfers"]
compress_pct <- 100 * (1 - mean(gini_rows$gini[
  gini_rows$state == "after transfers"] /
    gini_rows$gini[gini_rows$state == "before transfers"]))
gini_rise_pct <- 100 * (gini_pre1 / gini_pre0 - 1)

w_gini <- sized(pv_line(
  gini_rows, x = "year", y = "gini", series = "state", show_points = TRUE,
  xlab = NA, ylab = "Gini coefficient (population-weighted)",
  title = "Equalisation compresses the gap but cannot stop the climb",
  subtitle = sprintf(
    "Gini of per-capita resources across municipalities, %d–%d",
    fa_y0, fa_y1),
  source = quelle_lustat), 430)

analysis_gini <- sprintf(paste(
  "Put the whole period on one chart and the two findings separate",
  "cleanly. The upper line is concentration as the tax bases produce",
  "it: the Gini climbs from %s to %s — %s higher in %d than in %d,",
  "rising in nearly every year. There is no turning point to annotate;",
  "the climb is the story. The lower line is the same coefficient",
  "after the resource transfers, which strip out roughly %s of the",
  "inequality year after year (%s falling to %s means the after-line",
  "climbs too). Equalisation, in other words, is running to stand",
  "still: it compresses each year's gap while the underlying gap",
  "widens underneath it."),
  formatC(gini_pre0, format = "f", digits = 3),
  formatC(gini_pre1, format = "f", digits = 3),
  fmt_pct(gini_rise_pct, 0), fa_y1, fa_y0,
  fmt_pct(compress_pct, 0),
  formatC(gini_post0, format = "f", digits = 3),
  formatC(gini_post1, format = "f", digits = 3))

# ---- chart 14: the price of a weak tax base --------------------------------

tax <- fa_last[, c("gnr", "gname", "ri", "mStf")]
tax_fit <- stats::lm(mStf ~ ri, data = tax)
tax_cor <- stats::cor(tax$ri, tax$mStf)
tax_slope100 <- -100 * stats::coef(tax_fit)[[2]]
tax_r2 <- summary(tax_fit)$r.squared
tax_low <- tax[which.min(tax$mStf), ]
tax_high <- tax[which.max(tax$mStf), ]

w_tax <- sized(pv_annotate(
  pv_trend(pv_scatter(
    tax, x = "ri", y = "mStf", label = "gname",
    xlab = "resource index (cantonal average = 100)",
    ylab = "municipal tax multiplier (units)",
    title = "The price of living on a weak tax base",
    subtitle = sprintf(
      "Resource index vs tax multiplier, %d equalisation year",
      fa_y1),
    source = quelle_lustat), method = "lm"),
  pv_vline(100, label = "cantonal average"),
  pv_note(tax_low$ri, tax_low$mStf, tax_low$gname, dx = -24, dy = 14),
  pv_note(tax_high$ri, tax_high$mStf, tax_high$gname, dx = 16, dy = -8)),
  460)

analysis_tax <- sprintf(paste(
  "What does fiscal weakness cost the people who live in it? The tax",
  "multiplier answers: municipalities set their own, and the",
  "correlation with the resource index is r = %s — among the tightest",
  "relationships in this story (R² = %s). Along the fitted line, a",
  "hundred additional index points go with a multiplier about %s units",
  "lower. At the extremes, %s taxes at %s units on an index of %s",
  "while %s charges %s on %s. The direction of causation runs both",
  "ways — weak bases force high rates, and high rates repel the mobile",
  "tax base — which is precisely why the equalisation of the previous",
  "charts exists. The map, the Gini, and this line are one phenomenon",
  "seen three ways."),
  fmt_2(tax_cor), fmt_2(tax_r2), fmt_2(tax_slope100),
  tax_low$gname, fmt_2(tax_low$mStf), fmt_1(tax_low$ri),
  tax_high$gname, fmt_2(tax_high$mStf), fmt_1(tax_high$ri))

# ============================================================================
# Chapter 4 - Familien von Gemeinden: a cluster analysis
# ============================================================================

# Ten standardised features per municipality. Sources: the scenario file
# (size, projected growth, age, nationality, projected flows), the
# equalisation accounts (fiscal strength, tax multiplier, recent actual
# growth of the population base), and STATPOP (one-person household share).
fa25 <- fa[fa$fa_jahr == 2025, c("gnr", "ri", "mStf")]
fa_a <- fa[fa$fa_jahr == fa_y0, c("gnr", "mwb_bj3_bj5")]
fa_b <- fa[fa$fa_jahr == fa_y1, c("gnr", "mwb_bj3_bj5")]
fa_ab <- merge(fa_a, fa_b, by = "gnr", suffixes = c("_0", "_1"))
fa_ab$past_growth <- 100 *
  ((fa_ab$mwb_bj3_bj5_1 / fa_ab$mwb_bj3_bj5_0)^(1 / (fa_y1 - fa_y0)) - 1)

phh_muni <- phh[grepl("^1[01][0-9][0-9]$", phh$geo_unit_code) &
                  phh$time_period == max(phh$time_period) &
                  !is.na(phh$obs_value), ]
phh_tot <- tapply(phh_muni$obs_value[phh_muni$hh_size_code == "_T"],
                  phh_muni$geo_unit_code[phh_muni$hh_size_code == "_T"], sum)
phh_one <- tapply(phh_muni$obs_value[phh_muni$hh_size_code == "1"],
                  phh_muni$geo_unit_code[phh_muni$hh_size_code == "1"], sum)
one_share <- data.frame(
  gnr = as.integer(names(phh_tot)),
  one_share = 100 * as.numeric(phh_one[names(phh_tot)]) /
    as.numeric(phh_tot))

base25 <- ref[ref$jahr == yr_first, ]
f_share65 <- 100 * tapply(base25$swb[base25$alter >= 65],
                          base25$gnr[base25$alter >= 65], sum) /
  tapply(base25$swb, base25$gnr, sum)
f_foreign <- 100 * tapply(base25$swb[base25$nation == "A"],
                          base25$gnr[base25$nation == "A"], sum) /
  tapply(base25$swb, base25$gnr, sum)

feats <- data.frame(
  gnr = as.integer(names(muni_now)),
  municipality = unname(muni_names[names(muni_now)]),
  pop = as.numeric(muni_now),
  growth = 100 * (as.numeric(muni_end[names(muni_now)]) /
                    as.numeric(muni_now) - 1),
  share65 = as.numeric(f_share65[names(muni_now)]),
  foreign = as.numeric(f_foreign[names(muni_now)]))
feats <- merge(feats, cum_muni[, c("gnr", "natural_rate", "mig_rate")],
               by = "gnr")
feats <- merge(feats, fa25, by = "gnr")
feats <- merge(feats, fa_ab[, c("gnr", "past_growth")], by = "gnr")
feats <- merge(feats, one_share, by = "gnr")
n_cluster <- nrow(feats)
n_cluster_dropped <- n_muni - n_cluster

feature_cols <- c("log residents 2025" = "log_pop",
                  "projected growth 2025–55 (%)" = "growth",
                  "recent growth ~2016–23 (%/yr)" = "past_growth",
                  "residents 65+ (%)" = "share65",
                  "foreign nationals (%)" = "foreign",
                  "net migration 2025–55 (% of base)" = "mig_rate",
                  "natural change 2025–55 (% of base)" = "natural_rate",
                  "resource index" = "ri",
                  "tax multiplier" = "mStf",
                  "one-person households (%)" = "one_share")
feats$log_pop <- log10(feats$pop)
Z <- scale(as.matrix(feats[, unname(feature_cols)]))
rownames(Z) <- feats$municipality

hc <- stats::hclust(stats::dist(Z), method = "ward.D2")

# Mean silhouette for each candidate cut, computed by hand on the same
# distances, so the appendix can report how the chosen k scores.
D <- as.matrix(stats::dist(Z))
sil_mean <- vapply(2:6, function(k) {
  cl <- stats::cutree(hc, k)
  mean(vapply(seq_len(nrow(Z)), function(i) {
    a <- mean(D[i, cl == cl[i] & seq_len(nrow(Z)) != i])
    b <- min(vapply(setdiff(unique(cl), cl[i]),
                    function(g) mean(D[i, cl == g]), numeric(1)))
    (b - a) / max(a, b)
  }, numeric(1)))
}, numeric(1))
names(sil_mean) <- 2:6

k_chosen <- 4
cl_raw <- stats::cutree(hc, k_chosen)
# Number the families along the fiscal gradient (ascending mean resource
# index), which happens to be the urban-rural story order too.
ri_by_cl <- tapply(feats$ri, cl_raw, mean)
family_of <- match(cl_raw, as.integer(names(sort(ri_by_cl))))
feats$family <- family_of
fam_sizes <- as.integer(table(feats$family))

family_names <- c("1 · Emptying hinterland", "2 · Quiet middle",
                  "3 · Growth corridor", "4 · Lakeshore wealth")
feats$family_name <- family_names[feats$family]

w_dendro <- sized(pv_dendrogram(
  hc, k = k_chosen,
  title = sprintf("%d municipalities, cut into %d families", n_cluster,
                  k_chosen),
  subtitle = "Ward clustering on ten standardised features — hover a junction to see its subtree",
  source = quelle_mixed), 980)

analysis_dendro <- sprintf(paste(
  "Rather than assert that the canton has 'types' of municipality, let",
  "the data propose them. Each of the %d municipalities becomes a point",
  "in ten dimensions — size, projected and recent growth, age, foreign",
  "share, both projected flows, fiscal strength, tax multiplier, and",
  "one-person-household share, each standardised — and Ward's method",
  "merges the closest pairs upward into this tree. Cutting it at four",
  "families of %d, %d, %d, and %d municipalities is a judgement call",
  "the appendix defends honestly: a plain silhouette score prefers the",
  "coarser two-way cut (%s vs %s at k = 4), but the two extra families",
  "the k = 4 cut separates — the emptying hinterland and the lakeshore",
  "enclave — are exactly the structures the earlier chapters kept",
  "meeting. The tree shows both cuts; the colours mark the one we",
  "read."),
  n_cluster, fam_sizes[1], fam_sizes[2], fam_sizes[3], fam_sizes[4],
  fmt_2(sil_mean[["2"]]), fmt_2(sil_mean[["4"]]))

# ---- chart 16: the families on the map -------------------------------------

w_family_map <- sized(pv_choropleth(
  feats, id = "gnr", value = "family", palette = "sequential",
  title = "The families assemble into regions",
  subtitle = paste("Cluster families 1 (emptying hinterland) to 4",
                   "(lakeshore wealth) — the algorithm never saw the map"),
  source = quelle_lustat_map), 530)

analysis_family_map <- sprintf(paste(
  "The clustering used no geography — no coordinates, no distances, no",
  "neighbourhoods — yet painted on the map, the families assemble into",
  "almost contiguous regions: the %d hinterland municipalities fill the",
  "rural south-west around the Entlebuch, the %d-strong middle belt",
  "wraps around them, the %d corridor municipalities trace the",
  "agglomeration around the capital and the axis out toward Sursee,",
  "and the %d lakeshore communities sit together on the lake's",
  "northern shore. That geography",
  "emerges from demography and money alone is the point: place in this",
  "canton is destiny enough that ten statistical features recover the",
  "map. (%d of %d municipalities carry all ten features; any without",
  "a complete record stays grey.)"),
  fam_sizes[1], fam_sizes[2], fam_sizes[3], fam_sizes[4],
  n_cluster, n_muni)

# ---- chart 17: what makes each family a family -----------------------------

prof_rows <- do.call(rbind, lapply(seq_along(feature_cols), function(j) {
  z_by_fam <- tapply(Z[, j], feats$family, mean)
  data.frame(feature = names(feature_cols)[j],
             family = family_names[as.integer(names(z_by_fam))],
             z = round(as.numeric(z_by_fam), 2))
}))
prof_rows <- prof_rows[order(match(prof_rows$feature, names(feature_cols)),
                             prof_rows$family), ]

fam_means <- aggregate(
  cbind(pop, growth, share65, foreign, ri, mStf, one_share) ~ family,
  feats, mean)

w_profiles <- sized(pv_heatmap(
  prof_rows, x = "family", y = "feature", value = "z",
  palette = "diverging", cell_values = TRUE, truncate_labels = 34,
  title = "What makes each family a family",
  subtitle = "Mean of each standardised feature by family — 0 is the cantonal average, ±1 is one s.d.",
  source = quelle_mixed), 470)

analysis_profiles <- sprintf(paste(
  "Reading the columns names the families. Family 1 (%d municipalities,",
  "mean %s residents) is the emptying hinterland: resource index %s,",
  "the canton's highest tax multipliers, projected change %s. Family 2",
  "(%d municipalities) is the quiet middle — near the cantonal average",
  "on almost every row, which is what makes it a family. Family 3 (%d",
  "municipalities, mean %s residents) is the growth corridor: the",
  "urban belt, %s projected growth, a foreign share of %s against the",
  "hinterland's %s. And",
  "family 4 (%d municipalities — %s) is lakeshore wealth: resource",
  "index %s, multiplier %s, the oldest population and the most",
  "one-person households — rich, grey, and slow-growing by design."),
  fam_sizes[1], fmt_n(fam_means$pop[1]), fmt_1(fam_means$ri[1]),
  fmt_pct(fam_means$growth[1], 1),
  fam_sizes[2], fam_sizes[3], fmt_n(fam_means$pop[3]),
  fmt_pct(fam_means$growth[3], 1),
  fmt_pct(fam_means$foreign[3], 1), fmt_pct(fam_means$foreign[1], 1),
  fam_sizes[4],
  paste(sort(feats$municipality[feats$family == 4]), collapse = ", "),
  fmt_1(fam_means$ri[4]), fmt_2(fam_means$mStf[4]))

# ============================================================================
# Chapter 5 - Was sich wissen laesst: three predictions, three answers
# ============================================================================

# ---- chart 18: is the projection just momentum? ----------------------------

feats$growth_yr <- 100 * ((1 + feats$growth / 100)^(1 / (yr_last - yr_first)) - 1)
mom_cor <- stats::cor(feats$past_growth, feats$growth_yr)

w_momentum <- sized(pv_trend(pv_scatter(
  feats, x = "past_growth", y = "growth_yr", label = "municipality",
  xlab = "recent actual growth, ≈2016–23 (%/yr)",
  ylab = sprintf("projected growth %d–%d (%%/yr)", yr_first, yr_last),
  title = "The projection is not just momentum",
  subtitle = sprintf(
    "Recent growth of the population base vs projected growth, %d municipalities",
    n_cluster),
  source = quelle_lustat), method = "lm"), 440)

analysis_momentum <- sprintf(paste(
  "A cheap theory of any projection is that it extrapolates the recent",
  "past. Test it: the equalisation accounts carry each municipality's",
  "actual population base (a three-year average that trails the",
  "calendar by three to five years), so its recent growth — roughly",
  "%d to %d — can be set against the projected rate. The correlation",
  "is r = %s: recent momentum explains %s of the variation in the",
  "projected map (R² = %s). Momentum matters, but LUSTAT's model is",
  "clearly drawing on more than yesterday's trend — zoning capacity,",
  "age structure, planned development. What that 'more' consists of,",
  "the next chart reverse-engineers."),
  fa_y0 - 4, fa_y1 - 4, fmt_2(mom_cor),
  fmt_pct(100 * mom_cor^2, 0),
  formatC(mom_cor^2, format = "f", digits = 2))

# ---- chart 19: reverse-engineering the projection --------------------------

loo_fit <- function(data, formula) {
  vapply(seq_len(nrow(data)), function(i) {
    stats::predict(stats::lm(formula, data = data[-i, ]),
                   newdata = data[i, ])
  }, numeric(1))
}

form_proj <- growth ~ log_pop + share65 + foreign + ri + mStf +
  past_growth + one_share
feats$pred_growth <- loo_fit(feats, form_proj)
proj_rmse <- rmse(feats$growth, feats$pred_growth)
proj_base <- rmse(feats$growth,
                  vapply(seq_len(nrow(feats)),
                         function(i) mean(feats$growth[-i]), numeric(1)))
proj_gain <- 100 * (1 - proj_rmse / proj_base)
proj_r2 <- summary(stats::lm(form_proj, data = feats))$r.squared

w_predict_proj <- sized(pv_trend(pv_scatter(
  feats, x = "pred_growth", y = "growth", label = "municipality",
  xlab = "growth predicted from seven present-day features (%)",
  ylab = "projected growth in the scenario (%)",
  title = "Most of the projected map is already visible today",
  subtitle = sprintf(
    "Leave-one-out predictions vs the scenario, %d municipalities",
    n_cluster),
  source = quelle_mixed), method = "lm"), 440)

analysis_predict_proj <- sprintf(paste(
  "How much of the scenario's municipal geography can be recovered from",
  "facts observable today? Feed seven present-day features — size, age,",
  "foreign share, fiscal strength, tax multiplier, recent growth,",
  "household structure — into a linear model, and score it honestly:",
  "each municipality is predicted by a model fitted to the other %d",
  "(leave-one-out cross-validation). The cross-validated error is %s",
  "growth points, against %s for simply guessing the cantonal mean —",
  "%s smaller. In-sample the model explains %s of the variance. The",
  "scenario's geography, in other words, is largely today's structure",
  "projected forward, which is reassuring about the model and sobering",
  "about the hinterland: nothing in today's observables points its",
  "way up."),
  n_cluster - 1, fmt_1(proj_rmse), fmt_1(proj_base),
  fmt_pct(proj_gain, 0), fmt_pct(100 * proj_r2, 0))

# ---- chart 20: and the one thing nothing predicts --------------------------

fisc <- merge(
  merge(beta[, c("gnr", "gname", "rp_pEinw0", "g")],
        fa[fa$fa_jahr == fa_y0, c("gnr", "mStf")], by = "gnr"),
  feats[, c("gnr", "log_pop", "share65", "foreign", "past_growth")],
  by = "gnr")
form_fisc <- g ~ log(rp_pEinw0) + mStf + log_pop + share65 + foreign +
  past_growth
fisc$pred <- loo_fit(fisc, form_fisc)
fisc_rmse <- rmse(fisc$g, fisc$pred)
fisc_base <- rmse(fisc$g,
                  vapply(seq_len(nrow(fisc)),
                         function(i) mean(fisc$g[-i]), numeric(1)))
fisc_r2 <- summary(stats::lm(form_fisc, data = fisc))$r.squared
fisc_mean_g <- mean(fisc$g)

w_predict_fisc <- sized(pv_annotate(
  pv_scatter(
    fisc, x = "pred", y = "g", label = "gname",
    xlab = "fiscal growth predicted from six features (%/yr)",
    ylab = sprintf("actual growth of per-capita resources %d–%d (%%/yr)",
                   fa_y0, fa_y1),
    title = "The one thing nothing here predicts: fiscal futures",
    subtitle = sprintf(
      "Leave-one-out predictions vs actual growth, %d municipalities — the cloud is the finding",
      nrow(fisc)),
    source = quelle_lustat),
  pv_hline(fisc_mean_g, label = "the cantonal mean — the better forecast")),
  440)

analysis_predict_fisc <- sprintf(paste(
  "Run the same honest machinery on a real future — which tax bases",
  "grew fastest over %d–%d — and it collapses. The six features manage",
  "a leave-one-out error of %s points of annual growth; guessing the",
  "cantonal mean for every municipality scores %s. The model is worse",
  "than knowing nothing — the signature of a regression fitting noise",
  "(even in-sample, R² reaches only %s). This is the finding, not a",
  "failure: municipal fiscal fortunes over a few years ride on firm",
  "relocations, property deals, and a handful of big taxpayers —",
  "none of it visible in structural statistics. Where chapter three",
  "found no convergence, this chart explains why no simple story",
  "fits: year-to-year fiscal change is, to these features, noise."),
  fa_y0, fa_y1, fmt_2(fisc_rmse), fmt_2(fisc_base),
  fmt_2(fisc_r2))

# ============================================================================
# Chapter 6 - Der Kanton im Land: the national frame
# ============================================================================

# ---- chart 21: the capital on the national ladder --------------------------

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

# How common Luzern's arc is among the chart's eight cities (the bump
# chart follows the top 8 by the latest year's residents). A city
# "hollowed out" if it later dropped more than two percent below its
# earlier peak, and "climbed back" if its latest count passes that peak.
ladder_latest <- pv_city_population[pv_city_population$year == luz_latest_yr, ]
ladder_cities <- ladder_latest$city[order(-ladder_latest$population)][1:8]
arc <- vapply(ladder_cities, function(ci) {
  d <- pv_city_population[pv_city_population$city == ci, ]
  d <- d[order(d$year), ]
  pk_i <- which.max(d$population[d$year < luz_latest_yr])
  pk <- d$population[pk_i]
  ap <- d$population[d$year > d$year[pk_i]]
  dipped <- min(ap) < 0.98 * pk
  c(dipped, dipped && d$population[nrow(d)] >= pk)
}, logical(2))
n_dipped <- sum(arc[1, ])
n_recovered <- sum(arc[2, ])

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
  "residents. The arc is the national rule, not an exception: %d of",
  "the eight cities on this ladder trace the same hollowing-out, and",
  "only %d have yet",
  "climbed back past their old peak. It is the growth corridor of",
  "chapter four in national miniature: the region grows while the",
  "core city merely persists."),
  rank_first, yr_city_first, rank_latest, luz_latest_yr,
  fmt_n(luz_peak), luz_peak_yr, fmt_n(luz_peak - luz_trough),
  luz_trough_yr, luz_peak_yr, luz_recover_yr, fmt_n(luz_latest),
  n_dipped, n_recovered)

# ---- chart 22: every Swiss city's growth engine ----------------------------

bil_ok <- bil[!is.na(bil$obs_value) & bil$ssv_swiss_city_code != "_ST", ]
bil_year <- unique(bil_ok$period)[1]
nat_1000 <- bil_ok[bil_ok$ssv_pop_bil_code == "acc_nat_1000",
                   c("ssv_swiss_city", "obs_value")]
mig_1000 <- bil_ok[bil_ok$ssv_pop_bil_code == "mig_1000",
                   c("ssv_swiss_city", "obs_value")]
engines <- merge(nat_1000, mig_1000, by = "ssv_swiss_city",
                 suffixes = c("_nat", "_mig"))
names(engines) <- c("city", "natural", "migration")
n_cities_bil <- nrow(engines)
n_nat_deficit <- sum(engines$natural < 0)
luz_engine <- engines[engines$city == "Luzern", ]

w_engines <- sized(pv_annotate(
  pv_scatter(
    engines, x = "natural", y = "migration", label = "city",
    xlab = "natural change per 1,000 residents",
    ylab = "net migration per 1,000 residents",
    title = "Every Swiss city's growth engine, one dot each",
    subtitle = sprintf(
      "Births minus deaths vs net migration, %d statistical cities, %s",
      n_cities_bil, bil_year),
    source = quelle_bfs),
  pv_hline(0), pv_vline(0),
  pv_note(luz_engine$natural, luz_engine$migration, "Luzern",
          dx = 18, dy = -10)), 460)

analysis_engines <- sprintf(paste(
  "Lucerne's migration-driven growth is not a local quirk. Across the",
  "%d Swiss statistical cities in %s, %d — %s — already record more",
  "deaths than births; whatever growth they still manage arrives by",
  "van. The capital sits",
  "at %s per 1,000 on the natural axis and %s per 1,000 on migration:",
  "%s. The pattern chapter one projected for the canton's future is",
  "the present tense of urban Switzerland — the scenario models are",
  "not forecasting something exotic, they are spreading something",
  "already here."),
  n_cities_bil, bil_year, n_nat_deficit,
  fmt_pct(100 * n_nat_deficit / n_cities_bil, 0),
  fmt_1(luz_engine$natural), fmt_1(luz_engine$migration),
  if (luz_engine$natural < 0) {
    "every resident it gains, and some it merely replaces, arrives by van"
  } else {
    "a modest natural surplus topped up several times over by arrivals"
  })

# ---- chart 23: what the capital does for a living --------------------------

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
# One decimal at the lollipop heads - the raw shares carry float noise.
top_sec$share <- round(top_sec$share, 1)

health_share <- luz_sec$share[luz_sec$sector_code == "Q"]
second_share <- luz_sec$share[2]
second_name <- sector_short[luz_sec$sector[2]]
if (is.na(second_name)) second_name <- luz_sec$sector[2]
health_all <- pv_city_sectors[pv_city_sectors$sector_code == "Q", ]
n_cities_sec <- length(unique(pv_city_sectors$city))
n_health_higher <- sum(health_all$share > health_share)

w_sectors <- sized(pv_lollipop(
  top_sec, x = "sector_label", y = "share",
  title = "A city that cares for a living",
  subtitle = "Ten largest sectors of the city of Luzern by share of employees",
  source = quelle_bfs), 430)

analysis_sectors <- sprintf(paste(
  "One more national comparison rounds out the ageing story. %s of the",
  "city of Luzern's employees work in health and social work, %.1f",
  "times the share of the next sector, %s (%s); only %d of the %d",
  "Swiss statistical cities lean harder on health. An economy already",
  "organised around care meets a canton whose 80-plus population, the",
  "first chapter computed, will multiply by %.1f — the capital's",
  "biggest sector is the one demographic arithmetic most guarantees."),
  fmt_pct(health_share, 1), health_share / second_share,
  tolower(second_name), fmt_pct(second_share, 1),
  n_health_higher, n_cities_sec, n80_end / n80_now)

# ============================================================================
# Appendix - Methoden und Daten
# ============================================================================

datasets_tbl <- data.frame(
  Dataset = c("szbv-lu-2025-2055 (referenz / hoch / tief)",
              "fa-lu-ra",
              "DF_STATPOP_PHH",
              "DF_SSV_POP_BIL",
              "pv_city_population (bundled)",
              "pv_city_sectors (bundled)",
              "pv_lucerne_map (bundled)"),
  Publisher = c("LUSTAT Statistik Luzern",
                "LUSTAT Statistik Luzern",
                "Bundesamt für Statistik",
                "Bundesamt für Statistik",
                "Bundesamt für Statistik",
                "Bundesamt für Statistik",
                "BFS, ThemaKart"),
  Terms = c("OPEN BY ASK", "OPEN BY ASK", "OPEN BY", "OPEN BY",
            "OPEN BY", "OPEN BY", "© BFS"),
  Rows = c(sum(vapply(szbv, nrow, integer(1))), nrow(fa), nrow(phh),
           nrow(bil), nrow(pv_city_population), nrow(pv_city_sectors),
           length(pv_lucerne_map$features)),
  Span = c(sprintf("%d–%d", yr_first, yr_last),
           sprintf("%d–%d", fa_y0, fa_y1),
           sprintf("%d–%d", hh_y0, hh_y1),
           as.character(bil_year),
           sprintf("%d–%d", yr_city_first, luz_latest_yr),
           "2022", "—"),
  Feeds = c("Chapters 1, 2, 4, 5", "Chapters 3, 4, 5", "Chapters 2, 4",
            "Chapter 6", "Chapter 6", "Chapter 6", "All maps"))

w_datasets <- sized(pv_table(
  datasets_tbl, digits = c(Rows = 0), sortable = FALSE,
  title = "Every dataset on this page",
  subtitle = "Fetched live through polyviz's open-data fetchers and cached locally; licences print on every fetch",
  source = "Quelle: LUSTAT Statistik Luzern; Bundesamt für Statistik"), 430)

methods_html <- sprintf(paste0(
  "<h3 id='methods-data'>Data</h3>",
  "<p>Everything on this page comes from openly licensed Swiss",
  " statistics, fetched at build time through the package's own",
  " fetchers (<code>pv_fetch_lustat()</code>, <code>pv_fetch_bfs()</code>)",
  " and cached locally, so the build re-runs offline. LUSTAT files carry",
  " the opendata.swiss <em>OPEN BY ASK</em> terms, stats.swiss files",
  " <em>OPEN BY</em>; the exact terms print on every fetch. Every number",
  " in the prose is computed in <code>data-raw/build-story.R</code> from",
  " these files at build time — none is typed in from knowledge.</p>",

  "<h3>Convergence (chapter 3)</h3>",
  "<p><strong>Beta:</strong> annualised growth of per-capita resource",
  " potential %d–%d regressed on the initial resource index, for the",
  " %d municipalities present in both years (%d dropped to mergers).",
  " Slope %s points of annual growth per 100 index points, 95%% CI",
  " [%s, %s], R² = %s. <strong>Sigma:</strong> the coefficient of",
  " variation and the s.d. of log per-capita resources, per year,",
  " unweighted across municipalities. Both are association tests on a",
  " short window with n ≈ %d; they rule out visible convergence in",
  " this period, nothing stronger.</p>",

  "<h3>Inequality (chapter 3)</h3>",
  "<p>Gini coefficients are population-weighted: municipalities are",
  " sorted by per-capita resource potential, residents accumulate along",
  " the x-axis using the equalisation file's own population base",
  " (<code>mwb_bj3_bj5</code>), and the Gini is one minus twice the",
  " area under the Lorenz curve. 'After equalisation' adds each",
  " municipality's resource-equalisation transfer, divided by the same",
  " population, to its per-capita resources — an approximation that",
  " ignores the burden-sharing instruments, so it understates the full",
  " system's compression. %d values: before %s, after %s.</p>",

  "<h3>Families (chapter 4)</h3>",
  "<p>Ward clustering (<code>hclust</code>, ward.D2, Euclidean) on ten",
  " standardised features, for the %d of the canton's %d municipalities",
  " with a complete fiscal history and household record. Mean",
  " silhouette: %s (k = 2), %s (k = 3), %s (k = 4), %s (k = 5). The",
  " silhouette prefers k = 2; we cut at k = 4 because the two",
  " additional families are interpretable, geographically coherent,",
  " and match structures found independently in chapters 2 and 3 —",
  " a documented judgement, not a statistic.</p>",

  "<h3>Prediction (chapter 5)</h3>",
  "<p>Linear models scored by leave-one-out cross-validation: each",
  " municipality is predicted by a model fitted to all others, and the",
  " RMSE of those held-out predictions is compared with the RMSE of",
  " guessing the (held-out) mean. Projected growth: RMSE %s vs %s",
  " (%s better). Actual fiscal growth: RMSE %s vs %s — the model",
  " loses, which is reported as the finding, not smoothed over; even",
  " before cross-validation its in-sample R² reaches only %s.</p>",

  "<h3>Limits</h3>",
  "<p>The population scenarios are LUSTAT's model output, not",
  " observations — chapters 1, 2, and 5 analyse what the model",
  " asserts, and say so. The equalisation file's population base is a",
  " lagged three-year average, so 'recent growth' spans roughly",
  " %d–%d. With ~%d municipalities every regression is small-sample;",
  " confidence intervals are reported where a claim depends on them.",
  " Associations are never read as causes. And one dataset hunt",
  " failed honestly: municipal-level historical population, dwellings,",
  " and vacancy series exist on stats.swiss but only as full-table",
  " downloads too large for this build, so the historical record here",
  " rests on the equalisation base and the household statistics.</p>"),
  # convergence
  fa_y0, fa_y1, n_beta, n_dropped_beta, fmt_2(beta_slope100),
  fmt_2(beta_ci100[[1]]), fmt_2(beta_ci100[[2]]),
  formatC(beta_r2, format = "f", digits = 3), n_beta,
  # inequality
  fa_y1, formatC(gini_pre_last, format = "f", digits = 3),
  formatC(gini_post_last, format = "f", digits = 3),
  # families
  n_cluster, n_muni,
  fmt_2(sil_mean[["2"]]), fmt_2(sil_mean[["3"]]), fmt_2(sil_mean[["4"]]),
  fmt_2(sil_mean[["5"]]),
  # prediction
  fmt_1(proj_rmse), fmt_1(proj_base), fmt_pct(proj_gain, 0),
  fmt_2(fisc_rmse), fmt_2(fisc_base), fmt_2(fisc_r2),
  # limits
  fa_y0 - 4, fa_y1 - 4, n_muni)

# ---- assemble the page -----------------------------------------------------

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
    ".methods { color: var(--secondary); line-height: 1.65;\n",
    "           max-width: 720px; }\n",
    ".methods h3 { color: var(--primary); font-size: 16px;\n",
    "              letter-spacing: -0.01em; margin: 26px 0 6px; }\n",
    ".methods p { margin: 0 0 10px; }\n",
    ".methods code { font-size: 0.92em; }\n",
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
  tags$title("Anatomy of a Swiss canton"),
  tags$style(HTML(story_css))
), tags$body(tags$main(
  tags$header(
    tags$p(class = "eyebrow", "A polyviz data investigation"),
    tags$h1("Anatomy of a Swiss canton"),
    tags$p(class = "lede", sprintf(paste(
      "Canton Lucerne counted %s people at the end of %d, and its",
      "statistical office opens enough data to take the place apart",
      "properly. This page asks four questions of it. How many people",
      "will the canton hold, and how old will they be? Where does the",
      "growth land, and what arrives with it? Do poor municipalities",
      "catch up with rich ones — the oldest question in regional",
      "economics — or drift further behind? And how much of any of",
      "this could you have predicted from what is visible today? The",
      "answers come from LUSTAT Statistik Luzern's open municipal data",
      "and the Bundesamt für Statistik's federal series, fetched live",
      "when this page is built. Every chart is interactive — hover,",
      "and take any of them along via the download button in its",
      "top-right corner — and every number in the text is computed",
      "from the data at build time, none typed in from memory. The",
      "methods, and their limits, are laid out in the appendix."),
      fmt_n(pop_now), yr_first))
  ),
  tags$nav(
    tags$a(href = "#zukuenfte", "1 · Drei Zukünfte"),
    tags$a(href = "#wachstum", "2 · Wo das Wachstum landet"),
    tags$a(href = "#kassen", "3 · Ungleiche Kassen"),
    tags$a(href = "#familien", "4 · Familien von Gemeinden"),
    tags$a(href = "#wissen", "5 · Was sich wissen lässt"),
    tags$a(href = "#kontext", "6 · Der Kanton im Land"),
    tags$a(href = "#methoden", "Appendix"),
    tags$a(href = "index.html", "← polyviz gallery")
  ),
  theme_section(
    "zukuenfte", "Drei Zukünfte", "three futures",
    paste("LUSTAT projects every municipality's population to 2055,",
          "three ways, one year and one year of age at a time. The",
          "question this chapter asks of the scenarios is not how big",
          "the canton gets — all three agree it grows — but what the",
          "growth is made of, and which parts of the future no",
          "scenario can bend."),
    chart_block("scenarios", "Three roads out of the present",
                analysis_scenarios, w_scenarios),
    chart_block("components", "What the growth is made of",
                analysis_components, w_components),
    chart_block("dependency", "The ratio no scenario can bend",
                analysis_dependency, w_dependency),
    chart_block("ageing", "The ageing wave", analysis_ageing, w_ageing)
  ),
  theme_section(
    "wachstum", "Wo das Wachstum landet", "where the growth lands",
    paste("A canton does not grow in the aggregate; particular houses",
          "get built in particular villages. This chapter drops the",
          "projected growth onto the map and asks which municipalities",
          "carry it, whether it arrives in cradles or moving vans, and",
          "why the demand for homes grows faster than the count of",
          "people."),
    chart_block("growth-map", "Where the growth lands",
                analysis_growth, w_growth),
    chart_block("growth-size", "Who carries the growth",
                analysis_swarm, w_swarm),
    chart_block("cradles", "Cradles or moving vans",
                analysis_cradles, w_cradles),
    chart_block("households", "The households it arrives in",
                analysis_households, w_households)
  ),
  theme_section(
    "kassen", "Ungleiche Kassen", "unequal coffers",
    paste("Do poor municipalities catch up with rich ones? Growth",
          "theory says they should; this chapter tests it on eight",
          "years of Lucerne's equalisation accounts — the classic",
          "convergence regressions first, then the concentration of",
          "the tax base, what the equalisation transfers actually",
          "achieve against it, and what fiscal weakness costs the",
          "people who live in it."),
    chart_block("fiscal", "The fiscal map", analysis_fiscal, w_fiscal),
    chart_block("beta", "The catch-up that isn't happening",
                analysis_beta, w_beta),
    chart_block("sigma", "The spread, measured twice",
                analysis_sigma, w_sigma),
    chart_block("lorenz", "How far the curve bends",
                analysis_lorenz, w_lorenz),
    chart_block("gini", "Running to stand still",
                analysis_gini, w_gini),
    chart_block("tax", "The price of a weak tax base",
                analysis_tax, w_tax)
  ),
  theme_section(
    "familien", "Familien von Gemeinden", "families of municipalities",
    paste("Eighty municipal stories are too many to hold in the head,",
          "so this chapter lets the data sort them: every municipality",
          "becomes ten standardised numbers, Ward's method builds the",
          "family tree, and the map and the profiles say what makes",
          "each family a family — and whether the families the",
          "algorithm finds are the regions the earlier chapters kept",
          "meeting."),
    chart_block("dendrogram", "The family tree",
                analysis_dendro, w_dendro),
    chart_block("family-map", "The families on the map",
                analysis_family_map, w_family_map),
    chart_block("profiles", "What makes each family a family",
                analysis_profiles, w_profiles)
  ),
  theme_section(
    "wissen", "Was sich wissen lässt", "what can be known",
    paste("A projection is a claim about the future, and claims can be",
          "interrogated: what is this one made of? This chapter runs",
          "three honest prediction exercises — each scored out of",
          "sample, each reported with the boring baseline it must",
          "beat — and finds that the projected map is mostly today's",
          "structure, that momentum alone is a poor guide, and that",
          "one future in this story is genuinely unpredictable."),
    chart_block("momentum", "Momentum is not the model",
                analysis_momentum, w_momentum),
    chart_block("predict-proj", "Reverse-engineering the projection",
                analysis_predict_proj, w_predict_proj),
    chart_block("predict-fisc", "The unpredictable part",
                analysis_predict_fisc, w_predict_fisc)
  ),
  theme_section(
    "kontext", "Der Kanton im Land", "the canton in the country",
    paste("None of this happens on an island. Three federal series",
          "place Lucerne in the Swiss frame: the capital's rank among",
          "the cities, the growth engine every city runs on, and the",
          "sector that already dominates the capital's economy just as",
          "the ageing wave heads its way."),
    chart_block("cities", "The capital on the national ladder",
                analysis_cities, w_cities),
    chart_block("engines", "Every city's growth engine",
                analysis_engines, w_engines),
    chart_block("sectors", "What the capital does for a living",
                analysis_sectors, w_sectors)
  ),
  tags$section(
    id = "methoden", class = "theme",
    tags$h2("Methoden und Daten", tags$span(" — methods and data")),
    tags$p(class = "theme-intro", paste(
      "What was computed, how, and what it cannot show. Shorter than a",
      "journal's methods section, longer than a caption — enough to",
      "re-run every number on this page.")),
    tags$div(class = "methods", HTML(methods_html)),
    tags$div(class = "chart-block", w_datasets)
  ),
  tags$footer(HTML(paste0(
    "Data: LUSTAT Statistik Luzern via data.lustat.ch, published under ",
    "the opendata.swiss “OPEN BY ASK” terms — free use ",
    "with source citation (“Quelle: LUSTAT Statistik Luzern”); ",
    "commercial use requires the data owner's permission. Federal ",
    "series: Bundesamt für Statistik via stats.swiss (“OPEN ",
    "BY” — free use, source citation required). Boundaries ",
    "© BFS, ThemaKart. Charts rendered by ",
    "<a href='https://d3js.org'>d3.js</a> v7, type set in ",
    "<a href='https://rsms.me/inter/'>Inter</a>. Story built with the ",
    "MIT-licensed R package ",
    "<a href='https://github.com/jastephan63/polyviz'>jastephan63/",
    "polyviz</a> on ", format(Sys.Date(), "%Y-%m-%d"),
    "; every figure and every number in the prose is recomputed from ",
    "the source data on each build. ",
    "<a href='index.html'>Back to the gallery</a>.")))
)))

dir.create("docs", showWarnings = FALSE)
save_html(page, "docs/story.html", libdir = "lib")
cat("story written to docs/story.html\n")
