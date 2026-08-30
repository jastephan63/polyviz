# Turns the raw open-data downloads (data-raw/raw/, fetched from official
# Swiss portals - see data-raw/README for URLs) into the tidy data frames the
# package ships. Run from the package root after data-raw/fetch (or re-run
# the curl commands in git history).
#
# Sources and licenses:
#   * stats.swiss files (pop1930, sectors, landuse): Bundesamt fuer Statistik,
#     opendata.swiss "OPEN BY" terms - free use, must cite the source.
#   * pendler.csv: Kanton Zug, Fachstelle Statistik, "OPEN BY".
#   * fa-lu-ra.csv, grwahlen-lu.csv: LUSTAT Statistik Luzern, opendata.swiss
#     "OPEN BY ASK" terms - free use with source citation; commercial use
#     requires the data owner's permission.

raw <- function(f) file.path("data-raw/raw", f)

# The stats.swiss files are SDMX "csv with labels": every dimension comes as
# a code column immediately followed by its human-readable label column, so
# we address columns by position.
read_sdmx <- function(f) {
  utils::read.csv(raw(f), check.names = FALSE, header = TRUE)
}

# ---- Swiss city population since 1930 (BFS) -------------------------------
# The population indicator comes in several flavours: explicit census years
# (pop_ref_period_1930 ... _2000), the current and ten-years-ago reference
# periods (whose label column carries the year, e.g. "2024"), plus density
# and percent-change variants we don't want. Keeping only rows whose label
# is a bare four-digit year selects exactly the population counts. The
# pseudo-city "_ST" is the all-cities total and is dropped throughout.
x <- read_sdmx("pop1930.csv")
keep <- startsWith(x[[9]], "pop_ref_period") &
  grepl("^\\d{4}$", x[[10]]) & x[[5]] != "_ST"
pv_city_population <- data.frame(
  city = x[[6]][keep],
  size_class = x[[8]][keep],
  year = as.integer(x[[10]][keep]),
  population = as.numeric(x[[11]][keep])
)
pv_city_population <- pv_city_population[
  !is.na(pv_city_population$population), ]
pv_city_population <- pv_city_population[
  order(pv_city_population$city, pv_city_population$year), ]
rownames(pv_city_population) <- NULL

# ---- Employment shares by economic sector (BFS / STATENT) -----------------
x <- read_sdmx("sectors.csv")
keep <- x[[9]] %in% LETTERS & !is.na(as.numeric(x[[11]])) & x[[5]] != "_ST"
title_case <- function(s) {
  s <- tolower(s)
  gsub("(^|[ -])([a-z])", "\\1\\U\\2", s, perl = TRUE)
}
pv_city_sectors <- data.frame(
  city = x[[6]][keep],
  sector_code = x[[9]][keep],
  sector = title_case(x[[10]][keep]),
  share = as.numeric(x[[11]][keep])
)
pv_city_sectors <- pv_city_sectors[
  order(pv_city_sectors$city, pv_city_sectors$sector_code), ]
rownames(pv_city_sectors) <- NULL

# ---- Land use by city (BFS Arealstatistik) --------------------------------
x <- read_sdmx("landuse.csv")
landuse_labels <- c(
  sur_bat = "Buildings", sur_ind = "Industrial", sur_tran = "Transport",
  sur_vert = "Urban green", sur_agr = "Agriculture", sur_bois = "Forest",
  sur_eau = "Water", sur_unpr = "Unproductive"
)
landuse_groups <- c(
  sur_bat = "Settlement", sur_ind = "Settlement", sur_tran = "Settlement",
  sur_vert = "Settlement", sur_agr = "Cultivated", sur_bois = "Cultivated",
  sur_eau = "Natural", sur_unpr = "Natural"
)
keep <- x[[9]] %in% names(landuse_labels) & !is.na(as.numeric(x[[11]])) &
  x[[5]] != "_ST"
pv_city_landuse <- data.frame(
  city = x[[6]][keep],
  group = unname(landuse_groups[x[[9]][keep]]),
  category = unname(landuse_labels[x[[9]][keep]]),
  hectares = as.numeric(x[[11]][keep])
)
pv_city_landuse <- pv_city_landuse[
  order(pv_city_landuse$city, pv_city_landuse$group), ]
rownames(pv_city_landuse) <- NULL

# ---- Commuter flows to and from Canton Zug --------------------------------
x <- utils::read.csv(raw("pendler.csv"))
pv_commuters <- data.frame(
  period = x$periode,
  region = x$gebiet,
  direction = ifelse(x$kennzahl == "Pendler nach Zug", "to Zug", "from Zug"),
  commuters = as.integer(x$anzahl)
)

# ---- Lucerne municipal fiscal equalization (LUSTAT) -----------------------
x <- utils::read.csv(raw("fa-lu-ra.csv"), sep = ";")
pv_fiscal <- data.frame(
  year = as.integer(x$fa_jahr),
  municipality_id = as.integer(x$gnr),
  municipality = x$gname,
  resource_per_capita = round(as.numeric(x$rp_pEinw), 2),
  resource_index = round(as.numeric(x$ri), 1),
  equalization_chf = round(as.numeric(x$ra))
)
pv_fiscal <- pv_fiscal[order(pv_fiscal$year, pv_fiscal$municipality), ]
rownames(pv_fiscal) <- NULL

# ---- Lucerne municipal council elections (LUSTAT) -------------------------
x <- utils::read.csv(raw("grwahlen-lu.csv"), sep = ";")
pv_elections <- data.frame(
  year = as.integer(x$jahr),
  municipality_id = as.integer(x$gnr),
  municipality = x$gemeinde,
  party = x$partei_kurz,
  sex = ifelse(x$sex == "m", "male", "female"),
  status = ifelse(x$kand_status == "bisher", "incumbent", "new"),
  elected = x$gewaehlt == "ja"
)

for (nm in c("pv_city_population", "pv_city_sectors", "pv_city_landuse",
             "pv_commuters", "pv_fiscal", "pv_elections")) {
  save(list = nm, file = file.path("data", paste0(nm, ".rda")),
       compress = "bzip2")
  cat(nm, ":", nrow(get(nm)), "rows\n")
}
