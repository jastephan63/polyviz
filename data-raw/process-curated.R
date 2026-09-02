# Builds the two curated bundled datasets (pv_electricity, pv_tourism) so
# examples and tests never have to touch the network. Run from the package
# root with the package loaded (devtools::load_all(".")): the electricity
# balance goes through pv_fetch_opendata() - exercising the fetcher for
# real - and the tourism cube comes from the BFS PXWeb API, which needs a
# POST and therefore can't run through the GET-only fetchers. That raw
# answer is kept under data-raw/raw/ and only re-downloaded when missing.
#
# Sources and licenses:
#   * Schweizerische Elektrizitaetsbilanz - Monatswerte (ogd35): Bundesamt
#     fuer Energie BFE, opendata.swiss "OPEN BY" terms - free use, must
#     cite the source ("Quelle: Bundesamt fuer Energie").
#   * Hotellerie: Ankuenfte und Logiernaechte nach Kanton und Herkunftsland
#     (PX cube px-x-1003020000_102): Bundesamt fuer Statistik, opendata.swiss
#     "OPEN BY" terms - free use, must cite the source
#     ("Quelle: Bundesamt fuer Statistik").

raw <- function(f) file.path("data-raw/raw", f)

# ---- Swiss electricity production by source (BFE) -------------------------
# The monthly national electricity balance, one wide row per month since
# 2000. Wind and solar only got their own columns in 2020 (before that
# they hide inside "andere"), so the bundled slice starts there: six fully
# split sources across the six definitive years 2020-2025. Early rows pad
# the unsplit columns with the string "NULL", which is why they arrive as
# character.
bal <- pv_fetch_opendata(paste0(
  "schweizerische-elektrizitatsstatistik-",
  "schweizerische-elektrizitatsbilanz-monatswerte"))
num <- function(v) as.numeric(replace(v, v == "NULL", NA))

sources <- c(
  Erzeugung_Laufwerk_GWh = "River hydro",
  Erzeugung_Speicherwerk_GWh = "Storage hydro",
  Erzeugung_Kernkraftwerk_GWh = "Nuclear",
  Erzeugung_Thermische_GWh = "Thermal",
  Erzeugung_Windkraft_GWh = "Wind",
  Erzeugung_Photovoltaik_GWh = "Solar"
)
keep <- bal$Jahr %in% 2020:2025 & bal$Definitiv == 1
stopifnot(sum(keep) == 72)
mix <- vapply(names(sources), function(cl) num(bal[[cl]][keep]), numeric(72))
# The six sources must add up to the balance's own national production
# column, or the source mapping is wrong.
stopifnot(all(abs(rowSums(mix) - num(bal$Landeserzeugung_GWh[keep])) <= 1))

pv_electricity <- data.frame(
  date = rep(as.Date(sprintf("%d-%02d-01", bal$Jahr[keep], bal$Monat[keep])),
             times = length(sources)),
  year = rep(as.integer(bal$Jahr[keep]), times = length(sources)),
  month = rep(as.integer(bal$Monat[keep]), times = length(sources)),
  source = rep(unname(sources), each = sum(keep)),
  gwh = as.vector(mix)
)
pv_electricity <- pv_electricity[
  order(pv_electricity$date,
        match(pv_electricity$source, sources)), ]
rownames(pv_electricity) <- NULL

# ---- Hotel nights by canton and guest origin (BFS HESTA) ------------------
# Annual arrivals and overnight stays per canton for the ten biggest guest
# markets, 2005-2025, from the PXWeb cube px-x-1003020000_102. The canton
# codes in the cube are the official BFS canton numbers, so the frame joins
# pv_swiss_cantons as-is. Everything outside the ten named markets is
# folded into "Other countries" (the cube's total minus the named rows),
# so summing over origins recovers the published cantonal totals.
hesta_origins <- c(
  "1" = "Switzerland", "11" = "Germany", "13" = "France", "14" = "Italy",
  "16" = "United Kingdom", "18" = "Netherlands", "41" = "United States",
  "72" = "China", "62" = "India", "64" = "Japan"
)
if (!file.exists(raw("hesta.json"))) {
  query <- list(
    query = list(
      list(code = "Jahr",
           selection = list(filter = "item", values = as.character(2005:2025))),
      list(code = "Monat",
           selection = list(filter = "item", values = I("YYYY"))),
      list(code = "Kanton",
           selection = list(filter = "item", values = as.character(1:26))),
      list(code = "Herkunftsland",
           selection = list(filter = "item",
                            values = c("00", names(hesta_origins)))),
      list(code = "Indikator",
           selection = list(filter = "item", values = c("1", "2")))
    ),
    response = list(format = "json-stat2")
  )
  qfile <- tempfile(fileext = ".json")
  jsonlite::write_json(query, qfile, auto_unbox = TRUE)
  status <- system2("curl", c(
    "-sSf", "-m", "120", "-X", "POST",
    "-H", shQuote("Content-Type: application/json"),
    "-A", shQuote(pv_user_agent),
    "-d", paste0("@", qfile),
    "-o", shQuote(raw("hesta.json")),
    shQuote(paste0("https://www.pxweb.bfs.admin.ch/api/v1/en/",
                   "px-x-1003020000_102/px-x-1003020000_102.px"))))
  stopifnot(status == 0)
}

js <- jsonlite::fromJSON(raw("hesta.json"), simplifyVector = FALSE)
dims <- unlist(js$id)
stopifnot(identical(
  dims, c("Jahr", "Monat", "Kanton", "Herkunftsland", "Indikator")))
# Category codes of each dimension, in the cube's storage order.
codes <- lapply(js$dimension[dims], function(d) {
  names(sort(unlist(d$category$index)))
})
# json-stat2 stores the values row-major (last dimension fastest), which
# is exactly an R array over the reversed dimensions.
a <- array(
  vapply(js$value,
         function(v) if (is.null(v)) NA_real_ else as.numeric(v), numeric(1)),
  dim = rev(unlist(js$size)), dimnames = rev(codes))
long <- as.data.frame.table(a, stringsAsFactors = FALSE,
                            responseName = "value")
arr <- long[long$Indikator == "1", c("Jahr", "Kanton", "Herkunftsland")]
ngt <- long[long$Indikator == "2", c("Jahr", "Kanton", "Herkunftsland")]
rownames(arr) <- rownames(ngt) <- NULL
stopifnot(identical(arr, ngt), !anyNA(long$value))
arr$arrivals <- long$value[long$Indikator == "1"]
arr$nights <- long$value[long$Indikator == "2"]

# Fold everything outside the named markets into "Other countries": the
# cube's origin total minus the named origins, per canton and year.
tot <- arr[arr$Herkunftsland == "00", ]
nmd <- arr[arr$Herkunftsland != "00", ]
at <- cbind(tot$Jahr, tot$Kanton)
tot$arrivals <- tot$arrivals -
  tapply(nmd$arrivals, list(nmd$Jahr, nmd$Kanton), sum)[at]
tot$nights <- tot$nights -
  tapply(nmd$nights, list(nmd$Jahr, nmd$Kanton), sum)[at]
stopifnot(all(tot$arrivals >= 0), all(tot$nights >= 0))
tot$Herkunftsland <- "other"

hesta <- rbind(nmd, tot)
origin_order <- c(unname(hesta_origins), "Other countries")
pv_tourism <- data.frame(
  year = as.integer(hesta$Jahr),
  canton_id = as.integer(hesta$Kanton),
  canton = unname(unlist(
    js$dimension$Kanton$category$label)[hesta$Kanton]),
  origin = ifelse(hesta$Herkunftsland == "other", "Other countries",
                  unname(hesta_origins[hesta$Herkunftsland])),
  arrivals = as.integer(hesta$arrivals),
  nights = as.integer(hesta$nights)
)
# A guest arriving stays at least one night, named market or remainder -
# anything else means the reshape scrambled rows.
stopifnot(all(pv_tourism$nights >= pv_tourism$arrivals))
pv_tourism <- pv_tourism[
  order(pv_tourism$year, pv_tourism$canton_id,
        match(pv_tourism$origin, origin_order)), ]
rownames(pv_tourism) <- NULL

for (nm in c("pv_electricity", "pv_tourism")) {
  save(list = nm, file = file.path("data", paste0(nm, ".rda")),
       compress = "xz")
  cat(nm, ":", nrow(get(nm)), "rows\n")
}
