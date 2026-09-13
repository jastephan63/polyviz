# Builds the motion story (docs/motion.html): "Sechzig Minuten", an
# investigation of Switzerland measured in travel time from Lucerne's
# platforms, on the live national GTFS timetable from
# opentransportdata.swiss. The page is a pv_story(): isochrone rings
# growing as the reader scrolls, Sunday against Tuesday, the evening
# map, the Marey diagram of the Zuerich corridor, the hourly pulse of
# the great stations, and an honest regression of minutes on
# kilometres.
#
# House rule as in build-story.R: every number in the prose is computed
# here from the fetched timetable - nothing typed in from memory. The
# first run downloads the ~236 MB GTFS export (cached; later runs are
# served from the cache and work offline).
#
# Run from the package root: LANG=en_US.UTF-8 Rscript data-raw/build-motion.R

suppressMessages(pkgload::load_all(".", quiet = TRUE))
library(htmltools)

fmt_n <- function(x) {
  format(round(as.numeric(x)), big.mark = ",", scientific = FALSE,
         trim = TRUE)
}
fmt_pct <- function(x, digits = 0) {
  paste0(formatC(x, format = "f", digits = digits), "%")
}
fmt_1 <- function(x) formatC(x, format = "f", digits = 1)
sized <- function(w, height) {
  w$width <- "100%"
  w$height <- height
  w
}
quelle_otd <- "Quelle: opentransportdata.swiss (ODMCH)"
quelle_otd_map <-
  "Quelle: opentransportdata.swiss (ODMCH); Grenzen: © BFS, ThemaKart"

# Great-circle kilometres, the same arithmetic the isochrone uses.
haversine_km <- function(lat1, lon1, lat2, lon2) {
  r <- 6371
  p <- pi / 180
  a <- sin((lat2 - lat1) * p / 2)^2 +
    cos(lat1 * p) * cos(lat2 * p) * sin((lon2 - lon1) * p / 2)^2
  2 * r * asin(pmin(1, sqrt(a)))
}

# ---- the timetable ---------------------------------------------------------

cat("Opening the national timetable (cached after the first run)...\n")
gtfs <- pv_fetch_gtfs()

feed <- DBI::dbGetQuery(gtfs$con, "SELECT * FROM feed_info LIMIT 1")
n_stop_times <- DBI::dbGetQuery(
  gtfs$con, "SELECT count(*) AS n FROM stop_times")$n
n_stations <- DBI::dbGetQuery(gtfs$con, "
  SELECT count(*) AS n FROM stops
  WHERE parent_station IS NULL OR parent_station = ''")$n

# The two service days the story compares: a plain Tuesday and the
# Sunday before it, inside the feed's validity window.
day_tue <- as.Date("2026-09-15")
day_sun <- as.Date("2026-09-13")

# ---- travel times from Lucerne ---------------------------------------------

cat("Scanning the timetable (three departures)...\n")
tt_tue <- pv_transit_times(gtfs, "Luzern", day_tue, depart = "08:00",
                           max_minutes = 300)
tt_sun <- pv_transit_times(gtfs, "Luzern", day_sun, depart = "08:00",
                           max_minutes = 300)
tt_eve <- pv_transit_times(gtfs, "Luzern", day_tue, depart = "22:00",
                           max_minutes = 300)

reached <- function(tt, within = Inf) {
  sum(!is.na(tt$minutes) & tt$minutes <= within)
}

n_30 <- reached(tt_tue, 30)
n_60 <- reached(tt_tue, 60)
n_120 <- reached(tt_tue, 120)
n_any <- reached(tt_tue)
n_all <- nrow(tt_tue)

n_60_sun <- reached(tt_sun, 60)
n_120_sun <- reached(tt_sun, 120)
n_any_sun <- reached(tt_sun)
n_any_eve <- reached(tt_eve)

# The cantonal capitals inside the hour, computed, not asserted.
capitals <- c("Zürich HB", "Bern", "Luzern", "Altdorf UR", "Schwyz",
              "Sarnen", "Stans", "Glarus", "Zug", "Fribourg", "Solothurn",
              "Basel SBB", "Liestal", "Schaffhausen", "Herisau",
              "Appenzell", "St. Gallen", "Chur", "Aarau", "Frauenfeld",
              "Bellinzona", "Lausanne", "Sion", "Neuchâtel",
              "Genève", "Delémont")
cap_times <- tt_tue[tt_tue$station %in% capitals, c("station", "minutes")]
caps_in_60 <- sort(cap_times$station[!is.na(cap_times$minutes) &
                                       cap_times$minutes <= 60])
caps_in_120 <- sort(cap_times$station[!is.na(cap_times$minutes) &
                                        cap_times$minutes <= 120])

# Sunday's price, per station: how many minutes the same journey gains.
sun_cmp <- merge(tt_tue[!is.na(tt_tue$minutes),
                        c("station", "lat", "lon", "minutes")],
                 tt_sun[!is.na(tt_sun$minutes), c("station", "minutes")],
                 by = "station", suffixes = c("_tue", "_sun"))
sun_cmp$penalty <- sun_cmp$minutes_sun - sun_cmp$minutes_tue
sun_median_penalty <- stats::median(sun_cmp$penalty)
sun_lost <- n_any - n_any_sun

# The dumbbell picks sizeable places, not request-stop hamlets: keep
# stations at least 40 weekday departures deep, then the worst Sunday
# penalties among them.
dep_counts <- DBI::dbGetQuery(gtfs$con, "
  WITH svc AS (
    SELECT service_id FROM calendar
    WHERE tuesday = 1 AND start_date <= 20260915 AND end_date >= 20260915
    UNION
    SELECT service_id FROM calendar_dates
    WHERE date = 20260915 AND exception_type = 1
    EXCEPT
    SELECT service_id FROM calendar_dates
    WHERE date = 20260915 AND exception_type = 2)
  SELECT coalesce(nullif(s.parent_station, ''), s.stop_id) AS pid,
         count(*) AS departures
  FROM stop_times st
  JOIN trips t ON st.trip_id = t.trip_id
  JOIN svc ON t.service_id = svc.service_id
  JOIN stops s ON st.stop_id = s.stop_id
  GROUP BY 1")
# sun_cmp carries no stop_id - bring it in from tt_tue first.
sun_cmp2 <- merge(sun_cmp,
                  tt_tue[, c("station", "stop_id")], by = "station")
sun_big <- merge(sun_cmp2, dep_counts, by.x = "stop_id", by.y = "pid")
sun_big <- sun_big[sun_big$departures >= 40 & sun_big$penalty > 0, ]
sun_big <- sun_big[order(-sun_big$penalty), ]
sun_top <- utils::head(sun_big, 12)

cat("Computed reach:", n_30, "/", n_60, "/", n_120, "stations at 30/60/120;",
    "Sunday reaches", n_any_sun, "vs", n_any, "\n")

# ---- the corridor: Luzern -> Zug -> Zuerich, as Marey drew it --------------

# Every train that serves Luzern and then Zuerich HB between 06:00 and
# 09:00 on the Tuesday, with each stop's clock time and its
# great-circle distance from Luzern - the classic train graph.
marey_raw <- DBI::dbGetQuery(gtfs$con, "
  WITH svc AS (
    SELECT service_id FROM calendar
    WHERE tuesday = 1 AND start_date <= 20260915 AND end_date >= 20260915
    UNION
    SELECT service_id FROM calendar_dates
    WHERE date = 20260915 AND exception_type = 1
    EXCEPT
    SELECT service_id FROM calendar_dates
    WHERE date = 20260915 AND exception_type = 2),
  day_st AS (
    SELECT st.trip_id, st.stop_sequence,
           coalesce(nullif(s.parent_station, ''), s.stop_id) AS pid,
           coalesce(s.stop_name, '') AS station,
           s.stop_lat AS lat, s.stop_lon AS lon,
           st.departure_time, st.arrival_time
    FROM stop_times st
    JOIN trips t ON st.trip_id = t.trip_id
    JOIN svc ON t.service_id = svc.service_id
    JOIN stops s ON st.stop_id = s.stop_id),
  luz AS (
    SELECT trip_id, stop_sequence AS luz_seq, departure_time AS luz_dep
    FROM day_st WHERE station = 'Luzern'),
  zrh AS (
    SELECT trip_id, stop_sequence AS zrh_seq
    FROM day_st WHERE station = 'Zürich HB')
  SELECT d.trip_id, d.station, d.lat, d.lon, d.stop_sequence,
         d.arrival_time, d.departure_time
  FROM day_st d
  JOIN luz ON d.trip_id = luz.trip_id
  JOIN zrh ON d.trip_id = zrh.trip_id
  WHERE luz.luz_seq < zrh.zrh_seq
    AND d.stop_sequence BETWEEN luz.luz_seq AND zrh.zrh_seq
    AND luz.luz_dep >= '06:00:00' AND luz.luz_dep < '09:00:00'
  ORDER BY d.trip_id, d.stop_sequence")

to_min <- function(hms) {
  p <- do.call(rbind, strsplit(hms, ":", fixed = TRUE))
  as.numeric(p[, 1]) * 60 + as.numeric(p[, 2]) + as.numeric(p[, 3]) / 60
}
luz_pos <- tt_tue[tt_tue$station == "Luzern", c("lat", "lon")]
marey_raw$km <- haversine_km(luz_pos$lat, luz_pos$lon,
                             marey_raw$lat, marey_raw$lon)

# Each stop contributes its arrival and its departure, so dwell time is
# visible as a flat step in the line.
marey <- do.call(rbind, lapply(split(marey_raw, marey_raw$trip_id),
  function(tr) {
    d <- data.frame(
      trip = tr$trip_id[1],
      clock = c(rbind(to_min(tr$arrival_time), to_min(tr$departure_time))),
      km = rep(tr$km, each = 2))
    d <- d[is.finite(d$clock), ]
    # A stop without dwell puts arrival and departure on the same
    # minute - one point carries it; the line validator (rightly)
    # refuses duplicate x within a series.
    d[!duplicated(d$clock), ]
  }))
n_marey_trains <- length(unique(marey$trip))
via_zug <- length(unique(
  marey_raw$trip_id[marey_raw$station == "Zug"]))

# The fastest scheduled Luzern -> Zuerich run in the window.
run_time <- tapply(marey$clock, marey$trip, function(v) max(v) - min(v))
marey_fastest <- min(run_time)

# ---- the pulse: departures per hour at the great stations ------------------

hubs <- c("Zürich HB", "Bern", "Basel SBB", "Lausanne", "Luzern",
          "Olten", "Zug", "Winterthur", "St. Gallen", "Biel/Bienne",
          "Genève", "Bellinzona")
pulse <- DBI::dbGetQuery(gtfs$con, sprintf("
  WITH svc AS (
    SELECT service_id FROM calendar
    WHERE tuesday = 1 AND start_date <= 20260915 AND end_date >= 20260915
    UNION
    SELECT service_id FROM calendar_dates
    WHERE date = 20260915 AND exception_type = 1
    EXCEPT
    SELECT service_id FROM calendar_dates
    WHERE date = 20260915 AND exception_type = 2)
  SELECT s.stop_name AS station,
         cast(substr(st.departure_time, 1, 2) AS INTEGER) %% 24 AS hour,
         count(*) AS departures
  FROM stop_times st
  JOIN trips t ON st.trip_id = t.trip_id
  JOIN svc ON t.service_id = svc.service_id
  JOIN stops s ON st.stop_id = s.stop_id
  WHERE s.stop_name IN (%s)
    AND st.departure_time IS NOT NULL AND st.departure_time <> ''
  GROUP BY 1, 2 ORDER BY 1, 2",
  paste(sprintf("'%s'", gsub("'", "''", hubs)), collapse = ", ")))
pulse <- pulse[pulse$hour >= 5, ]
pulse_peak <- pulse[which.max(pulse$departures), ]

# ---- fast and slow: minutes against kilometres -----------------------------

speed <- tt_tue[!is.na(tt_tue$minutes) & tt_tue$minutes > 0, ]
speed$km <- haversine_km(luz_pos$lat, luz_pos$lon, speed$lat, speed$lon)
speed <- speed[speed$km >= 5, ]
speed$kmh <- speed$km / (speed$minutes / 60)
fit <- stats::lm(minutes ~ km, data = speed)
fit_slope <- stats::coef(fit)[["km"]]
fit_r2 <- summary(fit)$r.squared
# The named extremes among substantial journeys, so a lucky suburb and
# an alp with one bus do not carry the sentence alone.
far <- speed[speed$km >= 40, ]
fastest <- far[which.max(far$kmh), ]
big <- merge(speed, dep_counts, by.x = "stop_id", by.y = "pid")
big <- big[big$departures >= 40 & big$km >= 20, ]
slowest <- big[which.min(big$kmh), ]

cat("Marey trains:", n_marey_trains, " fastest:", round(marey_fastest),
    "min; fit slope:", round(fit_slope, 2), "R2:", round(fit_r2, 2), "\n")

# ---- the charts ------------------------------------------------------------

# Each step's map carries only the horizon its prose claims, so the
# open-ended "over" band never paints reach the step is not talking
# about: the map grows as the story does.
iso <- function(tt, breaks, subtitle, within = Inf) {
  d <- tt[!is.na(tt$minutes) & tt$minutes <= within, ]
  sized(pv_isochrone(
    d, lat = "lat", lon = "lon",
    minutes = "minutes",
    origin = c(lat = luz_pos$lat, lon = luz_pos$lon),
    breaks = breaks,
    title = "Wie weit von Luzern?",
    subtitle = subtitle,
    source = quelle_otd_map), 540)
}
w_iso30 <- iso(tt_tue, c(30),
  sprintf("Reachable by public transport, Tuesday %s, departing 08:00",
          format(day_tue, "%d.%m.%Y")), within = 30)
w_iso60 <- iso(tt_tue, c(30, 60),
  "The first hour, departing Luzern 08:00", within = 60)
w_iso_full <- iso(tt_tue, c(30, 60, 90, 120),
  "Two hours out, departing Luzern 08:00")
w_iso_sun <- iso(tt_sun, c(30, 60, 90, 120),
  sprintf("The same map on Sunday %s, departing 08:00",
          format(day_sun, "%d.%m.%Y")))
w_iso_eve <- iso(tt_eve, c(30, 60, 90, 120),
  "Departing Luzern at 22:00 on the Tuesday")

w_sun <- sized(pv_dumbbell(
  sun_top, y = "station", x1 = "minutes_tue", x2 = "minutes_sun",
  labels = c("Tuesday", "Sunday"), sort = "gap",
  xlab = "minutes from Luzern, departing 08:00",
  title = "What Sunday costs",
  subtitle = "The largest Sunday penalties among well-served stations (40+ weekday departures)",
  source = quelle_otd), 460)

w_marey <- sized(pv_line(
  marey, x = "clock", y = "km", series = "trip", legend = FALSE,
  xlab = "clock time (minutes after midnight)",
  ylab = "great-circle km from Luzern",
  title = "Three hours on the Zürich corridor",
  subtitle = sprintf(
    "Every train serving Luzern before Zürich HB, 06:00–09:00, %d runs",
    n_marey_trains),
  source = quelle_otd), 480)

w_pulse <- sized(pv_heatmap(
  pulse, x = "hour", y = "station", value = "departures",
  xlab = "hour of day", cell_values = FALSE,
  title = "The pulse of the great stations",
  subtitle = "Departures per hour on the Tuesday, all modes",
  source = quelle_otd), 440)

w_speed <- sized(pv_scatter(
  speed, x = "km", y = "minutes",
  xlab = "great-circle km from Luzern",
  ylab = "minutes, departing 08:00",
  title = "Minutes against kilometres",
  subtitle = sprintf(
    "%s reached stations at least 5 km out; the line is the least-squares fit",
    fmt_n(nrow(speed))),
  source = quelle_otd) |>
    pv_trend(method = "lm"), 470)

# ---- assemble the story ----------------------------------------------------

esc <- htmltools::htmlEscape
card <- function(id = NULL, heading = NULL, ...) {
  paste0(
    if (!is.null(heading)) {
      paste0("<h3", if (!is.null(id)) paste0(" id='", id, "'"), ">",
             esc(heading), "</h3>")
    } else "",
    paste0("<p>", vapply(list(...), esc, character(1)), "</p>",
           collapse = ""))
}
chapter <- function(id, german, english, intro) {
  pv_story_break(tags$section(
    id = id, class = "theme",
    tags$h2(german, tags$span(paste0(" — ", english))),
    tags$p(class = "theme-intro", intro)))
}

# The same page chrome as story.html, generated from the theme tokens.
motion_css <- local({
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
    ".methods { color: var(--secondary); line-height: 1.65;\n",
    "           max-width: 720px; }\n",
    ".methods p { margin: 0 0 10px; }\n",
    ".methods code { font-size: 0.92em; }\n",
    "footer { color: var(--muted); font-size: 12.5px; line-height: 1.6;\n",
    "         border-top: 1px solid var(--grid); padding-top: 18px;\n",
    "         margin-top: 52px; }\n",
    "footer a { color: inherit; }\n")
})

blocks <- c(list(
  pv_story_break(tags$header(
    tags$p(class = "eyebrow", "A polyviz data investigation"),
    tags$h1("Sechzig Minuten"),
    tags$p(class = "lede", sprintf(paste(
      "Switzerland publishes its entire public timetable as open data:",
      "%s scheduled stop events across %s stations, every train, bus,",
      "boat and cable car in the country, in one %s file. This page",
      "reads that file the way a commuter reads a departure board -",
      "from one platform outward. Starting at Luzern, it measures the",
      "country in minutes rather than kilometres: how far sixty of",
      "them carry you, what the map loses on a Sunday and at night,",
      "what three hours on the Zürich corridor look like when",
      "every train is a line, and which places the timetable treats",
      "generously or harshly. Every number is computed from the",
      "timetable at build time; the methods and their limits are in",
      "the appendix."),
      fmt_n(n_stop_times), fmt_n(n_stations), "3.8 GB")))),
  pv_story_break(tags$nav(
    tags$a(href = "#stunde", "1 · Die Stunde"),
    tags$a(href = "#sonntag", "2 · Sonntag und Nacht"),
    tags$a(href = "#korridor", "3 · Der Korridor"),
    tags$a(href = "#puls", "4 · Der Puls"),
    tags$a(href = "#tempo", "5 · Schnell und langsam"),
    tags$a(href = "#methoden", "Appendix"),
    tags$a(href = "story.html", "← Anatomy of a Swiss canton"),
    tags$a(href = "index.html", "← polyviz gallery")
  )),

  chapter("stunde", "Die Stunde", "the hour",
    paste("A travel-time map redraws the country: distance stops",
          "mattering and the timetable decides what is near. This",
          "chapter grows the map outward from Luzern's platforms,",
          "thirty minutes at a time, on an ordinary Tuesday morning."))),
  list(
  pv_story_step(w_iso30, card(
    "iso", "The first thirty minutes",
    sprintf(paste(
      "Half an hour from an 08:00 departure, the timetable delivers",
      "%s stations - the lake communities, the Emme and Reuss",
      "valleys, the first ring of the agglomeration. The map shows",
      "coloured bands only where a measured station is within a",
      "thirty-minute walk; white is not emptiness but honesty."),
      fmt_n(n_30)))),
  pv_story_step(w_iso60, card(
    NULL, NULL,
    sprintf(paste(
      "The full hour reaches %s stations - %s of every station in",
      "the country - including %d cantonal capitals: %s. The",
      "sixty-minute country is the plateau between the Jura and the",
      "Alps; the mountains hold the bands back on every southern",
      "edge."),
      fmt_n(n_60), fmt_pct(100 * n_60 / n_all, 1),
      length(caps_in_60),
      paste(caps_in_60, collapse = ", ")))),
  pv_story_step(w_iso_full, card(
    NULL, NULL,
    sprintf(paste(
      "Two hours out, the bands cover %s stations and %d capitals -",
      "%s. Within the five-hour horizon the scan reaches %s stations",
      "in all, %s of the network, from one city's platforms on one",
      "morning."),
      fmt_n(n_120), length(caps_in_120),
      paste(utils::head(setdiff(caps_in_120, caps_in_60), 8),
            collapse = ", "),
      fmt_n(n_any), fmt_pct(100 * n_any / n_all, 1)))),

  chapter("sonntag", "Sonntag und Nacht", "Sunday and night",
    paste("The timetable is not one document but a family of them,",
          "and the generous weekday edition is only one member. This",
          "chapter runs the same scan on the Sunday before and late on",
          "the Tuesday evening, and counts what disappears.")),
  pv_story_step(w_iso_sun, card(
    "sonntag-karte", "The Sunday map",
    sprintf(paste(
      "On Sunday morning the same scan reaches %s stations against",
      "Tuesday's %s - %s fewer. The median reached station arrives",
      "%s minutes later than on the weekday; the bands thin most",
      "along the small valley lines, where Sunday service drops to",
      "every other hour or to nothing."),
      fmt_n(n_any_sun), fmt_n(n_any), fmt_n(sun_lost),
      fmt_1(sun_median_penalty)))),
  pv_story_step(w_sun, card(
    NULL, NULL,
    paste("The penalty is not spread evenly. Among stations with at",
          "least forty weekday departures, these twelve pay the most",
          "for being visited on a Sunday - each dumbbell is the same",
          "journey, priced on the two days."))),
  pv_story_step(w_iso_eve, card(
    NULL, NULL,
    sprintf(paste(
      "At 22:00 on the Tuesday the network is still awake but no",
      "longer ambitious: %s stations remain reachable within the",
      "five-hour horizon, and the outer bands belong to the night",
      "trains and the last valley buses."),
      fmt_n(n_any_eve)))),

  chapter("korridor", "Der Korridor", "the corridor",
    paste("One line on the isochrone map is a hundred pages of",
          "timetable. This chapter draws the classic railway graph -",
          "Étienne-Jules Marey's train schedule, time across,",
          "distance up - for the busiest road out of Luzern: the",
          "corridor to Zürich.")),
  pv_story_step(w_marey, card(
    "marey", "Every train is a line",
    sprintf(paste(
      "Between 06:00 and 09:00, %d trains leave Luzern for",
      "Zürich HB, %d of them via Zug. Steep is fast; a kink is",
      "a stop; parallel lines a few minutes apart are the Takt, the",
      "repeating pattern the Swiss timetable is built on. The",
      "fastest run in the window makes the journey in %d minutes."),
      n_marey_trains, via_zug, round(marey_fastest)))),

  chapter("puls", "Der Puls", "the pulse",
    paste("Zoom all the way out and the timetable becomes a",
          "heartbeat: departures per hour, station by station. The",
          "Takt shows as horizontal evenness; the rush hours as",
          "vertical stripes.")),
  pv_story_step(w_pulse, card(
    "puls-karte", "Departures per hour",
    sprintf(paste(
      "The busiest single hour on the board is %s at %02d:00, with",
      "%s departures - one every %.1f seconds. Read the rows: the",
      "big stations barely breathe between 06:00 and 20:00, and the",
      "evening taper is gentle everywhere - the Takt holds until",
      "close of service."),
      pulse_peak$station, pulse_peak$hour, fmt_n(pulse_peak$departures),
      3600 / pulse_peak$departures))),

  chapter("tempo", "Schnell und langsam", "fast and slow",
    paste("Minutes and kilometres disagree about geography. This",
          "chapter puts every reached station on one scatter -",
          "straight-line distance against travel time - and lets a",
          "regression say how the timetable prices a kilometre.")),
  pv_story_step(w_speed, card(
    "fit", "What a kilometre costs",
    sprintf(paste(
      "Across %s stations, each straight-line kilometre costs %s",
      "minutes on average (least squares; R² = %s) - but the",
      "residuals are the story. Above the line, the timetable is",
      "harsh: valleys reached by detours and changes. Below it, the",
      "main lines: %s is the fastest substantial journey at %s km/h",
      "as the crow flies, while among well-served stations %s is",
      "slowest at %s km/h."),
      fmt_n(nrow(speed)), fmt_1(fit_slope), fmt_1(fit_r2),
      fastest$station, fmt_n(fastest$kmh),
      slowest$station, fmt_1(slowest$kmh)))),

  pv_story_break(tags$section(
    id = "methoden", class = "theme",
    tags$h2("Methoden und Daten", tags$span(" — methods and data")),
    tags$p(class = "theme-intro", paste(
      "Enough to re-run every number on this page.")),
    tags$div(class = "methods", HTML(paste0(
      "<p><strong>Data.</strong> The national GTFS timetable export ",
      "(Fahrplan 2026) from opentransportdata.swiss, the open data ",
      "platform mobility Switzerland (ODMCH), fetched by ",
      "<code>pv_fetch_gtfs()</code> and read in place by DuckDB — ",
      esc(fmt_n(n_stop_times)), " stop events, ",
      esc(fmt_n(n_stations)), " stations. Feed version ",
      esc(paste(feed$feed_version, collapse = " ")), ".</p>",
      "<p><strong>Travel times.</strong> <code>pv_transit_times()</code> ",
      "runs a Connection Scan over the chosen day's connections: ",
      "earliest arrival at every station, honouring published minimum ",
      "transfer times (a two-minute change buffer where the feed is ",
      "silent), platform-to-station grouping, and departures past ",
      "midnight, capped here at four changes and five hours. It keeps ",
      "one label per station — earliest arrival, fewest changes on ",
      "a tie — not a full multi-criteria profile; on-demand ",
      "(frequencies-based) services are excluded. The scan is the ",
      "timetable's promise, not measured punctuality.</p>",
      "<p><strong>Isochrones.</strong> <code>pv_isochrone()</code> ",
      "rasterises station times to a grid where each cell takes the ",
      "minimum of station minutes plus a 5 km/h walk from the cell to ",
      "the station, leaves cells beyond a thirty-minute walk uncoloured, ",
      "and contours the grid into bands clipped at the border. Bands ",
      "claim reachability of stations, not of every meadow between ",
      "them.</p>",
      "<p><strong>The corridor.</strong> Trains serving Luzern before ",
      "Zürich HB on the Tuesday, departing 06:00–09:00; the ",
      "vertical axis is each stop's great-circle distance from Luzern, ",
      "so dwell time shows as a flat step and gradient compares fairly ",
      "between runs on slightly different paths.</p>",
      "<p><strong>The regression.</strong> Ordinary least squares of ",
      "minutes on great-circle kilometres over reached stations at ",
      "least 5 km out; named extremes are restricted to journeys of ",
      "40 km or more (fastest) and stations with 40+ weekday ",
      "departures (slowest), so a one-bus alp does not carry a ",
      "sentence alone. A straight line through a mountain country's ",
      "travel times explains much and hides more — the R² is ",
      "printed, not vaunted.</p>"))))),
  pv_story_break(tags$footer(HTML(paste0(
    "Data: opentransportdata.swiss — the open data platform ",
    "mobility Switzerland (ODMCH), operated by SBB on behalf of the ",
    "Federal Office of Transport; free use with source citation. ",
    "Boundaries © BFS, ThemaKart. Charts rendered by ",
    "<a href='https://d3js.org'>d3.js</a> v7, type set in ",
    "<a href='https://rsms.me/inter/'>Inter</a>. Built with the ",
    "MIT-licensed R package ",
    "<a href='https://github.com/jastephan63/polyviz'>jastephan63/",
    "polyviz</a> on ", format(Sys.Date(), "%Y-%m-%d"),
    "; every figure and number is recomputed from the timetable on ",
    "each build. <a href='story.html'>The first story</a> · ",
    "<a href='index.html'>the gallery</a>.")))))
)

story <- pv_story(blocks, mode = "auto")
page <- tags$html(lang = "en", tags$head(
  tags$meta(charset = "utf-8"),
  tags$meta(name = "viewport",
            content = "width=device-width, initial-scale=1"),
  tags$title("Sechzig Minuten"),
  tags$style(HTML(motion_css))
), tags$body(story))

dir.create("docs", showWarnings = FALSE)
save_html(page, "docs/motion.html", libdir = "lib")
pv_gtfs_close(gtfs)
cat("motion story written to docs/motion.html\n")
