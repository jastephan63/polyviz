# Locale-aware number and date formatting. The bundled data is Swiss,
# but d3's stock formatting is US English - "1,000" and "January".
# pv_locale() stores one of the packaged locale definitions beside the
# theme in the package environment (`the`, python.R), pv_widget()
# attaches it to every payload built while it is set, and the JavaScript
# side builds d3.formatLocale / d3.timeFormatLocale instances from it.
# R only ships the separator marks and the month/day names; d3 does the
# actual formatting at render time, as always.

# One d3 locale definition per Swiss language region: a number locale
# (d3.formatLocale) and a time locale (d3.timeFormatLocale) each.
# Numbers follow the Federal Chancellery's conventions - German,
# Italian, and English Switzerland group thousands with an apostrophe
# and keep the decimal point ("10'000.5"); French Switzerland groups
# with a non-breaking space and writes the decimal comma ("10 000,5").
# Dates are dd.mm.yyyy everywhere. `grouping` stays a list so it
# reaches d3 as the JSON array it requires even under the serialiser's
# auto-unboxing; length-2+ character vectors survive as arrays anyway.
pv_locales <- list(
  "de-CH" = list(
    number = list(
      decimal = ".", thousands = "'", grouping = list(3L),
      currency = list("CHF\u00a0", "")
    ),
    time = list(
      dateTime = "%A, der %e. %B %Y, %X",
      date = "%d.%m.%Y", time = "%H:%M:%S", periods = c("AM", "PM"),
      days = c("Sonntag", "Montag", "Dienstag", "Mittwoch",
               "Donnerstag", "Freitag", "Samstag"),
      shortDays = c("So", "Mo", "Di", "Mi", "Do", "Fr", "Sa"),
      months = c("Januar", "Februar", "M\u00e4rz", "April", "Mai",
                 "Juni", "Juli", "August", "September", "Oktober",
                 "November", "Dezember"),
      shortMonths = c("Jan", "Feb", "M\u00e4r", "Apr", "Mai", "Jun",
                      "Jul", "Aug", "Sep", "Okt", "Nov", "Dez")
    )
  ),
  "fr-CH" = list(
    number = list(
      decimal = ",", thousands = "\u00a0", grouping = list(3L),
      currency = list("", "\u00a0CHF")
    ),
    time = list(
      dateTime = "%A %e %B %Y \u00e0 %X",
      date = "%d.%m.%Y", time = "%H:%M:%S", periods = c("AM", "PM"),
      days = c("dimanche", "lundi", "mardi", "mercredi", "jeudi",
               "vendredi", "samedi"),
      shortDays = c("dim.", "lun.", "mar.", "mer.", "jeu.", "ven.",
                    "sam."),
      months = c("janvier", "f\u00e9vrier", "mars", "avril", "mai",
                 "juin", "juillet", "ao\u00fbt", "septembre", "octobre",
                 "novembre", "d\u00e9cembre"),
      shortMonths = c("janv.", "f\u00e9vr.", "mars", "avr.", "mai",
                      "juin", "juil.", "ao\u00fbt", "sept.", "oct.",
                      "nov.", "d\u00e9c.")
    )
  ),
  "it-CH" = list(
    number = list(
      decimal = ".", thousands = "'", grouping = list(3L),
      currency = list("CHF\u00a0", "")
    ),
    time = list(
      dateTime = "%A %e %B %Y, %X",
      date = "%d.%m.%Y", time = "%H:%M:%S", periods = c("AM", "PM"),
      days = c("Domenica", "Luned\u00ec", "Marted\u00ec",
               "Mercoled\u00ec", "Gioved\u00ec", "Venerd\u00ec",
               "Sabato"),
      shortDays = c("Dom", "Lun", "Mar", "Mer", "Gio", "Ven", "Sab"),
      months = c("Gennaio", "Febbraio", "Marzo", "Aprile", "Maggio",
                 "Giugno", "Luglio", "Agosto", "Settembre", "Ottobre",
                 "Novembre", "Dicembre"),
      shortMonths = c("Gen", "Feb", "Mar", "Apr", "Mag", "Giu", "Lug",
                      "Ago", "Set", "Ott", "Nov", "Dic")
    )
  ),
  "en-CH" = list(
    number = list(
      decimal = ".", thousands = "'", grouping = list(3L),
      currency = list("CHF\u00a0", "")
    ),
    time = list(
      dateTime = "%A, %e %B %Y, %X",
      date = "%d.%m.%Y", time = "%H:%M:%S", periods = c("AM", "PM"),
      days = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday",
               "Friday", "Saturday"),
      shortDays = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"),
      months = c("January", "February", "March", "April", "May",
                 "June", "July", "August", "September", "October",
                 "November", "December"),
      shortMonths = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                      "Aug", "Sep", "Oct", "Nov", "Dec")
    )
  )
)

#' Set a session-wide chart locale
#'
#' Switches every chart built afterwards in this session to
#' locale-aware number and date formatting. The packaged locales cover
#' Switzerland's language regions: `"de-CH"`, `"it-CH"`, and `"en-CH"`
#' group thousands with the Swiss apostrophe and keep the decimal point
#' (`10'000.5`), while `"fr-CH"` groups with a non-breaking space and
#' writes the decimal comma (`10 000,5`); each carries the month and
#' weekday names of its language, so date axes, calendar tooltips, and
#' race timestamps read `février` or `März` instead of
#' `February`.
#'
#' What changes on a chart: tooltips take the locale's grouping and
#' decimal marks (compact value labels its decimal mark), time axes and
#' date read-outs take its month and weekday names, and axis ticks of
#' `10000` and up print
#' grouped in full (`10'000` — the Swiss print convention) up to a
#' million, above which the compact form stays but wears the locale's
#' decimal mark (`1,5M` in `fr-CH`). Ticks below 10'000 stay ungrouped,
#' exactly as without a locale, so year axes keep reading `2020`.
#'
#' With no locale set (the default, and after `pv_locale(NULL)`) every
#' chart renders byte-identically to previous polyviz versions: d3's
#' stock US-English output.
#'
#' The setting travels inside each widget's payload, so a chart built
#' while a locale is active keeps it wherever it goes - a saved
#' `.html` file, [pv_save()] captures, a Shiny app - and charts built
#' with different locales coexist on one page.
#'
#' @param locale One of `"de-CH"`, `"fr-CH"`, `"it-CH"`, `"en-CH"`, or
#'   `NULL` (the default) to reset to d3's stock US-English formatting.
#' @return The locale definition now in effect (a list with `tag`,
#'   `number`, and `time` entries), or `NULL` after a reset, invisibly.
#' @examples
#' pv_locale("de-CH")
#' sales <- aggregate(revenue ~ region, pv_sales, sum)
#' pv_bar(sales, x = "region", y = "revenue",
#'        title = "Umsatz nach Region")  # ticks like 10'000
#' pv_locale(NULL)
#' @export
pv_locale <- function(locale = NULL) {
  if (is.null(locale)) {
    the$locale <- NULL
    return(invisible(NULL))
  }
  if (!is.character(locale) || length(locale) != 1 || is.na(locale) ||
      !locale %in% names(pv_locales)) {
    rlang::abort(sprintf(
      "`locale` must be one of %s, or NULL to reset.",
      paste0('"', names(pv_locales), '"', collapse = ", ")))
  }
  # The tag rides along so the JavaScript side can cache the built d3
  # locale instances per locale rather than per chart.
  the$locale <- c(list(tag = locale), pv_locales[[locale]])
  invisible(the$locale)
}

# Hands pv_widget() the active locale definition, or NULL when none is
# set. Assigning NULL to a list element is a no-op, so payloads built
# without a locale carry no `locale` field at all and stay
# byte-identical to what they were before locales existed.
pv_locale_payload <- function() {
  the$locale
}
