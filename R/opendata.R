# Discovery and download of datasets catalogued on opendata.swiss, the
# Swiss federal open-government-data portal, through its CKAN API. The
# actual files live on the publishing organisations' servers; downloads run
# through the shared cache in R/fetch.R, and anything without an open
# licence is refused rather than silently delivered.

ckan_api <- "https://ckan.opendata.swiss/api/3/action/"

# One CKAN API call, parsed. API answers are never cached - searches and
# dataset lookups should always reflect the live catalogue.
pv_ckan <- function(action, params) {
  qs <- paste(names(params),
              vapply(params, function(v) {
                utils::URLencode(as.character(v), reserved = TRUE)
              }, character(1)),
              sep = "=", collapse = "&")
  dl <- pv_download(paste0(ckan_api, action, "?", qs), cache = FALSE)
  txt <- paste(readLines(dl$path, warn = FALSE, encoding = "UTF-8"),
               collapse = "\n")
  ans <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
  if (!isTRUE(ans$success)) {
    rlang::abort(sprintf("The opendata.swiss API call `%s` failed.", action))
  }
  ans$result
}

# opendata.swiss metadata fields are multilingual lists; pick one language,
# preferring English, then the national languages.
pick_lang <- function(x) {
  if (is.null(x)) return(NA_character_)
  if (!is.list(x)) return(as.character(x)[1])
  for (lang in c("en", "de", "fr", "it")) {
    v <- x[[lang]]
    if (length(v) == 1 && is.character(v) && nzchar(v)) return(v)
  }
  v <- unlist(x, use.names = FALSE)
  v <- v[nzchar(v)]
  if (length(v)) v[1] else NA_character_
}

# Classifies a CKAN licence/rights id. opendata.swiss uses four
# terms-of-use grades (OPEN, OPEN BY, OPEN ASK, OPEN BY ASK - all open
# data, differing in attribution and commercial-use conditions), older
# datasets carry the legacy DCAT ids spelling the same grades out, and a
# few publishers use standard open licences directly. Anything with a
# non-commercial/no-derivatives restriction, a closed id, or no stated
# licence at all is treated as not open.
pv_licence_info <- function(rights) {
  rights <- rights %||% ""
  if (!is.character(rights) || length(rights) != 1 || is.na(rights)) {
    rights <- ""
  }
  info <- function(label, open, by = FALSE, ask = FALSE) {
    list(id = rights, label = label, open = open, by = by, ask = ask)
  }
  has <- function(p) grepl(p, rights, ignore.case = TRUE)
  if (!nzchar(rights)) {
    return(info("no licence stated", open = FALSE))
  }
  # restriction markers first: "NonCommercialAllowed-CommercialNotAllowed-..."
  # contains the substring "CommercialAllowed", so the open branches below
  # must never see these ids
  if (has("-nc") || has("-nd") || has("NonCommercialNotAllowed") ||
      has("CommercialNotAllowed") || has("Closed")) {
    return(info(rights, open = FALSE))
  }
  if (has("terms_by_ask") || has("CommercialWithPermission.*ReferenceRequired") ||
      has("ReferenceRequired.*CommercialWithPermission")) {
    return(info("OPEN BY ASK", open = TRUE, by = TRUE, ask = TRUE))
  }
  if (has("terms_by") ||
      (has("CommercialAllowed") && has("ReferenceRequired"))) {
    return(info("OPEN BY", open = TRUE, by = TRUE))
  }
  if (has("terms_ask") || has("CommercialWithPermission")) {
    return(info("OPEN ASK", open = TRUE, ask = TRUE))
  }
  if (has("terms_open") || has("cc-zero") || has("publicdomain") ||
      has("pddl") ||
      (has("NonCommercialAllowed") && has("CommercialAllowed"))) {
    return(info("open (public domain)", open = TRUE))
  }
  if (has("cc-by-sa") || has("/by-sa/")) {
    return(info("CC BY-SA", open = TRUE, by = TRUE))
  }
  if (has("cc-by") || has("/by/")) {
    return(info("CC BY", open = TRUE, by = TRUE))
  }
  if (has("odbl") || has("opendefinition")) {
    return(info(rights, open = TRUE, by = TRUE))
  }
  info(rights, open = FALSE)
}

# Turns a licence classification into the sentence printed with each fetch,
# naming the organisation the citation must credit.
pv_licence_text <- function(info, organisation) {
  terms <- if (info$by && !is.na(organisation)) {
    sprintf('free use with source citation ("Quelle: %s")', organisation)
  } else if (info$by) {
    "free use with source citation"
  } else {
    "free use"
  }
  if (info$ask) {
    terms <- paste0(terms,
                    "; commercial use requires the data owner's permission")
  }
  sprintf("%s terms - %s.", info$label, terms)
}

#' Search datasets on opendata.swiss
#'
#' Queries the opendata.swiss catalogue (the Swiss open-government-data
#' portal) and returns the matching datasets as a compact data frame. Feed
#' a result's `id` to [pv_fetch_opendata()] to download its data.
#'
#' @param query Search terms, e.g. `"Finanzausgleich Luzern"`.
#' @param rows Maximum number of datasets to return (1 to 100).
#' @return A data frame with one row per dataset: `title`, `id` (the
#'   dataset slug [pv_fetch_opendata()] takes), `organisation` (the
#'   publisher), `licence` (the terms its resources are published under),
#'   and `formats` (the file formats on offer).
#' @seealso [pv_fetch_opendata()]
#' @examples
#' \dontrun{
#' pv_search_opendata("Finanzausgleich Luzern")
#' }
#' @export
pv_search_opendata <- function(query, rows = 10) {
  if (!is.character(query) || length(query) != 1 || is.na(query) ||
      !nzchar(query)) {
    rlang::abort("`query` must be a single non-empty search string.")
  }
  if (!is.numeric(rows) || length(rows) != 1 || is.na(rows) ||
      rows < 1 || rows > 100) {
    rlang::abort("`rows` must be a single number between 1 and 100.")
  }
  res <- pv_ckan("package_search", list(q = query, rows = as.integer(rows)))
  results <- res$results %||% list()
  rows_out <- lapply(results, function(r) {
    rights <- unique(vapply(r$resources, function(x) {
      pv_licence_info(x$rights %||% x$license)$label
    }, character(1)))
    formats <- unique(toupper(vapply(r$resources, function(x) {
      x$format %||% ""
    }, character(1))))
    data.frame(
      title = pick_lang(r$title),
      id = r$name,
      organisation = pick_lang(r$organization$title),
      licence = paste(rights[nzchar(rights)], collapse = "; "),
      formats = paste(sort(formats[nzchar(formats)]), collapse = ", "))
  })
  out <- do.call(rbind, rows_out)
  if (is.null(out)) {
    out <- data.frame(title = character(), id = character(),
                      organisation = character(), licence = character(),
                      formats = character())
  }
  message(sprintf(
    'opendata.swiss: %d dataset(s) match "%s"; showing %d.',
    res$count %||% nrow(out), query, nrow(out)))
  out
}

#' Fetch a dataset's CSV data from opendata.swiss
#'
#' Looks a dataset up on opendata.swiss, downloads one of its CSV
#' resources from the publishing organisation's server, and parses it into
#' a data frame. Downloads are cached (see [pv_cache_status()]); the source
#' and licence terms are printed on every fetch and attached to the result
#' (`pv_source`, `pv_licence`, `pv_url`).
#'
#' Only openly licensed resources are delivered. A resource whose licence
#' is not one of the opendata.swiss open-data terms (or another open
#' licence) is refused with a message - including datasets that state no
#' licence at all. Note that "OPEN ASK"/"OPEN BY ASK" resources are open
#' data whose commercial use requires the data owner's permission; the
#' printed terms say so.
#'
#' @param dataset Dataset slug or id on opendata.swiss, e.g.
#'   `"finanzausgleich-kanton-luzern"` - the `id` column of a
#'   [pv_search_opendata()] result.
#' @param resource Which resource to download when the dataset has several:
#'   `NULL` (the default) takes the dataset's first CSV resource, a number
#'   `n` its `n`-th CSV resource, and a string is matched against resource
#'   ids, URLs, and file names.
#' @inheritParams pv_fetch_bfs
#' @return A data frame; source and licence are attached as attributes and
#'   printed on every fetch.
#' @seealso [pv_search_opendata()], [pv_cache_status()]
#' @examples
#' \dontrun{
#' fiscal <- pv_fetch_opendata("finanzausgleich-kanton-luzern",
#'                             resource = "fa-lu-ra.csv")
#' str(fiscal)
#' }
#' @export
pv_fetch_opendata <- function(dataset, resource = NULL, refresh = FALSE) {
  if (!is.character(dataset) || length(dataset) != 1 || is.na(dataset) ||
      !nzchar(dataset)) {
    rlang::abort("`dataset` must be a single dataset slug or id.")
  }
  if (!isTRUE(refresh) && !isFALSE(refresh)) {
    rlang::abort("`refresh` must be TRUE or FALSE.")
  }
  pkg <- tryCatch(
    pv_ckan("package_show", list(id = dataset)),
    polyviz_download_error = function(e) {
      rlang::abort(sprintf(paste0(
        'Could not look up dataset "%s" on opendata.swiss - it may not ',
        "exist, or the portal may be unreachable."), dataset), parent = e)
    })
  pv_deliver_ckan_resource(pkg, resource = resource, refresh = refresh)
}

# The delivery half of pv_fetch_opendata, split off so the resource
# selection and licence gate can be exercised on fixture metadata without
# the catalogue lookup.
pv_deliver_ckan_resource <- function(pkg, resource = NULL, refresh = FALSE) {
  dataset <- pkg$name %||% "?"
  resources <- pkg$resources %||% list()
  urls <- vapply(resources, function(r) {
    as.character(r$download_url %||% r$url %||% "")
  }, character(1))
  is_csv <- vapply(seq_along(resources), function(i) {
    identical(toupper(resources[[i]]$format %||% ""), "CSV") ||
      grepl("\\.csv($|\\?)", urls[i], ignore.case = TRUE)
  }, logical(1))

  if (is.null(resource)) {
    idx <- which(is_csv)[1]
    if (is.na(idx)) {
      formats <- unique(toupper(vapply(resources, function(r) {
        r$format %||% ""
      }, character(1))))
      rlang::abort(sprintf(
        'Dataset "%s" has no CSV resource%s.', dataset,
        if (any(nzchar(formats))) {
          sprintf(" (available: %s)",
                  paste(formats[nzchar(formats)], collapse = ", "))
        } else ""))
    }
  } else if (is.numeric(resource) && length(resource) == 1) {
    idx <- which(is_csv)[resource]
    if (is.na(idx)) {
      rlang::abort(sprintf(
        'Dataset "%s" has %d CSV resource(s); there is no number %s.',
        dataset, sum(is_csv), format(resource)))
    }
  } else if (is.character(resource) && length(resource) == 1) {
    ids <- vapply(resources, function(r) {
      as.character(r$id %||% "")
    }, character(1))
    idx <- which(ids == resource | urls == resource |
                   basename(urls) == resource |
                   basename(urls) == paste0(resource, ".csv"))[1]
    if (is.na(idx)) {
      rlang::abort(sprintf(
        'No resource "%s" in dataset "%s". Its resources are: %s.',
        resource, dataset, paste(basename(urls), collapse = ", ")))
    }
  } else {
    rlang::abort("`resource` must be NULL, a single number, or a single string.")
  }

  r <- resources[[idx]]
  organisation <- pick_lang(pkg$organization$title)
  info <- pv_licence_info(r$rights %||% r$license %||% pkg$license_id)
  if (!info$open) {
    rlang::abort(sprintf(paste0(
      'Resource "%s" of dataset "%s" is not published under an open ',
      "licence (%s), so polyviz will not deliver it. Check the dataset ",
      "page on opendata.swiss for its access conditions."),
      basename(urls[idx]), dataset, info$label))
  }
  url <- urls[idx]
  if (!nzchar(url)) {
    rlang::abort(sprintf(
      'Resource "%s" of dataset "%s" has no download URL.',
      as.character(r$id %||% idx), dataset))
  }

  dl <- pv_download(url, refresh = refresh)
  d <- pv_read_swiss_csv(dl$path)
  pv_deliver(
    d,
    source = sprintf('%s, "%s" via opendata.swiss (dataset "%s")',
                     organisation, pick_lang(r$title %||% r$name), dataset),
    licence = paste0("opendata.swiss ", pv_licence_text(info, organisation)),
    url = url)
}
