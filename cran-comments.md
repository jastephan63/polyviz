# cran-comments

## Submission

This is a new package: first submission to CRAN.

## R CMD check results

0 errors | 0 warnings | 2 notes

* checking data for non-ASCII characters ... NOTE
    Note: found marked UTF-8 strings

  This is intentional. The bundled datasets are Swiss open government
  data (Bundesamt für Statistik, LUSTAT Statistik Luzern, MeteoSwiss),
  and Swiss place names contain non-ASCII characters — "Zürich",
  "Escholzmatt-Marbach", "Altbüron". The strings are stored as, and
  deliberately marked, UTF-8; DESCRIPTION declares `Encoding: UTF-8`.

* checking installed package size ... NOTE
    installed size is 5.8Mb

  The size comes from the rendered vignettes (`doc`, 2.2Mb), which
  embed live interactive charts so they can be read offline, and from
  the help pages for 24 chart types plus the bundled D3 library and
  Inter font that every chart needs to render.

## Test environments

* Local: macOS (Apple Silicon), R release
* GitHub Actions: ubuntu-latest, R release

## Downstream dependencies

There are none; this is a first release.
