# cran-comments

## Submission

This is a new package: first submission to CRAN.

## R CMD check results

0 errors | 0 warnings | 1 note

* checking data for non-ASCII characters ... NOTE
    Note: found marked UTF-8 strings

  This is intentional. The bundled datasets are Swiss open government
  data (Bundesamt für Statistik, LUSTAT Statistik Luzern, MeteoSwiss),
  and Swiss place names contain non-ASCII characters — "Zürich",
  "Escholzmatt-Marbach", "Altbüron". The strings are stored as, and
  deliberately marked, UTF-8; DESCRIPTION declares `Encoding: UTF-8`.

## Test environments

* Local: macOS (Apple Silicon), R release
* GitHub Actions: ubuntu-latest, R release

## Downstream dependencies

There are none; this is a first release.
