# A small private stash for things we only want to work out once per
# session: whether Python is usable, and the imported Python module.
the <- new.env(parent = emptyenv())

#' Is the Python backend available?
#'
#' `TRUE` when `reticulate` is installed, a Python interpreter can be
#' found, and the bundled `polyviz.py` module imports cleanly. The result
#' is cached for the session. polyviz never requires Python: every
#' Python-backed function has an identical pure-R fallback.
#'
#' @return `TRUE` or `FALSE`.
#' @export
pv_py_available <- function() {
  if (!is.null(the$py_available)) {
    return(the$py_available)
  }
  the$py_available <- tryCatch({
    requireNamespace("reticulate", quietly = TRUE) &&
      !is.null(pv_py_module())
  }, error = function(e) FALSE)
  the$py_available
}

# Imports the polyviz.py file that ships inside this package and keeps it
# around for later calls. reticulate handles moving values between R and
# Python for us.
pv_py_module <- function() {
  if (is.null(the$py_module)) {
    the$py_module <- reticulate::import_from_path(
      "polyviz",
      path = system.file("python", package = "polyviz"),
      convert = TRUE
    )
  }
  the$py_module
}

# Decides which backend actually runs. "auto" means: use Python if we can,
# otherwise quietly fall back to R. Asking for Python explicitly when it is
# not available is an error rather than a silent downgrade.
resolve_engine <- function(engine = c("auto", "python", "r")) {
  engine <- match.arg(engine)
  if (engine == "python" && !pv_py_available()) {
    rlang::abort(paste(
      "The Python backend is not available.",
      "Install the `reticulate` package and a Python >= 3.8,",
      "or use `engine = \"r\"`."))
  }
  if (engine == "auto") {
    engine <- if (pv_py_available()) "python" else "r"
  }
  engine
}

# Sample skewness, excess kurtosis, and the Jarque-Bera normality test for
# a vector with the missing values already dropped. Mirrors _moments() in
# inst/python/polyviz.py: plain moment estimators g1 = m3 / m2^1.5 and
# g2 = m4 / m2^2 - 3, central moments with an n denominator. With fewer
# than four values, or when every value is the same, the shape of the
# distribution is not meaningfully estimable, so all four come back NaN.
profile_moments <- function(clean) {
  none <- c(skewness = NaN, kurtosis = NaN, jb_stat = NaN, jb_p = NaN)
  n <- length(clean)
  if (n < 4) {
    return(none)
  }
  mu <- mean(clean)
  m2 <- mean((clean - mu)^2)
  if (m2 == 0) {
    return(none)
  }
  skew <- mean((clean - mu)^3) / m2^1.5
  kurt <- mean((clean - mu)^4) / m2^2 - 3
  jb <- n / 6 * (skew^2 + kurt^2 / 4)
  # The Jarque-Bera statistic is compared against a chi-squared with 2 df,
  # whose upper tail is exactly exp(-x / 2) - the Python side uses that
  # closed form and pchisq gives the same number.
  c(skewness = skew, kurtosis = kurt, jb_stat = jb,
    jb_p = stats::pchisq(jb, df = 2, lower.tail = FALSE))
}

#' Profile the numeric columns of a data frame
#'
#' Per-column counts, missingness, moments, quartiles, distribution shape,
#' and a normality check. Runs on the bundled Python module when available
#' (see [pv_py_available()]) and on an identical pure-R implementation
#' otherwise — results match to numerical precision either way.
#'
#' @param data A data frame.
#' @param engine `"auto"` (default: Python when available), `"python"`, or
#'   `"r"`.
#' @return A data frame with one row per numeric column: `variable`, `n`,
#'   `n_missing`, `mean`, `sd`, `min`, `q25`, `median`, `q75`, `max`,
#'   `skewness` (how lopsided the distribution is — 0 for symmetric,
#'   positive for a long right tail), `kurtosis` (excess kurtosis: how
#'   heavy the tails are compared to a normal distribution, which scores
#'   0), and `jb_stat` with `jb_p` (the Jarque-Bera normality test built
#'   from those two; a small `jb_p` means the column's shape would be
#'   surprising for normal data). The shape columns are `NA` for columns
#'   with fewer than four observed values or zero variance. The engine
#'   that produced the result is recorded in `attr(, "engine")`.
#' @examples
#' pv_profile(mtcars, engine = "r")
#' @export
pv_profile <- function(data, engine = c("auto", "python", "r")) {
  engine <- resolve_engine(engine)
  # Only numeric columns make sense here; everything else is set aside.
  num <- data[vapply(data, is.numeric, logical(1))]
  if (!length(num)) {
    rlang::abort("`data` has no numeric columns to profile.")
  }

  if (engine == "python") {
    # Hand the columns over as plain lists of numbers - that way the
    # Python side needs nothing beyond the standard library. Each result
    # row comes back as a dict, which binds into a data frame.
    payload <- lapply(num, as.numeric)
    rows <- pv_py_module()$profile_columns(payload)
    out <- do.call(rbind, lapply(rows, as.data.frame))
  } else {
    out <- do.call(rbind, lapply(names(num), function(nm) {
      x <- as.numeric(num[[nm]])
      clean <- x[!is.na(x)]
      stats_row <- if (length(clean)) {
        q <- stats::quantile(clean, c(0.25, 0.5, 0.75), names = FALSE)
        mom <- profile_moments(clean)
        data.frame(mean = mean(clean),
                   sd = if (length(clean) > 1) stats::sd(clean) else NaN,
                   min = min(clean), q25 = q[1], median = q[2], q75 = q[3],
                   max = max(clean),
                   skewness = mom[["skewness"]],
                   kurtosis = mom[["kurtosis"]],
                   jb_stat = mom[["jb_stat"]], jb_p = mom[["jb_p"]])
      } else {
        data.frame(mean = NaN, sd = NaN, min = NaN, q25 = NaN,
                   median = NaN, q75 = NaN, max = NaN,
                   skewness = NaN, kurtosis = NaN, jb_stat = NaN, jb_p = NaN)
      }
      cbind(data.frame(variable = nm, n = length(x),
                       n_missing = sum(is.na(x))),
            stats_row)
    }))
  }

  rownames(out) <- NULL
  attr(out, "engine") <- engine
  out
}

#' Flag outliers in a numeric vector
#'
#' `"iqr"` flags values outside `[Q1 - k*IQR, Q3 + k*IQR]`; `"zscore"`
#' flags values more than `z` sample standard deviations from the mean.
#' Missing values are never flagged. Like [pv_profile()], the work runs in
#' Python when available and in R otherwise, with identical results.
#'
#' @param x A numeric vector.
#' @param method `"iqr"` (default) or `"zscore"`.
#' @param k IQR multiplier (default 1.5).
#' @param z Z-score threshold (default 3).
#' @param engine `"auto"`, `"python"`, or `"r"`.
#' @return A logical vector the same length as `x`.
#' @examples
#' x <- c(rnorm(50), 12)
#' which(pv_outliers(x, engine = "r"))
#' @export
pv_outliers <- function(x, method = c("iqr", "zscore"), k = 1.5, z = 3,
                        engine = c("auto", "python", "r")) {
  method <- match.arg(method)
  engine <- resolve_engine(engine)
  x <- as.numeric(x)

  if (engine == "python") {
    flags <- pv_py_module()$detect_outliers(x, method = method, k = k, z = z)
    return(as.logical(unlist(flags)))
  }

  # Pure R fallback. With fewer than two real values there is no spread to
  # measure, so nothing can be called an outlier.
  clean <- x[!is.na(x)]
  if (length(clean) < 2) {
    return(rep(FALSE, length(x)))
  }
  if (method == "iqr") {
    q <- stats::quantile(clean, c(0.25, 0.75), names = FALSE)
    iqr <- q[2] - q[1]
    lo <- q[1] - k * iqr
    hi <- q[2] + k * iqr
  } else {
    mu <- mean(clean)
    s <- stats::sd(clean)
    if (s == 0) {
      return(rep(FALSE, length(x)))
    }
    lo <- mu - z * s
    hi <- mu + z * s
  }
  flags <- x < lo | x > hi
  flags[is.na(flags)] <- FALSE
  flags
}
