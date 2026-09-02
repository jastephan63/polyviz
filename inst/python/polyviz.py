"""Numeric profiling backend for the polyviz R package.

Standard library only, so it runs on any Python >= 3.8 that reticulate can
find -- no pandas/numpy required. The R side sends numeric columns as a
dict of lists; missing values arrive as None or NaN.
"""

import math
import statistics


def _clean(values):
    """Drop missing values (None or NaN); return floats."""
    out = []
    for v in values:
        if v is None:
            continue
        f = float(v)
        if math.isnan(f):
            continue
        out.append(f)
    return out


def _quartiles(clean):
    """Quartiles matching R's default (type 7) quantiles."""
    if len(clean) == 1:
        return clean[0], clean[0], clean[0]
    q = statistics.quantiles(clean, n=4, method="inclusive")
    return q[0], q[1], q[2]


def _chisq2_sf(x):
    """Upper-tail probability of the chi-squared distribution with 2 df.

    With two degrees of freedom the chi-squared distribution is an
    exponential with mean 2, so the survival function is exactly
    exp(-x / 2). This matches R's pchisq(x, 2, lower.tail = FALSE).
    """
    if x <= 0:
        return 1.0
    return math.exp(-x / 2.0)


def _moments(clean):
    """Sample skewness, excess kurtosis, and the Jarque-Bera test.

    Uses the plain moment estimators g1 = m3 / m2^1.5 and
    g2 = m4 / m2^2 - 3, where m_k is the k-th central moment with an n
    denominator. With fewer than four values, or when every value is the
    same, the shape of the distribution is not meaningfully estimable, so
    all four results are NaN.
    """
    n = len(clean)
    nan = float("nan")
    if n < 4:
        return nan, nan, nan, nan
    mu = statistics.fmean(clean)
    m2 = math.fsum((v - mu) ** 2 for v in clean) / n
    if m2 == 0:
        return nan, nan, nan, nan
    m3 = math.fsum((v - mu) ** 3 for v in clean) / n
    m4 = math.fsum((v - mu) ** 4 for v in clean) / n
    skew = m3 / m2 ** 1.5
    kurt = m4 / m2 ** 2 - 3.0
    jb = n / 6.0 * (skew ** 2 + kurt ** 2 / 4.0)
    return skew, kurt, jb, _chisq2_sf(jb)


def profile_columns(data):
    """Profile a dict of {name: list of numbers}.

    Returns a list of dicts, one per column, with n, n_missing, mean, sd,
    min, quartiles, max, skewness, excess kurtosis, and the Jarque-Bera
    normality test (statistic and p-value). Columns with no non-missing
    values report counts only.
    """
    rows = []
    for name, values in data.items():
        clean = _clean(values)
        row = {
            "variable": str(name),
            "n": len(values),
            "n_missing": len(values) - len(clean),
        }
        if clean:
            q25, q50, q75 = _quartiles(clean)
            skew, kurt, jb, jb_p = _moments(clean)
            row.update({
                "mean": statistics.fmean(clean),
                "sd": statistics.stdev(clean) if len(clean) > 1 else float("nan"),
                "min": min(clean),
                "q25": q25,
                "median": q50,
                "q75": q75,
                "max": max(clean),
                "skewness": skew,
                "kurtosis": kurt,
                "jb_stat": jb,
                "jb_p": jb_p,
            })
        else:
            row.update({k: float("nan") for k in
                        ("mean", "sd", "min", "q25", "median", "q75", "max",
                         "skewness", "kurtosis", "jb_stat", "jb_p")})
        rows.append(row)
    return rows


def detect_outliers(values, method="iqr", k=1.5, z=3.0):
    """Flag outliers in a list of numbers.

    method="iqr": outside [Q1 - k*IQR, Q3 + k*IQR].
    method="zscore": |value - mean| / sd > z.
    Missing values are never flagged. Returns a list of booleans the same
    length as the input.
    """
    clean = _clean(values)
    n = len(values)
    if len(clean) < 2:
        return [False] * n

    if method == "iqr":
        q25, _, q75 = _quartiles(clean)
        iqr = q75 - q25
        lo, hi = q25 - k * iqr, q75 + k * iqr
    elif method == "zscore":
        mu = statistics.fmean(clean)
        sd = statistics.stdev(clean)
        if sd == 0:
            return [False] * n
        lo, hi = mu - z * sd, mu + z * sd
    else:
        raise ValueError("method must be 'iqr' or 'zscore'")

    flags = []
    for v in values:
        if v is None or math.isnan(float(v)):
            flags.append(False)
        else:
            f = float(v)
            flags.append(f < lo or f > hi)
    return flags
