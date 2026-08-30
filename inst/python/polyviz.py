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


def profile_columns(data):
    """Profile a dict of {name: list of numbers}.

    Returns a list of dicts, one per column, with n, n_missing, mean, sd,
    min, quartiles, and max. Columns with no non-missing values report
    counts only.
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
            row.update({
                "mean": statistics.fmean(clean),
                "sd": statistics.stdev(clean) if len(clean) > 1 else float("nan"),
                "min": min(clean),
                "q25": q25,
                "median": q50,
                "q75": q75,
                "max": max(clean),
            })
        else:
            row.update({k: float("nan") for k in
                        ("mean", "sd", "min", "q25", "median", "q75", "max")})
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
