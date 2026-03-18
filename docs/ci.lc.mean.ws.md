# [`spTools`](https://github.com/cwendorf/spTools/)

## Confidence Interval for a Linear Contrast of Means (Within-Subject)

**Aliases:**

- `ci.lc.mean.ws`

### Description

Computes a confidence interval for a linear contrast of means
from within-subject (repeated measures) data, accounting for the variance-covariance
structure given by standard deviations and either a correlation matrix or a single
average correlation value.

### Usage

```r
ci.lc.mean.ws(alpha, m, s, R, n, q)
```

### Arguments

- **`alpha`**: Numeric scalar. Significance level (e.g., 0.05 for 95% CI).
- **`m`**: Numeric vector of length J. Means of the J conditions.
- **`s`**: Numeric vector of length J. Standard deviations of the J conditions.
- **`R`**: Either a scalar (average correlation among all condition pairs) or a J x J
correlation matrix among conditions.
- **`n`**: Integer scalar or vector of length J. Sample size (number of subjects).
If a vector is provided, all elements must be equal.
- **`q`**: Numeric vector of length J. Contrast weights.

### Details

If R is a scalar, it is treated as the common correlation among all pairs of
conditions and is expanded into a J x J matrix with 1's on the diagonal and the
specified correlation on the off-diagonal. If a matrix is provided, it must be a
symmetric J x J correlation matrix.

The function computes the variance-covariance matrix \(V = D R D\), where \(D\)
is a diagonal matrix of standard deviations. The variance of the contrast is
calculated as \(v' V v / n\). The confidence interval is constructed using the
t-distribution with \(n-1\) degrees of freedom. If n is a vector, all
values must be equal, and the common value will be used.

### Value

A 1-row matrix with named columns:

EstimateLinear contrast estimate.
SEStandard error of the contrast estimate.
dfDegrees of freedom (n - 1).
LLLower limit of the confidence interval.
ULUpper limit of the confidence interval.

### Examples

```r
m <- c(5.2, 6.1, 7.3)
s <- c(1.1, 1.2, 1.4)
R <- matrix(c(
  1, 0.8, 0.6,
  0.8, 1, 0.7,
  0.6, 0.7, 1
), 3, 3, byrow = TRUE)
q <- c(-1, 0, 1)

# Using a correlation matrix
ci.lc.mean.ws(0.05, m, s, R, 30, q)

# Using a scalar average correlation
ci.lc.mean.ws(0.05, m, s, 0.65, 30, q)

# Using vector n (all values equal)
ci.lc.mean.ws(0.05, m, s, R, rep(30, 3), q)
```

