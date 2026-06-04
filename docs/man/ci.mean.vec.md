# [`spTools`](https://github.com/cwendorf/spTools/)

## Confidence Intervals for a Set of Means

### Description

Calculates the confidence interval for a vector of sample means given
standard deviations and sample sizes. Returns a matrix with the estimate,
standard error, degrees of freedom, and lower and upper limits of the
confidence interval.

### Usage

```r
ci.mean.vec(alpha, m, sd, n)
```

### Arguments

- **`alpha`**: Significance level.
- **`m`**: A named numeric vector of sample means.
- **`sd`**: A numeric vector of sample standard deviations.
- **`n`**: A numeric vector of sample sizes (number of observations per group).

### Value

A matrix with rows corresponding to group names (from means) and columns:

- Estimate: The sample mean.
- SE: Standard error of the mean.
- df: Degrees of freedom (n - 1).
- LL: Lower limit of the confidence interval.
- UL: Upper limit of the confidence interval.

### Examples

```r
# Named groups
ci.mean.vec(
  alpha = 0.05,
  m = c(A = 5.2, B = 6.1),
  sd = c(1.1, 1.3),
  n = c(30, 28)
)

# Unnamed groups: generic names will be assigned
ci.mean.vec(
  alpha = 0.05,
  m = c(5.2, 6.1),
  sd = c(1.1, 1.3),
  n = c(30, 28)
)
```

