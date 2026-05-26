# [`spTools`](https://github.com/cwendorf/spTools/)

## Confidence Intervals to Compare Two Paired Means and Their Difference

### Description

Returns confidence intervals for each paired measurement mean and a single
comparison row for the paired mean difference.

### Usage

```r
ci.mean.ps.compare(alpha, m, sd, cor, n)
```

### Arguments

- **`alpha`**: Numeric scalar. Significance level (e.g., 0.05 for 95% CI).
- **`m`**: Numeric vector of length 2. Means for measurements 1 and 2.
- **`sd`**: Numeric vector of length 2. Standard deviations for measurements 1 and 2.
- **`cor`**: Numeric scalar. Estimated correlation between paired measurements.
- **`n`**: Numeric scalar or numeric vector of length 2. Paired sample size.

### Details

If `n` is supplied as a scalar, it is expanded to length 2 for the
measurement rows.

### Value

A 3-row matrix with rows for measurement 1, measurement 2, and comparison.
Measurement rows contain output from `ci.mean.vec`. The comparison row contains
selected columns from `ci.mean.ps.vec` corresponding to the paired-difference
summary.

### Examples

```r
ci.mean.ps.compare(
  alpha = .05,
  m = c(58.2, 51.4),
  sd = c(7.43, 8.92),
  cor = .537,
  n = 30
)
```
