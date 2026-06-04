# [`spTools`](https://github.com/cwendorf/spTools/)

## Confidence Intervals for Two Between-Subjects Mean Contrasts and Their Difference

### Description

Returns confidence intervals for two linear contrasts of independent group means
and the difference between those contrasts.

### Usage

```r
ci.lc.mean.bs.complex(alpha, m, sd, n, q1, q2, labels = NULL)
```

### Arguments

- **`alpha`**: Numeric scalar. Significance level (e.g., 0.05 for 95% CI).
- **`m`**: Numeric vector of length J. Means for each group.
- **`sd`**: Numeric vector of length J. Standard deviations for each group.
- **`n`**: Numeric vector of length J. Sample sizes for each group.
- **`q1`**: Numeric vector of length J. Contrast weights for the first contrast.
- **`q2`**: Numeric vector of length J. Contrast weights for the second contrast.
- **`labels`**: Character vector of length 2. Optional row labels for the two contrasts.

### Value

A 3-row matrix with rows for contrast 1, contrast 2, and their difference.
Each row contains `Estimate`, `SE`, `df`, `LL`, and `UL`. The contrast rows use
the equal-variances-assumed row from `statpsych::ci.lc.mean.bs`. The difference
row is computed from the contrast `q2 - q1`.

### Examples

```r
ci.lc.mean.bs.complex(
  alpha = .05,
  m = c(8, 11, 12),
  sd = c(1.414, 2.211, 2.449),
  n = c(10, 10, 10),
  q1 = c(1/3, 1/3, 1/3),
  q2 = c(1, 0, 0),
  labels = c("GrandMean", "L1Only")
)
```
