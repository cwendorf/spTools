# [`spTools`](https://github.com/cwendorf/spTools/)

## Confidence Intervals for Two Within-Subjects Mean Contrasts and Their Difference

### Description

Returns confidence intervals for two linear contrasts of within-subjects
(repeated measures) group means and the difference between those contrasts.

### Usage

```r
ci.lc.mean.ws.complex(alpha, m, s, R, n, q1, q2, labels = NULL)
```

### Arguments

- **`alpha`**: Numeric scalar. Significance level (e.g., 0.05 for 95% CI).
- **`m`**: Numeric vector of length J. Means for each condition.
- **`s`**: Numeric vector of length J. Standard deviations for each condition.
- **`R`**: Either a scalar (average correlation among all condition pairs) or a J x J correlation matrix among conditions.
- **`n`**: Integer scalar. Sample size (number of subjects).
- **`q1`**: Numeric vector of length J. Contrast weights for the first contrast.
- **`q2`**: Numeric vector of length J. Contrast weights for the second contrast.
- **`labels`**: Character vector of length 2. Optional row labels for the two contrasts.

### Value

A 3-row matrix with rows for contrast 1, contrast 2, and their difference.
Each row contains `Estimate`, `SE`, `df`, `LL`, and `UL`, as returned by
`ci.lc.mean.ws`. The difference row is computed from the contrast `q2 - q1`.

### Examples

```r
R <- matrix(c(1, .7, .7, .7, 1, .7, .7, .7, 1), 3, 3)
ci.lc.mean.ws.complex(
  alpha = .05,
  m = c(8, 11, 12),
  s = c(1.414, 2.211, 2.449),
  R = R,
  n = 10,
  q1 = c(1/3, 1/3, 1/3),
  q2 = c(1, 0, 0),
  labels = c("GrandMean", "L1Only")
)
```
