# [`spTools`](https://github.com/cwendorf/spTools/)

## Confidence Intervals to Compare Two Dependent Correlations and Their Difference

### Description

Returns confidence intervals for each correlation and a single comparison row for the dependent-correlation difference.

### Usage

```r
ci.cor.dep.compare(alpha, cor, cor12, n, s = 0)
```

### Arguments

- **`alpha`**: Numeric scalar. Significance level (e.g., 0.05 for 95% CI).
- **`cor`**: Numeric vector of length 2. Correlations for measures 1 and 2.
- **`cor12`**: Numeric scalar. Correlation between the two predictor variables associated with `cor[1]` and `cor[2]`.
- **`n`**: Numeric scalar or numeric vector of length 2. Common sample size.
- **`s`**: Numeric scalar. Number of control variables for the group-level confidence intervals (set to 0 for Pearson correlations).

### Details

If `n` is supplied as a vector, both values must be equal.

### Value

A 3-row matrix with rows for correlation 1, correlation 2, and comparison. Group rows contain output from `ci.cor.vec`. The comparison row contains output from `statpsych::ci.cor.dep`.

### Examples

```r
ci.cor.dep.compare(
  alpha = .05,
  cor = c(.396, .179),
  cor12 = .088,
  n = 166,
  s = 0
)
```
