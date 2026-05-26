# [`spTools`](https://github.com/cwendorf/spTools/)

## Confidence Intervals to Compare Two Correlations and Their Difference

### Description

Returns confidence intervals for each group correlation and a single comparison row for the two-group correlation difference.

### Usage

```r
ci.cor.compare(alpha, cor, s, n)
```

### Arguments

- **`alpha`**: Numeric scalar. Significance level (e.g., 0.05 for 95% CI).
- **`cor`**: Numeric vector of length 2. Correlations for groups 1 and 2.
- **`s`**: Numeric scalar or numeric vector of length 2. Number of control variables (set to 0 for Pearson correlations).
- **`n`**: Numeric vector of length 2. Sample sizes for groups 1 and 2.

### Details

If `s` is supplied as a scalar, it is used for both groups. If `s` is supplied as a vector, both values must be equal because the two-group difference method requires a common control-variable count.

### Value

A 3-row matrix with rows for group 1, group 2, and comparison.
Group rows contain output from `ci.cor.vec`. The comparison row contains output from the two-group correlation-difference confidence interval.

### Examples

```r
ci.cor.compare(
  alpha = .05,
  cor = c(.64, .31),
  s = 0,
  n = c(200, 200)
)
```
