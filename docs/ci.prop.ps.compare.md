# [`spTools`](https://github.com/cwendorf/spTools/)

## Confidence Intervals to Compare Two Paired Proportions and Their Difference

### Description

Returns confidence intervals for each paired-measurement proportion and a single comparison row for the paired proportion difference.

### Usage

```r
ci.prop.ps.compare(alpha, f00, f01, f10, f11)
```

### Arguments

- **`alpha`**: Numeric scalar. Significance level (e.g., 0.05 for 95% CI).
- **`f00`**: Cell count for outcome 0 at measure 1 and outcome 0 at measure 2.
- **`f01`**: Cell count for outcome 0 at measure 1 and outcome 1 at measure 2.
- **`f10`**: Cell count for outcome 1 at measure 1 and outcome 0 at measure 2.
- **`f11`**: Cell count for outcome 1 at measure 1 and outcome 1 at measure 2.

### Value

A 3-row matrix with rows for measure 1, measure 2, and comparison. Group rows contain one-sample proportion confidence intervals for each measure. The comparison row contains selected columns from `statpsych::ci.prop.ps`.

### Examples

```r
ci.prop.ps.compare(
  alpha = .05,
  f00 = 12,
  f01 = 4,
  f10 = 26,
  f11 = 6
)
```
