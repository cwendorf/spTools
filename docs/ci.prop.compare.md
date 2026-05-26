# [`spTools`](https://github.com/cwendorf/spTools/)

## Confidence Intervals to Compare Two Proportions

### Description

Returns confidence intervals for each group proportion and comparison rows for the proportion difference, the proportion ratio, or both.

### Usage

```r
ci.prop.compare(alpha, f, n, type = c("difference", "ratio", "both"))
```

### Arguments

- **`alpha`**: Numeric scalar. Significance level (e.g., 0.05 for 95% CI).
- **`f`**: Numeric vector of length 2. Frequencies for groups 1 and 2.
- **`n`**: Numeric vector of length 2. Sample sizes for groups 1 and 2.
- **`type`**: Character scalar indicating comparison rows to include: `"difference"`, `"ratio"`, or `"both"`.

### Details

The difference row is obtained from `ci.prop2.vec`. The ratio row is obtained from `statpsych::ci.ratio.prop2`. Because `ci.ratio.prop2` does not return a standard error column, `SE` is set to `NA` for the ratio row.

### Value

A matrix with rows for group 1, group 2, and one or two comparison rows depending on `type`. Columns are `Estimate`, `SE`, `LL`, and `UL`.

### Examples

```r
ci.prop.compare(
  alpha = .05,
  f = c(57, 15),
  n = c(100, 100),
  type = "both"
)
```
