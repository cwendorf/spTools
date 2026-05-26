# [`spTools`](https://github.com/cwendorf/spTools/)

## Confidence Intervals for a Set of Pearson (or Partial) Correlations

### Description

A wrapper function for `ci.cor` that accepts vectors of correlations, control-variable counts, and sample sizes.

### Usage

```r
ci.cor.vec(alpha, cor, s, n)
```

### Arguments

- **`alpha`**: Alpha level for 1-alpha confidence.
- **`cor`**: Numeric vector of estimated Pearson or partial correlations.
- **`s`**: Numeric vector of control-variable counts (set 0 for Pearson).
- **`n`**: Numeric vector of sample sizes.

### Value

A matrix with one row per element of `cor` and columns from `ci.cor`: `Estimate`, `SE`, `LL`, and `UL`.

### Examples

```r
ci.cor.vec(
  alpha = .05,
  cor = c(.60, .70),
  s = c(0, 1),
  n = c(150, 135)
)
```
