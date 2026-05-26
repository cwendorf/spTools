# [`spTools`](https://github.com/cwendorf/spTools/)

## Confidence Intervals for a Set of One-Sample Proportions

### Description

A wrapper function for `ci.prop` that accepts vectors of frequencies and sample sizes.

### Usage

```r
ci.prop.vec(alpha, f, n)
```

### Arguments

- **`alpha`**: Alpha level for 1-alpha confidence.
- **`f`**: Numeric vector of frequencies.
- **`n`**: Numeric vector of sample sizes.

### Value

A matrix with one row per element of `f` and columns from `ci.prop`: `Estimate`, `SE`, `LL`, and `UL`.

### Examples

```r
ci.prop.vec(.05, f = c(120, 95), n = c(300, 250))
```
