# [`spTools`](https://github.com/cwendorf/spTools/)

## Confidence Interval for Dependent Pearson Correlation Difference

### Description

A wrapper function for `ci.cor.dep` that accepts a vector for the two correlations and a scalar or repeated vector for the common sample size.

### Usage

```r
ci.cor.dep.vec(alpha, cor, cor12, n)
```

### Arguments

- **`alpha`**: Alpha level for 1-alpha confidence.
- **`cor`**: Numeric vector of length 2: correlations for measures 1 and 2.
- **`cor12`**: Numeric scalar: correlation between the two predictor variables associated with `cor[1]` and `cor[2]`.
- **`n`**: Numeric scalar or numeric vector of length 2 with equal values: common sample size.

### Value

A 1-row matrix identical to the output of `ci.cor.dep`.

### Examples

```r
ci.cor.dep.vec(.05, cor = c(.396, .179), cor12 = .088, n = 166)
```