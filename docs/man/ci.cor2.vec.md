# [`spTools`](https://github.com/cwendorf/spTools/)

## Confidence Interval for Independent Groups Pearson Correlation Difference

### Description

A wrapper function for `ci.cor2` that accepts vectors for group correlations and sample sizes.

### Usage

```r
ci.cor2.vec(alpha, cor, n)
```

### Arguments

- **`alpha`**: Alpha level for 1-alpha confidence.
- **`cor`**: Numeric vector of length 2: correlations for groups 1 and 2.
- **`n`**: Numeric vector of length 2: sample sizes for groups 1 and 2.

### Value

A 1-row matrix identical to the output of `ci.cor2`.

### Examples

```r
ci.cor2.vec(.05, cor = c(.64, .31), n = c(200, 200))
```
