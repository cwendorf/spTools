# [`spTools`](https://github.com/cwendorf/spTools/)

## Confidence Interval for Independent Groups Proportion Difference

### Description

A wrapper function for `ci.prop2` that accepts vectors for frequencies and sample sizes in two groups.

### Usage

```r
ci.prop2.vec(alpha, f, n)
```

### Arguments

- **`alpha`**: Alpha level for 1-alpha confidence.
- **`f`**: Numeric vector of length 2: frequencies for groups 1 and 2.
- **`n`**: Numeric vector of length 2: sample sizes for groups 1 and 2.

### Value

A 1-row matrix identical to the output of `ci.prop2`.

### Examples

```r
ci.prop2.vec(.05, f = c(57, 15), n = c(100, 100))
```
