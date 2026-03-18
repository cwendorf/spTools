# [`spTools`](https://github.com/cwendorf/spTools/)

## Wrapper for ci.stdmean2 with vector input

**Aliases:**

- `ci.stdmean2.vec`

### Description

A wrapper function for ci.stdmean2 that accepts vectors for means, standard deviations, and sample sizes.

### Usage

```r
ci.stdmean2.vec(alpha, m, sd, n)
```

### Arguments

- **`alpha`**: Alpha level for 1-alpha confidence
- **`m`**: Numeric vector of length 2: means for groups 1 and 2
- **`sd`**: Numeric vector of length 2: standard deviations for groups 1 and 2
- **`n`**: Numeric vector of length 2: sample sizes for groups 1 and 2

### Value

A 4-row matrix identical to the output of ci.stdmean2

### Examples

```r
ci.stdmean2.vec(.05, c(35.1, 26.7), c(7.32, 6.98), c(30, 30))
```

