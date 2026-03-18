# [`spTools`](https://github.com/cwendorf/spTools/)

## Wrapper for ci.stdmean.ps with vector input

**Aliases:**

- `ci.stdmean.ps.vec`

### Description

A wrapper function for ci.stdmean.ps that accepts vectors for means and standard deviations.

### Usage

```r
ci.stdmean.ps.vec(alpha, m, sd, cor, n)
```

### Arguments

- **`alpha`**: Alpha level for 1-alpha confidence
- **`m`**: Numeric vector of length 2: means for the two measurements
- **`sd`**: Numeric vector of length 2: standard deviations for the two measurements
- **`cor`**: Estimated correlation between the two measurements (scalar)
- **`n`**: Sample size (scalar)

### Value

A 3-row matrix identical to the output of ci.stdmean.ps

### Examples

```r
ci.stdmean.ps.vec(.05, c(110.4, 102.1), c(15.3, 14.6), .75, 25)
```

