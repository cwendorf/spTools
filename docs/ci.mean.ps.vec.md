# [`spTools`](https://github.com/cwendorf/spTools/)

## Wrapper for ci.mean.ps with vector input

**Aliases:**

- `ci.mean.ps.vec`

### Description

A wrapper function for ci.mean.ps that accepts vectors for means and standard deviations.

### Usage

```r
ci.mean.ps.vec(alpha, m, sd, cor, n)
```

### Arguments

- **`alpha`**: Alpha level for 1-alpha confidence
- **`m`**: Numeric vector of length 2: means for the two measurements
- **`sd`**: Numeric vector of length 2: standard deviations for the two measurements
- **`cor`**: Estimated correlation between the two measurements
- **`n`**: Sample size (scalar)

### Value

A 1-row matrix identical to the output of ci.mean.ps

### Examples

```r
ci.mean.ps.vec(.05, c(58.2, 51.4), c(7.43, 8.92), .537, 30)
```

