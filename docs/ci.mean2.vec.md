# [`spTools`](https://github.com/cwendorf/spTools/)

## Wrapper for ci.mean2 with vector input

**Aliases:**

- `ci.mean2.vec`

### Description

A wrapper function for ci.mean2 that accepts vectors for means, standard deviations, and sample sizes.

### Usage

```r
ci.mean2.vec(alpha, m, sd, n)
```

### Arguments

- **`alpha`**: Alpha level for 1-alpha confidence
- **`m`**: Numeric vector of length 2: means for groups 1 and 2
- **`sd`**: Numeric vector of length 2: standard deviations for groups 1 and 2
- **`n`**: Numeric vector of length 2: sample sizes for groups 1 and 2

### Value

A 2-row matrix identical to the output of ci.mean2

### Examples

```r
ci.mean2.vec(.05, c(15.4, 10.3), c(2.67, 2.15), c(30, 20))
```

