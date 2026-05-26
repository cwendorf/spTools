# [`spTools`](https://github.com/cwendorf/spTools/)

## Confidence Intervals for a Set of Spearman Correlations

### Description

A wrapper function for `ci.spear` that accepts lists of paired vectors.

### Usage

```r
ci.spear.vec(alpha, y, x)
```

### Arguments

- **`alpha`**: Alpha level for 1-alpha confidence.
- **`y`**: List of numeric vectors for y scores.
- **`x`**: List of numeric vectors for x scores (paired with `y`).

### Value

A matrix with one row per y/x pair and columns from `ci.spear`: `Estimate`, `SE`, `LL`, and `UL`.

### Examples

```r
y_list <- list(
  c(21, 4, 9, 12, 35, 18, 10, 22, 24, 1, 6, 8, 13, 16, 19),
  c(5, 7, 9, 10, 13, 15, 18, 20, 22, 25)
)
x_list <- list(
  c(67, 28, 30, 28, 52, 40, 25, 37, 44, 10, 14, 20, 28, 40, 51),
  c(3, 5, 8, 8, 11, 12, 16, 18, 19, 23)
)
ci.spear.vec(.05, y = y_list, x = x_list)
```
