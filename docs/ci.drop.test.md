# [`spTools`](https://github.com/cwendorf/spTools/)

## Remove Hypothesis Test Statistics from Confidence Interval Output

### Description

Removes hypothesis test statistics (`t` and `p`) from matrix or data-frame
output. This is useful when test statistics were added for computation or
checking but should be hidden in final displayed results.

### Usage

```r
ci.drop.test(x)
```

### Arguments

- **`x`**: A matrix or data frame.

### Value

The same object class with `t` and `p` columns removed when present.
If neither column exists, input is returned unchanged.

### Examples

```r
results <- matrix(
  c(10.5, 1.2, 8.1, 12.9),
  nrow = 1,
  dimnames = list(NULL, c("Estimate", "SE", "LL", "UL"))
)
out <- ci.add.test(results, n = 30)
ci.drop.test(out)
```
