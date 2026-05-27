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
out <- ci.add.test(ci.mean.ps.vec(.05, c(58.2, 51.4), c(7.43, 8.92), .537, 30), n = 30)
out |>
  ci.drop.test()
```
