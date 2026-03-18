# [`spTools`](https://github.com/cwendorf/spTools/)

## Clean and Reformat Tukey CI Output

**Aliases:**

- `sp.tukey`

### Description

This function takes the output from the Tukey confidence interval function (ci.tukey)
and reformats it by removing the pair columns and assigning more descriptive row names using the pair indices.
Row names will be formatted as "1 v 2", "1 v 3", etc.

### Usage

```r
sp.tukey(ci_out)
```

### Arguments

- **`ci_out`**: A matrix or data frame produced by ci.tukey, where the first two columns represent pairwise comparisons.

### Value

A cleaned matrix without the first two pair columns. The rows will be named according to the pairs compared,
using the format "1 v 2", "1 v 3", etc.

### Examples

```r
out <- ci.tukey(alpha = 0.05, m = c(5, 6, 7), sd = 2, n = 10)
sp.tukey(out)
```

