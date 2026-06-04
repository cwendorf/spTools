# [`spTools`](https://github.com/cwendorf/spTools/)

## Clean and Reformat Tukey Confidence Interval Output

### Description

Wrapper around `ci.tukey` that computes Tukey pairwise confidence intervals,
then reformats output by removing pair-index columns and assigning readable
row names such as "1 v 2".

### Usage

```r
ci.tukey.reformat(alpha, m, sd, n, ...)
```

### Arguments

- **`alpha`**: Type I error rate used by `ci.tukey`.
- **`m`**: Vector of group means.
- **`sd`**: Vector of group standard deviations.
- **`n`**: Vector of group sample sizes.
- **`...`**: Additional arguments passed through to `ci.tukey`.

### Value

A reformatted matrix with row names based on comparison pairs and pair-index columns removed.

### Examples

```r
ci.tukey.reformat(alpha = 0.05, m = c(5, 6, 7), sd = c(2, 2, 2), n = c(10, 10, 10))
```