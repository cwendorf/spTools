# [`spTools`](https://github.com/cwendorf/spTools/)

## Compare Two Means with Group and Difference Confidence Intervals

**Aliases:**

- `ci.mean2.compare`

### Description

Returns confidence intervals for each group mean and a single comparison row
for the two-group mean difference.

### Usage

```r
ci.mean2.compare(alpha, m, sd, n)
```

### Arguments

- **`alpha`**: Numeric scalar. Significance level (e.g., 0.05 for 95% CI).
- **`m`**: Numeric vector of length 2. Means for groups 1 and 2.
- **`sd`**: Numeric vector of length 2. Standard deviations for groups 1 and 2.
- **`n`**: Numeric vector of length 2. Sample sizes for groups 1 and 2.

### Details

The comparison row is computed from `ci.mean2.vec` using reversed group order,
then reduced to columns 1, 2, 4, 6, and 7 to align with the group output.

### Value

A 3-row matrix with rows for group 1, group 2, and comparison.
Group rows contain output from `ci.mean.vec`. The comparison row contains
selected columns from `ci.mean2.vec` corresponding to the two-group
difference summary.

### Examples

```r
ci.mean2.compare(
  alpha = .05,
  m = c(5.2, 6.1),
  sd = c(1.1, 1.3),
  n = c(30, 28)
)
```
