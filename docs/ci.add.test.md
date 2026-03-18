# [`spTools`](https://github.com/cwendorf/spTools/)

## Add test statistic, df, and p-value to CI output

**Aliases:**

- `ci.add.test`

### Description

Add test statistic, df, and p-value to CI output

### Usage

```r
ci.add.test(results, df = NULL, n = NULL, null_value = 0, conf_level = 0.95)
```

### Arguments

- **`results`**: 1-row matrix output from Bonett CI functions (with Estimate, SE, LL, UL)
- **`df`**: Optional degrees of freedom (if NULL, try to infer from n)
- **`n`**: Optional sample size to infer df if df is missing
- **`null_value`**: Hypothesized null value for test statistic (default 0)
- **`conf_level`**: Confidence level used to infer df from the CI width when df is not supplied

### Value

1-row matrix with columns:
Estimate, SE, t, df, p, LL, UL