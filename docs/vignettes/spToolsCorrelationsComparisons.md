# [`spTools`](https://github.com/cwendorf/spTools/)

## Confidence Intervals for Correlation Comparisons

This vignette compares the use of `statpsych` and `spTools` functions to
compute confidence intervals for correlation comparisons.

- [Independent Group Correlation Comparisons](#independent-group-correlation-comparisons)
- [Dependent Correlation Comparisons](#dependent-correlation-comparisons)

------------------------------------------------------------------------

### Independent Group Correlation Comparisons

Use `statpsych` functions to build group and comparison rows manually.

``` r
ci.cor.vec(alpha = .05, cor = c(.64, .31), s = c(0, 0), n = c(200, 200)) -> groups
ci.cor2.vec(alpha = .05, cor = c(.64, .31), n = c(200, 200))[1, c("Estimate", "SE", "LL", "UL")] -> compare
results <- rbind(groups, compare)
rownames(results) <- c("Group 1", "Group 2", "Comparison")
results
```

               Estimate      SE     LL     UL
    Group 1        0.64 0.04185 0.5490 0.7144
    Group 2        0.31 0.06408 0.1782 0.4296
    Comparison     0.33 0.07692 0.1797 0.4814

Or use the `spTools` helper function to produce the same
independent-group comparison table.

``` r
ci.cor.compare(
  alpha = .05,
  cor = c(.64, .31),
  s = c(0, 0),
  n = c(200, 200)
)
```

               Estimate      SE     LL     UL
    Group 1        0.64 0.04185 0.5490 0.7144
    Group 2        0.31 0.06408 0.1782 0.4296
    Comparison     0.33 0.07692 0.1797 0.4814

### Dependent Correlation Comparisons

Use `statpsych` functions to build measure and comparison rows manually
for the dependent-correlation case.

``` r
ci.cor.vec(alpha = .05, cor = c(.396, .179), s = c(0, 0), n = c(166, 166)) -> groups
ci.cor.dep.vec(alpha = .05, cor = c(.396, .179), cor12 = .088, n = 166)[1, c("Estimate", "SE", "LL", "UL")] -> compare
results <- rbind(groups, compare)
rownames(results) <- c("Measure 1", "Measure 2", "Comparison")
results
```

               Estimate      SE     LL     UL
    Measure 1     0.396 0.06564 0.2582 0.5162
    Measure 2     0.179 0.07536 0.0269 0.3220
    Comparison    0.217 0.10270 0.0132 0.4158

Or use the `spTools` helper function to produce the same
dependent-correlation comparison table.

``` r
ci.cor.dep.compare(
  alpha = .05,
  cor = c(.396, .179),
  cor12 = .088,
  n = 166,
  s = 0
)
```

               Estimate      SE     LL     UL
    Measure 1     0.396 0.06564 0.2582 0.5162
    Measure 2     0.179 0.07536 0.0269 0.3220
    Comparison    0.217 0.10270 0.0132 0.4158
