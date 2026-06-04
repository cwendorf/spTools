# [`spTools`](https://github.com/cwendorf/spTools/)

## Confidence Intervals for Proportion Comparisons

This vignette compares the use of `statpsych` and `spTools` functions to
compute confidence intervals for proportion comparisons.

- [Independent Group Proportion Comparisons](#independent-group-proportion-comparisons)
- [Paired Proportion Comparisons](#paired-proportion-comparisons)

------------------------------------------------------------------------

### Independent Group Proportion Comparisons

Use `statpsych` functions to build group and comparison rows manually
for the proportion difference.

``` r
ci.prop.vec(alpha = .05, f = c(57, 15), n = c(100, 100)) -> groups
ci.prop2.vec(alpha = .05, f = c(57, 15), n = c(100, 100))[1, c("Estimate", "SE", "LL", "UL")] -> compare
results <- rbind(groups, compare)
rownames(results) <- c("Group 1", "Group 2", "Difference")
results
```

                Estimate         SE         LL        UL
    Group 1    0.5673077 0.04858277 0.47208722 0.6625282
    Group 2    0.1634615 0.03626052 0.09239222 0.2345309
    Difference 0.4117647 0.06083948 0.29252152 0.5310079

Or use the `spTools` helper function to produce the same difference
table.

``` r
ci.prop.compare(
  alpha = .05,
  f = c(57, 15),
  n = c(100, 100)
)
```

                Estimate         SE         LL        UL
    Group 1    0.5673077 0.04858277 0.47208722 0.6625282
    Group 2    0.1634615 0.03626052 0.09239222 0.2345309
    Difference 0.4117647 0.06083948 0.29252152 0.5310079

### Paired Proportion Comparisons

Use `statpsych` functions to build measure and comparison rows manually
for paired proportions.

``` r
f00 <- 12
f01 <- 4
f10 <- 26
f11 <- 6

n <- f00 + f01 + f10 + f11
f <- c(f10 + f11, f01 + f11)

ci.prop.vec(alpha = .05, f = f, n = c(n, n)) -> groups
ci.prop.ps(alpha = .05, f00 = f00, f01 = f01, f10 = f10, f11 = f11)[1, c("Estimate", "SE", "LL", "UL")] -> compare
results <- rbind(groups, compare)
rownames(results) <- c("Measure 1", "Measure 2", "Comparison")
results
```

                Estimate         SE        LL        UL
    Measure 1  0.6538462 0.06597368 0.5245401 0.7831522
    Measure 2  0.2307692 0.05842727 0.1162539 0.3452846
    Comparison 0.4400000 0.09448809 0.2548067 0.6251933

Or use the `spTools` helper function to produce the same
paired-proportion comparison table.

``` r
results <- ci.prop.ps.compare(
  alpha = .05,
  f00 = 12,
  f01 = 4,
  f10 = 26,
  f11 = 6
)
results
```

                Estimate         SE        LL        UL
    Measure 1  0.6538462 0.06597368 0.5245401 0.7831522
    Measure 2  0.2307692 0.05842727 0.1162539 0.3452846
    Comparison 0.4400000 0.09448809 0.2548067 0.6251933
