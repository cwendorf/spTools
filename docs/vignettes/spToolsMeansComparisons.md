# [`spTools`](https://github.com/cwendorf/spTools/)

## Confidence Intervals for Mean Comparisons

This vignette compares the use of `statpsych` and `spTools` functions to
compute confidence intervals for mean comparisons.

- [Independent Group Mean Comparisons](#independent-group-mean-comparisons)
- [Paired Samples Mean Comparisons](#paired-samples-mean-comparisons)
- [Between-Subjects Mean Contrasts](#between-subjects-mean-contrasts)
- [Within-Subjects Mean Contrasts](#within-subjects-mean-contrasts)

------------------------------------------------------------------------

### Independent Group Mean Comparisons

Use `statpsych` functions to build group and comparison rows manually.

``` r
ci.mean.vec(alpha = .05, m = c(5.2, 6.1), sd = c(1.1, 1.3), n = c(30, 28)) -> groups
ci.mean2.vec(alpha = .05, m = c(6.1, 5.2), sd = c(1.3, 1.1), n = c(28, 30))[1, c(1, 2, 4, 6, 7)] -> compare
results <- rbind(groups,compare)
rownames(results) <- c("Group 1", "Group 2", "Comparison")
results
```

               Estimate        SE df        LL       UL
    Group 1         5.2 0.2008316 29 4.7892532 5.610747
    Group 2         6.1 0.2456769 27 5.5959126 6.604087
    Comparison      0.9 0.3154793 56 0.2680189 1.531981

Or use the `spTools` helper function to produce the same
independent-group comparison table.

``` r
ci.mean2.compare(
  alpha = .05,
  m = c(5.2, 6.1),
  sd = c(1.1, 1.3),
  n = c(30, 28)
)
```

               Estimate        SE df        LL       UL
    Group_1         5.2 0.2008316 29 4.7892532 5.610747
    Group_2         6.1 0.2456769 27 5.5959126 6.604087
    Comparison      0.9 0.3154793 56 0.2680189 1.531981

### Paired Samples Mean Comparisons

Use `statpsych` functions to build paired-sample group and comparison
rows manually.

``` r
ci.mean.vec(alpha = .05, m = c(58.2, 51.4), sd = c(7.43, 8.92), n = c(30, 30)) -> groups
ci.mean.ps.vec(alpha = .05, m = c(58.2, 51.4), sd = c(7.43, 8.92), cor = .537, n = 30)[1, c(1, 2, 4, 6, 7)] -> compare
results <- rbind(groups, compare)
rownames(results) <- c("Measure 1", "Measure 2", "Comparison")
results
```

               Estimate       SE df        LL        UL
    Measure 1      58.2 1.356526 29 55.425592 60.974408
    Measure 2      51.4 1.628562 29 48.069217 54.730783
    Comparison      6.8 1.455922 29  3.822304  9.777696

Or use the `spTools` helper function to produce the same paired-sample
comparison table.

``` r
ci.mean.ps.compare(
  alpha = .05,
  m = c(58.2, 51.4),
  sd = c(7.43, 8.92),
  cor = .537,
  n = 30
)
```

               Estimate       SE df        LL        UL
    Measure 1      58.2 1.356526 29 55.425592 60.974408
    Measure 2      51.4 1.628562 29 48.069217 54.730783
    Comparison      6.8 1.455922 29  3.822304  9.777696

### Between-Subjects Mean Contrasts

Use `statpsych` functions to build two contrasts and their difference
manually for independent groups. The first contrast is the grand mean;
the second identifies Level 1 specifically. Their difference captures
how Level 1 deviates from the grand mean.

``` r
q1 <- c(1/3, 1/3, 1/3)
q2 <- c(1, 0, 0)
ci.lc.mean.bs(alpha = .05, m = c(8, 11, 12), sd = c(1.414, 2.211, 2.449), n = c(10, 10, 10), v = q1)[1, c("Estimate", "SE", "df", "LL", "UL")] -> c1
ci.lc.mean.bs(alpha = .05, m = c(8, 11, 12), sd = c(1.414, 2.211, 2.449), n = c(10, 10, 10), v = q2)[1, c("Estimate", "SE", "df", "LL", "UL")] -> c2
ci.lc.mean.bs(alpha = .05, m = c(8, 11, 12), sd = c(1.414, 2.211, 2.449), n = c(10, 10, 10), v = q2 - q1)[1, c("Estimate", "SE", "df", "LL", "UL")] -> diff
results <- rbind(c1, c2, diff)
rownames(results) <- c("Grand Mean", "L1 Only", "Difference")
results
```

                Estimate        SE df        LL        UL
    Grand Mean 10.333333 0.3783813 27  9.556959 11.109708
    L1 Only     8.000000 0.6553757 27  6.655280  9.344720
    Difference -2.333333 0.5351120 27 -3.431292 -1.235374

Or use the `spTools` helper function to produce the same
between-subjects complex contrast table.

``` r
ci.lc.mean.bs.complex(
  alpha = .05,
  m = c(8, 11, 12),
  sd = c(1.414, 2.211, 2.449),
  n = c(10, 10, 10),
  q1 = c(1/3, 1/3, 1/3),
  q2 = c(1, 0, 0),
  labels = c("Grand Mean", "L1 Only")
)
```

                Estimate        SE df        LL        UL
    Grand Mean 10.333333 0.3783813 27  9.556959 11.109708
    L1 Only     8.000000 0.6553757 27  6.655280  9.344720
    Difference -2.333333 0.5351120 27 -3.431292 -1.235374

### Within-Subjects Mean Contrasts

Use `statpsych` functions to build two contrasts and their difference
manually for repeated measures. The first contrast is the grand mean;
the second identifies Level 1 specifically. Their difference captures
how Level 1 deviates from the grand mean.

``` r
R <- matrix(c(1, .7, .7, .7, 1, .7, .7, .7, 1), 3, 3)
q1 <- c(1/3, 1/3, 1/3)
q2 <- c(1, 0, 0)
ci.lc.mean.ws(alpha = .05, m = c(8, 11, 12), s = c(1.414, 2.211, 2.449), R = R, n = 10, q = q1) -> c1
ci.lc.mean.ws(alpha = .05, m = c(8, 11, 12), s = c(1.414, 2.211, 2.449), R = R, n = 10, q = q2) -> c2
ci.lc.mean.ws(alpha = .05, m = c(8, 11, 12), s = c(1.414, 2.211, 2.449), R = R, n = 10, q = q2 - q1) -> diff
results <- rbind(c1, c2, diff)
rownames(results) <- c("Grand Mean", "L1 Only", "Difference")
results
```

                Estimate        SE df        LL        UL
    Grand Mean 10.333333 0.5743701  9  9.034018 11.632649
    L1 Only     8.000000 0.4471461  9  6.988485  9.011515
    Difference -2.333333 0.2984119  9 -3.008388 -1.658279

Or use the `spTools` helper function to produce the same within-subjects
complex contrast table.

``` r
ci.lc.mean.ws.complex(
  alpha = .05,
  m = c(8, 11, 12),
  s = c(1.414, 2.211, 2.449),
  R = matrix(c(1, .7, .7, .7, 1, .7, .7, .7, 1), 3, 3),
  n = 10,
  q1 = c(1/3, 1/3, 1/3),
  q2 = c(1, 0, 0),
  labels = c("Grand Mean", "L1 Only")
)
```

                Estimate        SE df        LL        UL
    Grand Mean 10.333333 0.5743701  9  9.034018 11.632649
    L1 Only     8.000000 0.4471461  9  6.988485  9.011515
    Difference -2.333333 0.2984119  9 -3.008388 -1.658279
