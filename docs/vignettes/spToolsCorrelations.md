# [`spTools`](https://github.com/cwendorf/spTools/)

## Confidence Intervals for Correlations

This vignette compares the use of `statpsych` and `spTools` functions to
compute confidence intervals for correlations and differences between
correlations.

- [Confidence Intervals for a Set of Pearson (or Partial) Correlations](#confidence-intervals-for-a-set-of-pearson-(or-partial)-correlations)
- [Confidence Interval for Independent Groups Pearson Correlation Difference](#confidence-interval-for-independent-groups-pearson-correlation-difference)
- [Confidence Intervals for a Set of Spearman Correlations](#confidence-intervals-for-a-set-of-spearman-correlations)

------------------------------------------------------------------------

### Confidence Intervals for a Set of Pearson (or Partial) Correlations

Compare separate `statpsych` calls with one vectorized `spTools` call.

``` r
# statpsych version
ci.cor(alpha = .05, cor = .60, s = 0, n = 150)
```

     Estimate      SE    LL     UL
          0.6 0.05243 0.485 0.6925

``` r
ci.cor(alpha = .05, cor = .70, s = 1, n = 135)
```

     Estimate      SE     LL     UL
          0.7 0.04406 0.6002 0.7763

``` r
# spTools version
ci.cor.vec(alpha = .05, cor = c(.60, .70), s = c(0, 1), n = c(150, 135))
```

            Estimate      SE     LL     UL
    Group_1      0.6 0.05243 0.4850 0.6925
    Group_2      0.7 0.04406 0.6002 0.7763

### Confidence Interval for Independent Groups Pearson Correlation Difference

Compare two-group Pearson difference output between packages.

``` r
# statpsych version
ci.cor2(alpha = .05, cor1 = .64, cor2 = .31, n1 = 200, n2 = 200)
```

     Estimate      SE     LL     UL
         0.33 0.07692 0.1797 0.4814

``` r
# spTools version
ci.cor2.vec(alpha = .05, cor = c(.64, .31), n = c(200, 200))
```

     Estimate      SE     LL     UL
         0.33 0.07692 0.1797 0.4814

### Confidence Intervals for a Set of Spearman Correlations

Define two paired datasets.

``` r
y1 <- c(21, 4, 9, 12, 35, 18, 10, 22, 24, 1, 6, 8, 13, 16, 19)
x1 <- c(67, 28, 30, 28, 52, 40, 25, 37, 44, 10, 14, 20, 28, 40, 51)
y2 <- c(5, 7, 9, 10, 13, 15, 18, 20, 22, 25)
x2 <- c(3, 5, 8, 8, 11, 12, 16, 18, 19, 23)
```

Compare correlations from separate vs vectorized calls.

``` r
# statpsych version
ci.spear(alpha = .05, y = y1, x = x1)
```

     Estimate      SE     LL     UL
         0.87 0.08241 0.5841 0.9638

``` r
ci.spear(alpha = .05, y = y2, x = x2)
```

     Estimate     SE     LL     UL
        0.997 0.0028 0.9815 0.9995

``` r
# spTools version
ci.spear.vec(alpha = .05, y = list(y1, y2), x = list(x1, x2))
```

            Estimate      SE     LL     UL
    Group_1    0.870 0.08241 0.5841 0.9638
    Group_2    0.997 0.00280 0.9815 0.9995
