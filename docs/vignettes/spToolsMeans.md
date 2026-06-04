# [`spTools`](https://github.com/cwendorf/spTools/)

## Confidence Intervals for Means

This vignette compares the use of `statpsych` and `spTools` functions to
compute confidence intervals for means and differences between means.

- [Confidence Intervals for a Set of Means](#confidence-intervals-for-a-set-of-means)
- [Confidence Interval for Independent Groups Mean Difference](#confidence-interval-for-independent-groups-mean-difference)
- [Confidence Interval for Paired Samples Mean Difference](#confidence-interval-for-paired-samples-mean-difference)

------------------------------------------------------------------------

### Confidence Intervals for a Set of Means

Compare separate `statpsych` calls with one vectorized `spTools` call.

``` r
# statpsych version
ci.mean(alpha = .05, m = 5.2, sd = 1.1, n = 30)
```

     Estimate        SE       LL       UL
          5.2 0.2008316 4.789253 5.610747

``` r
ci.mean(alpha = .05, m = 6.1, sd = 1.3, n = 28)
```

     Estimate        SE       LL       UL
          6.1 0.2456769 5.595913 6.604087

``` r
# spTools version
ci.mean.vec(alpha = .05, m = c(5.2, 6.1), sd = c(1.1, 1.3), n = c(30, 28))
```

            Estimate        SE df       LL       UL
    Group_1      5.2 0.2008316 29 4.789253 5.610747
    Group_2      6.1 0.2456769 27 5.595913 6.604087

### Confidence Interval for Independent Groups Mean Difference

Compare independent-group mean difference output between packages.

``` r
# statpsych version
ci.mean2(alpha = .05, m1 = 15.4, m2 = 10.3, sd1 = 2.67, sd2 = 2.15, n1 = 30, n2 = 20)
```

                                 Estimate        SE      t    df p       LL
    Equal Variances Assumed:          5.1 0.7151214 7.1317 48.00 0 3.662152
    Equal Variances Not Assumed:      5.1 0.6846568 7.4490 46.17 0 3.721994
                                       UL
    Equal Variances Assumed:     6.537848
    Equal Variances Not Assumed: 6.478006

``` r
# spTools version
ci.mean2.vec(alpha = .05, m = c(15.4, 10.3), sd = c(2.67, 2.15), n = c(30, 20))
```

                                 Estimate        SE      t    df p       LL
    Equal Variances Assumed:          5.1 0.7151214 7.1317 48.00 0 3.662152
    Equal Variances Not Assumed:      5.1 0.6846568 7.4490 46.17 0 3.721994
                                       UL
    Equal Variances Assumed:     6.537848
    Equal Variances Not Assumed: 6.478006

Compare standardized mean difference output for independent groups.

``` r
# statpsych version
ci.stdmean2(alpha = .05, m1 = 35.1, m2 = 26.7, sd1 = 7.32, sd2 = 6.98, n1 = 30, n2 = 30)
```

                             Estimate adj Estimate      SE     LL     UL
    Unweighted standardizer:   1.1745       1.1592 0.28440 0.6171 1.7319
    Weighted standardizer:     1.1745       1.1592 0.28028 0.6251 1.7238
    Group 1 standardizer:      1.1475       1.1176 0.29756 0.5643 1.7307
    Group 2 standardizer:      1.2034       1.1720 0.31205 0.5918 1.8151

``` r
# spTools version
ci.stdmean2.vec(alpha = .05, m = c(35.1, 26.7), sd = c(7.32, 6.98), n = c(30, 30))
```

                             Estimate adj Estimate      SE     LL     UL
    Unweighted standardizer:   1.1745       1.1592 0.28440 0.6171 1.7319
    Weighted standardizer:     1.1745       1.1592 0.28028 0.6251 1.7238
    Group 1 standardizer:      1.1475       1.1176 0.29756 0.5643 1.7307
    Group 2 standardizer:      1.2034       1.1720 0.31205 0.5918 1.8151

### Confidence Interval for Paired Samples Mean Difference

Compare paired-sample mean difference output between packages.

``` r
# statpsych version
ci.mean.ps(alpha = .05, m1 = 58.2, m2 = 51.4, sd1 = 7.43, sd2 = 8.92, cor = .537, n = 30)
```

     Estimate       SE      t df     p       LL       UL
          6.8 1.455922 4.6706 29 6e-05 3.822304 9.777696

``` r
# spTools version
ci.mean.ps.vec(alpha = .05, m = c(58.2, 51.4), sd = c(7.43, 8.92), cor = .537, n = 30)
```

     Estimate       SE      t df     p       LL       UL
          6.8 1.455922 4.6706 29 6e-05 3.822304 9.777696

Compare standardized paired-sample effect size output.

``` r
# statpsych version
ci.stdmean.ps(alpha = .05, m1 = 110.4, m2 = 102.1, sd1 = 15.3, sd2 = 14.6, cor = .75, n = 25)
```

                                Estimate adj Estimate      SE     LL     UL
    Unweighted standardizer:      0.5550       0.5433 0.16099 0.2395 0.8706
    Measurement 1 standardizer:   0.5425       0.5254 0.16155 0.2259 0.8591
    Measurement 2 standardizer:   0.5685       0.5505 0.16930 0.2367 0.9003

``` r
# spTools version
ci.stdmean.ps.vec(alpha = .05, m = c(110.4, 102.1), sd = c(15.3, 14.6), cor = .75, n = 25)
```

                                Estimate adj Estimate      SE     LL     UL
    Unweighted standardizer:      0.5550       0.5433 0.16099 0.2395 0.8706
    Measurement 1 standardizer:   0.5425       0.5254 0.16155 0.2259 0.8591
    Measurement 2 standardizer:   0.5685       0.5505 0.16930 0.2367 0.9003
