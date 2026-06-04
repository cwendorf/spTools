# [`spTools`](https://github.com/cwendorf/spTools/)

## Confidence Intervals for Proportions

This vignette compares the use of `statpsych` and `spTools` functions to
compute confidence intervals for proportions and differences between
proportions.

- [Confidence Intervals for a Set of One-Sample Proportions](#confidence-intervals-for-a-set-of-one-sample-proportions)
- [Confidence Interval for Independent Groups Proportion Difference](#confidence-interval-for-independent-groups-proportion-difference)

------------------------------------------------------------------------

### Confidence Intervals for a Set of One-Sample Proportions

Compare separate `statpsych` calls with one vectorized `spTools` call.

``` r
# statpsych version
ci.prop(alpha = .05, f = 120, n = 300)
```

                    Estimate         SE        LL        UL
    Adjusted Wald  0.4013158 0.02811287 0.3462156 0.4564160
    Wilson with cc 0.4000000 0.02828427 0.3445577 0.4580464
    Exact          0.4000000 0.02828427 0.3441290 0.4578664

``` r
ci.prop(alpha = .05, f = 95, n = 250)
```

                    Estimate         SE        LL        UL
    Adjusted Wald  0.3818898 0.03048492 0.3221404 0.4416391
    Wilson with cc 0.3800000 0.03069853 0.3201680 0.4435837
    Exact          0.3800000 0.03069853 0.3195769 0.4433050

``` r
# spTools version
ci.prop.vec(alpha = .05, f = c(120, 95), n = c(300, 250))
```

             Estimate         SE        LL        UL
    Group_1 0.4013158 0.02811287 0.3462156 0.4564160
    Group_2 0.3818898 0.03048492 0.3221404 0.4416391

### Confidence Interval for Independent Groups Proportion Difference

Compare independent-groups proportion difference output between
packages.

``` r
# statpsych version
ci.prop2(alpha = .05, f1 = 57, f2 = 15, n1 = 100, n2 = 100)
```

      Estimate         SE        LL        UL
     0.4117647 0.06083948 0.2925215 0.5310079

``` r
# spTools version
ci.prop2.vec(alpha = .05, f = c(57, 15), n = c(100, 100))
```

      Estimate         SE        LL        UL
     0.4117647 0.06083948 0.2925215 0.5310079
