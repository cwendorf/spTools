# [`spTools`](https://github.com/cwendorf/spTools/)

## Confidence Intervals for 2x2 Within-Subjects Proportion Effects

### Description

Computes confidence intervals and tests for the AB interaction effect, main effect of A, main effect of B, simple main effects of A, and simple main effects of B in a 2x2 within-subjects design with dichotomous (0/1) responses.

### Usage

```r
ci.2x2.prop.ws(alpha, y11, y12, y21, y22)
```

### Arguments

- **`alpha`**: Alpha level for 1 - alpha confidence intervals.
- **`y11`**: Binary vector of responses at level 1 of A and level 1 of B.
- **`y12`**: Binary vector of responses at level 1 of A and level 2 of B.
- **`y21`**: Binary vector of responses at level 2 of A and level 1 of B.
- **`y22`**: Binary vector of responses at level 2 of A and level 2 of B.

### Value

A 7-row matrix (one row per effect) with columns:

- `Estimate`
- `SE`
- `z`
- `p`
- `LL`
- `UL`

### Examples

```r
y11 <- c(1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1)
y12 <- c(1, 0, 0, 1, 0, 1, 1, 0, 0, 0, 1, 1)
y21 <- c(1, 1, 0, 0, 0, 1, 1, 0, 1, 0, 1, 0)
y22 <- c(0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0)
ci.2x2.prop.ws(.05, y11, y12, y21, y22)
```
