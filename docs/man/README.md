# [`spTools`](https://github.com/cwendorf/spTools/)

## Reference

This section is the reference for `spTools`. Each page documents one exported function, including usage, arguments, return values, and examples.

### Additional Functions

These functions provide additional analyses missing from `statpsych` but still consistent with the package.

- [ci.lc.mean.ws](./ci.lc.mean.ws.md): Confidence Interval for a Within-Subjects Linear Contrast of Means.
- [ci.2x2.prop.ws](./ci.2x2.prop.ws.md): Confidence Intervals for 2x2 Within-Subjects Proportion Effects.

### Vector Input Wrappers

These functions are wrappers for existing `statpsych` functions that instead use vector input for parameters.

- [ci.mean.vec](./ci.mean.vec.md): Confidence Intervals for a Set of Means.
- [ci.mean.ps.vec](./ci.mean.ps.vec.md): Confidence Interval for Paired Samples Mean Difference.
- [ci.mean2.vec](./ci.mean2.vec.md): Confidence Interval for Independent Groups Mean Difference.
- [ci.stdmean.ps.vec](./ci.stdmean.ps.vec.md): Confidence Interval for a Paired Samples Standardized Mean Difference.
- [ci.stdmean2.vec](./ci.stdmean2.vec.md): Confidence Interval for a Independent Groups Standardized Mean Difference.
- [ci.cor.vec](./ci.cor.vec.md): Confidence Intervals for a Set of Pearson (or Partial) Correlations.
- [ci.cor2.vec](./ci.cor2.vec.md): Confidence Interval for Independent Groups Pearson Correlation Difference.
- [ci.cor.dep.vec](./ci.cor.dep.vec.md): Confidence Interval for Dependent Pearson Correlation Difference.
- [ci.spear.vec](./ci.spear.vec.md): Confidence Intervals for a Set of Spearman Correlations.
- [ci.prop.vec](./ci.prop.vec.md): Confidence Intervals for a Set of One-Sample Proportions.
- [ci.prop2.vec](./ci.prop2.vec.md): Confidence Interval for Independent Groups Proportion Difference.

### Comparison Wrappers

These functions provide one-table wrapper workflows that combine other functions for the sake of comparisons and contrasts.

- [ci.mean2.compare](./ci.mean2.compare.md): Confidence Intervals for Two Independent Means and Their Difference.
- [ci.mean.ps.compare](./ci.mean.ps.compare.md): Confidence Intervals for Two Paired Means and Their Difference.
- [ci.lc.mean.bs.complex](./ci.lc.mean.bs.complex.md): Confidence Intervals for Two Between-Subjects Mean Contrasts and Their Difference.
- [ci.lc.mean.ws.complex](./ci.lc.mean.ws.complex.md): Confidence Intervals for Two Within-Subjects Mean Contrasts and Their Difference.
- [ci.cor.compare](./ci.cor.compare.md): Confidence Intervals for Two Correlations and Their Difference.
- [ci.cor.dep.compare](./ci.cor.dep.compare.md): Confidence Intervals for Two Dependent Correlations and Their Difference.
- [ci.prop.compare](./ci.prop.compare.md): Confidence Intervals for Two Proportions and Their Difference or Ratio.
- [ci.prop.ps.compare](./ci.prop.ps.compare.md): Confidence Intervals for Two Paired Proportions and Their Difference.

### Formatters

These functions help extend, standardize, or reformat output from other functions for the sake of reporting.

- [ci.add.test](./ci.add.test.md): Add Hypothesis Test Statistics to Confidence Interval Output.
- [ci.drop.test](./ci.drop.test.md): Remove Hypothesis Test Statistics from Confidence Interval Output.
- [ci.tukey.reformat](./ci.tukey.reformat.md): Clean and Reformat Tukey Confidence Interval Output.
