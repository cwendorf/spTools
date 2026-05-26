# [`spTools`](https://github.com/cwendorf/spTools/)

## Reference

This section is the reference for `spTools`. Each page documents one exported function, including usage, arguments, return values, and examples.

### Vectorized Versions

These functions are versions of existing `statpsych` functions but use vectorized input for parameters.

- [ci.mean.vec](./ci.mean.vec.md): Confidence Intervals for a Set of Means.
- [ci.mean.ps.vec](./ci.mean.ps.vec.md): Confidence Interval for Paired Samples Mean Difference.
- [ci.mean2.vec](./ci.mean2.vec.md): Confidence Interval for Independent Groups Mean Difference.
- [ci.cor.vec](./ci.cor.vec.md): Confidence Intervals for a Set of Pearson (or Partial) Correlations.
- [ci.cor2.vec](./ci.cor2.vec.md): Confidence Interval for Independent Groups Pearson Correlation Difference.
- [ci.spear.vec](./ci.spear.vec.md): Confidence Intervals for a Set of Spearman Correlations.
- [ci.prop.vec](./ci.prop.vec.md): Confidence Intervals for a Set of One-Sample Proportions.
- [ci.prop2.vec](./ci.prop2.vec.md): Confidence Interval for Independent Groups Proportion Difference.
- [ci.stdmean.ps.vec](./ci.stdmean.ps.vec.md): Confidence Interval for a Paired Samples Standardized Mean Difference.
- [ci.stdmean2.vec](./ci.stdmean2.vec.md): Confidence Interval for a Independent Groups Standardized Mean Difference.

### Comparison Wrappers

These functions provide one-table wrapper workflows that combine group-level confidence intervals with comparison results.

- [ci.mean2.compare](./ci.mean2.compare.md): Confidence Intervals to Compare Two Independent Means and Their Difference.
- [ci.mean.ps.compare](./ci.mean.ps.compare.md): Confidence Intervals to Compare Two Paired Means and Their Difference.
- [ci.cor.compare](./ci.cor.compare.md): Confidence Intervals to Compare Two Correlations and Their Difference.
- [ci.cor.dep.compare](./ci.cor.dep.compare.md): Confidence Intervals to Compare Two Dependent Correlations and Their Difference.
- [ci.prop.compare](./ci.prop.compare.md): Confidence Intervals to Compare Two Proportions.
- [ci.prop.ps.compare](./ci.prop.ps.compare.md): Confidence Intervals to Compare Two Paired Proportions and Their Difference.

### Additional Functions

These functions provide additional analyses missing from `statpsych`.

- [ci.lc.mean.ws](./ci.lc.mean.ws.md): Confidence Interval for a Within-Subjects Linear Contrast of Means.
- [ci.2x2.prop.ws](./ci.2x2.prop.ws.md): Confidence Intervals for 2x2 Within-Subjects Proportion Effects.

### Formatters

These functions help standardize or reformat output for reporting.

- [ci.add.test](./ci.add.test.md): Add Test Statistics to Output.
- [ci.drop.test](./ci.drop.test.md): Remove Test Statistics from Output.
- [ci.tukey.reformat](./ci.tukey.reformat.md): Clean and Reformat Tukey Confidence Interval Output.
