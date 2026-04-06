# [`spTools`](https://github.com/cwendorf/spTools/)

## Reference

This section is the reference for `spTools`. Each page documents one exported function, including usage, arguments, return values, and examples.

### Vectorized Versions

These functions are wrappers for existing `statpsych` functions but use vectorized input for multiple means and standard deviations.

- [ci.mean.vec](./ci.mean.vec.md): Confidence Intervals for a Set of Means.
- [ci.mean.ps.vec](./ci.mean.ps.vec.md): Confidence Interval for Paired Samples Mean Difference.
- [ci.mean2.vec](./ci.mean2.vec.md): Confidence Interval for Independent Groups Mean Difference.
- [ci.stdmean.ps.vec](./ci.stdmean.ps.vec.md): Confidence Interval for a Paired Samples Standardized Mean Difference.
- [ci.stdmean2.vec](./ci.stdmean2.vec.md): Confidence Interval for a Independent Groups Standardized Mean Difference.

### Additional Functions

The functions provide additional analyses or additional formatting not present in other `statpsych` functions.

- [ci.lc.mean.ws](./ci.lc.mean.ws.md): Confidence Interval for a Within-Subjects Linear Contrast of Means.
- [ci.mean2.compare](./ci.mean2.compare.md): Confidence Intervals to Compare Two Independent Means and Their Difference.
- [ci.add.test](./ci.add.test.md): Add Test Statistics to Output.
- [ci.tukey.reformat](./ci.tukey.reformat.md): Clean and Reformat Tukey Confidence Interval Output.
