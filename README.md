# [`spTools`](https://github.com/cwendorf/spTools/)

## Additional Functions for the `statpsych` Package

[![Minimal R Version](https://img.shields.io/badge/R%3E%3D-3.6.2-blue?style=for-the-badge)](https://cran.r-project.org/)
[![GitHub Release](https://img.shields.io/github/v/release/cwendorf/spTools?style=for-the-badge)](https://github.com/cwendorf/spTools/releases/latest)
[![Project Status](https://img.shields.io/badge/Status-Active-success?style=for-the-badge)](https://www.repostatus.org/#active)

### Overview

`spTools` is a small collection of functions that add to the array of tools in Doug Bonett's `statpsych` package. Some functions provide calculations not present in `statpsych`, whereas other functions offer vector input versions of `statpsych` functions. Still other functions further standardize `statpsych` output.

### Installation

This package is not currently available on CRAN. It can be installed directly from GitHub:

``` r
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("cwendorf/spTools")
library(spTools)
```

If you prefer not to install the package, you can load the current functions directly:

```r
source("https://raw.githubusercontent.com/cwendorf/spTools/main/source-spTools.R")
```

### Usage

This package includes the following documentation:

- [Introduction](./docs/README.md): Overview of the package, listing of associated packages, and links to all resources.
- [Reference](./docs/man/README.md): Documentation for all exported functions, including usage, arguments, return values, and examples.
- [Articles](./docs/vignettes/README.md): Examples demonstrating how to use `spTools` in conjunction with `statpsych`.

### Citation

Wendorf, C.A. (2026). *spTools: Additional functions for the statpsych package* [R Package]. [https://github.com/cwendorf/spTools](https://github.com/cwendorf/spTools)
