
# `spTools` 

## Additional Functions for the `statpsych` Package

[![minimal R version](https://img.shields.io/badge/R%3E%3D-3.6.2-6666ff.svg)](https://cran.r-project.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)

### Overview

`spTools` is a small collection of functions that add to the array of tools in Doug Bonett's `statpsych` package. Some functions provide calculations not present in `statpsych`, whereas other functions offer vectorized versions of `statpsych` functions. Other functions attempt to further standardize `statpsych` output. 

### Installation

This package is not currently on CRAN, but can be installed and loaded using these R commands

``` r
if (!require(remotes)) install.packages("remotes")
remotes::install_github("cwendorf/spTools")
library(spTools)
```

If you do not wish a full install, the latest functions can be made available using these R commands:

```r
source("http://raw.githubusercontent.com/cwendorf/spTools/main/source-spTools.R")
```
### Contact Me

- GitHub Issues: [https://github.com/cwendorf/spTools/issues](https://github.com/cwendorf/spTools/issues) 
- Author Email: [cwendorf@uwsp.edu](mailto:cwendorf@uwsp.edu)
- Author Homepage: [https://github.com/cwendorf](https://github.com/cwendorf)

### Citation

Wendorf, C.A. (2025). *spTools: Additional functions for the statpsych package* [R Package]. [https://github.com/cwendorf/spTools](https://github.com/cwendorf/spTools)
