
<!-- README.md is generated from README.Rmd. Please edit that file -->

# wireShiny

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Last-changedate](https://img.shields.io/badge/last%20change-2023--12--29-yellowgreen.svg)](https://github.com/YuhangTom/wireShiny/commits/main)
[![CRAN
status](https://www.r-pkg.org/badges/version/wireShiny.png)](https://CRAN.R-project.org/package=wireShiny)
[![R-CMD-check](https://github.com/YuhangTom/wireShiny/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/YuhangTom/wireShiny/actions/workflows/R-CMD-check.yaml)
<!-- !!!Add with use_github_action!!! [![Codecov test coverage](https://codecov.io/gh/YuhangTom/wireShiny/branch/main/graph/badge.svg)](https://app.codecov.io/gh/YuhangTom/wireShiny?branch=main) -->
<!-- badges: end -->

The goal of wireShiny is to …

## Installation

You can install the development version of wireShiny from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("YuhangTom/wireShiny")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(wireShiny)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.

You can also embed plots, for example:

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
