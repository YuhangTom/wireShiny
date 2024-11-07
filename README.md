
<!-- README.md is generated from README.Rmd. Please edit that file -->

# wireShiny <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Last-changedate](https://img.shields.io/badge/last%20change-2024--11--06-yellowgreen.svg)](https://github.com/YuhangTom/wireShiny/commits/main)
[![CRAN
status](https://www.r-pkg.org/badges/version/wireShiny.png)](https://CRAN.R-project.org/package=wireShiny)
[![R-CMD-check](https://github.com/YuhangTom/wireShiny/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/YuhangTom/wireShiny/actions/workflows/R-CMD-check.yaml)
<!-- !!!Add with use_github_action!!! [![Codecov test coverage](https://codecov.io/gh/YuhangTom/wireShiny/branch/main/graph/badge.svg)](https://app.codecov.io/gh/YuhangTom/wireShiny?branch=main) -->
<!-- badges: end -->

The goal of wireShiny is to run the Shiny app based on the
functionalities of [`wire`](https://yuhangtom.github.io/wire/).

## Installation

You can install the development version of wireShiny from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("YuhangTom/wireShiny")
```

## Example

Use the following code to run the Shiny app.

``` r
library(wireShiny)

run_wireShiny()
```

A Shiny app like the one below will open in your default browser.

![](man/figures/wireShiny_screenshot.png)
