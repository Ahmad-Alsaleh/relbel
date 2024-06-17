
<!-- README.md is generated from README.Rmd. Please edit that file -->

# relbel

<!-- badges: start -->

[![Conventional
Commits](https://img.shields.io/badge/Conventional%20Commits-1.0.0-%23FE5196?logo=conventionalcommits&logoColor=white)](https://www.conventionalcommits.org/)
[![License: GPL
v3](https://img.shields.io/badge/License-GPL_v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![R-CMD-check](https://github.com/Ahmad-Alsaleh/relbel/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Ahmad-Alsaleh/relbel/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/Ahmad-Alsaleh/relbel/branch/main/graph/badge.svg)](https://app.codecov.io/gh/Ahmad-Alsaleh/relbel?branch=main)
<!-- badges: end -->

`relbel` computes the Relative Belief Ratio, a k-sample Bayesian Test
for Mean.

## Installation

You can install the development version of relbel from
[GitHub](https://github.com/) with:

``` r
install.packages("devtools")
devtools::install_github("Ahmad-Alsaleh/relbel")
```

## Example

``` r
library(relbel)
initialization_function <- function(feature) {
  list(a = 0, b = 2, s_1 = 2, s_2 = 4)
}
repetition <- 5000
L <- 20
i_0 <- 1

dataset <- data.frame(
  x1 = c(-0.8606568, 1.4141698, 0.2235239, -1.1968656, 0.7677741),
  x2 = c(-0.318, -0.712, -1.320, -0.008, 0.048),
  y = c("A", "B", "A", "B", "B")
)

relbel(
  dataset[1:2], dataset["y"],
  initialization_function, repetition, L, i_0
)
#>              x1     x2
#> relbel   1.0840 1.0920
#> strength 0.6646 0.7742
```
