
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Multidimentional Top Scoring for Creativity Research

<!-- badges: start -->

[![codecov](https://codecov.io/gh/jakub-jedrusiak/mtscr/branch/master/graph/badge.svg?token=N3UGHFK5QN)](https://codecov.io/gh/jakub-jedrusiak/mtscr)
[![R-CMD-check](https://github.com/jakub-jedrusiak/mtscr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jakub-jedrusiak/mtscr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

This package includes an R adaptation of Multidimentional Top Scoring
method presented by Forthmann, Karwowski and Beaty
([2023](https://doi.org/10.1037/aca0000571)) adapted from public code in
the [OSF database](https://osf.io/7rgsp/). The code was adapted to use
the [tidyverse](https://www.tidyverse.org/) framework for greater
flexibility.

## Installation

You can install the development version of mtscr from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jakub-jedrusiak/mtscr")
```

The mtscr package is not on CRAN yet.

## Usage

Basic usage involves scoring participants’ responses to a divergent
thinking task. The package includes a sample dataset `mtscr_creativity`
with 4652 responses to the [Alternative Uses
Task](https://en.wikipedia.org/wiki/Alternative_uses_test) with semantic
distance scored. The dataset comes from the original paper (Forthmann,
Karwowski and Beaty, [2023](https://doi.org/10.1037/aca0000571)).

The main function is `mtscr_scores()` which can return a df with scores
for each participant, each item or each participant-item combination.
Scores come from two models – `all_max` and `all_top2`. A value for a
participant is the maximum value predicted by the model for that
participant.

``` r
library("mtscr")
data("mtscr_creativity", package = "mtscr")

mtscr_score(mtscr_creativity, id, item, SemDis_MEAN)
#> # A tibble: 149 × 3
#>       id .all_max .all_top2
#>    <dbl>    <dbl>     <dbl>
#>  1 84176    0.783    0.516 
#>  2 84177    0.133   -0.0466
#>  3 84178    0.568    0.348 
#>  4 84188    1.17     0.975 
#>  5 84193    0.342    0.0979
#>  6 84206    0.331    0.148 
#>  7 84211    0.595    0.484 
#>  8 84226    0.879    0.658 
#>  9 84228    0.778    0.587 
#> 10 84236    1.10     0.870 
#> # ℹ 139 more rows
```

`mtscr_score()` does everything automatically. You can also use
`mtscr_prepare()` to get your data prepared for modelling by hand and
`mtscr_model()` to get the model object. See the functions’
documentation for more details.

## Contact

Correspondence concerning the merithorical side of these solutions
should be addressed to Boris Forthmann, Institute of Psychology,
University of Münster, Fliednerstrasse 21, 48149 Münster, Germany.
Email: <boris.forthmann@wwu.de>.

The mainteiner of the R package is Jakub Jędrusiak and the technical
concerns should be directed to him. Well, me. Best way is to open a
[discussion on
GitHub](https://github.com/jakub-jedrusiak/mtscr/discussions).
