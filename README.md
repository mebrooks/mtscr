
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Multidimensional Top Scoring for Creativity Research

<!-- badges: start -->

[![codecov](https://codecov.io/gh/jakub-jedrusiak/mtscr/branch/master/graph/badge.svg?token=N3UGHFK5QN)](https://codecov.io/gh/jakub-jedrusiak/mtscr)
[![R-CMD-check](https://github.com/jakub-jedrusiak/mtscr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jakub-jedrusiak/mtscr/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

An R adaptation of Multidimentional Top Scoring method presented by
Forthmann, Karwowski and Beaty
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

### Graphical User Interface

This package includes a Shiny app which can be used as a GUI. Execute
`mtscr_app()` to run it.

![](./man/figures/README-GUI-example.gif)

First thing you see after running the app is
[`datamods`](https://github.com/dreamRs/datamods) window for importing
your data. You can use the data already loaded in your environment or
any other option. Then you’ll see four dropdown lists used to choose
arguments for `mtscr_model()` and `mtscr_score()` functions. Consult
these functions’ documentation for more details (execute `?mtscr_score`
in the console). When the parameters are chosen, click “Generate model”
button. After a while (up to a dozen or so seconds) models’ parameters
and are shown along with a scored dataframe.

You can download your data as a .csv or an .xlsx file using buttons in
the sidebar. You can either download the scores only (i.e. the dataframe
you see displayed) or your whole data with `.all_max` and `.all_top2`
columns added.

For testing purposes, you may use `mtscr_creativity` dataframe. In the
importing window change “Global Environment” to “mtscr” and our
dataframe should appear in the upper dropdown list. Use `id` for the ID
column, `item` for the item column and `SemDis_MEAN` for the score
column.

## Contact

Correspondence concerning the meritorical side of these solutions should
be addressed to Boris Forthmann, Institute of Psychology, University of
Münster, Fliednerstrasse 21, 48149 Münster, Germany. Email:
<boris.forthmann@wwu.de>.

The maintainer of the R package is Jakub Jędrusiak and the technical
concerns should be directed to him. Well, me. Best way is to open a
[discussion on
GitHub](https://github.com/jakub-jedrusiak/mtscr/discussions). Technical
difficulties may deserve an
[issue](https://github.com/jakub-jedrusiak/mtscr/issues).
