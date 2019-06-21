tibbletest
================

[![Travis build
status](https://travis-ci.org/MethodsConsultants/tibbletest.svg?branch=master)](https://travis-ci.org/MethodsConsultants/tibbletest)
[![Codecov test
coverage](https://codecov.io/gh/MethodsConsultants/tibbletest/branch/master/graph/badge.svg)](https://codecov.io/gh/MethodsConsultants/tibbletest?branch=master)

## Installation

Install the development version from [GitHub](https://github.com/) with:

``` r
devtools::install_github("MethodsConsultants/tibbletest")
```

## Usage

``` r
library(tibbletest)

example_dat %>% 
  descriptives(
    treatment = "treat", 
    variables = c("age", "sugar_factor", "gender", "happiness", "happy")
  )
```

    ## # A tibble: 9 x 5
    ##   Variable     Label  candy         `ice cream`   `P Value`
    ##   <chr>        <chr>  <chr>         <chr>             <dbl>
    ## 1 gender       female 124 (48.25%)  110 (45.27%)     0.563 
    ## 2 gender       male   133 (51.75%)  133 (54.73%)     0.563 
    ## 3 happiness    happy  185 (76.76%)  181 (78.7%)      0.694 
    ## 4 happiness    sad    56 (23.24%)   49 (21.3%)       0.694 
    ## 5 happy        no     56 (23.24%)   49 (21.3%)       0.855 
    ## 6 happy        yes    47 (19.5%)    44 (19.13%)      0.855 
    ## 7 happy        Yes    138 (57.26%)  137 (59.57%)     0.855 
    ## 8 age          ""     42.26 (22.62) 42.39 (21.61)    0.946 
    ## 9 sugar_factor ""     0.46 (0.3)    0.52 (0.29)      0.0129

``` r
example_dat %>%
  dplyr::select(treat, age, gender) %>%
  descriptives(
    treatment = "treat"
  )
```

    ## # A tibble: 3 x 5
    ##   Variable Label  candy         `ice cream`   `P Value`
    ##   <chr>    <chr>  <chr>         <chr>             <dbl>
    ## 1 gender   female 124 (48.25%)  110 (45.27%)      0.563
    ## 2 gender   male   133 (51.75%)  133 (54.73%)      0.563
    ## 3 age      ""     42.26 (22.62) 42.39 (21.61)     0.946

``` r
example_dat %>%
  dplyr::select(treat, weight, age, gender) %>%
  descriptives(
    treatment = "treat",
    weights = "weight"
  )
```

    ## # A tibble: 3 x 5
    ##   Variable Label  candy         `ice cream`   `P Value`
    ##   <chr>    <chr>  <chr>         <chr>             <dbl>
    ## 1 gender   female 48.65%        44.97%            0.475
    ## 2 gender   male   51.35%        55.03%            0.475
    ## 3 age      ""     42.17 (22.55) 42.43 (21.93)     0.897
