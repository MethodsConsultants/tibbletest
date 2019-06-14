tibbletest
================

[![Travis build
status](https://travis-ci.org/MethodsConsultants/tibbletest.svg?branch=master)](https://travis-ci.org/MethodsConsultants/tibbletest)

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

    ## # A tibble: 9 x 4
    ##   Variable     Label  candy         `ice cream`  
    ##   <chr>        <chr>  <chr>         <chr>        
    ## 1 gender       female 124 (48.25%)  110 (45.27%) 
    ## 2 gender       male   133 (51.75%)  133 (54.73%) 
    ## 3 happiness    happy  185 (76.76%)  181 (78.7%)  
    ## 4 happiness    sad    56 (23.24%)   49 (21.3%)   
    ## 5 happy        no     56 (23.24%)   49 (21.3%)   
    ## 6 happy        yes    47 (19.5%)    44 (19.13%)  
    ## 7 happy        Yes    138 (57.26%)  137 (59.57%) 
    ## 8 age          ""     42.26 (22.62) 42.39 (21.61)
    ## 9 sugar_factor ""     0.46 (0.3)    0.52 (0.29)
