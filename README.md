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

#### `propensity_weighting`

``` r
library(tibbletest)

example_dat %>%
  add_propensity_weights(
    treatment = "treat",
    ivs = c("age", "sugar_factor", "gender")
  )
```

    ## Error: df[[treatment]] contains 1 missing values

``` r
example_dat %>%
  tidyr::drop_na(treat, age) %>%
  add_propensity_weights(
    treatment = "treat",
    ivs = c("age", "sugar_factor", "gender")
  ) %>%
  dplyr::glimpse()
```

    ## Observations: 495
    ## Variables: 10
    ## $ gender            <fct> male, female, male, female, male, female, fe...
    ## $ age               <int> 80, 60, 8, 61, 53, 73, 6, 26, 63, 76, 31, 18...
    ## $ sugar_factor      <dbl> 0.89372538, 0.83360390, 0.22198429, 0.665734...
    ## $ treat             <fct> ice cream, candy, candy, candy, candy, ice c...
    ## $ happiness         <fct> happy, happy, happy, happy, happy, happy, ha...
    ## $ happy             <fct> Yes, Yes, yes, Yes, Yes, Yes, yes, Yes, Yes,...
    ## $ weight            <dbl> 1.3788863, 0.9021300, 1.3989398, 1.3817288, ...
    ## $ no_weight         <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,...
    ## $ treat2            <chr> "pizza", "pizza", "ice cream", "pizza", "ice...
    ## $ propensity_weight <dbl> 0.9032606, 0.8181604, 0.8252251, 1.0340621, ...

#### `descriptives`

``` r
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

    ## Weights were normalized to a mean of 1 to preserve sample size in significance tests

    ## # A tibble: 3 x 5
    ##   Variable Label  candy         `ice cream`   `P Value`
    ##   <chr>    <chr>  <chr>         <chr>             <dbl>
    ## 1 gender   female 48.65%        44.97%            0.475
    ## 2 gender   male   51.35%        55.03%            0.475
    ## 3 age      ""     42.17 (22.55) 42.43 (21.93)     0.897

``` r
example_dat %>% 
  descriptives(
    treatment = "treat", 
    variables = c("age", "sugar_factor", "gender", "happiness", "happy"),
    nonparametric = c("age")
  )
```

    ## # A tibble: 9 x 5
    ##   Variable     Label  candy        `ice cream`  `P Value`
    ##   <chr>        <chr>  <chr>        <chr>            <dbl>
    ## 1 gender       female 124 (48.25%) 110 (45.27%)    0.563 
    ## 2 gender       male   133 (51.75%) 133 (54.73%)    0.563 
    ## 3 happiness    happy  185 (76.76%) 181 (78.7%)     0.694 
    ## 4 happiness    sad    56 (23.24%)  49 (21.3%)      0.694 
    ## 5 happy        no     56 (23.24%)  49 (21.3%)      0.855 
    ## 6 happy        yes    47 (19.5%)   44 (19.13%)     0.855 
    ## 7 happy        Yes    138 (57.26%) 137 (59.57%)    0.855 
    ## 8 sugar_factor ""     0.46 (0.3)   0.52 (0.29)     0.0129
    ## 9 age          ""     42 [22, 62]  41 [26, 61]     0.888

#### `format_tbl`

``` r
univariate <- example_dat %>% 
  descriptives(
    variables = c("age", "sugar_factor", "gender", "happiness", "happy")
  )

univariate
```

    ## # A tibble: 9 x 3
    ##   Variable     Label  Statistics  
    ##   <chr>        <chr>  <chr>       
    ## 1 gender       female 235 (46.91%)
    ## 2 gender       male   266 (53.09%)
    ## 3 happiness    happy  367 (77.75%)
    ## 4 happiness    sad    105 (22.25%)
    ## 5 happy        no     105 (22.25%)
    ## 6 happy        yes    91 (19.28%) 
    ## 7 happy        Yes    276 (58.47%)
    ## 8 age          ""     42.3 (22.1) 
    ## 9 sugar_factor ""     0.49 (0.3)

``` r
univariate %>%
  format_tbl()
```

    ## # A tibble: 12 x 3
    ##    Variable     Label  `Statistics (N=501)`
    ##    <chr>        <chr>  <chr>               
    ##  1 gender       ""     ""                  
    ##  2 ""           female 235 (46.91%)        
    ##  3 ""           male   266 (53.09%)        
    ##  4 happiness    ""     ""                  
    ##  5 ""           happy  367 (77.75%)        
    ##  6 ""           sad    105 (22.25%)        
    ##  7 happy        ""     ""                  
    ##  8 ""           no     105 (22.25%)        
    ##  9 ""           yes    91 (19.28%)         
    ## 10 ""           Yes    276 (58.47%)        
    ## 11 age          ""     42.3 (22.1)         
    ## 12 sugar_factor ""     0.49 (0.3)

``` r
bivariate <- example_dat %>% 
  descriptives(
    treatment = "treat",
    variables = c("age", "sugar_factor", "gender", "happiness", "happy")
  )

bivariate
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
bivariate %>%
  format_tbl()
```

    ## # A tibble: 12 x 5
    ##    Variable     Label  `candy (N=257)` `ice cream (N=243)` `P Value`
    ##    <chr>        <chr>  <chr>           <chr>               <chr>    
    ##  1 gender       ""     ""              ""                  0.563    
    ##  2 ""           female 124 (48.25%)    110 (45.27%)        ""       
    ##  3 ""           male   133 (51.75%)    133 (54.73%)        ""       
    ##  4 happiness    ""     ""              ""                  0.694    
    ##  5 ""           happy  185 (76.76%)    181 (78.7%)         ""       
    ##  6 ""           sad    56 (23.24%)     49 (21.3%)          ""       
    ##  7 happy        ""     ""              ""                  0.855    
    ##  8 ""           no     56 (23.24%)     49 (21.3%)          ""       
    ##  9 ""           yes    47 (19.5%)      44 (19.13%)         ""       
    ## 10 ""           Yes    138 (57.26%)    137 (59.57%)        ""       
    ## 11 age          ""     42.26 (22.62)   42.39 (21.61)       0.946    
    ## 12 sugar_factor ""     0.46 (0.3)      0.52 (0.29)         0.013
