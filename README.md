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

#### `preprocess`

The `preprocess()` function is used to clean variable names, add
descriptive variable labels, and convert binary variables to logical
variables for better presentation in a descriptives table.

``` r
library(tibbletest)

# defaults
example_dat %>% 
  dplyr::rename(`sugar   factor` = sugar_factor) %>% 
  preprocess()
```

    ## [Step 1]: Converting binary variables to logical variables
    ## Variables (happy, no_weight) were converted to logical.

    ## 

    ## [Step 2]: Cleaning variable names
    ## The following variable names were cleaned with the mapping:
    ## 
    ## Original Name    Cleaned Name 
    ## ---------------  -------------
    ## sugar   factor   sugar_factor

    ## 

    ## [Step 3]: Adding variable labels
    ## The following variable labels were specified with the mapping:
    ## 
    ## Variable       Label        
    ## -------------  -------------
    ## gender         Gender       
    ## age            Age          
    ## sugar_factor   Sugar Factor 
    ## treat          Treat        
    ## happiness      Happiness    
    ## happy          Happy        
    ## weight         Weight       
    ## no_weight      No Weight    
    ## treat2         Treat 2

    ## 

    ## # A tibble: 501 x 9
    ##    gender   age sugar_factor treat  happiness happy weight no_weight treat2
    ##    <chr>  <int>        <dbl> <chr>  <chr>     <lgl>  <dbl> <lgl>     <chr> 
    ##  1 male      80       0.894  ice c… happy     TRUE   1.38  TRUE      pizza 
    ##  2 female    60       0.834  candy  happy     TRUE   0.902 TRUE      pizza 
    ##  3 male       8       0.222  candy  happy     TRUE   1.40  TRUE      ice c…
    ##  4 female    61       0.666  candy  happy     TRUE   1.38  TRUE      pizza 
    ##  5 male      53       0.586  candy  happy     TRUE   1.12  TRUE      ice c…
    ##  6 female    73       0.793  ice c… happy     TRUE   0.538 TRUE      candy 
    ##  7 female     6       0.358  candy  happy     TRUE   0.721 TRUE      ice c…
    ##  8 male      26       0.401  ice c… happy     TRUE   0.912 TRUE      candy 
    ##  9 male      63       0.206  candy  happy     TRUE   0.998 TRUE      candy 
    ## 10 male      76       0.0423 candy  happy     TRUE   1.72  TRUE      candy 
    ## # … with 491 more rows

``` r
# non-defaults
mylabels <- list(
  gender           = "Sex",
  sugar_factor     = "Sugar Amount",
  treat2           = "Alternative Treatment"
)

example_dat %>% 
  dplyr::rename(`sugar   factor` = sugar_factor) %>% 
  preprocess(clean_names = TRUE, convert_to_logical = FALSE, var_labels = mylabels)
```

    ## [Step 1]: Converting binary variables to logical variables
    ## SKIPPED

    ## 

    ## [Step 2]: Cleaning variable names
    ## The following variable names were cleaned with the mapping:
    ## 
    ## Original Name    Cleaned Name 
    ## ---------------  -------------
    ## sugar   factor   sugar_factor

    ## 

    ## [Step 3]: Adding variable labels
    ## The following variable labels were specified with the mapping:
    ## 
    ## Variable       Label                 
    ## -------------  ----------------------
    ## gender         Sex                   
    ## age            Age                   
    ## sugar_factor   Sugar Amount          
    ## treat          Treat                 
    ## happiness      Happiness             
    ## happy          Happy                 
    ## weight         Weight                
    ## no_weight      No Weight             
    ## treat2         Alternative Treatment

    ## 

    ## # A tibble: 501 x 9
    ##    gender   age sugar_factor treat  happiness happy weight no_weight treat2
    ##    <fct>  <int>        <dbl> <fct>  <fct>     <fct>  <dbl>     <dbl> <chr> 
    ##  1 male      80       0.894  ice c… happy     Yes    1.38          1 pizza 
    ##  2 female    60       0.834  candy  happy     Yes    0.902         1 pizza 
    ##  3 male       8       0.222  candy  happy     yes    1.40          1 ice c…
    ##  4 female    61       0.666  candy  happy     Yes    1.38          1 pizza 
    ##  5 male      53       0.586  candy  happy     Yes    1.12          1 ice c…
    ##  6 female    73       0.793  ice c… happy     Yes    0.538         1 candy 
    ##  7 female     6       0.358  candy  happy     yes    0.721         1 ice c…
    ##  8 male      26       0.401  ice c… happy     Yes    0.912         1 candy 
    ##  9 male      63       0.206  candy  happy     Yes    0.998         1 candy 
    ## 10 male      76       0.0423 candy  happy     Yes    1.72          1 candy 
    ## # … with 491 more rows

``` r
# suppress messages
example_dat %>% 
  dplyr::rename(`sugar   factor` = sugar_factor) %>% 
  preprocess(quiet = TRUE)
```

    ## # A tibble: 501 x 9
    ##    gender   age sugar_factor treat  happiness happy weight no_weight treat2
    ##    <chr>  <int>        <dbl> <chr>  <chr>     <lgl>  <dbl> <lgl>     <chr> 
    ##  1 male      80       0.894  ice c… happy     TRUE   1.38  TRUE      pizza 
    ##  2 female    60       0.834  candy  happy     TRUE   0.902 TRUE      pizza 
    ##  3 male       8       0.222  candy  happy     TRUE   1.40  TRUE      ice c…
    ##  4 female    61       0.666  candy  happy     TRUE   1.38  TRUE      pizza 
    ##  5 male      53       0.586  candy  happy     TRUE   1.12  TRUE      ice c…
    ##  6 female    73       0.793  ice c… happy     TRUE   0.538 TRUE      candy 
    ##  7 female     6       0.358  candy  happy     TRUE   0.721 TRUE      ice c…
    ##  8 male      26       0.401  ice c… happy     TRUE   0.912 TRUE      candy 
    ##  9 male      63       0.206  candy  happy     TRUE   0.998 TRUE      candy 
    ## 10 male      76       0.0423 candy  happy     TRUE   1.72  TRUE      candy 
    ## # … with 491 more rows

#### `descriptives`

The `descriptives()` function is used to quickly create descriptives
tables for reports or presentations.

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
