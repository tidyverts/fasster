# Extract Components from a FASSTER Model

Decomposes a FASSTER model into its individual components, allowing you
to examine the contribution of each term to the fitted values. This is
useful for understanding which components drive the model's predictions
and how well different aspects of the model fit the data.

## Usage

``` r
# S3 method for class 'FASSTER'
components(object, ...)
```

## Arguments

- object:

  A FASSTER model object.

- ...:

  Additional arguments (currently unused).

## Value

A dable (decomposition table) containing the response variable and each
model component as separate columns. The components sum to the response
variable.

## Examples

``` r
if (requireNamespace("tsibbledata", quietly = TRUE)) {
# Fit a FASSTER model and extract components
library(tsibble)
library(dplyr)
fit <- tsibbledata::aus_retail |>
  filter(
    State == "Victoria",
    Industry == "Cafes, restaurants and catering services"
  ) |>
  model(fasster = FASSTER(Turnover ~ trend(1) + season("year")))

# Extract and view components
components(fit)
}
#> 
#> Attaching package: ‘tsibble’
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, union
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
#> # A dable: 441 x 7 [1M]
#> # Key:     State, Industry, .model [1]
#> # :        Turnover = `season("year")` + `trend(1)`
#>    State    Industry        .model    Month Turnover `season("year")` `trend(1)`
#>    <chr>    <chr>           <chr>     <mth>    <dbl>            <dbl>      <dbl>
#>  1 Victoria Cafes, restaur… fasst… 1982 Apr     36.4           -2.08        37.9
#>  2 Victoria Cafes, restaur… fasst… 1982 May     36.2           -2.01        38.1
#>  3 Victoria Cafes, restaur… fasst… 1982 Jun     35.7           -1.95        38.2
#>  4 Victoria Cafes, restaur… fasst… 1982 Jul     34.6           -1.39        37.9
#>  5 Victoria Cafes, restaur… fasst… 1982 Aug     32.5           -1.46        37.1
#>  6 Victoria Cafes, restaur… fasst… 1982 Sep     33.9           -0.799       35.7
#>  7 Victoria Cafes, restaur… fasst… 1982 Oct     37.7            1.95        35.2
#>  8 Victoria Cafes, restaur… fasst… 1982 Nov     40.3            3.81        35.5
#>  9 Victoria Cafes, restaur… fasst… 1982 Dec     45.2            8.71        35.9
#> 10 Victoria Cafes, restaur… fasst… 1983 Jan     36.9           -1.38        36.2
#> # ℹ 431 more rows
```
