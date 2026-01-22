# Extract residuals from a FASSTER model

Returns the one-step-ahead forecast errors (residuals) from a FASSTER
model.

## Usage

``` r
# S3 method for class 'FASSTER'
residuals(object, ...)
```

## Arguments

- object:

  A fitted FASSTER model object.

- ...:

  Additional arguments (currently unused).

## Value

A numeric vector of residuals with the same length as the training data.

## Examples

``` r
library(tsibble)
fit <- as_tsibble(mdeaths) |>
  model(FASSTER(value ~ trend(1) + fourier(12)))

# Extract residuals
residuals(fit)
#> # A tsibble: 72 x 3 [1M]
#> # Key:       .model [1]
#>    .model                                     index  .resid
#>    <chr>                                      <mth>   <dbl>
#>  1 FASSTER(value ~ trend(1) + fourier(12)) 1974 Jan   49.6 
#>  2 FASSTER(value ~ trend(1) + fourier(12)) 1974 Feb -184.  
#>  3 FASSTER(value ~ trend(1) + fourier(12)) 1974 Mar  -19.0 
#>  4 FASSTER(value ~ trend(1) + fourier(12)) 1974 Apr  296.  
#>  5 FASSTER(value ~ trend(1) + fourier(12)) 1974 May  157.  
#>  6 FASSTER(value ~ trend(1) + fourier(12)) 1974 Jun  -13.2 
#>  7 FASSTER(value ~ trend(1) + fourier(12)) 1974 Jul   68.6 
#>  8 FASSTER(value ~ trend(1) + fourier(12)) 1974 Aug   -8.01
#>  9 FASSTER(value ~ trend(1) + fourier(12)) 1974 Sep   84.9 
#> 10 FASSTER(value ~ trend(1) + fourier(12)) 1974 Oct  116.  
#> # ℹ 62 more rows
```
