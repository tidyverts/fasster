# Extract fitted values from a FASSTER model

Returns the one-step-ahead fitted values from a FASSTER model,
calculated during the Kalman filtering process. These represent the
model's predictions at each time point using only information available
up to that point.

## Usage

``` r
# S3 method for class 'FASSTER'
fitted(object, ...)
```

## Arguments

- object:

  A fitted FASSTER model object.

- ...:

  Additional arguments (currently unused).

## Value

A numeric vector of fitted values with the same length as the training
data.

## Examples

``` r
library(tsibble)
fit <- as_tsibble(mdeaths) |>
  model(FASSTER(value ~ trend(1) + fourier(12)))

# Extract fitted values
fitted(fit)
#> # A tsibble: 72 x 3 [1M]
#> # Key:       .model [1]
#>    .model                                     index .fitted
#>    <chr>                                      <mth>   <dbl>
#>  1 FASSTER(value ~ trend(1) + fourier(12)) 1974 Jan   2084.
#>  2 FASSTER(value ~ trend(1) + fourier(12)) 1974 Feb   2047.
#>  3 FASSTER(value ~ trend(1) + fourier(12)) 1974 Mar   1896.
#>  4 FASSTER(value ~ trend(1) + fourier(12)) 1974 Apr   1581.
#>  5 FASSTER(value ~ trend(1) + fourier(12)) 1974 May   1335.
#>  6 FASSTER(value ~ trend(1) + fourier(12)) 1974 Jun   1262.
#>  7 FASSTER(value ~ trend(1) + fourier(12)) 1974 Jul   1211.
#>  8 FASSTER(value ~ trend(1) + fourier(12)) 1974 Aug   1139.
#>  9 FASSTER(value ~ trend(1) + fourier(12)) 1974 Sep   1124.
#> 10 FASSTER(value ~ trend(1) + fourier(12)) 1974 Oct   1376.
#> # ℹ 62 more rows
```
