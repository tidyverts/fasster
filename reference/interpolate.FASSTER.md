# Interpolate missing values in a FASSTER model

Fills in missing values in the response variable using the model's
fitted values. This method only works for interpolating data used to
estimate the model and cannot be used for new data.

## Usage

``` r
# S3 method for class 'FASSTER'
interpolate(object, new_data, specials, ...)
```

## Arguments

- object:

  A fitted FASSTER model object.

- new_data:

  A tsibble containing the data to interpolate. Must be the same data
  used to fit the model.

- specials:

  A list of special terms (passed by fabletools).

- ...:

  Additional arguments (currently unused).

## Value

A tsibble with missing values in the response variable replaced by
fitted values.

## Details

This method identifies missing values (NAs) in the response variable and
replaces them with the corresponding fitted values from the model. It
only works when `new_data` has the same length as the data used to fit
the model.

## Examples

``` r
library(tsibble)
library(dplyr)

# Create data with missing values
deaths_na <- as_tsibble(mdeaths) |>
  mutate(value = if_else(row_number() %in% c(10, 20, 30), NA_real_, value))

# Fit model
fit <- deaths_na |>
  model(FASSTER(value ~ trend(1) + fourier(12)))

# Interpolate missing values
interpolate(fit, deaths_na)
#> # A tsibble: 72 x 2 [1M]
#>       index value
#>       <mth> <dbl>
#>  1 1974 Jan 2134 
#>  2 1974 Feb 1863 
#>  3 1974 Mar 1877 
#>  4 1974 Apr 1877 
#>  5 1974 May 1492 
#>  6 1974 Jun 1249 
#>  7 1974 Jul 1280 
#>  8 1974 Aug 1131 
#>  9 1974 Sep 1209 
#> 10 1974 Oct 1355.
#> # ℹ 62 more rows
```
