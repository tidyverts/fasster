# Refit a FASSTER model

Applies a fitted FASSTER model to a new dataset.

## Usage

``` r
# S3 method for class 'FASSTER'
refit(object, new_data, specials = NULL, reestimate = FALSE, ...)
```

## Arguments

- object:

  A fitted FASSTER model.

- new_data:

  A tsibble containing the new data.

- specials:

  (passed by
  [`fabletools::refit.mdl_df()`](https://generics.r-lib.org/reference/refit.html)).

- reestimate:

  If `TRUE`, the model parameters will be re-estimated to suit the new
  data using the heuristic approach. If `FALSE`, the existing model
  structure and parameters are applied to the new data without
  modification.

- ...:

  Additional arguments passed to the training function.

## Value

A refitted FASSTER model.

## Examples

``` r
library(tsibble)

# Fit model to male deaths
fit_male <- as_tsibble(mdeaths) |>
  model(FASSTER(value ~ trend(1) + fourier(12)))

# Refit to female deaths without re-estimating parameters
refit(fit_male, as_tsibble(fdeaths), reestimate = FALSE)
#> # A mable: 1 x 1
#>   `FASSTER(value ~ trend(1) + fourier(12))`
#>                                     <model>
#> 1                                 <FASSTER>

# Refit to female deaths with re-estimated parameters
refit(fit_male, as_tsibble(fdeaths), reestimate = TRUE)
#> # A mable: 1 x 1
#>   `FASSTER(value ~ trend(1) + fourier(12))`
#>                                     <model>
#> 1                                 <FASSTER>
```
