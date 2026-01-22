# Glance at a FASSTER model

Constructs a single-row summary of the model's goodness-of-fit
statistics. This method follows the broom package conventions and is
used by fabletools to provide model selection metrics.

## Usage

``` r
# S3 method for class 'FASSTER'
glance(x, ...)
```

## Arguments

- x:

  A FASSTER model object.

- ...:

  Additional arguments (currently unused).

## Value

A one-row tibble containing:

- sigma2:

  The estimated observation variance (V). If the model has multivariate
  observations, this is a list containing the variance matrix.

- log_lik:

  The log-likelihood of the model.

- AIC:

  Akaike Information Criterion.

- AICc:

  Corrected AIC for small sample sizes.

- BIC:

  Bayesian Information Criterion.

## Examples

``` r
library(tsibble)
fit <- as_tsibble(mdeaths) |>
  model(FASSTER(value ~ trend(1) + fourier(12)))

# Get model fit statistics
glance(fit)
#> # A tibble: 1 × 6
#>   .model                                  sigma2 log_lik   AIC  AICc   BIC
#>   <chr>                                    <dbl>   <dbl> <dbl> <dbl> <dbl>
#> 1 FASSTER(value ~ trend(1) + fourier(12)) 11359.    409.  842.  848.  870.
```
