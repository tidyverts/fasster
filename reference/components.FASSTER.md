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
if (FALSE) { # \dontrun{
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
} # }
```
