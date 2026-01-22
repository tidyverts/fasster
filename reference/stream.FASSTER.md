# Stream new data through a FASSTER model

Extends a fitted FASSTER model by filtering new observations through the
existing state space model. The model's states and parameters are
updated sequentially as new data arrives, allowing for online learning
without refitting from scratch.

## Usage

``` r
# S3 method for class 'FASSTER'
stream(object, new_data, specials = NULL, ...)
```

## Arguments

- object:

  A fitted FASSTER model object

- new_data:

  A tsibble containing new observations to stream through the model

- specials:

  A list of special terms (switching variables, etc.) parsed from the
  model formula

- ...:

  Additional arguments (currently unused)

## Value

An updated FASSTER model object with:

- Extended state estimates incorporating the new data

- Updated model variance

- Appended fitted values and residuals

- Updated DLM components for future forecasting

## Details

The streaming process:

1.  Constructs the design matrix from new data

2.  Applies the Kalman filter to sequentially update states

3.  Updates model variance based on all residuals

4.  Prepares the model for subsequent forecasting or streaming

## Examples

``` r
if (FALSE) { # \dontrun{
library(tsibble)
library(fasster)

# Fit initial model on training data
fit <- as_tsibble(head(USAccDeaths, -12)) |>
  model(fasster = FASSTER(value ~ trend() + season("year")))
tidy(fit)
tail(fitted(fit), 20)

# Stream new data through the model
fit_updated <- fit |>
  stream(as_tsibble(tail(USAccDeaths, 12)))
tidy(fit_updated)
tail(fitted(fit_updated), 20)

} # }
```
