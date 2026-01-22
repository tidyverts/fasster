# Forecast a FASSTER model

Produces forecasts from a trained FASSTER model using the Kalman filter
framework. This method generates point forecasts and prediction
intervals by propagating the state space forward through time.

## Usage

``` r
# S3 method for class 'FASSTER'
forecast(object, new_data, specials = NULL, ...)
```

## Arguments

- object:

  A trained FASSTER model object.

- new_data:

  A tsibble containing future time points to forecast. Must be regularly
  spaced and contain any required exogenous regressors.

- specials:

  A list of special formulations generated from the model formula, used
  to construct the design matrix for time-varying components.

- ...:

  Additional arguments (currently unused).

## Value

A distribution vector (from the distributional package) containing
normal distributions with forecasted means and standard errors for each
future time point. This integrates with fable's forecast distribution
structure.

## Details

The forecast method implements a Kalman filter to propagate the state
space model forward in time:

1.  For each forecast horizon, constructs time-varying system matrices
    (FF, GG, V, W) using exogenous variables from `new_data`

2.  Computes the state forecast: \\a\_{t+1} = G_t a_t\\

3.  Computes the state covariance: \\R\_{t+1} = G_t R_t G_t' + W_t\\

4.  Computes the observation forecast: \\f_t = F_t a\_{t+1}\\

5.  Computes the forecast variance: \\Q_t = F_t R\_{t+1} F_t' + V_t\\

The method handles switching components by matching exogenous variables
in `new_data` with the model's design matrix, adding zero columns for
any missing levels.
