# Changelog

## fasster (development version)

### New features

- **FASSTER model implementation**: Fast Additive Switching of
  Seasonality, Trend and Exogenous Regressors (FASSTER) is a state space
  model designed for forecasting time series with multiple seasonal
  patterns. The model extends traditional state space models by
  introducing a switching component to the measurement equation,
  enabling flexible modeling of complex seasonal patterns, time-varying
  effects, and multiple seasonalities.

- **Model specification**: Flexible formula interface supporting:

  - `trend()` for polynomial trends
  - `season()` for seasonal factors
  - `fourier()` for trigonometric seasonal terms
  - `ARMA()` for autoregressive moving average components
  - `xreg()` for exogenous regressors
  - `%S%` switching operator for group-specific model structures
  - `%?%` conditional operator for time-varying components

- **Model methods**: Full integration with the fable framework:

  - [`fitted()`](https://rdrr.io/r/stats/fitted.values.html) and
    [`residuals()`](https://rdrr.io/r/stats/residuals.html) for model
    diagnostics
  - [`augment()`](https://generics.r-lib.org/reference/augment.html) for
    augmenting data with model estimates
  - [`tidy()`](https://generics.r-lib.org/reference/tidy.html) for
    extracting coefficients (initial state estimates)
  - [`glance()`](https://generics.r-lib.org/reference/glance.html) for
    model summary statistics (AIC, BIC, log-likelihood)
  - [`report()`](https://fabletools.tidyverts.org/reference/report.html)
    for displaying estimated state and observation variances
  - [`components()`](https://generics.r-lib.org/reference/components.html)
    for decomposing fitted values into trend and seasonal components
  - [`forecast()`](https://generics.r-lib.org/reference/forecast.html)
    for generating predictions
  - [`interpolate()`](https://generics.r-lib.org/reference/interpolate.html)
    for filling missing values
  - [`refit()`](https://generics.r-lib.org/reference/refit.html) for
    applying a fitted model to new data with optional re-estimation
  - [`stream()`](https://fabletools.tidyverts.org/reference/stream.html)
    for extending models with new observations

- **Heuristic estimation**: Model parameters are estimated using a
  heuristic approach based on filtering and smoothing to obtain initial
  state parameters and variance estimates.
