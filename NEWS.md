# fasster 0.2.0

Fast Additive Switching of Seasonality, Trend and Exogenous Regressors
(FASSTER) is a state space model designed for forecasting time series with
multiple seasonal patterns. The model extends traditional state space models
by introducing a switching component to the measurement equation, enabling
flexible modeling of complex seasonal patterns, and time series dynamics with
rapid structural changes.

* FASSTER model implementation: 

* Model specification: Flexible formula interface supporting:
  - `trend()` for polynomial trends
  - `season()` for seasonal factors
  - `fourier()` for trigonometric seasonal terms
  - `ARMA()` for autoregressive moving average components
  - `xreg()` for exogenous regressors
  - `%S%` switching operator for group-specific model structures
  - `%?%` conditional operator for time-varying components

* Model methods: Full integration with the fable framework:
  - `fitted()` and `residuals()` for model diagnostics
  - `augment()` for augmenting data with model estimates
  - `tidy()` for extracting coefficients (initial state estimates)
  - `glance()` for model summary statistics (AIC, BIC, log-likelihood)
  - `report()` for displaying estimated state and observation variances
  - `components()` for decomposing fitted values into trend and seasonal components
  - `forecast()` for generating predictions
  - `interpolate()` for filling missing values
  - `refit()` for applying a fitted model to new data with optional re-estimation
  - `stream()` for extending models with new observations

* Heuristic estimation: Model parameters are estimated using a heuristic 
  approach based on filtering and smoothing to obtain initial state parameters 
  and variance estimates.

