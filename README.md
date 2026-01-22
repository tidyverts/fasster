<!-- README.md is generated from README.Rmd. Please edit that file -->

# fasster <a href="https://fasster.tidyverts.org"><img src="man/figures/logo.png" align="right" height="139" alt="fasster website" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/tidyverts/fasster/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tidyverts/fasster/actions/workflows/R-CMD-check.yaml)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/fasster)](https://github.com/tidyverts/fasster)
[![Downloads](http://cranlogs.r-pkg.org/badges/fasster?color=brightgreen)](https://github.com/tidyverts/fasster)
<!-- badges: end -->

An implementation of the FASSTER (Forecasting with Additive Switching of
Seasonality, Trend and Exogenous Regressors) model in R. This model is
designed to capture patterns of multiple seasonality in a state space
framework by using state switching. The *fasster* package prioritizes
flexibility, computational speed and accuracy to provide convenient
tools for modelling, predicting and understanding high frequency
time-series.

## Model information

This model was developed in 2017 for an honours thesis, and the clearest
description with examples of using fasster can be found in my useR! 2018
talk: [slides](https://slides.mitchelloharawild.com/user2018/),
[video](https://www.youtube.com/watch?v=6YlboftSalY),
[source](https://github.com/mitchelloharawild/talk-user2018-fasster).

The model uses a forward filtering backward smoothing heuristic for
estimating the initial states in the model. It is essentially an
opinionated wrapper of the `{dlm}` package aimed at making it easier to
specify a sophisticated model suitable for forecasting complex seasonal
patterns.

## Installation

The **stable** version can be installed from CRAN:

``` r
install.packages("fasster")
```

The **development** version can be installed from GitHub using:

``` r
# install.packages("devtools")
devtools::install_github("tidyverts/fasster")
```

## Usage

### Model specification

*fasster* allows flexible model specification by allowing the user to
specify the model structure with standard formula conventions.

``` r
library(fasster)
library(tidyverse)
library(lubridate)
library(tsibble)
library(fable)

lung_deaths <- as_tsibble(cbind(mdeaths, fdeaths), pivot_longer = FALSE)
fit <- lung_deaths |>
  model(fasster = FASSTER(fdeaths ~ mdeaths))
fit |> report()
#> Series: fdeaths 
#> Model: FASSTER 
#> 
#> Estimated variances:
#>  State noise variances (W):
#>   mdeaths
#>    1.7119e-34
#> 
#>  Observation noise variance (V):
#>   1.6631e+03
#> 
#> Initial states (m0):
#>   mdeaths: 3.7711e-01
```

Commonly used state space components can be added using the following
convenience functions:

- `trend(n)` to include an n-th order polynomial
- `season(s)` to include a seasonal factor of frequency s
- `fourier(s, q)` to include seasonal fourier terms of frequency s with
  q harmonics
- `arma(ar, ma)` to include an ARMA term (where ar and ma are vectors of
  coefficients)
- Exogenous regressors can be added by referring to their name

For example, to create a model with trend and monthly seasonality, you
can use:

``` r
fit <- as_tsibble(USAccDeaths) |> 
  model(fasster = FASSTER(value ~ trend(1) + fourier(12)))
fit |> report()
#> Series: value 
#> Model: FASSTER 
#> 
#> Estimated variances:
#>  State noise variances (W):
#>   fourier(12)
#>    3.5590e-13 2.3103e-13 3.5905e-13 4.3745e-13 3.6531e-13 8.4136e-13 2.6433e-13 3.4998e-13 4.1144e-13 3.6736e-13 1.3359e-13
#>   trend(1)
#>    5.9382e+03
#> 
#>  Observation noise variance (V):
#>   2.0543e+04
#> 
#> Initial states (m0):
#>   fourier(12): -7.2510e+02
#>   fourier(12): -7.4524e+02
#>   fourier(12):  4.1713e+02
#>   fourier(12):  8.1375e+01
#>   fourier(12):  1.5506e+02
#>   fourier(12): -1.9470e+02
#>   fourier(12): -1.0038e+01
#>   fourier(12):  1.5899e+02
#>   fourier(12):  1.5997e+02
#>   fourier(12):  2.0428e+02
#>   fourier(12): -1.5165e+01
#>   trend(1):  9.7398e+03
```

The interface for creating a FASSTER model introduces a new formula
construct, `%S%`, known as the switch operator. This allows modelling of
more complex patterns such as multiple seasonality by modelling the
components for each group separately and switching between them.

``` r
elec_tr <- tsibbledata::vic_elec |>
  filter(
    Time < lubridate::ymd("2012-03-01")
  ) |> 
  mutate(WorkDay = wday(Time) %in% 2:6 & !Holiday)

elec_fit <- elec_tr |>
  model(
    fasster = fasster(log(Demand) ~ 
      WorkDay %S% (fourier(48, 16) + trend(1)) + Temperature + I(Temperature^2)
    )
  )
```

### Decomposing

Fitted FASSTER models can be decomposed to provide a description of how
the underlying states function. Decomposing a FASSTER model provides
aggregates of its components such as trends and seasonalities.

These components can accessed from a fitted model using the
`components()` function:

``` r
fit |> 
  components()
#> # A dable: 72 x 5 [1M]
#> # Key:     .model [1]
#> # :        value = `fourier(12)` + `trend(1)`
#>    .model     index value `fourier(12)` `trend(1)`
#>    <chr>      <mth> <dbl>         <dbl>      <dbl>
#>  1 fasster 1973 Jan  9007        -795.       9740.
#>  2 fasster 1973 Feb  8106       -1546.       9754.
#>  3 fasster 1973 Mar  8928        -758.       9719.
#>  4 fasster 1973 Apr  9137        -536.       9706.
#>  5 fasster 1973 May 10017         322.       9693.
#>  6 fasster 1973 Jun 10826         802.       9694.
#>  7 fasster 1973 Jul 11317        1669.       9830.
#>  8 fasster 1973 Aug 10744         974.       9755.
#>  9 fasster 1973 Sep  9713         -65.7      9761.
#> 10 fasster 1973 Oct  9938         233.       9768.
#> # ℹ 62 more rows
```

``` r
elec_fit |>
  components()
#> # A dable: 2,880 x 9 [30m] <Australia/Melbourne>
#> # Key:     .model [1]
#> # :        log(Demand) = `WorkDay_FALSE/fourier(48, 16)` +
#> #   `WorkDay_FALSE/trend(1)` + `WorkDay_TRUE/fourier(48, 16)` +
#> #   `WorkDay_TRUE/trend(1)` + Temperature + `I(Temperature^2)`
#>    .model  Time                `log(Demand)` `WorkDay_FALSE/fourier(48, 16)`
#>    <chr>   <dttm>                      <dbl>                           <dbl>
#>  1 fasster 2012-01-01 00:00:00          8.39                        -0.00345
#>  2 fasster 2012-01-01 00:30:00          8.36                        -0.0234 
#>  3 fasster 2012-01-01 01:00:00          8.31                        -0.0971 
#>  4 fasster 2012-01-01 01:30:00          8.26                        -0.105  
#>  5 fasster 2012-01-01 02:00:00          8.30                        -0.117  
#>  6 fasster 2012-01-01 02:30:00          8.26                        -0.0812 
#>  7 fasster 2012-01-01 03:00:00          8.21                        -0.251  
#>  8 fasster 2012-01-01 03:30:00          8.18                        -0.144  
#>  9 fasster 2012-01-01 04:00:00          8.14                        -0.374  
#> 10 fasster 2012-01-01 04:30:00          8.12                        -0.202  
#> # ℹ 2,870 more rows
#> # ℹ 5 more variables: `WorkDay_FALSE/trend(1)` <dbl>,
#> #   `WorkDay_TRUE/fourier(48, 16)` <dbl>, `WorkDay_TRUE/trend(1)` <dbl>,
#> #   Temperature <dbl>, `I(Temperature^2)` <dbl>
```

The tools made available by *fasster* are designed to integrate
seamlessly with the tidyverse of packages, enabling familiar data
manipulation and visualisation capabilities.

### Forecasting

*fasster* conforms to the object structure from the *fable* package,
allowing common visualisation and analysis tools to be applied on
FASSTER models.

``` r
fit |> 
  forecast(h=24) |>
  autoplot(as_tsibble(USAccDeaths))
```

![](man/figure/forecast-1.png)<!-- -->

Future index values are automatically produced and used where necessary
in the model specification. If additional information is required by the
model (such as `WorkDay` and `Temperature`) they must be included in a
`tsibble` of future values passed to `new_data`.

``` r
elec_ts <- tsibbledata::vic_elec |>
  filter(
    yearmonth(Time) == yearmonth("2012 Mar")
  ) |> 
  mutate(WorkDay = wday(Time) %in% 2:6 & !Holiday) |> 
  select(-Demand)
elec_fit |> 
  forecast(new_data = elec_ts) |> 
  autoplot(elec_tr)
```

![](man/figure/complex_fc-1.png)<!-- -->

------------------------------------------------------------------------

Please note that this project is released with a [Contributor Code of
Conduct](.github/CODE_OF_CONDUCT.md). By participating in this project
you agree to abide by its terms.
