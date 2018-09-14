<!-- README.md is generated from README.Rmd. Please edit that file -->
fasster <img src="man/figure/logo.png" align="right" />
=======================================================

[![Travis-CI Build
Status](https://travis-ci.org/tidyverts/fasster.svg?branch=master)](https://travis-ci.org/tidyverts/fasster)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Coverage
status](https://codecov.io/gh/tidyverts/fasster/branch/master/graph/badge.svg)](https://codecov.io/github/tidyverts/fasster?branch=master)
<!-- [![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/fasster)](https://cran.r-project.org/package=fasster) -->
<!-- [![Downloads](http://cranlogs.r-pkg.org/badges/fasster?color=brightgreen)](https://cran.r-project.org/package=fasster) -->

An implementation of the FASSTER (Forecasting with Additive Switching of
Seasonality, Trend and Exogenous Regressors) model in R. This model is
designed to capture patterns of multiple seasonality in a state space
framework by using state switching. The *fasster* package prioritizes
flexibility, computational speed and accuracy to provide convenient
tools for modelling, predicting and understanding high frequency
time-series.

Development cycle
-----------------

This package is early in development, and there are plans to make
substantial changes in the future.

The latest usage examples of using fasster can be found in my useR! 2018
talk: [slides](https://www.mitchelloharawild.com/user2018/#1),
[video](https://www.youtube.com/watch?v=6YlboftSalY),
[source](https://github.com/mitchelloharawild/fasster_user2018).

There are further plans to improve the heuristic optimisation techniques
and better use sparse matrix algebra (removing the dlm package
dependency) to make fasster even faster. Implementing this will likely
result in a revision of the model object structure, but user directed
functionality should remain the same.

Installation
------------

<!-- The **stable** version can be installed from CRAN: -->
<!-- ```{r, eval = FALSE} -->
<!-- install.packages("fasster") -->
<!-- ``` -->
The **development** version can be installed from GitHub using:

``` r
# install.packages("devtools")
devtools::install_github("tidyverts/fasster")
```

Usage
-----

### Model specification

*fasster* allows flexible model specification by allowing the user to
specify the model structure with standard formula conventions.

``` r
fit <- tsibbledata::UKLungDeaths %>%
  FASSTER(fdeaths ~ mdeaths)

fit %>% summary
#> FASSTER Model:
#>  fdeaths ~ mdeaths 
#> 
#> Estimated variances:
#>  State noise variances (W):
#>   mdeaths
#>    1.7119e-34
#> 
#>  Observation noise variance (V):
#>   1.6631e+03
```

Commonly used state space components can be added using the following
convenience functions:

-   `poly(n)` to include an n-th order polynomial
-   `seas(s)` to include a seasonal factor of frequency s
-   `trig(s, q)` to include seasonal fourier terms of frequency s with q
    harmonics
-   `arma(ar, ma)` to include an ARMA term (where ar and ma are vectors
    of coefficients)
-   Exogenous regressors can be added by referring to their name

For example, to create a model with trend and monthly seasonality, you
can use:

``` r
fit <- USAccDeaths %>% 
  as_tsibble %>% 
  FASSTER(value ~ poly(1) + trig(12))
fit %>% summary
#> FASSTER Model:
#>  value ~ poly(1) + trig(12) 
#> 
#> Estimated variances:
#>  State noise variances (W):
#>   poly(1)
#>    5.9382e+03
#>   trig(12)
#>    1.4370e-12 7.8660e-13 7.4156e-13 1.1844e-12 2.8774e-13 6.4809e-13 2.6654e-13 4.1593e-13 5.5689e-13 1.4806e-13 2.7203e-13
#> 
#>  Observation noise variance (V):
#>   2.0543e+04
```

The interface for creating a FASSTER model introduces a new formula
construct, `%S%`, known as the switch operator. This allows modelling of
more complex patterns such as multiple seasonality by modelling the
components for each group seperately and switching between them.

``` r
elec_tr <- tsibbledata::elecdemand %>%
  dplyr::filter(index < lubridate::ymd("2014-03-01"))

elec_fit <- elec_tr %>%
  fasster(
    log(Demand) ~ 
      WorkDay %S% (trig(48, 16) + poly(1)) + 
      Temperature + I(Temperature^2)
  )
```

### Decomposing

Fitted FASSTER models can be decomposed to provide a description of how
the underlying states function. Decomposing a FASSTER model provides
aggregates of its components such as trends and seasonalities.

These components can accessed from a fitted model using the
`components()` function:

``` r
fit %>% 
  components
#> # A tsibble: 72 x 3 [1M]
#>       index `poly(1)` `trig(12)`
#>       <mth>     <dbl>      <dbl>
#>  1 1973 Jan     9740.     -795. 
#>  2 1973 Feb     9754.    -1546. 
#>  3 1973 Mar     9719.     -758. 
#>  4 1973 Apr     9706.     -536. 
#>  5 1973 May     9693.      322. 
#>  6 1973 Jun     9694.      802. 
#>  7 1973 Jul     9830.     1669. 
#>  8 1973 Aug     9755.      974. 
#>  9 1973 Sep     9761.      -65.7
#> 10 1973 Oct     9768.      233. 
#> # ... with 62 more rows
```

``` r
elec_fit %>%
  components
#> # A tsibble: 2,832 x 7 [30m]
#>    index               `I(Temperature^… Temperature `WorkDay_0/poly…
#>    <dttm>                         <dbl>       <dbl>            <dbl>
#>  1 2014-01-01 00:00:00         0.000512     -0.0189             1.55
#>  2 2014-01-01 00:30:00         0.000512     -0.0189             1.55
#>  3 2014-01-01 01:00:00         0.000512     -0.0189             1.55
#>  4 2014-01-01 01:30:00         0.000512     -0.0189             1.55
#>  5 2014-01-01 02:00:00         0.000510     -0.0188             1.55
#>  6 2014-01-01 02:30:00         0.000509     -0.0186             1.54
#>  7 2014-01-01 03:00:00         0.000510     -0.0187             1.54
#>  8 2014-01-01 03:30:00         0.000509     -0.0187             1.53
#>  9 2014-01-01 04:00:00         0.000521     -0.0195             1.54
#> 10 2014-01-01 04:30:00         0.000505     -0.0184             1.52
#> # ... with 2,822 more rows, and 3 more variables: `WorkDay_0/trig(48,
#> #   16)` <dbl>, `WorkDay_1/poly(1)` <dbl>, `WorkDay_1/trig(48, 16)` <dbl>
```

The tools made available by *fasster* are designed to integrate
seamlessly with the tidyverse of packages, enabling familiar data
manipulation and visualisation capabilities.

### Forecasting

*fasster* conforms to the object structure from the *forecast* package,
allowing common visualisation and analysis tools to be applied on
FASSTER models.

``` r
library(fable)

fit %>% 
  forecast(h=24) %>%
  autoplot
```

![](man/figure/forecast-1.png)

Future index values are automatically produced and used where necessary
in the model specification. If additional information is required by the
model (such as `WorkDay` and `Temperature`)

``` r
elec_ts <- tsibbledata::elecdemand %>%
  filter(index >= lubridate::ymd("2014-03-01"),
         index < lubridate::ymd("2014-04-01")) %>% 
  select(-Demand)
elec_fit %>% 
  forecast(newdata = elec_ts) %>% 
  autoplot
```

![](man/figure/complex_fc-1.png)

------------------------------------------------------------------------

Please note that this project is released with a [Contributor Code of
Conduct](.github/CODE_OF_CONDUCT.md). By participating in this project
you agree to abide by its terms.
