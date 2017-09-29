<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- [![Travis-CI Build Status](https://travis-ci.org/mitchelloharawild/ggquiver.svg?branch=master)](https://travis-ci.org/mitchelloharawild/ggquiver) -->
<!-- [![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/ggquiver)](https://cran.r-project.org/package=ggquiver) -->
<!-- [![Downloads](http://cranlogs.r-pkg.org/badges/ggquiver?color=brightgreen)](https://cran.r-project.org/package=ggquiver) -->
fasster
=======

An implementation of the FASSTER (Forecasting with Additive Switching of Seasonality, Trend and Exogenous Regressors) model in R. The *fasster* package implements the FASSTER model for forecasting time-series that can handle multiple seasonal patterns using model switching. It prioritizes flexibility, computational speed and accuracy to provide convenient tools for modelling, predicting and understanding high frequency time-series.

*fasster* builds upon the *dlm* package to create a user-friendly interface for creating switching

Installation
------------

<!-- The **stable** version can be installed from CRAN: -->
<!-- ```{r, eval = FALSE} -->
<!-- install.packages("ggquiver") -->
<!-- ``` -->
The **development** version can be installed from GitHub using:

``` r
# install.packages("devtools")
devtools::install_github("mitchelloharawild/fasster")
```

Usage
-----

*fasster* allows flexible model specification by allowing the user to specify the model structure with standard formula conventions.

``` r
fasster(fdeaths ~ mdeaths) %>% ggfitted
```

![](man/figure/xreg-1.png)

Commonly used state space components can be added using the following convenience functions:

-   `poly(n)` to include an n-th order polynomial
-   `seas(s)` to include a seasonal factor of frequency s
-   `trig(s, q)` to include seasonal fourier terms of frequency s with q harmonics
-   `arma(ar, ma)` to include an ARMA term (where ar and ma are vectors of coefficients)
-   Exogenous regressors can be added by referring to their name

For example, to create a model with trend and monthly seasonality, you can use:

``` r
fit <- fasster(USAccDeaths ~ poly(1) + trig(12))
fit %>% ggfitted
```

![](man/figure/component-1.png)

*fasster* conforms to the object structure from the *forecast* package, allowing common visualisation and analysis tools to be applied on FASSTER models.

``` r
library(forecast)
fit %>% accuracy
#>                     ME     RMSE      MAE        MPE     MAPE      MASE
#> Training set -36.11544 323.7525 238.7999 -0.4834674 2.794908 0.5461405
#>                    ACF1
#> Training set 0.07681231

fit %>% 
  forecast %>%
  autoplot
```

![](man/figure/forecast-1.png)

The tools made available by *fasster* are designed to integrate seamlessly with the tidyverse of packages, enabling familiar data manipulation and visualisation capabilities.

The interface for creating a FASSTER model introduces a new formula construct, `%G%`, known as the group operator. This allows modelling of more complex patterns such as multiple seasonality by modelling the components for each group seperately and switching between them.

``` r
tibble(taylor) %>%
  mutate(DateTime = seq(ymd_h("2000-6-5 00"), by="30 mins", length.out=length(taylor)),
         DayType = ifelse(wday(DateTime) %in% 2:6, "Weekday", "Weekend")) %>% 
  fasster(taylor ~ DayType %G% (poly(1) + trig(48, 10))) %>%
  ggfitted
```

![](man/figure/complex-1.png)
