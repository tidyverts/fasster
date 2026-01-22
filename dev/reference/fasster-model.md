# Fast Additive Switching of Seasonality, Trend and Exogenous Regressors

Implements FASSTER

## Usage

``` r
FASSTER(formula, include = NULL, ...)
```

## Arguments

- formula:

  An object of class "formula" (refer to 'Specials' section for usage)

- include:

  How many terms should be included to fit the model

- ...:

  Not used

## Value

Returns a mable containing the fitted FASSTER model.

## Details

The fasster model extends commonly used state space models by
introducing a switching component to the measurement equation. This is
implemented using a time-varying DLM with the switching behaviour
encoded in the measurement matrix.

## Specials

The *specials* define the model structure for `FASSTER`. The model can
include trend, seasonal, ARMA, and exogenous regressor components, with
optional switching behaviour controlled by grouping factors.

### trend

The `trend` special specifies polynomial trend components.

    trend(n, ...)

|       |                                                                                                                                                                                                               |
|-------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `n`   | The order of the polynomial trend. Use 1 for level, 2 for linear trend, etc.                                                                                                                                  |
| `...` | Additional arguments passed to [`dlm::dlmModPoly()`](https://rdrr.io/pkg/dlm/man/dlmModPoly.html). Common arguments include `dV` (observation variance) and `dW` (state variance, can be a scalar or vector). |

### season

The `season` special specifies seasonal factors using indicator
variables.

    season(period = NULL, ...)

|          |                                                                                                                                            |
|----------|--------------------------------------------------------------------------------------------------------------------------------------------|
| `period` | The seasonal period. If `NULL`, automatically detected from the data (uses the smallest frequency). Can be a number or text like "1 year". |
| `...`    | Additional arguments passed to [`dlm::dlmModSeas()`](https://rdrr.io/pkg/dlm/man/dlmModSeas.html). Common arguments include `dV` and `dW`. |

### fourier

The `fourier` special specifies seasonal components using Fourier terms
(trigonometric functions).

    fourier(period = NULL, K = floor(period/2), ...)

|          |                                                                                                                                                                        |
|----------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `period` | The seasonal period. If `NULL`, automatically detected from the data.                                                                                                  |
| `K`      | The number of Fourier terms (harmonics) to include. Maximum is `floor(period/2)`. More harmonics capture more complex seasonal patterns but increase model complexity. |
| `...`    | Additional arguments passed to [`dlm::dlmModTrig()`](https://rdrr.io/pkg/dlm/man/dlmModTrig.html).                                                                     |

### ARMA

The `ARMA` special includes autoregressive moving average components.

    ARMA(...)

|       |                                                                                                                                                                                   |
|-------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `...` | Arguments passed to [`dlm::dlmModARMA()`](https://rdrr.io/pkg/dlm/man/dlmModARMA.html). Typically includes `ar` (vector of AR coefficients) and `ma` (vector of MA coefficients). |

### xreg

The `xreg` special includes exogenous regressors in the model.

    xreg(...)

|       |                                                                                                                                                              |
|-------|--------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `...` | Bare expressions for the exogenous regressors. These are evaluated in the context of the data, so you can use transformations like `log(x)` or interactions. |

### custom

The `custom` special allows you to specify custom DLM structures.

    custom(...)

|       |                                                                                                            |
|-------|------------------------------------------------------------------------------------------------------------|
| `...` | Arguments passed to [`dlm::dlm()`](https://rdrr.io/pkg/dlm/man/dlm.html) to create a custom DLM component. |

### `%S%` (Switching operator)

The `%S%` operator creates switching models where different model
structures apply to different groups defined by a factor variable.

    group 

|         |                                                                                                                                                                         |
|---------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `group` | A factor variable (or expression that evaluates to a factor) defining the groups. Each level of the factor will have its own version of the model component on the RHS. |
| `spec`  | The model specifications to replicate for each group. This can be any combination of `trend()`, `season()`, `fourier()`, etc.                                           |

For example, `group %S% trend(1)` creates a separate level for each
value of `group`, allowing the mean to switch between groups.

### `%?%` (Conditional operator)

The `%?%` operator creates conditional models where a component is only
active when a logical condition is TRUE.

    condition 

|             |                                                                                               |
|-------------|-----------------------------------------------------------------------------------------------|
| `condition` | A logical variable or expression. The model component will only contribute when this is TRUE. |
| `spec`      | The model component to conditionally include.                                                 |

For example, `(year > 2000) %?% trend(1)` includes a level component
only for observations after 2000.

## Heuristic

The model parameters are estimated using the following heuristic:

1.  Filter the data using the specified model with non-zero state
    variances

2.  Obtain smoothed states \\(\theta^{(s)}t=\theta_t\|D_T)\\ to
    approximate correct behaviour

3.  The initial state parameters taken from the first smoothed state:
    \\m_0=E(\theta^{(s)}\_0)\\, \\C_0=Var(\theta^{(s)}\_0)\\

4.  Obtain state noise variances from the smoothed variance of \\w_t\\:
    \\W=Var(w^{(s)}\_t)=Var(\theta^{(s)}\_t-G\theta^{(s)}\_{t-1})\\
    Obtain measurement noise variance from smoothed variance of \\v_t\\:
    \\V=Var(v^{(s)}\_t)=Var(y_t-F_t\theta^{(s)}\_t)\\

5.  Repair restricted state variances for seasonal factors and ARMA
    terms

## Examples

``` r
# Basic model with trend and seasonality
cbind(mdeaths, fdeaths) %>%
  as_tsibble(pivot_longer = FALSE) %>%
  model(FASSTER(mdeaths ~ trend(1) + fourier(12)))
#> # A mable: 1 x 1
#>   `FASSTER(mdeaths ~ trend(1) + fourier(12))`
#>                                       <model>
#> 1                                   <FASSTER>

# Model with exogenous regressor
cbind(mdeaths, fdeaths) %>%
  as_tsibble(pivot_longer = FALSE) %>%
  model(FASSTER(mdeaths ~ fdeaths + trend(1) + fourier(12)))
#> # A mable: 1 x 1
#>   `FASSTER(mdeaths ~ fdeaths + trend(1) + fourier(12))`
#>                                                 <model>
#> 1                                             <FASSTER>

# Switching model with different trends for different periods
cbind(mdeaths, fdeaths) %>%
  as_tsibble(pivot_longer = FALSE) %>%
  model(
    FASSTER(mdeaths ~ (lubridate::year(index) > 1977) %S% trend(1) + fourier(12))
  )
#> # A mable: 1 x 1
#>   FASSTER(mdeaths ~ (lubridate::year(index) > 1977) %S% trend(1) + \n    fouri…¹
#>                                                                          <model>
#> 1                                                                      <FASSTER>
#> # ℹ abbreviated name:
#> #   ¹​`FASSTER(mdeaths ~ (lubridate::year(index) > 1977) %S% trend(1) + \n    fourier(12))`
```
