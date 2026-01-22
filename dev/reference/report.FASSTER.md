# Report on a FASSTER model

Prints a detailed report of the estimated variance parameters for a
FASSTER model. This includes the state noise variances (W) for each
model component and the observation noise variance (V).

## Usage

``` r
# S3 method for class 'FASSTER'
report(object, ...)
```

## Arguments

- object:

  A FASSTER model object.

- ...:

  Additional arguments (currently unused).

## Value

Invisibly returns NULL. Called for its side effect of printing the
variance report to the console.

## Details

The report displays:

- State noise variances (W): The variance of the random innovations for
  each state component, grouped by model term.

- Observation noise variance (V): The variance of the measurement error.

## Examples

``` r
library(tsibble)
fit <- as_tsibble(mdeaths) |>
  model(FASSTER(value ~ trend(1) + fourier(12)))

# Print variance report
report(fit)
#> Series: value 
#> Model: FASSTER 
#> 
#> Estimated variances:
#>  State noise variances (W):
#>   fourier(12)
#>    5.8919e-15 9.9142e-15 1.0420e-14 2.5508e-14 4.4283e-14 3.0217e-14 3.1047e-14 2.6410e-14 2.8499e-14 4.3367e-14 9.1290e-15
#>   trend(1)
#>    1.9118e+03
#> 
#>  Observation noise variance (V):
#>   1.1359e+04
```
