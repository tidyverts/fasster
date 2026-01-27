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
#>    2.1705e-14 3.1066e-14 1.0578e-14 1.3287e-14 9.4937e-15 1.0161e-14 9.8552e-15 2.1821e-14 1.1519e-14 6.7253e-15 6.6709e-15
#>   trend(1)
#>    1.9118e+03
#> 
#>  Observation noise variance (V):
#>   1.1359e+04
#> 
#> Initial states (m0):
#>   fourier(12):  3.1018e+02
#>   fourier(12):  4.3228e+02
#>   fourier(12): -2.6396e+00
#>   fourier(12):  1.0825e+02
#>   fourier(12):  7.9098e-01
#>   fourier(12): -6.9576e+00
#>   fourier(12):  1.6938e+00
#>   fourier(12):  4.8947e+01
#>   fourier(12):  5.1493e+00
#>   fourier(12):  2.3894e+01
#>   fourier(12):  6.0760e+00
#>   trend(1):  1.4712e+03
```
