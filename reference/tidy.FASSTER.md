# Extract coefficients from a FASSTER model

Obtains the mean and variance of the estimated initial states from a
FASSTER model. Values in the `estimate` column are contains the mean,
and the `std.error` column contains the standard deviation of the
initial states.

## Usage

``` r
# S3 method for class 'FASSTER'
tidy(x, ...)
```

## Arguments

- x:

  An object containing a FASSTER model.

- ...:

  Unused.

## Value

A tibble with three columns:

- term:

  The name of each state variable in the model.

- estimate:

  The mean of the estimated initial state for each term.

- std.error:

  The standard deviation of the estimated initial state for each term.
