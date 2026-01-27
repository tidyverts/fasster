# Stream new data through a FASSTER model

Extends a fitted FASSTER model by filtering new observations through the
existing state space model. The model's states and parameters are
updated sequentially as new data arrives, allowing for online learning
without refitting from scratch.

## Usage

``` r
# S3 method for class 'FASSTER'
stream(object, new_data, specials = NULL, ...)
```

## Arguments

- object:

  A fitted FASSTER model object

- new_data:

  A tsibble containing new observations to stream through the model

- specials:

  A list of special terms (switching variables, etc.) parsed from the
  model formula

- ...:

  Additional arguments (currently unused)

## Value

An updated FASSTER model object with:

- Extended state estimates incorporating the new data

- Updated model variance

- Appended fitted values and residuals

- Updated DLM components for future forecasting

## Details

The streaming process:

1.  Constructs the design matrix from new data

2.  Applies the Kalman filter to sequentially update states

3.  Updates model variance based on all residuals

4.  Prepares the model for subsequent forecasting or streaming

## Examples

``` r
library(tsibble)
library(fasster)

# Fit initial model on training data
fit <- as_tsibble(head(USAccDeaths, -12)) |>
  model(fasster = FASSTER(value ~ trend() + season("year")))
tidy(fit)
#> # A tibble: 13 × 4
#>    .model  term               estimate std.error
#>    <chr>   <chr>                 <dbl>     <dbl>
#>  1 fasster "season(\"year\")"   -167.       1.91
#>  2 fasster "season(\"year\")"   -215.       1.92
#>  3 fasster "season(\"year\")"    278.       1.92
#>  4 fasster "season(\"year\")"    -64.0      1.91
#>  5 fasster "season(\"year\")"    991.       1.92
#>  6 fasster "season(\"year\")"   1515.       1.92
#>  7 fasster "season(\"year\")"    988.       1.92
#>  8 fasster "season(\"year\")"    264.       1.92
#>  9 fasster "season(\"year\")"   -488.       1.91
#> 10 fasster "season(\"year\")"   -695.       1.91
#> 11 fasster "season(\"year\")"  -1593.       1.93
#> 12 fasster "trend()"            9830.       2.92
#> 13 fasster "trend()"             -59.7      1.52
tail(fitted(fit), 20)
#> # A tsibble: 20 x 3 [1M]
#> # Key:       .model [1]
#>    .model     index .fitted
#>    <chr>      <mth>   <dbl>
#>  1 fasster 1976 May   9187.
#>  2 fasster 1976 Jun   9240.
#>  3 fasster 1976 Jul   9513.
#>  4 fasster 1976 Aug   9305.
#>  5 fasster 1976 Sep   8052.
#>  6 fasster 1976 Oct   8263.
#>  7 fasster 1976 Nov   7973.
#>  8 fasster 1976 Dec   7769.
#>  9 fasster 1977 Jan   7774.
#> 10 fasster 1977 Feb   7483.
#> 11 fasster 1977 Mar   7870.
#> 12 fasster 1977 Apr   7974.
#> 13 fasster 1977 May   9014.
#> 14 fasster 1977 Jun   9382.
#> 15 fasster 1977 Jul  10268.
#> 16 fasster 1977 Aug   9798.
#> 17 fasster 1977 Sep   8430.
#> 18 fasster 1977 Oct   8651.
#> 19 fasster 1977 Nov   8109.
#> 20 fasster 1977 Dec   8541.

# Stream new data through the model
fit_updated <- fit |>
  stream(as_tsibble(tail(USAccDeaths, 12)))
tidy(fit_updated)
#> # A tibble: 13 × 4
#>    .model  term               estimate std.error
#>    <chr>   <chr>                 <dbl>     <dbl>
#>  1 fasster "season(\"year\")"   -167.       1.91
#>  2 fasster "season(\"year\")"   -215.       1.92
#>  3 fasster "season(\"year\")"    278.       1.92
#>  4 fasster "season(\"year\")"    -64.0      1.91
#>  5 fasster "season(\"year\")"    991.       1.92
#>  6 fasster "season(\"year\")"   1515.       1.92
#>  7 fasster "season(\"year\")"    988.       1.92
#>  8 fasster "season(\"year\")"    264.       1.92
#>  9 fasster "season(\"year\")"   -488.       1.91
#> 10 fasster "season(\"year\")"   -695.       1.91
#> 11 fasster "season(\"year\")"  -1593.       1.93
#> 12 fasster "trend()"            9830.       2.92
#> 13 fasster "trend()"             -59.7      1.52
tail(fitted(fit_updated), 20)
#> # A tsibble: 20 x 3 [1M]
#> # Key:       .model [1]
#>    .model     index .fitted
#>    <chr>      <mth>   <dbl>
#>  1 fasster 1977 May   9014.
#>  2 fasster 1977 Jun   9382.
#>  3 fasster 1977 Jul  10268.
#>  4 fasster 1977 Aug   9798.
#>  5 fasster 1977 Sep   8430.
#>  6 fasster 1977 Oct   8651.
#>  7 fasster 1977 Nov   8109.
#>  8 fasster 1977 Dec   8541.
#>  9 fasster 1978 Jan   7963.
#> 10 fasster 1978 Feb   7300.
#> 11 fasster 1978 Mar   7756.
#> 12 fasster 1978 Apr   8058.
#> 13 fasster 1978 May   8996.
#> 14 fasster 1978 Jun   9494.
#> 15 fasster 1978 Jul  10666.
#> 16 fasster 1978 Aug   9504.
#> 17 fasster 1978 Sep   8721.
#> 18 fasster 1978 Oct   9493.
#> 19 fasster 1978 Nov   8656.
#> 20 fasster 1978 Dec   9035.
```
