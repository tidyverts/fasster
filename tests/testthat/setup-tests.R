context("setup-tests.R")

UKLungDeaths <- as_tsibble(cbind(mdeaths, fdeaths), gather = FALSE)
USAccDeaths <- as_tsibble(USAccDeaths)

USAccDeaths_fit <- USAccDeaths %>%
  model(fasster = fasster(value ~ poly(1) + seas(12)))
