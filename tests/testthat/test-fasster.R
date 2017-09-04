library(fpp2)
context("fasster")

test_that("Test grouping", {
  fit1 <- elecdemand %>%
    fasster(Demand ~  trig(24) + Temperature)
  fit2 <- elecdemand %>%
    fasster(Demand ~ WorkDay %G% (trig(24) + Temperature))

  USAccDeaths %>%
    data.frame() %>%
    fasster(`.` ~ poly(1) + trig(12)) %>%
    forecast.fasster() %>%
    autoplot()
  ## 902177.7
  ## 239660.5
  ## 266374
})
