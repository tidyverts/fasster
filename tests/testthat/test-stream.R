context("test-stream.R")

test_that("streaming works", {
  UKLungDeaths <- as_tsibble(
    cbind(mdeaths, fdeaths), 
    pivot_longer = FALSE
  )
  
  fit_p1 <- model(
    head(UKLungDeaths, -12),
    FASSTER(mdeaths ~ fdeaths + trend(1) + fourier(12))
  )

  fit_p2 <- stream(fit_p1, tail(UKLungDeaths, 12))

  expect_equal(fitted(fit_p1), head(fitted(fit_p2), -12))
})
