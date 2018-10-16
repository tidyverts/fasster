context("test-stream.R")

test_that("streaming works", {
  fit_p1 <- tsibbledata::UKLungDeaths %>%
    head(-12) %>%
    FASSTER(mdeaths ~ fdeaths + poly(1) + trig(12))

  fit_p2 <- fit_p1 %>%
    stream(tsibbledata::UKLungDeaths %>% tail(12))

  expect_equal(fitted(fit_p1), head(fitted(fit_p2), -12))
})
