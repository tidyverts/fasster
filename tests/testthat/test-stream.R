context("test-stream.R")

test_that("streaming works", {
  fit_p1 <- tsibbledata::UKLungDeaths %>%
    head(-12) %>%
    FASSTER(mdeaths ~ fdeaths + poly(1) + trig(12)) %>%
    {.$model[[1]]}

  fit_p2 <- fit_p1 %>%
    stream(tsibbledata::UKLungDeaths %>% tail(12))

  expect_equal(fit_p1$fitted, head(fit_p2$fitted, -12))
})
