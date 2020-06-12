context("test-stream.R")

test_that("streaming works", {
  UKLungDeaths <- cbind(mdeaths, fdeaths) %>% as_tsibble(pivot_longer = FALSE)
  fit_p1 <- UKLungDeaths %>%
    head(-12) %>%
    model(FASSTER(mdeaths ~ fdeaths + trend(1) + fourier(12)))

  fit_p2 <- fit_p1 %>%
    stream(UKLungDeaths %>% tail(12))

  expect_equal(fitted(fit_p1), head(fitted(fit_p2), -12))
})
