context("test-forecast.R")

test_that("Test fasster forecasting", {
  fc1 <- forecast(USAccDeaths_fit)
  fc1_sum <- summary(fc1)
  expect_equal(NROW(fc1_sum), 24)

  fc2 <- forecast(USAccDeaths_fit, h = 3)
  fc2_sum <- summary(fc2)
  expect_equal(NROW(fc2_sum), 3)

  future_idx <- tsibble::yearmonth(as.Date(USAccDeaths$index) + months(NROW(USAccDeaths)))
  fc3 <- forecast(USAccDeaths_fit, newdata = tsibble::as_tsibble(list(index = future_idx), index = index))
  fc3_sum <- summary(fc3)
  expect_equal(NROW(fc3_sum), NROW(USAccDeaths))

  expect_equal(fc1_sum$mean[1:3], fc2_sum$mean[1:3])
  expect_equal(fc1_sum$mean[1:3], fc3_sum$mean[1:3])
})
