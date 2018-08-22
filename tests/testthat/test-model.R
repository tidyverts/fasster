context("test-model.R")

test_that("Test fasster modelling", {
  fit1 <- USAccDeaths %>%
    fasster(value ~ poly(1) + seas(12))
  expect_s3_class(fit1, "mable")
  expect_output(print(fit1), "FASSTER", fixed = TRUE)
  expect_output(summary(fit1), "State noise variances (W):", fixed = TRUE)
  expect_output(summary(fit1), "Observation noise variance (V):", fixed = TRUE)
  expect_output(print(fit1[["model"]][[1]]), "value ~ poly(1) + seas(12)", fixed = TRUE)

  expect_equivalent(fit1[["model"]][[1]]$dlm$FF, c(1,1, rep(0, 10)))
  expect_equivalent(fit1[["model"]][[1]]$dlm$V, 8476.284)
  expect_equivalent(mean(fit1[["model"]][[1]]$dlm$C0), 0.04639303)
})
