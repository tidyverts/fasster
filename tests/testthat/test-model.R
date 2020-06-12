context("test-model.R")

test_that("Test fasster modelling", {
  expect_s3_class(USAccDeaths_fit, "mdl_df")
  expect_output(print(USAccDeaths_fit), "FASSTER", fixed = TRUE)
  expect_output(report(USAccDeaths_fit), "State noise variances (W):", fixed = TRUE)
  expect_output(report(USAccDeaths_fit), "Observation noise variance (V):", fixed = TRUE)
  expect_output(report(USAccDeaths_fit), "season(12)", fixed = TRUE)

  expect_equivalent(USAccDeaths_fit[["fasster"]][[1]]$fit$dlm$FF, c(1, rep(0, 10), 1))
  expect_equivalent(USAccDeaths_fit[["fasster"]][[1]]$fit$dlm$V, 8476.284)
  expect_equivalent(mean(USAccDeaths_fit[["fasster"]][[1]]$fit$dlm$C0), 0.04639303)
})
