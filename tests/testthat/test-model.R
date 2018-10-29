context("test-model.R")

test_that("Test fasster modelling", {
  expect_s3_class(USAccDeaths_fit, "mdl_df")
  expect_output(print(USAccDeaths_fit), "FASSTER", fixed = TRUE)
  expect_output(summary(USAccDeaths_fit), "State noise variances (W):", fixed = TRUE)
  expect_output(summary(USAccDeaths_fit), "Observation noise variance (V):", fixed = TRUE)
  expect_output(print(USAccDeaths_fit[["model"]][[1]]), "value ~ poly(1) + seas(12)", fixed = TRUE)

  expect_equivalent(USAccDeaths_fit[["model"]][[1]]$dlm$FF, c(1,1, rep(0, 10)))
  expect_equivalent(USAccDeaths_fit[["model"]][[1]]$dlm$V, 8476.284)
  expect_equivalent(mean(USAccDeaths_fit[["model"]][[1]]$dlm$C0), 0.04639303)
})
