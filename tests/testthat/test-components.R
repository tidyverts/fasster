context("test-components.R")

test_that("Test component extraction of fasster models", {
  fit_component <- components(USAccDeaths_fit)
  expect_identical(colnames(fit_component), c("index", "poly(1)", "seas(12)"))
  expect_identical(NROW(fit_component), NROW(USAccDeaths))
  expect_identical(fit_component["index"], USAccDeaths["index"])
})
