library(fpp2)
context("fasster")

test_that("Speed test", {
  fit1 <- elecdemand %>% fasster(Demand ~  trig(24) + Temperature, heuristic = FALSE)
  fit1_optim <- elecdemand %>% fasster(Demand ~  trig(24) + Temperature)


  fit2 <- elecdemand %>% fasster(Demand ~  trig(24) + Temperature, heuristic = TRUE)


  fit2 <- elecdemand %>% fasster(Demand ~  trig(24) + Temperature, heuristic = TRUE)

})
