library(fpp2)
context("fasster")

test_that("Test grouping", {
  fit1 <- elecdemand %>%
    fasster(Demand ~  trig(48) + Temperature)
  fit2 <- tail(elecdemand, 4500) %>%
    fasster(Demand ~ WorkDay %S% (trig(48) + poly(1)))

  USAccDeaths %>%
    data.frame() %>%
    fasster(`.` ~ poly(1) + trig(12)) %>%
    forecast() %>%
    autoplot()

  data.frame(taylor) %>%
    mutate(Weekday = rep(c(rep(1, 48*5), rep(0, 48*2)), length.out = NROW(taylor))) %>%
    fasster(taylor ~ Weekday %S% (poly(1) + trig(48, 16))) %>%
    forecast(newdata = data.frame(Weekday = rep(c(rep(1, 48*5), rep(0, 48*2)), length.out = 48*7))) %>%
    autoplot()
})

test_that("Test heuristic fit rates", {
  USAccDeaths %>%
    data.frame() %>%
    fasster(`.` ~ poly(1) + trig(12, 3)) -> a

  vt_vars_i <- numeric(length(a$optimFit$vt) - 4)
  for(i in 5:length(a$optimFit$vt)){
    vt_vars_i[i-4] <- var(a$optimFit$vt[1:i])
  }

  var(fit1$optimFit$vt)
  fit1$optimFit$vt %>% tail(500) %>% var
})


test_that("Test series length on forecasts", {
  fit1 <- elecdemand %>%
    fasster(Demand ~  trig(24) + Temperature)
  fit2 <- elecdemand %>%
    fasster(Demand ~ WorkDay %S% (trig(48, 20) + poly(1)), include=1000)
  fc2 <- forecast(fit2, newdata = tail(elecdemand, 300))
  fc2 %>% autoplot()
})

test_that("Line profiling"){
  library(lineprof)
  lineprofOut <- lineprof(fit2 <- elecdemand %>% fasster(Demand ~ WorkDay %S% (trig(48) + poly(1))))
}
