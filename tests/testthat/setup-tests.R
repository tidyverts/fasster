context("setup-tests.R")

UKLungDeaths <- as_tsibble(cbind(mdeaths, fdeaths), gather = FALSE)
USAccDeaths <- as_tsibble(USAccDeaths)
