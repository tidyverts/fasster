#' Fast Additive Switching of Seasonality, Trend and Exogenous Regressors
#'
#' Implementation of the FASSTER model for forecasting time series
#' with multiple seasonalities using switching states.
#'
#' \tabular{ll}{ Package: \tab fasster\cr Type: \tab Package\cr License: \tab
#' GPL3\cr LazyLoad: \tab yes\cr }
#'
#' @docType package
#' @name fasster-package
#' @author Mitchell O'Hara-Wild
#'
#' Maintainer: mail@@mitchelloharawild.com
#' @keywords package
NULL

#' @rawNamespace import(stats, except = c(simulate, filter, lag))
#' @import rlang
#' @import dlm
#' @import fablelite
#' @import tsibble
NULL

globalVariables(".")
