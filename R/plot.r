#' @inherit ggplot2::fortify
#'
#' @importFrom ggplot2 fortify
#' @export
fortify.fasster <- function(model, data=NULL, ...) {
  modTrigPlotData <- cbind(model$states, model$x, model$fitted)
  colnames(modTrigPlotData) <- c(colnames(model$model$FF), "Data", "Fitted")
  return(modTrigPlotData)
}

#' @inherit ggplot2::autoplot
#'
#' @importFrom ggplot2 autoplot facet_grid
#' @export
autoplot.fasster <- function(object, facet=FALSE, ...) {
  modTrigPlotData <- ts(ggplot2::fortify(object))
  if (facet) {
    autoplot(modTrigPlotData, facets = TRUE, ...)
  }
  autoplot(modTrigPlotData, ...) + facet_grid(as.numeric(series %in% c("Data", "Fitted")) ~ .)
}

#' Time-series plot of fitted and observed values
#'
#' Plots fitted and observed values with autoplot methods for \code{mts} using ggplot2.
#'
#' @param object A time-series model
#' @param ... Additional arguments to be passed to \code{\link[forecast]{autoplot.mts}}
#'
#' @seealso
#' \code{\link[forecast]{autoplot.mts}}
#'
#'
#' @export
#' @importFrom forecast getResponse
#' @importFrom ggplot2 autoplot
ggfitted <- function(object, ...){
  autoplot(cbind(getResponse(object), fitted(object)), ...)
}
