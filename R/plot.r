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
autoplot.fasster <- function(object, ...) {
  modTrigPlotData <- ts(ggplot2::fortify(object))
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
#' @importFrom dplyr bind_cols
#' @importFrom tsibble interval
ggfitted <- function(object, ...){
  if(is.ts(object$x)){
    autoplot(cbind(getResponse(object), fitted(object)), ...)
  }
  else if(is_tsibble(object$x)){
    object$x %>%
      bind_cols(Response = getResponse(object), Fitted = fitted(object)) %>%
      ggplot(aes_(x = index(.))) +
      geom_line(aes(y=Response, colour="Response")) +
      geom_line(aes(y=Fitted, colour="Fitted")) +
      xlab(paste0("Time (Interval: ", format(interval(object$x)), ")")) +
      ylab(quo_text(object$series)) +
      ggtitle(paste0("Fitted values from ", object$method))
  }
  else{
    error("This model is not supported")
  }
}
