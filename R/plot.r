#' @inheritParams fortify
#'
#' @importFrom ggplot2 fortify
#' @export
fortify.dlmFiltered <- function(model, data=NULL, ...) {
  modTrigPlotData <- cbind(model$states, model$y, model$f)
  colnames(modTrigPlotData) <- c(colnames(model$states), "Data", "Fitted")
  return(modTrigPlotData)
}

#' @inheritParams autoplot
#'
#' @importFrom ggplot2 autoplot facet_grid
#' @export
autoplot.dlmFiltered <- function(object, facet=FALSE, ...) {
  modTrigPlotData <- ts(ggplot2::fortify(object))
  if (facet) {
    autoplot(modTrigPlotData, facets = TRUE, ...)
  }
  autoplot(modTrigPlotData, ...) + facet_grid(as.numeric(series %in% c("Data", "Fitted")) ~ .)
}
