#' @inherit ggplot2::autoplot
#' @inheritParams forecast::autoplot.ets
#'
#' @importFrom ggplot2 fortify ggplot aes_ geom_line facet_grid xlab ylab ggtitle
#' @importFrom tsibble index gather
#' @export
autoplot.FASSTER <- function(object, range.bars = FALSE, ...) {
  plot_data <- components(object) %>%
    mutate(!!!set_names(exprs(object$fitted + object$residuals, object$fitted), c("Response", "Fitted")))
  index <- index(plot_data)
  suppressWarnings(plot_data <- plot_data %>%
                     select(expr_text(index(plot_data)), "Fitted", "Response", tidyselect::everything()) %>%
                     gather(".key", ".value", -!!index, factor_key = TRUE))
  plot_data %>%
    ggplot(aes_(x = index, y = ~.value)) +
    geom_line() +
    facet_grid(.key ~ ., scales="free_y", switch="y") +
    xlab(paste0("Time (Interval: ", format(interval(plot_data)), ")")) +
    ylab(NULL) +
    ggtitle("Components from FASSTER method")
}



#' Time-series plot of fitted and observed values
#'
#' Plots fitted and observed values with autoplot methods for \code{tbl_ts} using ggplot2.
#'
#' @param object A mable
#' @param ... Additional arguments to be passed to \code{\link[fable]{autoplot.tbl_ts}}
#'
#' @export
#' @importFrom ggplot2 autoplot ggplot aes geom_line facet_grid xlab ylab ggtitle
ggfitted <- function(object, ...){
  .Deprecated()
  if(inherits(object, "mable")){
    fable::getResponse(object) %>%
      left_join(fitted(object), by = expr_text(index(.))) %>%
      gather("type", "value") %>%
      autoplot(var=value) +
      ggtitle(paste0("Fitted values from ", pillar_shaft(object$model)[[1]]))
  }
  else{
    stop("This object is not supported")
  }
}
