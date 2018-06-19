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
  p <- plot_data %>%
    ggplot(aes_(x = index, y = ~.value)) +
    geom_line() +
    facet_grid(.key ~ ., scales="free_y", switch="y") +
    xlab(paste0("Time (Interval: ", format(interval(plot_data)), ")")) +
    ylab(NULL) +
    ggtitle("Decomposition by FASSTER method")

  if(is.null(range.bars)){
    range.bars <- is.null(object$lambda)
  }
  if(range.bars){
    stop("Currently not supported")
    xrange <- range(plot_data %>% pull(!!index))
    rangebar_data <- plot_data %>%
      group_by(!!as_quosure(sym(".key"))) %>%
      summarise(!!"min" := min(!!as_quosure(sym(".value"))), !!"max" := max(!!as_quosure(sym(".value")))) %>%
      mutate(!!"middle" := mean(c(min, max)), !!"length" := !!as_quosure(sym("max")) - !!as_quosure(sym("min")),
             !!"width" := (1/64)*diff(xrange),
             !!"left" := xrange[2] + !!as_quosure(sym("width")), !!"right" := xrange[2] + !!as_quosure(sym("width"))*2,
             !!"top" := !!as_quosure(sym("middle")) + !!as_quosure(sym("length"))/2,
             !!"bottom" := !!as_quosure(sym("middle")) - !!as_quosure(sym("length"))/2,
             !!".value" := NA)
    p <- p + ggplot2::geom_rect(ggplot2::aes_(xmin = ~left, xmax = ~right, ymax = ~top, ymin = ~bottom),
                                data=rangebar_data, fill="gray75", colour="black", size=1/3)
  }
  p
}
