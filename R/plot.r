#' @inherit ggplot2::fortify
#'
#' @importFrom ggplot2 fortify
#' @importFrom dplyr as_tibble bind_cols
#' @export
fortify.fasster <- function(model, data=NULL, ...) {
  states <- model$model$FF %>%
    colnames() %>%
    factor %>%
    spread_groups() %>%
    t

  states <- (states * model$model$FF[rep(1,NROW(states)),]) %*% t(model$states) %>% t %>% as_tibble

  if(!is.null(model$lambda)){
    warning("State decompositions are BoxCox transformed")
  }

  model$x %>%
    bind_cols(Fitted = fitted(model), States = states)
}

#' @inherit ggplot2::fortify
#'
#' @importFrom ggplot2 fortify
#' @importFrom dplyr as_tibble bind_cols
#' @export
fortify.tbl_forecast <- function(model, data=NULL, ...) {
  model$forecast
}


#' @inherit ggplot2::autoplot
#' @inheritParams forecast::autoplot.ets
#'
#' @importFrom ggplot2 fortify ggplot aes_ geom_line facet_grid xlab ylab ggtitle
#' @importFrom tsibble index
#' @importFrom tidyr gather
#' @importFrom rlang as_quosure sym
#' @export
autoplot.fasster <- function(object, range.bars = FALSE, ...) {
  plot_data <- fortify(object)
  index <- index(plot_data)
  suppressWarnings(plot_data <- plot_data %>%
    gather(".key", ".value", -!!index, factor_key = TRUE))
  p <- plot_data %>%
    ggplot(aes_(x = index, y = ~.value)) +
    geom_line() +
    facet_grid(.key ~ ., scales="free_y", switch="y") +
    xlab(paste0("Time (Interval: ", format(interval(object$x)), ")")) +
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
#' @importFrom ggplot2 autoplot ggplot aes geom_line facet_grid xlab ylab ggtitle
#' @importFrom dplyr bind_cols
#' @importFrom tsibble index interval
ggfitted <- function(object, ...){
  if(is.ts(object$x)){
    autoplot(cbind(getResponse(object), fitted(object)), ...)
  }
  else if(is_tsibble(object$x)){
    object$x %>%
      mutate(!!"Response" := getResponse(object),
             !!"Fitted" := fitted(object)) %>%
      ggplot(aes_(x = index(object$x))) +
      geom_line(aes_(y=~Response, colour=~"Response")) +
      geom_line(aes_(y=~Fitted, colour=~"Fitted")) +
      xlab(paste0("Time (Interval: ", format(interval(object$x)), ")")) +
      ylab(quo_text(object$series)) +
      ggtitle(paste0("Fitted values from ", object$method))
  }
  else{
    stop("This model is not supported")
  }
}

#' @inherit forecast::autoplot.forecast
#' @export
#' @importFrom ggplot2 ggplot aes_ xlab ylab ggtitle
#' @importFrom tsibble index
autoplot.tbl_forecast <- function(object, ...){
  object$x %>%
    ggplot(aes_(x = index(object$x), y = object$series)) +
    geom_line() +
    autolayer(object, ...) +
    xlab(paste0("Time (Interval: ", format(interval(object$x)), ")")) +
    ylab(quo_text(object$series)) +
    ggtitle(paste0("Forecasts from ", object$method))
}

#' @inherit forecast::autolayer.forecast
#' @export
#' @importFrom forecast autolayer geom_forecast
#' @importFrom tidyr gather separate spread
#' @importFrom dplyr bind_rows mutate
#' @importFrom rlang UQE sym
autolayer.tbl_forecast <- function(object, series = NULL, PI = TRUE, showgap = TRUE, ...){
  PI <- PI & !is.null(object$level)
  fc_data <- fortify(object) %>%
    gather(".key", ".value", -!!index(object$forecast))

  fc_point <- fc_data %>%
    dplyr::filter(UQE(sym(".key")) == "PointForecast")
  fc_interval <- fc_data %>%
                dplyr::filter(UQE(sym(".key")) != "PointForecast") %>%
                separate(".key", c("Type", "Level")) %>%
                spread("Type", ".value")
  fc_data <- bind_rows(fc_point, fc_interval) %>%
    mutate(!!"Level" := as.numeric(!!as_quosure(sym("Level"))))

  mapping <- ggplot2::aes_(x = index(object$forecast), y = ~.value)
  if(!is.null(object$series)){
    fc_data <- fc_data %>%
      mutate(series = quo_text(object$series))
  }
  if(!is.null(series)){
    fc_data <- fc_data %>%
      mutate(series = series)
    mapping$colour <- quote(series)
  }
  if(PI){
    mapping$level <- quote(Level)
    mapping$ymin <- quote(Lower)
    mapping$ymax <- quote(Upper)
  }
  geom_forecast(mapping=mapping, data=fc_data, stat="identity", ...)
}
