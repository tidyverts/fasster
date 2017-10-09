#' @importFrom forecast accuracy
#' @export
accuracy.fasster <- function(f, x, test, ...){
  attr(f, "class") <- "ets" # TODO: Implement fasster specific accuracy function
  accuracy(f)
}

#' @inherit stats::fitted
#' @export
fitted.fasster <- function(object, ...){
  # %TODO: Add refilter option for full set of residuals
  object$fitted
}

#' @inherit stats::residuals
#' @export
residuals.fasster <- function(object, ...){
  # %TODO: Add refilter option for full set of residuals
  if(!is.null(object$residuals)){
    object$residuals
  }
  else{
    object$fitted - object$x
  }
}

#' @inherit forecast::getResponse
#'
#' @seealso
#' \code{\link[forecast]{getResponse}}
#'
#' @export
getResponse.fasster <- function(object, ...){
  object$x
}

#' @export
print.fasster <- function(x, ...){
  cat(paste(x$method, "\n\n"))
  cat(paste("Call:\n", deparse(x$call), "\n\n"))
}
