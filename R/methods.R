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
  cat(paste("Call:\n", deparse(x$call), "\n\n"))
  cat("Estimated variances:\n")
  cat(" State noise variances (W):\n")
  data.frame(term = colnames(x$model$FF), W = diag(x$model$W)) %>%
    group_by(term) %>%
    summarise(W=paste(round(W,2), collapse=" ")) %>%
    transmute(Val = paste0("  ", term, "\n   ", W)) %>%
    .$Val %>%
    paste(collapse="\n") %>%
    paste0("\n\n") %>%
    cat
  cat(paste(" Observation noise variance (V):\n ", round(x$model$V,2)))
}

#' @export
summary.fasster <- function(x, ...){
  print(x)
  if(NROW(x$fitted) < NROW(x$x)){
    cat("\nApproximate training set error measures:\n")
  }
  else{
    cat("\nTraining set error measures:\n")
  }
  print(accuracy(x))
}
