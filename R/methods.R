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
    object$fitted - getResponse(x)
  }
}

#' @inherit forecast::getResponse
#'
#' @seealso
#' \code{\link[forecast]{getResponse}}
#'
#' @export
getResponse.fasster <- function(object, ...){
  object$x %>% pull(!!object$series)
}

#' @export
#' @importFrom dplyr group_by summarise transmute
print.fasster <- function(x, ...){
  cat(paste("Call:\n", deparse(x$call), "\n\n"))
  cat("Estimated variances:\n")
  cat(" State noise variances (W):\n")
  data.frame(term = colnames(x$model$FF), W = diag(x$model$W)) %>%
    group_by(term) %>%
    summarise(W=paste(format(W, digits=5, scientific=TRUE), collapse=" ")) %>%
    transmute(Val = paste0("  ", term, "\n   ", W)) %>%
    .$Val %>%
    paste(collapse="\n") %>%
    paste0("\n\n") %>%
    cat
  cat(paste(" Observation noise variance (V):\n ", format(x$model$V, digits=5, scientific = TRUE)))
}

#' @export
summary.fasster <- function(x, ...){
  print(x)
  cat("\nTraining set error measures (on included data):\n")
  print(accuracy(x))
}
