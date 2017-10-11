#' @importFrom forecast accuracy
#' @export
accuracy.fasster <- function(f, x, test=NULL, d=NULL, D=NULL, ...){
  trainset <- (is.list(f))
  testset <- (!missing(x))
  if(testset & !is.null(test))
    trainset <- FALSE
  if(!trainset & !testset)
    stop("Unable to compute forecast accuracy measures")

  # Find d and D
  if(is.null(D) & is.null(d))
  {
    if(testset)
    {
      d <- as.numeric(frequency(x) == 1)
      D <- as.numeric(frequency(x) > 1)
    }
    else if(trainset)
    {
      if(!is.null(f$mean))
      {
        d <- as.numeric(frequency(f$mean) == 1)
        D <- as.numeric(frequency(f$mean) > 1)
      }
      else
      {
        d <- as.numeric(frequency(f$x) == 1)
        D <- as.numeric(frequency(f$x) > 1)
      }
    }
    else
    {
      d <- as.numeric(frequency(f)==1)
      D <- as.numeric(frequency(f) > 1)
    }
  }


  if(trainset)
  {
    trainout <- trainingaccuracy(f,test,d,D)
    trainnames <- names(trainout)
  }
  else
    trainnames <- NULL
  if(testset)
  {
    testout <- testaccuracy(f,x,test,d,D)
    testnames <- names(testout)
  }
  else
    testnames <- NULL
  outnames <- unique(c(trainnames,testnames))

  out <- matrix(NA,nrow=2,ncol=length(outnames))
  colnames(out) <- outnames
  rownames(out) <- c("Training set","Test set")
  if(trainset)
    out[1,names(trainout)] <- trainout
  if(testset)
    out[2,names(testout)] <- testout

  if(!testset)
    out <- out[1,,drop=FALSE]
  if(!trainset)
    out <- out[2,,drop=FALSE]
  return(out)
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
summary.fasster <- function(object, ...){
  print(object)
  cat("\nTraining set error measures (on included data):\n")
  print(accuracy(object))
}

#' @export
print.tbl_forecast <- function(x, ...){
  x$forecast %>%
    as.data.frame %>%
    col2rowname(index(x$forecast)) %>%
    print
}
