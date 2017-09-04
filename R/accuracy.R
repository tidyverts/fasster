#' @importFrom forecast accuracy
#' @export
accuracy.fasster <- function(f, x, test, ...){
  attr(f, "class") <- "ets" # TODO: Implement fasster specific accuracy function
  accuracy(f)
}
