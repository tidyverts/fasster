enclass <- function(x, subclass, ...){
  structure(x, c(subclass, setdiff(class(.), subclass)), ...)
}
