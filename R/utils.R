enclass <- function(x, subclass, ...){
  structure(x, class = c(subclass, setdiff(class(x), subclass)), ...)
}
