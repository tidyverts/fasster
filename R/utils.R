add_class <- function(x, new_class){
  `class<-`(x, union(new_class, class(x)))
}
