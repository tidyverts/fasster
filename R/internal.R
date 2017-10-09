#' @importFrom utils tail.matrix
tail.ts <- function (x, n = 6L, ...)
{
  tspx <- tsp(x)
  if (NCOL(x) > 1)
    hx <- ts(tail.matrix(as.matrix(x), n = n, ...),
             end = tspx[2], frequency = tspx[3])
  else if ((length(x) + n) > 0)
    hx <- ts(tail(c(x), n = n, ...), end = tspx[2], frequency = tspx[3])
  else hx <- numeric(0)
  return(hx)
}
