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

testaccuracy <- function(f,x,test,d,D)
{
  dx <- getResponse(f)
  if(is.data.frame(x))
  {
    responsevar <- as.character(formula(f$model))[2]
    if(is.element(responsevar, colnames(x)))
      x <- x[,responsevar]
    else
      stop("I can't figure out what data to use.")
  }
  if(is.list(f))
  {
    if(is.element("mean",names(f)))
      f <- f$mean
    else
      stop("Unknown list structure")
  }
  if(is.ts(x) & is.ts(f))
  {
    tspf <- tsp(f)
    tspx <- tsp(x)
    start <- max(tspf[1],tspx[1])
    end <- min(tspf[2],tspx[2])
    # Adjustment to allow for floating point issues
    start <- min(start,end)
    end <- max(start,end)
    f <- window(f,start=start,end=end)
    x <- window(x,start=start,end=end)
  }
  n <- length(x)
  if(is.null(test))
    test <- 1:n
  else if(min(test) < 1 | max(test) > n)
  {
    warning("test elements must be within sample")
    test <- test[test >= 1 & test <= n]
  }

  ff <- f
  xx <- x

  # Check length of f
  if(length(f) < n)
    stop("Not enough forecasts. Check that forecasts and test data match.")

  error <- (xx-ff[1:n])[test]
  pe <- error/xx[test] * 100

  me <- mean(error, na.rm=TRUE)
  mse <- mean(error^2, na.rm=TRUE)
  mae <- mean(abs(error), na.rm=TRUE)
  mape <- mean(abs(pe), na.rm=TRUE)
  mpe <-  mean(pe, na.rm=TRUE)
  out <- c(me,sqrt(mse),mae,mpe,mape)
  names(out) <- c("ME","RMSE","MAE","MPE","MAPE")

  # Compute MASE if historical data available
  if(!is.null(dx))
  {
    tspdx <- tsp(dx)
    if (!is.null(tspdx)) {
      if (D > 0) { # seasonal differencing
        nsd <- diff(dx, lag = round(tspdx[3L]), differences = D)
      } else { # non seasonal differencing
        nsd <- dx
      }
      if (d > 0) {
        nd <- diff(nsd, differences = d)
      } else {
        nd <- nsd
      }
      scale <- mean(abs(nd), na.rm = TRUE)
    } else { # not time series
      scale <- mean(abs(dx-mean(dx, na.rm=TRUE)),na.rm=TRUE)
    }
    mase <- mean(abs(error/scale), na.rm=TRUE)
    out <- c(out,mase)
    names(out)[length(out)] <- "MASE"
  }

  # Additional time series measures
  if(!is.null(tsp(x)) & n>1)
  {
    fpe <- (c(ff[2:n])/c(xx[1:(n-1)]) - 1)[test-1]
    ape <- (c(xx[2:n])/c(xx[1:(n-1)]) - 1)[test-1]
    theil <- sqrt(sum((fpe - ape)^2, na.rm=TRUE)/sum(ape^2, na.rm=TRUE))
    if(length(error) > 1)
      r1 <- acf(error,plot=FALSE,lag.max=2,na.action=na.pass)$acf[2,1,1]
    else
      r1 <- NA
    nj <- length(out)
    out <- c(out,r1,theil)
    names(out)[nj+(1:2)] <- c("ACF1","Theil's U")
  }

  return(out)
}


trainingaccuracy <- function(f,test,d, D)
{
  # Make sure x is an element of f when f is a fitted model rather than a forecast
  #if(!is.list(f))
  #  stop("f must be a forecast object or a time series model object.")
  dx <- getResponse(f)
  if(is.element("splineforecast",class(f)))
    fits <- f$onestepf
  else
    fits <- fitted(f)    # Don't use f$resid as this may contain multiplicative errors.

  res <- dx-fits
  n <- length(res)
  if(is.null(test))
    test <- 1:n
  if(min(test) < 1 | max(test) > n)
  {
    warning("test elements must be within sample")
    test <- test[test >= 1 & test <= n]
  }

  tspdx <- tsp(dx)

  res <- res[test]
  dx <- dx[test]
  pe <- res/dx * 100 # Percentage error

  me <- mean(res,na.rm=TRUE)
  mse <- mean(res^2,na.rm=TRUE)
  mae <- mean(abs(res),na.rm=TRUE)
  mape <- mean(abs(pe),na.rm=TRUE)
  mpe <-  mean(pe,na.rm=TRUE)
  out <- c(me,sqrt(mse),mae,mpe,mape)
  names(out) <- c("ME","RMSE","MAE","MPE","MAPE")

  # Compute MASE if historical data available
  if(!is.null(dx))
  {
    if (!is.null(tspdx)) {
      if (D > 0) { # seasonal differencing
        nsd <- diff(dx, lag = round(tspdx[3L]), differences = D)
      } else { # non seasonal differencing
        nsd <- dx
      }
      if (d > 0) {
        nd <- diff(nsd, differences = d)
      } else {
        nd <- nsd
      }
      scale <- mean(abs(nd), na.rm = TRUE)
    } else { # not time series
      scale <- mean(abs(dx-mean(dx, na.rm=TRUE)),na.rm=TRUE)
    }
    mase <- mean(abs(res/scale), na.rm=TRUE)
    out <- c(out,mase)
    names(out)[length(out)] <- "MASE"
  }

  # Additional time series measures
  if(!is.null(tspdx))
  {
    if(length(res) > 1)
      r1 <- acf(res,plot=FALSE,lag.max=2,na.action=na.pass)$acf[2,1,1]
    else
      r1 <- NA
    nj <- length(out)
    out <- c(out,r1)
    names(out)[nj+1] <- "ACF1"
  }

  return(out)
}

#' @importFrom dplyr pull select
col2rowname <- function(.data, col){
  rownames(.data) <- .data %>% pull(!!col)
  .data %>% select(-!!col)
}
