#' @importFrom dlm dlmSvd2var
#' @importFrom forecast forecast
#' @importFrom dplyr tibble
#' @export
forecast.fasster <- function(object, newdata=NULL, h=floor(NROW(object$x)/10), level=c(80, 95), lambda = object$lambda, biasadj = NULL) {
  mod <- object$model_future
  ytsp <- tsp(object$x)

  if (is.null(lambda)) {
    biasadj <- FALSE
  }
  else {
    if (is.null(biasadj)) {
      biasadj <- attr(lambda, "biasadj")
    }
    if (!is.logical(biasadj)) {
      warning("biasadj information not found, defaulting to FALSE.")
      biasadj <- FALSE
    }
  }

  if(!is.null(newdata)){
    # Build model on newdata
    model_struct <- formula_parse_groups(object$formula)
    dlmModel <- build_FASSTER_group(model_struct, newdata) %>%
      ungroup_struct()
    X <- dlmModel$X
  }
  else{
    X <- NULL
  }

  fit <- mod

  nAhead <- if(!is.null(mod$X)){
    if(is.null(X)){
      stop("X must be given")
    }
    NROW(X)
  }
  else{
    h
  }

  p <- length(mod$m0)
  m <- nrow(mod$FF)
  a <- rbind(mod$m0, matrix(0, nAhead, p))
  R <- vector("list", nAhead + 1)
  R[[1]] <- mod$C0
  f <- matrix(0, nAhead, m)
  Q <- vector("list", nAhead)
  for (it in 1:nAhead) {
    # Time varying components
    XFF <- mod$FF
    XFF[mod$JFF != 0] <- X[it, mod$JFF]
    XV <- mod$V
    XV[mod$JV != 0] <- X[it, mod$JV]
    XGG <- mod$GG
    XGG[mod$JGG != 0] <- X[it, mod$JGG]
    XW <- mod$W
    XW[mod$JW != 0] <- X[it, mod$JW]

    # Kalman Filter Forecast
    a[it + 1, ] <- XGG %*% a[it, ]
    R[[it + 1]] <- XGG %*% R[[it]] %*% t(XGG) + XW
    f[it, ] <- XFF %*% a[it + 1, ]
    Q[[it]] <- XFF %*% R[[it + 1]] %*% t(XFF) + XV
  }
  a <- a[-1,, drop = FALSE]
  R <- R[-1]

  Q <- unlist(Q)
  lower <- matrix(NA, ncol = length(level), nrow = nAhead)
  upper <- lower
  for (i in seq_along(level)){
    qq <- qnorm(0.5 * (1 + level[i] / 100))
    lower[, i] <- f - qq * sqrt(Q)
    upper[, i] <- f + qq * sqrt(Q)
  }

  if (!is.null(lambda)) {
    f <- InvBoxCox(f, lambda, biasadj, ans)
    lower <- InvBoxCox(lower, lambda)
    upper <- InvBoxCox(upper, lambda)
  }

  colnames(upper) <- paste("Upper", level, sep="_")
  colnames(lower) <- paste("Lower", level, sep="_")

  seq_by <- unclass(interval(object$x))
  if(length(seq_by) > 1){
    seq_by <- object$x %>% pull(!!index(object$x)) %>% diff %>% median
  }
  else if(names(seq_by) != "unit"){
    seq_by <- paste0(seq_by, " ", names(seq_by), "s")
  }

  fcIndex <- seq(max(object$x %>% pull(!!index(object$x))), length.out = nAhead + 1, by = seq_by)
  class(fcIndex) <- object$x %>% pull(!!index(object$x)) %>% class

  tsibble_index <- tibble(!!quo_text(index(object$x)) := fcIndex) %>%
    tail(-1) %>%
    as_tsibble(index = !!index(object$x))

  ans <- structure(list(model = object, x = object$x, fitted = fit$f,
                        forecast = tsibble_index %>% bind_cols(PointForecast=c(f), as_tibble(upper), as_tibble(lower)),
                        level = level,
                        method = "FASSTER", series = object$series, residuals = residuals(object)),
                   class = c("tbl_forecast", "forecast"))

  return(ans)
}
