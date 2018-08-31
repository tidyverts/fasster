#' @importFrom dlm dlmSvd2var
#' @importFrom utils tail
#' @export
forecast.FASSTER <- function(object, data, newdata = NULL, ...){
  if(!is_regular(newdata)){
    abort("Forecasts must be regularly spaced")
  }
  h <- NROW(newdata)

  mod <- object$"dlm_future"

  # Build model on newdata
  specials <- child_env(caller_env())
  specials <- new_specials_env(
    !!!fasster_specials,
    .env = specials,
    .bury = FALSE,
    .vals = list(
      .data = newdata,
      .specials = specials
    )
  )

  X <- parse_model_rhs(model_rhs((object%@%"fable")$model), data = newdata, specials = specials)$specials %>%
    unlist(recursive = FALSE) %>%
    reduce(`+`) %>%
    .$X

  fit <- mod

  p <- length(mod$m0)
  m <- nrow(mod$FF)
  a <- rbind(mod$m0, matrix(0, h, p))
  R <- vector("list", h + 1)
  R[[1]] <- mod$C0
  f <- matrix(0, h, m)
  Q <- vector("list", h)
  for (it in 1:h) {
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

  se <- sqrt(unlist(Q))

  fablelite::construct_fc(newdata, f, se, new_fcdist(qnorm, c(f), sd = se, abbr = "N"))
}
