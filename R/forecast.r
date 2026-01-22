#' Forecast a FASSTER model
#'
#' Produces forecasts from a trained FASSTER model using the Kalman filter
#' framework. This method generates point forecasts and prediction intervals
#' by propagating the state space forward through time.
#'
#' @param object A trained FASSTER model object.
#' @param new_data A tsibble containing future time points to forecast. Must be
#'   regularly spaced and contain any required exogenous regressors.
#' @param specials A list of special formulations generated from the model
#'   formula, used to construct the design matrix for time-varying components.
#' @param ... Additional arguments (currently unused).
#'
#' @return A distribution vector (from the distributional package) containing
#'   normal distributions with forecasted means and standard errors for each
#'   future time point. This integrates with fable's forecast distribution
#'   structure.
#'
#' @details
#' The forecast method implements a Kalman filter to propagate the state space
#' model forward in time:
#' \enumerate{
#'   \item For each forecast horizon, constructs time-varying system matrices
#'     (FF, GG, V, W) using exogenous variables from \code{new_data}
#'   \item Computes the state forecast: \eqn{a_{t+1} = G_t a_t}
#'   \item Computes the state covariance: \eqn{R_{t+1} = G_t R_t G_t' + W_t}
#'   \item Computes the observation forecast: \eqn{f_t = F_t a_{t+1}}
#'   \item Computes the forecast variance: \eqn{Q_t = F_t R_{t+1} F_t' + V_t}
#' }
#'
#' The method handles switching components by matching exogenous variables
#' in \code{new_data} with the model's design matrix, adding zero columns
#' for any missing levels.
#'
#' @importFrom dlm dlmSvd2var
#' @importFrom utils tail
#' @export
forecast.FASSTER <- function(object, new_data, specials = NULL, ...){
  if(!is_regular(new_data)){
    abort("Forecasts must be regularly spaced")
  }
  h <- NROW(new_data)

  mod <- object$"dlm_future"

  X <- unlist(specials, recursive = FALSE)
  X <- reduce(X, `+`)$X

  # Add missing levels of switching
  if(!is.null(mod$X)) {
    X_match <- match(colnames(mod$X), colnames(X))
    X_missing <- is.na(X_match)
    X_extra <- matrix(0, nrow = nrow(X), ncol = sum(X_missing), dimnames = list(NULL, colnames(mod$X)[X_missing]))
    X_match[X_missing] <- seq_len(sum(X_missing)) + sum(!X_missing)
    X <- cbind(X, X_extra)[,X_match,drop=FALSE]
  }

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

  distributional::dist_normal(c(f), se)
}
