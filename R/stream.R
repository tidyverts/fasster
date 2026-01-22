#' Stream new data through a FASSTER model
#'
#' Extends a fitted FASSTER model by filtering new observations through the
#' existing state space model. The model's states and parameters are updated
#' sequentially as new data arrives, allowing for online learning without
#' refitting from scratch.
#'
#' @param object A fitted FASSTER model object
#' @param new_data A tsibble containing new observations to stream through the model
#' @param specials A list of special terms (switching variables, etc.) parsed from the model formula
#' @param ... Additional arguments (currently unused)
#'
#' @return An updated FASSTER model object with:
#' \itemize{
#'   \item Extended state estimates incorporating the new data
#'   \item Updated model variance
#'   \item Appended fitted values and residuals
#'   \item Updated DLM components for future forecasting
#' }
#'
#' @details
#' The streaming process:
#' \enumerate{
#'   \item Constructs the design matrix from new data
#'   \item Applies the Kalman filter to sequentially update states
#'   \item Updates model variance based on all residuals
#'   \item Prepares the model for subsequent forecasting or streaming
#' }
#'
#' @examples
#' \dontrun{
#' library(tsibble)
#' library(fasster)
#'
#' # Fit initial model on training data
#' fit <- as_tsibble(head(USAccDeaths, -12)) |>
#'   model(fasster = FASSTER(value ~ trend() + season("year")))
#' tidy(fit)
#' tail(fitted(fit), 20)
#' 
#' # Stream new data through the model
#' fit_updated <- fit |>
#'   stream(as_tsibble(tail(USAccDeaths, 12)))
#' tidy(fit_updated)
#' tail(fitted(fit_updated), 20)
#' 
#' }
#'
#' @export
stream.FASSTER <- function(object, new_data, specials = NULL, ...){
  # Extend model
  X <- specials %>%
    unlist(recursive = FALSE) %>%
    reduce(`+`) %>%
    .$X

  mod <- object$dlm_future

  # Add missing levels of switching
  if(!is.null(mod$X)) {
    X_match <- match(colnames(mod$X), colnames(X))
    X_missing <- is.na(X_match)
    X_extra <- matrix(0, nrow = nrow(X), ncol = sum(X_missing), dimnames = list(NULL, colnames(mod$X)[X_missing]))
    X_match[X_missing] <- seq_len(sum(X_missing)) + sum(!X_missing)
    X <- cbind(X, X_extra)[,X_match,drop=FALSE]
  }

  est <- transmute(new_data, !!parse_expr(measured_vars(object$est)[1]))
  response <- est[[measured_vars(est)]]

  mod$X <- X

  filtered <- dlmFilter(response, mod)
  if(!is.matrix(filtered$a)){
    filtered$a <- matrix(filtered$a)
  }

  # Rebuild return structure
  mod$X <- rbind(object$dlm$X, X)
  resid <- c(object$residuals, filtered$y - filtered$f)
  states <- rbind(object$states, filtered$a)

  # Update model variance
  filtered$mod$V <- resid %>%
    as.numeric() %>%
    var(na.rm = TRUE)

  # Model to start forecasting from
  modFuture <- mod
  lastObsIndex <- NROW(filtered$m)
  modFuture$C0 <- with(filtered, dlmSvd2var(
    U.C[[lastObsIndex]],
    D.C[lastObsIndex, ]
  ))
  wt <- states[seq_len(NROW(states) - 1) + 1, ] - states[seq_len(NROW(states) - 1), ]%*%t(mod$GG)
  modFuture$W <- var(wt)
  modFuture$m0 <- filtered$m %>% tail(1) %>% as.numeric()

  object$dlmModel <- mod
  object$dlm_future <- modFuture
  object$states <- states
  object$est <- dplyr::bind_rows(object$est, mutate(est, .fitted = filtered$f, .resid = filtered$y - filtered$f))

  object
}


