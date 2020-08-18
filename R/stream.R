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


