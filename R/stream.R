#' @export
stream.FASSTER <- function(object, new_data, ...){
  # Extend model
  mdl <- object$definition
  mdl$data <- new_data
  X <- parse_model_rhs(mdl)$specials %>%
    unlist(recursive = FALSE) %>%
    reduce(`+`) %>%
    .$X

  est <- transmute(new_data, !!model_lhs(mdl))
  response <- est[[measured_vars(est)]]

  dlmModel <- object$dlm_future
  dlmModel$X <- X

  filtered <- dlmFilter(response, dlmModel)
  if(!is.matrix(filtered$a)){
    filtered$a <- matrix(filtered$a)
  }

  # Rebuild return structure
  dlmModel$X <- rbind(object$dlm$X, X)
  resid <- c(object$residuals, filtered$y - filtered$f)
  states <- rbind(object$states, filtered$a)

  # Update model variance
  filtered$mod$V <- resid %>%
    as.numeric() %>%
    var(na.rm = TRUE)

  # Model to start forecasting from
  modFuture <- dlmModel
  lastObsIndex <- NROW(filtered$m)
  modFuture$C0 <- with(filtered, dlmSvd2var(
    U.C[[lastObsIndex]],
    D.C[lastObsIndex, ]
  ))
  wt <- states[seq_len(NROW(states) - 1) + 1, ] - states[seq_len(NROW(states) - 1), ]%*%t(dlmModel$GG)
  modFuture$W <- var(wt)
  modFuture$m0 <- filtered$m %>% tail(1) %>% as.numeric()

  object$dlmModel <- dlmModel
  object$dlm_future <- modFuture
  object$states <- states
  object$est <- rbind(object$est, est %>% mutate(.fitted = filtered$f, .resid = filtered$y - filtered$f))

  object
}


