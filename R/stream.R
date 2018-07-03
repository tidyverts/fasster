#' @importFrom fable stream
#' @export
stream.FASSTER <- function(object, data, ...){
  # Define specials
  specials <- new_specials_env(
    !!!fasster_specials,
    parent_env = child_env(caller_env(), .data = data)
  )

  # Extend model
  X <- parse_model_rhs(model_rhs(object%@%"model"), data = data, specials = specials)$args %>%
    unlist(recursive = FALSE) %>%
    reduce(`+`) %>%
    .$X

  response <- eval_tidy(model_lhs(object%@%"model"), data = data)

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
  fitted <- c(object$fitted, invert_transformation(object%@%"transformation")(filtered$f))

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
  object$resid <- resid
  object$states <- states
  object$fitted <- fitted
  object$index <- c(object$index, data %>% .[[expr_text(index(data))]])

  object
}


