#' @importFrom dlm dlmSvd2var
#' @importFrom utils tail
#' @export
forecast.FASSTER <- function(object, data, newdata = NULL, h = NULL, ...){
  mod <- object$"dlm_future"

  if(!is.null(newdata)){
    h <- NROW(newdata)
  }
  else{
    if(is.null(h)){
      h <- get_frequencies("smallest", data)*2
    }
  }

  if(!is_tsibble(newdata)){
    idx <- data[[expr_text(index(data))]]
    future_idx <- seq(tail(idx, 1), length.out = h + 1, by = time_unit(idx)) %>% tail(-1)
    newdata <- c(list(future_idx), as.list(newdata))
    names(newdata)[1] <- expr_text(index(data))
    newdata <- as_tsibble(newdata, index = !!index(data))
  }
  else{
    future_idx <- newdata %>% .[[expr_text(index(.))]]
  }

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

  X <- parse_model_rhs(model_rhs(object%@%"model"), data = newdata, specials = specials)$args %>%
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

  tsibble(!!index(data) := future_idx,
          mean = biasadj(invert_transformation(object%@%"transformation"), se^2)(c(f)),
          distribution = new_fcdist(qnorm, c(f), sd = se, transformation = invert_transformation(object%@%"transformation"), abbr = "N"),
          index = !!index(data))
}
