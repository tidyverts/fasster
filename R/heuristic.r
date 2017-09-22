#' #' @importFrom forecast seasonal trendcycle remainder
#' var_stl_decomp <- function(stlobj) {
#'   list(
#'     seasonal = var(diff(stlobj[, "seasonal"], frequency(stlobj))),
#'     trendcycle = var(diff(stlobj[, "trend"])),
#'     remainder = var(stlobj[, "remainder"]),
#'     stlobj = stlobj
#'   )
#' }
#'
#' #' @export
#' #' @import purrr
#' #' @importFrom magrittr extract2
#' fasster_stl <- function(response, groupData, s.window, stlFreqs) {
#'
#'   ## Rough structure
#'   # - Take in object, determine grouping and required variables
#'   # - Split by each group and perfrom stl decomposition on the response, with frequency according to each seasonal term in the switching model
#'
#'   ## Estimate optimal parameters specific to model groupings
#'   browser()
#'   out <- response %>%
#'     split(groupData) %>%
#'     map(~ list(.x) %>%
#'       map2(stlFreqs, ~ .x %>%
#'         ts(frequency = .y) %>%
#'         stl(s.window = s.window) %>%
#'         extract2("time.series") %>%
#'         var_stl_decomp()
#'       )
#'     )
#'   # out$`_total` <- out %>%
#'   #   map(~ .x %>%
#'   #         extract2("stlobj")) %>%
#'   #   invoke(rbind, .) %>%
#'   #   var_stl_decomp()
#' }


dlm_lmHeuristic <- function(y, dlmModel){
  ## Generate optimisation Z
  ZF <- as.matrix(dlmModel$FF[rep(1, length(y)), ])
  ZF[, dlmModel$JFF != 0] <- dlmModel$X[, dlmModel$JFF]
  G_i <- dlmModel$GG
  Z <- ZF
  for (i in seq_along(y)) {
    Z[i, ] <- ZF[i, ] %*% G_i
    G_i <- G_i %*% dlmModel$GG
  }

  ## Fit heuristic models
  step_len <- NCOL(Z) * 4 ## TODO: Improve block selection as a fn of NROW with s
  dlmModel$vt <- numeric(length(y) - step_len)
  dlmModel$xt <- matrix(nrow = length(y) - step_len, ncol = NCOL(Z))
  for (i in seq_len(length(y) - step_len)) {
    idx <- seq_len(step_len) + i - 1
    optimFit <- .lm.fit(Z[idx, ], y[idx], tol = 1e-6)
    dlmModel$xt[i, ] <- optimFit$coefficients
    dlmModel$vt[i] <- optimFit$residuals[1]
  }
  wt <- dlmModel$xt[seq_len(NROW(dlmModel$xt) - 1), ] - (dlmModel$xt[seq_len(NROW(dlmModel$xt) - 1) + 1, ] %*% dlmModel$GG)

  dlmModel$m0 <- dlmModel$xt[1, ]
  dlmModel$V <- var(dlmModel$vt)
  dlmModel$W <- dlmModel$C0 <- var(wt)

  return(dlmModel)
}

dlm_lmHeuristic_saturated <- function(y, dlmModel, dlmModelSaturated){
  dlmModel <- dlm_lmHeuristic(y, dlmModel)
  dlmModelSaturated <- dlm_lmHeuristic(y, dlmModelSaturated)
  dlmModel$V <- dlmModelSaturated$V
  dlmModel$W <- dlmModel$W * mean(dlmModelSaturated$W)/mean(dlmModel$W)
  return(dlmModel)
}

#' @importFrom dlm dlmSmooth dlmSvd2var
dlm_filterSmoothHeuristic <- function(y, dlmModel){
  filtered <- dlmFilter(y, dlmModel)
  smoothed <- dlmSmooth(filtered)
  dlmModel$m0 <- smoothed$s[1,]
  dlmModel$C0 <- with(smoothed, dlmSvd2var(
    U.S[[1]],
    D.S[1, ]
  ))
  dlmModel$W <- with(smoothed, dlmSvd2var(
    U.S[[length(U.S)]],
    D.S[length(U.S), ]
  ))
  dlmModel
}
