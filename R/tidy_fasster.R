#' @importFrom fable new_specials_env parse_model parse_model_rhs traverse multi_univariate
#' @importFrom dplyr tibble
#' @importFrom purrr reduce imap
FASSTER <- function(data, formula, heuristic=c("filterSmooth", "lmSaturated", "lm"), include=NULL, ...){
  # Capture user call
  cl <- call_standardise(match.call())

  # Coerce data
  data <- as_tsibble(data)

  # Handle multivariate inputs
  if(n_keys(data) > 1){
    return(multi_univariate(data, cl))
  }

  # Define specials
  specials <- new_specials_env(
    `%S%` = function(group, expr){
      group_expr <- enexpr(group)
      lhs <- factor(group)
      groups <- levels(lhs) %>% map(~ as.numeric(lhs == .x)) %>% set_names(levels(lhs))

      rhs <- parse_model_rhs(enexpr(expr), data = data, specials = specials)$args %>%
        unlist(recursive = FALSE) %>%
        reduce(`+`)

      groups %>%
        imap(function(X, groupVal){
          if(is.null(rhs$JFF)){
            rhs$JFF <- rhs$FF
            rhs$X <- matrix(X, ncol = 1)
          }
          else{
            rhs$X <- rhs$X * X
            if(any(new_X_pos <- rhs$FF!=0 & rhs$JFF==0)){
              new_X_col <- NCOL(rhs$X) + 1
              rhs$JFF[new_X_pos] <- new_X_col
              rhs$X <- cbind(rhs$X, X)
            }
          }
          colnames(rhs$X) <- paste0(expr_text(group_expr), "_", groupVal, "/", colnames(rhs$X))
          rhs
        }) %>%
        reduce(`+`)
    },
    `(` = function(expr){
      parse_model_rhs(enexpr(expr), data = data, specials = specials)$args %>%
        unlist(recursive = FALSE) %>%
        reduce(`+`)
    },
    poly = function(...){
      dlmModPoly(...)
    },
    seas = function(...){
      dlmModSeas(...)
    },
    seasonal = function(...){
      dlmModSeas(...)
    },
    trig = function(...){
      dlmModTrig(...)
    },
    fourier = function(...){
      dlmModTrig(...)
    },
    ARMA = function(...){
      dlmModARMA(...)
    },
    custom = function(...){
      dlm(...)
    },
    xreg = function(...){
      dlmModReg(cbind(...), addInt = FALSE)
    },
    parent_env = caller_env()
  )

  model_inputs <- parse_model(data, formula, specials = specials)

  dlmModel <- model_inputs$args %>%
    eval_tidy %>%
    unlist(recursive = FALSE) %>%
    reduce(`+`)

  dlmModel

}
