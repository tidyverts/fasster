#' @importFrom fable new_specials_env parse_model parse_model_rhs model_lhs traverse multi_univariate invert_transformation
#' @importFrom dplyr tibble
#' @importFrom purrr reduce imap
FASSTER <- function(data, formula, include=NULL, ...){
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

  model_inputs <- parse_model(data, formula, specials = specials) %>%
    map(eval_tidy)

  dlmModel <- model_inputs$args %>%
    unlist(recursive = FALSE) %>%
    reduce(`+`)

  response <- eval_tidy(model_lhs(model_inputs$model), data = data)

  dlmModel <- response %>%
    dlm_filterSmoothHeuristic(dlmModel)

  # Fit model
  filtered <- dlmFilter(response, dlmModel)

  if(!is.matrix(filtered$a)){
    filtered$a <- matrix(filtered$a)
  }

  # Update model variance
  resid <- filtered$y - filtered$f
  filtered$mod$V <- resid %>%
    as.numeric() %>%
    var()

  # Model to start forecasting from
  modFuture <- filtered$mod
  lastObsIndex <- NROW(filtered$m)
  modFuture$C0 <- with(filtered, dlmSvd2var(
    U.C[[lastObsIndex]],
    D.C[lastObsIndex, ]
  ))
  wt <- filtered$a[seq_len(NROW(filtered$a) - 1) + 1, ] - filtered$a[seq_len(NROW(filtered$a) - 1), ]%*%t(dlmModel$GG)
  modFuture$W <- var(wt)
  modFuture$m0 <- filtered$m %>% tail(1) %>% as.numeric()

  fitted <- invert_transformation(eval_tidy(model_inputs$transformation))(filtered$f)

  fit <- list(dlm = dlmModel, dlm_future = modFuture,
              fitted = fitted, residuals = resid,
              states = filtered$a, heuristic = "filterSmooth") %>%
    enclass("FASSTER",
            !!!model_inputs[c("model", "transformation", "response")])

  data %>%
    grouped_df(key_vars(.)) %>%
    nest %>%
    mutate(model = list(fit)) %>%
    enclass("mable")
}

#' @export
pillar_shaft.FASSTER <- function(x){
  rep("FASSTER", length(x))
}

#' @export
format.FASSTER <- function(x){
  "FASSTER"
}

#' @export
print.FASSTER <- function(x, ...){
  cat("FASSTER")
}

