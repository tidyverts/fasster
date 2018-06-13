#' Fast Additive Switching of Seasonality, Trend and Exogenous Regressors
#'
#' Implements FASSTER
#'
#' @param data A \code{ts}/\code{mts}, \code{tsibble} or \code{data.frame} that contains the variables in the model.
#' @param formula An object of class "formula" (refer to 'Formula' for usage)
#' @param include How many terms should be included to fit the model
#' @param ... Not used
#'
#' @return Returns a mable containing the fitted FASSTER model.
#'
#' @details
#' The fasster model extends commonly used state space models by introducing a switching component to the measurement equation.
#' This is implemented using a time-varying DLM with the switching behaviour encoded in the measurement matrix.
#'
#' @section Formula:
#' \code{fasster} inherits the standard formula specification from \code{\link[stats]{lm}} for specifying exogenous regressors, including interactions and \code{\link[base]{I}()} functionality as described in \code{\link[stats]{formula}}.
#'
#' Special DLM components can be specified using special functions defined below:
#' \itemize{
#'    \item seas(s): Creates seasonal factors with seasonality s
#'    \item trig(s): Creates seasonal fourier terms with seasonality s
#'    \item poly(n): Creates a polynomial of order n (poly(1) creates a level, poly(2) creates a trend)
#'    \item ARMA(ar, ma): Creates ARMA terms with coefficient vectors ar and ma
#'    \item custom(dlm): Creates a custom dlm structure, using \code{\link[dlm]{dlm}}
#' }
#'
#' The switching operator, \code{\%S\%} requires the switching factor variable on the LHS, and the model to switch over on the RHS (as built using the above components)
#'
#' @examples
#' tsibbledata::UKLungDeaths %>%
#'   FASSTER(mdeaths ~ fdeaths + poly(1) + trig(12))
#'
#' @rdname FASSTER2
#' @importFrom fable new_specials_env parse_model parse_model_rhs model_lhs traverse multi_univariate invert_transformation mable
#' @importFrom dplyr tibble
#' @importFrom purrr reduce imap map_chr
#' @export
FASSTER <- function(data, formula, include=NULL, ...){
  # Capture user call
  cl <- call_standardise(match.call())

  # Coerce data
  data <- as_tsibble(data)

  # Handle multivariate inputs
  if(n_keys(data) > 1){
    return(multi_univariate(data, cl))
  }

  # Include only the end of the data
  if (!is.null(include)){
    data <- tail(data, include)
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
          colnames(rhs$FF) <- paste0(expr_text(group_expr), "_", groupVal, "/", colnames(rhs$FF))
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
      cl <- match.call()
      out <- dlmModPoly(...)
      colnames(out$FF) <- rep(deparse(cl), NCOL(out$FF))
      out
    },
    seas = function(...){
      cl <- match.call()
      out <- dlmModSeas(...)
      colnames(out$FF) <- rep(deparse(cl), NCOL(out$FF))
      out
    },
    seasonal = function(...){
      cl <- match.call()
      out <- dlmModSeas(...)
      colnames(out$FF) <- rep(deparse(cl), NCOL(out$FF))
      out
    },
    trig = function(...){
      cl <- match.call()
      out <- dlmModTrig(...)
      colnames(out$FF) <- rep(deparse(cl), NCOL(out$FF))
      out
    },
    fourier = function(...){
      cl <- match.call()
      out <- dlmModTrig(...)
      colnames(out$FF) <- rep(deparse(cl), NCOL(out$FF))
      out
    },
    ARMA = function(...){
      cl <- match.call()
      out <- dlmModARMA(...)
      colnames(out$FF) <- rep(deparse(cl), NCOL(out$FF))
      out
    },
    custom = function(...){
      cl <- match.call()
      out <- dlm(...)
      colnames(out$FF) <- rep(deparse(cl), NCOL(out$FF))
      out
    },
    xreg = function(...){
      cn <- enexprs(...) %>% map_chr(expr_text)
      out <- dlmModReg(cbind(...), addInt = FALSE)
      colnames(out$FF) <- cn
      out
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
              fitted = fitted, residuals = resid, index = data %>% pull(!!index(data)),
              states = filtered$a, heuristic = "filterSmooth") %>%
    enclass("FASSTER",
            !!!model_inputs[c("model", "transformation", "response")])

  mable(
    key_vals = as.list(data)[key_vars(data)],
    data = (data %>%
              dplyr::grouped_df(key_vars(.)) %>%
              nest)$data,
    model = list(fit)
  )
}

#' @importFrom fable model_sum
#' @export
model_sum.FASSTER <- function(x){
  "FASSTER"
}

#' @export
format.FASSTER <- function(x){
  "FASSTER"
}

#' @export
print.FASSTER <- function(x, ...){
  cat(paste("FASSTER Model:\n", deparse(x%@%"model"), "\n\n"))
}

#' @export
#' @importFrom dplyr group_by summarise transmute
#' @importFrom rlang as_quosure sym
#' @importFrom dplyr pull
summary.FASSTER <- function(x, ...){
  cat(paste("FASSTER Model:\n", deparse(x%@%"model"), "\n\n"))
  cat("Estimated variances:\n")
  cat(" State noise variances (W):\n")
  data.frame(term = colnames(x$dlm$FF), W = diag(x$dlm$W)) %>%
    group_by(!!sym("term")) %>%
    summarise(!!"W" := paste(format(!!sym("W"), digits=5, scientific=TRUE), collapse=" ")) %>%
    transmute(!!"Val" := paste0("  ", !!sym("term"), "\n   ", !!sym("W"))) %>%
    pull(!!sym("Val")) %>%
    paste(collapse="\n") %>%
    paste0("\n\n") %>%
    cat
  cat(paste(" Observation noise variance (V):\n ", format(x$dlm$V, digits=5, scientific = TRUE)))
}


#' @importFrom dlm dlmSvd2var
#' @importFrom forecast forecast
#' @importFrom tsibblestats get_frequencies
#' @importFrom fable parse_model_rhs model_rhs
#' @export
forecast.FASSTER <- function(object, data, newdata = NULL, h = NULL, ...){
  mod <- object$"dlm_future"

  if(!is.null(newdata)){
    newdata <- as_tsibble(newdata)

    # Build model on newdata
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

    X <- parse_model_rhs(object%@%"model", data = newdata, specials = specials)$args %>%
      unlist(recursive = FALSE) %>%
      reduce(`+`) %>%
      .$X
  }
  else{
    X <- NULL
  }

  fit <- mod

  nAhead <- if(!is.null(mod$X)){
    if(is.null(X)){
      stop("X must be given")
    }
    NROW(X)
  }
  else{
    if(is.null(h)){
      h <- get_frequencies("smallest", data)*2
    }
    h
  }

  p <- length(mod$m0)
  m <- nrow(mod$FF)
  a <- rbind(mod$m0, matrix(0, nAhead, p))
  R <- vector("list", nAhead + 1)
  R[[1]] <- mod$C0
  f <- matrix(0, nAhead, m)
  Q <- vector("list", nAhead)
  for (it in 1:nAhead) {
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
  idx <- data %>% pull(!!index(.))
  future_idx <- seq(tail(idx, 1), length.out = nAhead + 1, by = time_unit(idx)) %>% tail(-1)

  tsibble(!!index(data) := future_idx,
          mean = biasadj(invert_transformation(object%@%"transformation"), se^2)(c(f)),
          distribution = new_fcdist(qnorm, c(f), sd = se, transformation = invert_transformation(object%@%"transformation"), abbr = "N"),
          index = !!index(data))
}
