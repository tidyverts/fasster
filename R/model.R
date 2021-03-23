globalVariables("self")
train_fasster <- function(.data, formula, specials, include = NULL){
  if(length(measured_vars(.data)) > 1){
    abort("Only univariate responses are supported by FASSTER.")
  }

  dlmModel <- specials %>%
    unlist(recursive = FALSE) %>%
    reduce(`+`)

  # Include only the end of the data
  if (!is.null(include)){
    .data <- tail(.data, include)
    dlmModel$X <- tail(dlmModel$X, include)
  }

  response <- .data[[measured_vars(.data)]]

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
    var(na.rm = TRUE)

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

  structure(
    list(dlm = dlmModel, dlm_future = modFuture,
         est = .data %>% mutate(.fitted = filtered$f, .resid = resid),
         states = filtered$a),
    class = "FASSTER")
}

.specials <- new_specials(
  `%S%` = function(group, expr){
    group_expr <- enexpr(group)
    lhs <- factor(eval_tidy(group_expr, data = self$data, env = env_parent(self$specials)))
    groups <- levels(lhs) %>% map(~ as.numeric(lhs == .x)) %>% set_names(levels(lhs))

    formula <- self$formula
    on.exit(self$formula <- formula)
    f_rhs(self$formula) <- enexpr(expr)

    rhs <- parse_model_rhs(self) %>%
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
  `%?%` = function(group, spec){
    group_expr <- enexpr(group)
    group <- eval_tidy(group_expr, data = self$data, env = env_parent(self$specials))

    if(is.null(spec$JFF)){
      spec$JFF <- spec$FF
      spec$X <- matrix(as.numeric(group), ncol = 1)
    }
    else{
      spec$X <- spec$X * as.numeric(group)
      if(any(new_X_pos <- spec$FF!=0 & spec$JFF==0)){
        new_X_col <- NCOL(spec$X) + 1
        spec$JFF[new_X_pos] <- new_X_col
        spec$X <- cbind(spec$X, as.numeric(group))
      }
    }
    colnames(spec$X) <- paste0(expr_text(group_expr), "_TRUE/", colnames(spec$X))
    colnames(spec$FF) <- paste0(expr_text(group_expr), "_TRUE/", colnames(spec$FF))
    spec
  },
  `(` = function(expr){
    formula <- self$formula
    on.exit(self$formula <- formula)
    f_rhs(self$formula) <- enexpr(expr)

    parse_model_rhs(self) %>%
      unlist(recursive = FALSE) %>%
      reduce(`+`)
  },
  poly = function(...){
    .Deprecated("trend")
    cl <- sys.call()
    out <- dlmModPoly(...)
    colnames(out$FF) <- rep(deparse(cl), NCOL(out$FF))
    out
  },
  trend = function(...){
    cl <- sys.call()
    out <- dlmModPoly(...)
    colnames(out$FF) <- rep(deparse(cl), NCOL(out$FF))
    out
  },
  seas = function(period = NULL, ...){
    .Deprecated("season")
    period <- get_frequencies(period, self$data, .auto = "smallest")
    cl <- sys.call()
    out <- dlmModSeas(period, ...)
    colnames(out$FF) <- rep(deparse(cl), NCOL(out$FF))
    out
  },
  season = function(period = NULL, ...){
    period <- get_frequencies(period, self$data, .auto = "smallest")
    cl <- sys.call()
    out <- dlmModSeas(period, ...)
    colnames(out$FF) <- rep(deparse(cl), NCOL(out$FF))
    out
  },
  seasonal = function(period = NULL, ...){
    .Deprecated("season")
    period <- get_frequencies(period, self$data, .auto = "smallest")
    cl <- sys.call()
    out <- dlmModSeas(period, ...)
    colnames(out$FF) <- rep(deparse(cl), NCOL(out$FF))
    out
  },
  trig = function(period = NULL, ...){
    .Deprecated("fourier")
    period <- get_frequencies(period, self$data, .auto = "smallest")
    cl <- sys.call()
    out <- dlmModTrig(period, ...)
    colnames(out$FF) <- rep(deparse(cl), NCOL(out$FF))
    out
  },
  fourier = function(period = NULL, K = floor(period/2), ...){
    period <- get_frequencies(period, self$data, .auto = "smallest")
    cl <- sys.call()
    out <- dlmModTrig(period, q = K, ...)
    colnames(out$FF) <- rep(deparse(cl), NCOL(out$FF))
    out
  },
  ARMA = function(...){
    cl <- sys.call()
    out <- dlmModARMA(...)
    colnames(out$FF) <- rep(deparse(cl), NCOL(out$FF))
    out
  },
  custom = function(...){
    cl <- sys.call()
    out <- dlm(...)
    colnames(out$FF) <- rep(deparse(cl), NCOL(out$FF))
    out
  },
  xreg = function(...){
    model_formula <- new_formula(
      lhs = NULL,
      rhs = reduce(c(0, enexprs(...)), ~ call2("+", .x, .y))
    )
    mm <- model.matrix(model_formula, self$data)
    out <- dlmModReg(mm, addInt = FALSE)
    colnames(out$FF) <- colnames(mm)
    out
  }
)

#' Fast Additive Switching of Seasonality, Trend and Exogenous Regressors
#'
#' Implements FASSTER
#'
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
#'    \item seas(s): Creates seasonal factors with seasonal period s
#'    \item fourier(s, K): Creates seasonal fourier terms with seasonal period s and K harmonics
#'    \item poly(n): Creates a polynomial of order n (poly(1) creates a level, poly(2) creates a trend)
#'    \item ARMA(ar, ma): Creates ARMA terms with coefficient vectors ar and ma
#'    \item custom(dlm): Creates a custom dlm structure, using \code{\link[dlm]{dlm}}
#' }
#'
#' The switching operator, \code{\%S\%} requires the switching factor variable on the LHS, and the model to switch over on the RHS (as built using the above components)
#'
#' @section Heuristic:
#' The model parameters are estimated using the following heuristic:
#' \enumerate{
#'    \item Filter the data using the specified model with non-zero state variances
#'    \item Obtain smoothed states \eqn{(\theta^{(s)}t=\theta_t|D_T)} to approximate correct behaviour
#'    \item The initial state parameters taken from the first smoothed state: \eqn{m_0=E(\theta^{(s)}_0)}, \eqn{C_0=Var(\theta^{(s)}_0)}
#'    \item Obtain state noise variances from the smoothed variance of \eqn{w_t}: \eqn{W=Var(w^{(s)}_t)=Var(\theta^{(s)}_t-G\theta^{(s)}_{t-1})}
#'    Obtain measurement noise variance from smoothed variance of \eqn{v_t}: \eqn{V=Var(v^{(s)}_t)=Var(y_t-F_t\theta^{(s)}_t)}
#'    \item Repair restricted state variances for seasonal factors and ARMA terms
#' }
#'
#' @examples
#' cbind(mdeaths, fdeaths) %>%
#'   as_tsibble(pivot_longer = FALSE) %>%
#'   model(FASSTER(mdeaths ~ fdeaths + trend(1) + fourier(12)))
#'
#' @rdname fasster-model
#' @importFrom purrr reduce imap map_chr map
#' @export
FASSTER <- function(formula, include = NULL, ...){
  fasster_model <- new_model_class("FASSTER", train_fasster, .specials)
  new_model_definition(fasster_model, !!enquo(formula), include = include, ...)
}

#' @export
#' @rdname fasster-model
#' @usage NULL
fasster <- FASSTER

#' @export
fitted.FASSTER <- function(object, ...){
  object$est[[".fitted"]]
}

#' @export
residuals.FASSTER <- function(object, ...){
  object$est[[".resid"]]
}

#' @export
model_sum.FASSTER <- function(x){
  "FASSTER"
}

#' @export
format.FASSTER <- function(x, ...){
  "FASSTER Model"
}

#' @export
print.FASSTER <- function(x, ...){
  cat(format(x))
}

#' Extract coefficients from a FASSTER model
#'
#' Obtains the mean and variance of the estimated initial states from a FASSTER
#' model. Values in the `estimate` column are contains the mean, and the
#' `std.error` column contains the standard deviation of the initial states.
#'
#' @param x An object containing a FASSTER model.
#' @param ... Unused.
#'
#' @export
tidy.FASSTER <- function(x, ...){
  tibble(
    term = colnames(x[["dlm"]][["FF"]]),
    estimate = x[["dlm"]][["m0"]],
    std.error = sqrt(diag(x[["dlm"]][["C0"]]))
  )
}

#' @export
#' @importFrom rlang as_quosure sym
report.FASSTER <- function(object, ...){
  cat("\nEstimated variances:\n")
  cat(" State noise variances (W):\n")
  data.frame(term = colnames(object$dlm$FF), W = diag(object$dlm$W)) %>%
    group_by(!!sym("term")) %>%
    summarise(!!"W" := paste(format(!!sym("W"), digits=5, scientific=TRUE), collapse=" ")) %>%
    transmute(!!"Val" := paste0("  ", !!sym("term"), "\n   ", !!sym("W"))) %>%
    .[["Val"]] %>%
    paste(collapse="\n") %>%
    paste0("\n\n") %>%
    cat
  cat(paste(" Observation noise variance (V):\n ", format(object$dlm$V, digits=5, scientific = TRUE)))
}

#' @export
interpolate.FASSTER <- function(object, new_data, specials) {
  # Get missing values
  y <- unclass(new_data)[[measured_vars(new_data)]]
  miss_val <- which(is.na(y))
  fits <- fitted(object)
  if(length(y) != length(fits)) {
    abort("Interpolation for fasster models is only supported for data used to estimate the model.")
  }

  # Update data
  y[miss_val] <- fits[miss_val]
  new_data[[measured_vars(new_data)]] <- y
  new_data
}
