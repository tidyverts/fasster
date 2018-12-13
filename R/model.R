train_fasster <- function(.data, formula, specials, include = NULL){
  if(length(measured_vars(.data)) > 1){
    abort("Only univariate responses are supported by FASSTER.")
  }

  # Include only the end of the data
  if (!is.null(include)){
    .data <- tail(.data, include)
  }

  dlmModel <- specials %>%
    unlist(recursive = FALSE) %>%
    reduce(`+`)

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
         states = filtered$a, formula = set_env(formula, new_environment())),
    class = "FASSTER")

}

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
#' @section Heuristic:
#' The model parameters are estimated using the following heuristic:
#' \enumerate{
#'    \item Filter the data using the specified model with non-zero state variances
#'    \item Obtain smoothed states \eqn{(\theta^{(s)}t=\theta_t|D_T)} to approximate correct behaviour
#'    \item The initial state parameters taken from the first smoothed state: \eqn{m_0=E(\theta^{(s)}_0)}, \eqn{C_0=Var(\theta^{(s)}_0)}
#'    \item Obtain state noise variances from the smoothed variance of \eqn{w_t}: \eqn{W=Var(w^{(s)}_t)=Var(\theta^{(s)}_t−G\theta^{(s)}_{t−1})}
#'    Obtain measurement noise variance from smoothed variance of \eqn{v_t}: \eqn{V=Var(v^{(s)}_t)=Var(y_t−F_t\theta^{(s)}_t)}
#'    \item Repair restricted state variances for seasonal factors and ARMA terms
#' }
#'
#' @examples
#' tsibbledata::UKLungDeaths %>%
#'   FASSTER(mdeaths ~ fdeaths + poly(1) + trig(12))
#'
#' @rdname fasster-model
#' @importFrom purrr reduce imap map_chr map
#' @export
FASSTER <- fablelite::define_model(
  train = train_fasster,
  specials = .specials
)

#' @export
#' @rdname fasster-model
#' @usage NULL
fasster <- FASSTER

#' @export
fitted.FASSTER <- function(object, ...){
  select(object$est, ".fitted")
}

#' @export
residuals.FASSTER <- function(object, ...){
  select(object$est, ".resid")
}

#' @export
augment.FASSTER <- function(x, ...){
  x$est
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

#' @export
#' @importFrom tsibble group_by summarise transmute
#' @importFrom rlang as_quosure sym
summary.FASSTER <- function(object, ...){
  print(object)
  cat("Estimated variances:\n")
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
