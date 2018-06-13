#' Fast Additive Switching of Seasonality, Trend and Exogenous Regressors
#'
#' Implementation of the FASSTER model for forecasting time series
#' with multiple seasonalities using switching states.
#'
#' \tabular{ll}{ Package: \tab fasster\cr Type: \tab Package\cr License: \tab
#' GPL3\cr LazyLoad: \tab yes\cr }
#'
#' @docType package
#' @name fasster-package
#' @author Mitchell O'Hara-Wild
#'
#' Maintainer: mail@@mitchelloharawild.com
#' @keywords package
NULL

#' @importFrom dplyr %>%
#' @export
dplyr::`%>%`

#' @importFrom fable components
#' @export
fable::components

#' @rawNamespace import(stats, except = filter)
#' @import rlang
#' @import dlm
#' @import tsibble
NULL

spread_groups <- function(object) {
  out <- NULL
  for (group in levels(object)) {
    out <- cbind(out, as.numeric(object == group))
  }
  colnames(out) <- levels(object)
  out
}

reduce_dlm_list <- function(dlmList) {
  while (length(dlmList) > 1) {
    dlmList[[1]] <- dlmList[[1]] + dlmList[[2]]
    dlmList[[2]] <- NULL
  }
  dlmList
}

ungroup_struct <- function(fasster_struct) {
  for (model in seq_along(fasster_struct)) {
    if (names(fasster_struct)[model] != ".model") {
      fasster_struct[[model]] <- ungroup_struct(fasster_struct[[model]])
    }
    else {
      fasster_struct[[model]] <- reduce_dlm_list(fasster_struct[[model]])[[1]]
    }
  }
  reduce_dlm_list(fasster_struct)[[1]]
}

#' @importFrom purrr imap
build_FASSTER_group <- function(model_struct, data, groups=NULL, internal = "regular") {
  if (!is.null(groups)) {
    ## Grouped model
    groupVar <- as.formula(paste("~", paste(groups, collapse = " + ")))
    terms_groups <- terms(groupVar, data = data)
    attr(terms_groups, "intercept") <- 0
    groupData <- model.frame(terms_groups, data) %>%
      as.data.frame() %>%
      interaction(sep="/", drop=TRUE)
  }

  if (!is.null(model_struct[[".model"]])) {
    if (is.null(groups)) {
      groupX <- list(.root = NULL)
    }
    else {
      groupX <- groupData %>%
        spread_groups() %>%
        as.data.frame()
      colnames(groupX) <- paste0(paste(groups, sep="/"), colnames(groupX), sep=":")
    }
    model_struct[[".model"]] <- groupX %>%
      imap(~ build_FASSTER(model_struct[[".model"]], data, X = .x, group = .y, internal=internal))
  }

  ## Recursively explore groups
  for (next_group in names(model_struct)) {
    if (next_group != ".model") {
      model_struct[[next_group]] <- build_FASSTER_group(model_struct[[next_group]], data, groups = c(groups, next_group), internal=internal)
    }
  }
  return(model_struct)
}

#' @importFrom rlang eval_tidy
#' @importFrom purrr map map2 imap
#' @importFrom dlm dlmModPoly dlmModSeas dlmModTrig dlmModReg dlmModARMA
build_FASSTER <- function(formula, data, X = NULL, group = NULL, internal = "regular") {
  if(group == ".root"){
    group <- NULL
  }

  dlmTerms <- list()
  ## Deparse model specification
  specials <- c("poly", "trig", "fourier", "seas", "seasonality",  "seasonal", "ARMA", "custom")

  ## Overwrite pre-existing functions
  for(fn in specials){
    assign(fn, function(...) stop("Cannot apply transformations to special functions"))
  }

  mt <- terms(formula, data = data, specials = specials)
  attr(mt, "intercept") <- 0

  extractSpecialArgs <- function(x) {
    if (is.null(x)) NULL
      else eval_tidy(as.list(attr(mt, "variables")[[1L + x]])[-1])
  }
  specialTerms <- attr(mt, "specials") %>%
    as.list() %>%
    map(~ .x %>%
      lapply(extractSpecialArgs))

  specialIdx <- unlist(attr(mt, "specials"))

  if(internal == "saturate"){
    specialTerms$trig <- specialTerms$trig %>% map(~ list(.x[[1]]))
    specialTerms$seas <- specialTerms$seas %>% map(~ list(.x[[1]]))
  }

  ## Set up specials
  specialPathList <- NULL
  if (length(specialIdx) > 0) {
    dlmTerms <- specialTerms %>%
      imap(~ .x %>% map2(.y,
                         ~ do.call(switch(.y,
                                          poly = dlmModPoly,
                                          seas = dlmModSeas,
                                          seasonal = dlmModSeas,
                                          seasonality = dlmModSeas,
                                          trig = dlmModTrig,
                                          fourier = dlmModTrig,
                                          ARMA = dlmModARMA,
                                          custom = dlm), .x))) %>%
      unlist(recursive=FALSE)

    specialPathList <- dlmTerms %>%
      map2(specialIdx, ~ rep(paste0(group, attr(mt, "term.labels")[.y], collapse = ":"), length(.x[["FF"]]))) %>%
      unlist()

    dlmTerms <- reduce_dlm_list(dlmTerms)

    if (!is.null(X)) { ## Add switching
      dlmTerms[[1]]$JFF <- dlmTerms[[1]]$FF
      dlmTerms[[1]]$X <- matrix(X, ncol = 1)
    }
  }

  ## Set up xreg
  if (length(attr(mt, "term.labels")) > length(specialIdx)) {
    if (!is.null(specialIdx)) {
      mt <- mt[-specialIdx]
    }
    xreg <- model.matrix(mt, data)
    specialPathList <- c(specialPathList, paste0(group, ":", colnames(xreg)))
    if (!is.null(X)) {
      xreg <- xreg * X # xreg switching
    }
    dlmTerms <- append(dlmTerms, list(dlmModReg(xreg, addInt = FALSE)))
    dlmTerms <- reduce_dlm_list(dlmTerms)
  }

  colnames(dlmTerms[[1]]$FF) <- specialPathList
  dlmTerms[[1]]
}

#' Fast Additive Switching of Seasonality, Trend and Exogenous Regressors
#'
#' Implements FASSTER
#'
#' @param data A \code{ts}/\code{mts}, \code{tsibble} or \code{data.frame} that contains the variables in the model.
#' @param formula An object of class "formula" (refer to 'Formula' for usage)
#' @param heuristic Which estimation heuristic should be used (refer to 'Heuristics')
#' @param include How many terms should be included to fit the model
#' @param lambda Box-Cox transformation parameter. Ignored if NULL. Otherwise, data transformed before model is estimated. When lambda is specified, additive.only is set to TRUE.
#' @param biasadj Use adjusted back-transformed mean for Box-Cox transformations. If TRUE, point forecasts and fitted values are mean forecast. Otherwise, these points can be considered the median of the forecast densities.
#' @param ... Not used
#'
#' @return Returns a fitted FASSTER model, which is consistent with the model structure from the \code{\link[forecast]{forecast}} package.
#'
#' This object is a list of class "fasster" that contains:
#' \describe{
#'    \item{model}{The underlying dlm model}
#'    \item{model_future}{A model with updated variances appropriate for forecasting}
#'    \item{formula}{The specified model formula}
#'    \item{fitted}{The fitted values}
#'    \item{lambda}{The BoxCox parameter, if used}
#'    \item{residuals}{The fitted innovations}
#'    \item{states}{The underlying filtered states}
#'    \item{call}{The user call used to create the model}
#'    \item{x}{The dependent variable}
#'    \item{series}{The name of the dependent variable}
#'    \item{method}{The name of the model used, "FASSTER"}
#' }
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
#' @section Heuristics:
#' Currently three heuristics are usable for estimating the model parameters.
#'
#' The recommended heuristic is "filterSmooth", which estimates the parameters using the following algorithm:
#' \enumerate{
#'    \item Filter the data using the specified model with non-zero state variances
#'    \item Obtain smoothed states \eqn{(\theta^{(s)}t=\theta_t|D_T)} to approximate correct behaviour
#'    \item The initial state parameters taken from the first smoothed state: \eqn{m_0=E(\theta^{(s)}_0)}, \eqn{C_0=Var(\theta^{(s)}_0)}
#'    \item Obtain state noise variances from the smoothed variance of \eqn{w_t}: \eqn{W=Var(w^{(s)}_t)=Var(\theta^{(s)}_t−G\theta^{(s)}_{t−1})}
#'    Obtain measurement noise variance from smoothed variance of \eqn{v_t}: \eqn{V=Var(v^{(s)}_t)=Var(y_t−F_t\theta^{(s)}_t)}
#'    \item Repair restricted state variances for seasonal factors and ARMA terms
#' }
#'
#' Alternative heuristics are based on repeated linear models, which approximate the dynamic nature of the fasster model.
#' Generally, these heuristics are not recommended, however they can be useful for a well specified model structure.
#'
#' @examples
#' fasster(fdeaths ~ mdeaths) %>% ggfitted
#'
#' @export
#' @aliases print.fasster
#'
#' @importFrom forecast BoxCox InvBoxCox
#' @importFrom dlm dlmFilter dlmSvd2var
#' @importFrom dplyr bind_cols pull
#' @importFrom utils tail
#' @importFrom tsibble as_tsibble is_tsibble index key
#' @importFrom rlang sym as_quosure quo_text
fasster <- function(data, formula, heuristic=c("filterSmooth", "lmSaturated", "lm"), include=NULL, lambda=NULL, biasadj=FALSE, ...) {
  cl <- match.call()
  heuristic <- match.arg(heuristic)

  if(inherits(data, "formula")){
    formula <- data
    formula_vars <- get_all_vars(data)
  }
  else{
    if(missing(formula)){
      stop("Model formula missing")
    }
    if(is_tsibble(data)){
      if(length(key(data)) > 1){
        stop("Only univariate time series are currently supported.")
      }
      tsibble_index <- data %>% select(!!index(data), !!!key(data))
    }
    formula_vars <- get_all_vars(formula, data)
  }

  series <- all.vars(formula)[1]
  y <- formula_vars[, series]
  if(!is.null(dim(y))){
    y <- y[[1]]
  }

  if(is.ts(y) & !is_tsibble(data)){
    tsibble_index <- y %>% as_tsibble
  }
  else if(!is_tsibble(data)){
    warning("Time series information not found, please provide a tsibble or time-series object.")
    tsibble_index <- y %>% ts %>% as_tsibble
  }
  tsibble_index <- tsibble_index %>% select(!!index(tsibble_index))

  series <- as_quosure(sym(series))

  data <- tsibble_index %>%
    bind_cols(formula_vars)

  if (!is.null(lambda)){
    data <- data %>%
      mutate(!!quo_text(series, 500) := BoxCox(!!series, lambda))
  }

  if (!is.null(include)){
    data <- tail(data, include)
  }

  # Add missing values and sort tsibble
  data <- data %>% add_tsblNA

  # Parse formula into structure
  model_struct <- formula_parse_groups(formula)
  dlmModel <- build_FASSTER_group(model_struct, data) %>%
    ungroup_struct()

  if(heuristic == "lm")
    dlmModel <- dlm_lmHeuristic(data %>% pull(!!series) %>% c, dlmModel)
  else if(heuristic == "lmSaturated")
    dlmModel <- dlm_lmHeuristic_saturated(data %>% pull(!!series) %>% c, dlmModel, ungroup_struct(build_FASSTER_group(model_struct, data, internal = "saturate")))
  else if(heuristic == "filterSmooth")
    dlmModel <- dlm_filterSmoothHeuristic(data %>% pull(!!series) %>% c, dlmModel)

  # Fit model
  filtered <- dlmFilter(data %>% pull(!!series) %>% c, dlmModel)

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

  fitted <- filtered$f
  if(!is.null(lambda)){
    data <- data %>%
      mutate(!!quo_text(series, 500) := InvBoxCox(!!series, lambda))
    fitted <- InvBoxCox(fitted,lambda, biasadj, filtered$mod$V)
    attr(lambda, "biasadj") <- biasadj
  }

  return(structure(list(model = dlmModel, model_future = modFuture, formula = formula, lambda = lambda,
                        x = data, fitted = fitted, residuals = resid, states = filtered$a,
                        call = cl, heuristic = heuristic, series = series, method="FASSTER"), class = "fasster"))
}
