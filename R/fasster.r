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

#' @import stats
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
#'    \item{lambda}{The BoxCox paramater, if used}
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
#' \code{fasster} inherits the standard formula specification from \link[stats]{lm} for specifying exogenous regressors, including interactions and \code{\link[base]{I}()} functionality as described in \code{\link[stats]{formula}}.
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
#'    \item Obtain smoothed states \eqn{(θ^{(s)}t=θ_t|D_T)} to approximate correct behaviour
#'    \item The initial state parameters taken from the first smoothed state: \eqn{m_0=E(θ^{(s)}_0)}, \eqn{C_0=\text{Var}(θ^{(s)}_0)}
#'    \item Obtain state noise variances from the smoothed variance of \eqn{w_t}: \eqn{W=\text{Var}(w^{(s)}_t)=\text{Var}(θ^{(s)}_t−Gθ^{(s)}_{t−1})}
#'    Obtain measurement noise variance from smoothed variance of \eqn{v_t}: \eqn{V=\text{Var}(v^{(s)}_t)=\text{Var}(y_t−F_tθ^{(s)}_t)}
#'    \item Repair restricted state variances for seasonal factors and ARMA terms
#' }
#'
#' Alternative heuristics are based on repeated linear models, which approximate the dynamic nature of the fasster model.
#' Generally, these heuristics are not recommended, however they can be useful for a well specified model structure.
#'
#' @examples
#'
#' @export
#' @importFrom forecast BoxCox InvBoxCox
#' @importFrom dlm dlmFilter dlmSvd2var
#' @importFrom utils tail
fasster <- function(data, formula, heuristic=c("filterSmooth", "lmSaturated", "lm"), include=NULL, lambda=NULL, biasadj=FALSE, ...) {
  cl <- match.call()
  heuristic <- match.arg(heuristic)

  if(inherits(data, "formula")){
    formula <- data
    data <- get_all_vars(data)
  }
  else{
    if(missing(formula)){
      stop("Model formula missing")
    }
    data <- get_all_vars(formula, data)
  }

  series <- all.vars(formula)[1]
  y <- data[, series]
  if(!is.null(dim(y))){
    y <- y[[1]]
  }

  if (!is.null(lambda)){
    y <- BoxCox(y, lambda)
  }

  if (is.null(include)){
    include <- NROW(y)
  }

  # Parse formula into structure
  model_struct <- formula_parse_groups(formula)
  dlmModel <- build_FASSTER_group(model_struct, tail(data, include)) %>%
    ungroup_struct()

  if(heuristic == "lm")
    dlmModel <- dlm_lmHeuristic(tail(y, include), dlmModel)
  else if(heuristic == "lmSaturated")
    dlmModel <- dlm_lmHeuristic_saturated(tail(y, include), dlmModel,
                                          ungroup_struct(build_FASSTER_group(model_struct, tail(data, include), internal = "saturate")))
  else if(heuristic == "filterSmooth")
    dlmModel <- dlm_filterSmoothHeuristic(tail(y, include), dlmModel)

  # Fit model
  filtered <- dlmFilter(tail(y, include), dlmModel)

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
  wt <- filtered$a[seq_len(NROW(filtered$a) - 1), ] - (filtered$a[seq_len(NROW(filtered$a) - 1) + 1, ] %*% dlmModel$GG)
  modFuture$W <- var(wt)
  if (is.ts(filtered$m))
    modFuture$m0 <- window(filtered$m, start = end(filtered$m))
  else {
    modFuture$m0 <- window(filtered$m, start = lastObsIndex)
    tsp(modFuture$m0) <- NULL
  }

  fitted <- filtered$f
  if(!is.null(lambda)){
    fitted <- InvBoxCox(fitted,lambda, biasadj, filtered$mod$V)
    attr(lambda, "biasadj") <- biasadj
  }

  return(structure(list(model = dlmModel, model_future = modFuture, formula = formula, lambda = lambda,
                        x = data[, series], fitted = fitted, residuals = resid, states = filtered$a,
                        call = cl, series = series, method="FASSTER"), class = "fasster"))
}

#' @export
residuals.fasster <- function(object, ...){
  # %TODO: Add refilter option for full set of residuals
  if(!is.null(object$residuals)){
    object$residuals
  }
  else{
    object$fitted - object$x
  }
}

#' @inherit forecast::getResponse
#'
#' @seealso
#' \code{\link[forecast]{getResponse}}
#'
#' @export
getResponse.fasster <- function(object, ...){
  object$x
}

