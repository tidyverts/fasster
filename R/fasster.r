#' @keywords internal
"_PACKAGE"

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
    groupData <- model.matrix(terms_groups, data) %>%
      as.data.frame() %>%
      interaction(sep="/")
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
#' @importFrom dlm dlmModPoly dlmModSeas dlmModTrig dlmModReg
build_FASSTER <- function(formula, data, X = NULL, group = NULL, internal = "regular") {
  if(group == ".root"){
    group <- NULL
  }

  dlmTerms <- list()
  ## Deparse model specification
  triggerwords <- c("constant", "intercept", "slope", "trend")
  specials <- c("poly", "trig", "seas", "ARMA") # , "fourier", "seasonality", "seasonal")

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
                                          ARMA = dlmModARMA,
                                          trig = dlmModTrig), .x))) %>%
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

#' Fast Additive Seasonal Switching with Trend and Exogenous Regressors
#'
#' @param data A data.frame
#' @param model The fitted model
#' @param lambda Box-Cox transformation parameter. Ignored if NULL. Otherwise, data transformed before model is estimated. When lambda is specified, additive.only is set to TRUE.
#' @param include How many terms should be included to fit the model
#' @param ... Not used
#'
#' @return Returns a fitted FASSTER model
#' @export
#'
#' @examples
#'
#' @importFrom forecast BoxCox
#' @importFrom dlm dlmFilter dlmSvd2var
fasster <- function(data, model = y ~ intercept + trig(24) + trig(7 * 24) + xreg, lambda=NULL, heuristic=c("filterSmooth", "lm", "saturated"), include=NULL, ...) {
  heuristic <- match.arg(heuristic)

  series <- all.vars(model)[1]
  y <- data[, series]
  if (!is.null(lambda)){
    stop("Not yet implemented")
    y <- BoxCox(y, lambda)
  }

  if (is.null(include)){
    include <- NROW(y)
  }

  # Parse formula into structure
  model_struct <- formula_parse_groups(model)
  dlmModel <- build_FASSTER_group(model_struct, tail(data, include)) %>%
    ungroup_struct()

  if(heuristic == "lm")
    dlmModel <- dlm_lmHeuristic(tail(y, include), dlmModel)
  else if(heuristic == "saturated")
    dlmModel <- dlm_lmHeuristic_saturated(tail(y, include), dlmModel,
                                          ungroup_struct(build_FASSTER_group(model_struct, tail(data, include), internal = "saturate")))
  else if(heuristic == "filterSmooth")
    dlmModel <- dlm_filterSmoothHeuristic(tail(y, include), dlmModel)
  # if(!approx){
  #   # Setup function
  #   # Run dlmMLE (perhaps after fasster_stl)
  #   stop("Not yet implemented")
  #   getOptim <- dlmMLE(data[,series], 0, fn)
  # }

  # Fit model
  filtered <- dlmFilter(tail(y, include), dlmModel)

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
  if (is.ts(filtered$m))
    modFuture$m0 <- window(filtered$m, start = end(filtered$m))
  else {
    modFuture$m0 <- window(filtered$m, start = lastObsIndex)
    tsp(modFuture$m0) <- NULL
  }
  # optimFit = list(vt=filtered$mod$vt, wt=filtered$mod$xt)
  return(structure(list(model = dlmModel, model_future = modFuture, formula = model, x = filtered$y, fitted = filtered$f, call = match.call(), series = series, residuals = resid, states = filtered$a), class = "fasster"))
}

#' @export
residuals.fasster <- function(object){
  # %TODO: Add refilter option for full set of residuals
  if(!is.null(object$residuals)){
    object$residuals
  }
  else{
    object$fitted - object$x
  }
}
