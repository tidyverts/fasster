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

build_FASSTER_group <- function(model_struct, data, groups=NULL) {
  if (!is.null(groups)) {
    ## Grouped model
    groupVar <- as.formula(paste("~", paste(groups, collapse = " + ")))
    terms_groups <- terms(groupVar, data = data)
    attr(terms_groups, "intercept") <- 0
    groupData <- model.matrix(terms_groups, data) %>%
      as.data.frame() %>%
      interaction()
  }

  if (!is.null(model_struct[[".model"]])) {
    if (is.null(groups)) {
      groupX <- list(NULL)
    }
    else {
      groupX <- groupData %>%
        spread_groups() %>%
        as.data.frame()
    }
    model_struct[[".model"]] <- groupX %>%
      map(
        ~ build_FASSTER(model_struct[[".model"]], data, X = .x)
      )
  }

  ## Recursively explore groups
  for (next_group in names(model_struct)) {
    if (next_group != ".model") {
      model_struct[[next_group]] <- build_FASSTER_group(model_struct[[next_group]], data, groups = c(groups, next_group))
    }
  }
  return(model_struct)
}

#' @importFrom rlang eval_tidy
#' @importFrom purrr map
#' @importFrom dlm dlmModPoly dlmModSeas dlmModTrig dlmModReg
build_FASSTER <- function(formula, data, X = NULL) {
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

  ## Set up specials
  if (length(specialIdx) > 0) {
    for (term in specialTerms$poly) {
      dlmTerms <- append(dlmTerms, list(dlmModPoly(term[[1]])))
    }
    for (term in specialTerms$seas) {
      dlmTerms <- append(dlmTerms, list(dlmModSeas(term[[1]])))
    }
    for (term in specialTerms$ARMA) {
      dlmTerms <- append(dlmTerms, list(dlmModARMA(term[[1]], term[[2]])))
    }
    for (term in specialTerms$trig) {
      dlmTerms <- append(dlmTerms, list(dlmModTrig(term[[1]])))
    }

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
    if (!is.null(X)) {
      xreg <- xreg * X # xreg switching
    }
    dlmTerms <- append(dlmTerms, list(dlmModReg(xreg, addInt = FALSE)))
    dlmTerms <- reduce_dlm_list(dlmTerms)
  }

  dlmTerms[[1]]
}

#' Fast Additive Seasonal Switching with Trend and Exogenous Regressors
#'
#' @param data A data.frame
#' @param model The fitted model
#' @param lambda Box-Cox transformation parameter. Ignored if NULL. Otherwise, data transformed before model is estimated. When lambda is specified, additive.only is set to TRUE.
#' @param heuristic Should the lm heuristic be used
#' @param ... Not used
#'
#' @return Returns a fitted FASSTER model
#' @export
#'
#' @examples
#'
#' @importFrom purrr safely
#' @importFrom forecast BoxCox
#' @importFrom dlm dlmFilter
fasster <- function(data, model = y ~ intercept + trig(24) + trig(7 * 24) + xreg, lambda=NULL, heuristic=TRUE, ...) {
  series <- all.vars(model)[1]
  y <- data[, series]
  if (!is.null(lambda))
    y <- BoxCox(y, lambda)

  # Parse formula into structure
  model_struct <- formula_parse_groups(model)
  dlmModel <- build_FASSTER_group(model_struct, data) %>%
    ungroup_struct()

  if (heuristic) {
    dlmModel <- dlm_lmHeuristic(y, dlmModel)
  }

  # if(!approx){
  #   # Setup function
  #   # Run dlmMLE (perhaps after fasster_stl)
  #   stop("Not yet implemented")
  #   getOptim <- dlmMLE(data[,series], 0, fn)
  # }

  ## Fit model
  filtered <- dlmFilter(y, dlmModel)
  resid <- filtered$y - filtered$f
  filtered$mod$V <- resid %>%
    as.numeric() %>%
    var()

  return(structure(list(model = dlmModel, formula = model, x = filtered$y, fitted = filtered$f, call = match.call(), series = series, residuals = resid, optimFit = list(vt=filtered$mod$vt, wt=filtered$mod$xt)), class = "fasster"))
}
