#' @keywords internal
"_PACKAGE"

#' @importFrom magrittr %>%
#' @export
dplyr::`%>%`

formula_parse_infix <- function(.formula, infix=NULL){
  fn_name <- deparse(.formula[[1]])
  if(fn_name %in% infix){
    .formula[[1]] <- as.name(paste(".infix", substr(fn_name, 2, nchar(fn_name) - 1), sep="."))
  }
  for(pos in 2:length(.formula)){
    if(is_call(.formula[[pos]])){
      .formula[[pos]] <- formula_parse_infix(.formula[[pos]], infix)
    }
  }
  .formula
}

formula_parse_groups <- function(.formula){
  if(length(.formula) > 2){
    .formula[[2]] <- NULL
  }
  parse <- list()
  .formula <- formula_parse_infix(.formula, "%G%")
  mt <- terms(.formula, specials = ".infix.G", allowDotAsName=TRUE)
  groupIdx <- attr(mt, "specials")[[".infix.G"]]

  sub_mt <- if(is.null(groupIdx)) mt
            else mt[-groupIdx]

  ## Add current layer's formula
  if(length(attr(sub_mt, "variables")) > 1){
    parse[[".model"]] <- formula(sub_mt)
  }

  ## Explore more groups
  for(pos in groupIdx){
    groupCall <- as.list(attr(mt, "variables")[[1L + pos]])
    while(class(groupCall[[3]]) == "("){
      groupCall[[3]] <- groupCall[[3]][[2]]
    }
    parse[[deparse(groupCall[[2]])]] <- formula_parse_groups(as.formula(paste("~", deparse(groupCall[[3]], width.cutoff = 500))))
  }

  parse
}

spread_groups <- function(object){
  out <- NULL
  for(group in levels(object)){
    out <- cbind(out, as.numeric(object == group))
  }
  colnames(out) <- levels(object)
  out
}

build_FASSTER_group <- function(model_struct, data, groups=NULL){
  if(!is.null(groups)){
    ## Grouped model
    groupVar <- as.formula(paste("~", paste(groups, collapse =" + ")))
    terms_groups <- terms(groupVar, data=data)
    attr(terms_groups, "intercept") <- 0
    groupData <- model.matrix(terms_groups, data) %>% as.data.frame %>% interaction
  }

  if(!is.null(model_struct[[".model"]])){
    if(is.null(groups)){
      groupX <- list(NULL)
    }
    else{
      groupX <- groupData %>% spread_groups %>% as.data.frame
    }
    model_struct[[".model"]] <- groupX %>% map(
      ~ build_FASSTER(model_struct[[".model"]], data, X = .x)
    )
  }

  ## Recursively explore groups
  for(next_group in names(model_struct)){
    if(next_group!=".model"){
      model_struct[[next_group]] <- build_FASSTER_group(model_struct[[next_group]], data, groups=c(groups, next_group))
    }
  }
  return(model_struct)
}

#' @importFrom rlang eval_tidy
build_FASSTER <- function(formula, data, X = NULL){
  dlmTerms <- list()
  ## Deparse model specification
  triggerwords <- c("constant", "intercept", "slope", "trend")
  specials <- c("poly", "trig", "seas")#, "fourier", "seasonality", "seasonal")

  mt <- terms(formula, data = data, specials = specials)
  attr(mt, "intercept") <- 0

  extractSpecialArgs <- function(x){
    if(is.null(x)) NULL
    else eval_tidy(as.list(attr(mt, "variables")[[1L + x]])[-1])
  }
  specialTerms <- attr(mt, "specials") %>%
    as.list %>%
    map(~ .x %>% lapply(extractSpecialArgs))

  specialIdx <- unlist(attr(mt, "specials"))

  ## Set up xreg
  if(length(attr(mt, "term.labels")) > length(specialIdx)){
    if(!is.null(specialIdx)){
      mt <- mt[-c(specialIdx-1)]
    }
    xreg <- model.matrix(mt, data)
    if(!is.null(X)){
      xreg <- xreg*X # xreg switching
    }
    dlmTerms <- append(dlmTerms, dlmModReg(xreg))
  }
}

#' Fast Additive Seasonal Switching with Trend and Exogenous Regressors
#'
#' @param data A data.frame
#' @param model The fitted model
#' @param grouping The column in data for which the model switches.
#' @param lambda Box-Cox transformation parameter. Ignored if NULL. Otherwise, data transformed before model is estimated. When lambda is specified, additive.only is set to TRUE.
#' @param approx If `TRUE`, the fitted parameters for the model be approximated using stl decomposition. Otherwise, the parameters are optimised via the MLE.
#' @param ...
#'
#' @return Returns a fitted FASSTER model
#' @export
#'
#' @examples
#'
#' @importFrom lazyeval f_capture
#' @importFrom dplyr select
#' @importFrom rlang eval_tidy f_rhs
#'
fasster <- function(data, model = y ~ intercept + trig(24) + trig(7*24) + xreg, groupVar, lambda=NULL, approx=TRUE, s.window = 50, ...){
  series <- all.vars(model)[1]
  y <- data[,series]
  if(!is.null(lambda))
    y <- BoxCox(y, lambda)

  # Parse formula into structure
  infix_model <- formula_parse_infix(model, "%G%")
  model_struct <- formula_parse_groups(infix_model)

  out <- build_FASSTER_group(model_struct, data)
  #
  # stlFreq <- specialTerms[-1] %>% # Remove poly special
  #   map(~ .x %>% map_dbl(~ .x[[1]])) %>% # Extract first argument of seasonal specials
  #   unlist()
  #
  # initialOptim <- fasster_stl(y, groupData, s.window=s.window, stlFreq)


  ## Build modelFn
  # For each model group (including no-group), build a seperate dlm structure
  # - GroupVar (with specific model, compute number of required params)
  # -- GroupLevelMod (attach groupX)
  # -- GroupLevelMod (attach groupX)
  # - NoGroup (with specific model, compute number of required params)
  # -- noGroupMod

  if(!approx){
    # Setup function
    # Run dlmMLE (perhaps after fasster_stl)
    stop("Not yet implemented")
    getOptim <- dlmMLE(data[,series], 0, fn)
  }

  ## Fit model
  fit <- dlmFilter(y, out)
}
