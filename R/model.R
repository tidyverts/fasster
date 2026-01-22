globalVariables("self")
train_fasster <- function(.data, specials, include = NULL, ...){
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
  `%S%` = function(group, spec){
    group_expr <- enexpr(group)
    group_unique <- unique(group)
    groups <- group_unique %>% map(~ as.numeric(group == .x)) %>% set_names(group_unique)

    groups %>%
      imap(function(X, groupVal){
        if(is.null(spec$JFF)){
          spec$JFF <- spec$FF
          spec$X <- matrix(X, ncol = 1)
        }
        else{
          spec$X <- spec$X * X
          if(any(new_X_pos <- spec$FF!=0 & spec$JFF==0)){
            new_X_col <- NCOL(spec$X) + 1
            spec$JFF[new_X_pos] <- new_X_col
            spec$X <- cbind(spec$X, X)
          }
        }
        colnames(spec$X) <- paste0(expr_text(group_expr), "_", groupVal, "/", colnames(spec$X))
        colnames(spec$FF) <- paste0(expr_text(group_expr), "_", groupVal, "/", colnames(spec$FF))
        spec
      }) %>%
      reduce(`+`)
  },
  `%?%` = function(condition, spec){
    cond_expr <- enexpr(condition)

    if(is.null(spec$JFF)){
      spec$JFF <- spec$FF
      spec$X <- matrix(as.numeric(condition), ncol = 1)
    }
    else{
      spec$X <- spec$X * as.numeric(condition)
      if(any(new_X_pos <- spec$FF!=0 & spec$JFF==0)){
        new_X_col <- NCOL(spec$X) + 1
        spec$JFF[new_X_pos] <- new_X_col
        spec$X <- cbind(spec$X, as.numeric(condition))
      }
    }
    colnames(spec$X) <- paste0(expr_text(cond_expr), "_TRUE/", colnames(spec$X))
    colnames(spec$FF) <- paste0(expr_text(cond_expr), "_TRUE/", colnames(spec$FF))
    spec
  },
  `(` = function(expr){
    expr
  },
  poly = function(..., .call = sys.call()){
    out <- dlmModPoly(...)
    colnames(out$FF) <- rep(deparse(.call), NCOL(out$FF))
    out
  },
  trend = function(...){
    .specials$poly(..., .call = sys.call())
  },
  seas = function(period = NULL, ...){
    .Deprecated("season")
    .specials$season(period, ..., .call = sys.call())
  },
  season = function(period = NULL, ..., .call = sys.call()){
    period <- get_frequencies(period, self$data, .auto = "smallest")
    out <- dlmModSeas(period, ...)
    colnames(out$FF) <- rep(deparse(.call), NCOL(out$FF))
    out
  },
  seasonal = function(period = NULL, ...){
    .Deprecated("season")
    .specials$season(period, ..., .call = sys.call())
  },
  trig = function(period = NULL, ...){
    .Deprecated("fourier")
    .specials$fourier(period, ..., .call = sys.call())
  },
  fourier = function(period = NULL, K = floor(period/2), ..., .call = sys.call()){
    period <- get_frequencies(period, self$data, .auto = "smallest")
    # If period is integer
    if(period == floor(period)) {
      out <- dlmModTrig(period, q = K, ...)
    } else {
      out <- dlmModTrig(tau = period, q = K, ...)
    }
    colnames(out$FF) <- rep(deparse(.call), NCOL(out$FF))
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
#' @param formula An object of class "formula" (refer to 'Specials' section for usage)
#' @param include How many terms should be included to fit the model
#' @param ... Not used
#'
#' @return Returns a mable containing the fitted FASSTER model.
#'
#' @details
#' The fasster model extends commonly used state space models by introducing a switching component to the measurement equation.
#' This is implemented using a time-varying DLM with the switching behaviour encoded in the measurement matrix.
#'
#' @section Specials:
#'
#' The _specials_ define the model structure for `FASSTER`. The model can include
#' trend, seasonal, ARMA, and exogenous regressor components, with optional
#' switching behaviour controlled by grouping factors.
#'
#' \subsection{trend}{
#' The `trend` special specifies polynomial trend components.
#' \preformatted{
#' trend(n, ...)
#' }
#'
#' \tabular{ll}{
#'   `n`   \tab The order of the polynomial trend. Use 1 for level, 2 for linear trend, etc. \cr
#'   `...` \tab Additional arguments passed to [`dlm::dlmModPoly()`]. Common arguments include `dV` (observation variance) and `dW` (state variance, can be a scalar or vector). \cr
#' }
#' }
#'
#' \subsection{season}{
#' The `season` special specifies seasonal factors using indicator variables.
#' \preformatted{
#' season(period = NULL, ...)
#' }
#'
#' \tabular{ll}{
#'   `period` \tab The seasonal period. If `NULL`, automatically detected from the data (uses the smallest frequency). Can be a number or text like "1 year". \cr
#'   `...`    \tab Additional arguments passed to [`dlm::dlmModSeas()`]. Common arguments include `dV` and `dW`. \cr
#' }
#' }
#'
#' \subsection{fourier}{
#' The `fourier` special specifies seasonal components using Fourier terms (trigonometric functions).
#' \preformatted{
#' fourier(period = NULL, K = floor(period/2), ...)
#' }
#'
#' \tabular{ll}{
#'   `period` \tab The seasonal period. If `NULL`, automatically detected from the data. \cr
#'   `K`      \tab The number of Fourier terms (harmonics) to include. Maximum is `floor(period/2)`. More harmonics capture more complex seasonal patterns but increase model complexity. \cr
#'   `...`    \tab Additional arguments passed to [`dlm::dlmModTrig()`]. \cr
#' }
#' }
#'
#' \subsection{ARMA}{
#' The `ARMA` special includes autoregressive moving average components.
#' \preformatted{
#' ARMA(...)
#' }
#'
#' \tabular{ll}{
#'   `...` \tab Arguments passed to [`dlm::dlmModARMA()`]. Typically includes `ar` (vector of AR coefficients) and `ma` (vector of MA coefficients). \cr
#' }
#' }
#'
#' \subsection{xreg}{
#' The `xreg` special includes exogenous regressors in the model.
#' \preformatted{
#' xreg(...)
#' }
#'
#' \tabular{ll}{
#'   `...` \tab Bare expressions for the exogenous regressors. These are evaluated in the context of the data, so you can use transformations like `log(x)` or interactions. \cr
#' }
#' }
#'
#' \subsection{custom}{
#' The `custom` special allows you to specify custom DLM structures.
#' \preformatted{
#' custom(...)
#' }
#'
#' \tabular{ll}{
#'   `...` \tab Arguments passed to [`dlm::dlm()`] to create a custom DLM component. \cr
#' }
#' }
#' 
#' \subsection{`%S%` (Switching operator)}{
#' The `%S%` operator creates switching models where different model structures
#' apply to different groups defined by a factor variable.
#' \preformatted{
#' group %S% model
#' }
#'
#' \tabular{ll}{
#'   `group` \tab A factor variable (or expression that evaluates to a factor) defining the groups. Each level of the factor will have its own version of the model component on the RHS. \cr
#'   `spec` \tab The model specifications to replicate for each group. This can be any combination of `trend()`, `season()`, `fourier()`, etc. \cr
#' }
#'
#' For example, `group %S% trend(1)` creates a separate level for each value of `group`,
#' allowing the mean to switch between groups.
#' }
#'
#' \subsection{`%?%` (Conditional operator)}{
#' The `%?%` operator creates conditional models where a component is only active
#' when a logical condition is TRUE.
#' \preformatted{
#' condition %?% model
#' }
#'
#' \tabular{ll}{
#'   `condition` \tab A logical variable or expression. The model component will only contribute when this is TRUE. \cr
#'   `spec`     \tab The model component to conditionally include. \cr
#' }
#'
#' For example, `(year > 2000) %?% trend(1)` includes a level component only for observations after 2000.
#' }
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
#' # Basic model with trend and seasonality
#' cbind(mdeaths, fdeaths) %>%
#'   as_tsibble(pivot_longer = FALSE) %>%
#'   model(FASSTER(mdeaths ~ trend(1) + fourier(12)))
#'
#' # Model with exogenous regressor
#' cbind(mdeaths, fdeaths) %>%
#'   as_tsibble(pivot_longer = FALSE) %>%
#'   model(FASSTER(mdeaths ~ fdeaths + trend(1) + fourier(12)))
#'
#' # Switching model with different trends for different periods
#' cbind(mdeaths, fdeaths) %>%
#'   as_tsibble(pivot_longer = FALSE) %>%
#'   model(
#'     FASSTER(mdeaths ~ (lubridate::year(index) > 1977) %S% trend(1) + fourier(12))
#'   )
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

#' Extract fitted values from a FASSTER model
#'
#' Returns the one-step-ahead fitted values from a FASSTER model, calculated
#' during the Kalman filtering process. These represent the model's predictions
#' at each time point using only information available up to that point.
#'
#' @param object A fitted FASSTER model object.
#' @param ... Additional arguments (currently unused).
#'
#' @return A numeric vector of fitted values with the same length as the
#'   training data.
#'
#' @examples
#' library(tsibble)
#' fit <- as_tsibble(mdeaths) |>
#'   model(FASSTER(value ~ trend(1) + fourier(12)))
#' 
#' # Extract fitted values
#' fitted(fit)
#'
#' @export
fitted.FASSTER <- function(object, ...){
  object$est[[".fitted"]]
}

#' Extract residuals from a FASSTER model
#'
#' Returns the one-step-ahead forecast errors (residuals) from a FASSTER model.
#'
#' @param object A fitted FASSTER model object.
#' @param ... Additional arguments (currently unused).
#'
#' @return A numeric vector of residuals with the same length as the training
#'   data.
#'
#' @examples
#' library(tsibble)
#' fit <- as_tsibble(mdeaths) |>
#'   model(FASSTER(value ~ trend(1) + fourier(12)))
#' 
#' # Extract residuals
#' residuals(fit)
#'
#' @export
residuals.FASSTER <- function(object, ...){
  object$est[[".resid"]]
}

#' @keywords internal
#' @export
model_sum.FASSTER <- function(x){
  "FASSTER"
}

#' @keywords internal
#' @export
format.FASSTER <- function(x, ...){
  "FASSTER Model"
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

#' Glance at a FASSTER model
#'
#' Constructs a single-row summary of the model's goodness-of-fit statistics.
#' This method follows the broom package conventions and is used by fabletools
#' to provide model selection metrics.
#'
#' @param x A FASSTER model object.
#' @param ... Additional arguments (currently unused).
#'
#' @return A one-row tibble containing:
#' \describe{
#'   \item{sigma2}{The estimated observation variance (V). If the model has
#'     multivariate observations, this is a list containing the variance matrix.}
#'   \item{log_lik}{The log-likelihood of the model.}
#'   \item{AIC}{Akaike Information Criterion.}
#'   \item{AICc}{Corrected AIC for small sample sizes.}
#'   \item{BIC}{Bayesian Information Criterion.}
#' }
#'
#' @examples
#' library(tsibble)
#' fit <- as_tsibble(mdeaths) |>
#'   model(FASSTER(value ~ trend(1) + fourier(12)))
#' 
#' # Get model fit statistics
#' glance(fit)
#'
#' @export
glance.FASSTER <- function(x, ...){
  logL <- -dlmLL(x$est$.resid + x$est$.fitted, x$dlm)
  nobs <- nrow(x$est)
  k <- ncol(x[["dlm"]][["FF"]])
  tibble(
    sigma2 = if(is.matrix(x$dlm$V)) list(x$dlm$V) else x$dlm$V,
    # "The function returns the negative of the loglikelihood."
    log_lik = -logL,
    AIC = -2 * logL + 2 * k,
    AICc = -2 * logL + 2 * k * nobs / (nobs - k - 1),
    BIC = -2 * logL + log(nobs) * k
  )
}

#' Report on a FASSTER model
#'
#' Prints a detailed report of the estimated variance parameters for a FASSTER
#' model. This includes the state noise variances (W) for each model component
#' and the observation noise variance (V).
#'
#' @param object A FASSTER model object.
#' @param ... Additional arguments (currently unused).
#'
#' @return Invisibly returns NULL. Called for its side effect of printing the
#'   variance report to the console.
#'
#' @details
#' The report displays:
#' \itemize{
#'   \item State noise variances (W): The variance of the random innovations
#'     for each state component, grouped by model term.
#'   \item Observation noise variance (V): The variance of the measurement error.
#' }
#'
#' @examples
#' library(tsibble)
#' fit <- as_tsibble(mdeaths) |>
#'   model(FASSTER(value ~ trend(1) + fourier(12)))
#' 
#' # Print variance report
#' report(fit)
#'
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
  
  cat("\n\nInitial states (m0):\n")
  data.frame(term = colnames(object$dlm$FF), m0 = object$dlm$m0) %>%
    transmute(!!"Val" := paste0("  ", !!sym("term"), ": ", format(!!sym("m0"), digits=5, scientific=TRUE))) %>%
    .[["Val"]] %>%
    paste(collapse="\n") %>%
    cat
  cat("\n")
}


#' Interpolate missing values in a FASSTER model
#'
#' Fills in missing values in the response variable using the model's fitted
#' values. This method only works for interpolating data used to estimate the
#' model and cannot be used for new data.
#'
#' @param object A fitted FASSTER model object.
#' @param new_data A tsibble containing the data to interpolate. Must be the
#'   same data used to fit the model.
#' @param specials A list of special terms (passed by fabletools).
#' @param ... Additional arguments (currently unused).
#'
#' @return A tsibble with missing values in the response variable replaced by
#'   fitted values.
#'
#' @details
#' This method identifies missing values (NAs) in the response variable and
#' replaces them with the corresponding fitted values from the model. It only
#' works when `new_data` has the same length as the data used to fit the model.
#'
#' @examples
#' library(tsibble)
#' library(dplyr)
#' 
#' # Create data with missing values
#' deaths_na <- as_tsibble(mdeaths) |>
#'   mutate(value = if_else(row_number() %in% c(10, 20, 30), NA_real_, value))
#' 
#' # Fit model
#' fit <- deaths_na |>
#'   model(FASSTER(value ~ trend(1) + fourier(12)))
#' 
#' # Interpolate missing values
#' interpolate(fit, deaths_na)
#'
#' @export
interpolate.FASSTER <- function(object, new_data, specials, ...) {
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

#' Refit a FASSTER model
#'
#' Applies a fitted FASSTER model to a new dataset.
#'
#' @param object A fitted FASSTER model.
#' @param new_data A tsibble containing the new data.
#' @param specials (passed by [`fabletools::refit.mdl_df()`]).
#' @param reestimate If `TRUE`, the model parameters will be re-estimated to suit 
#'   the new data using the heuristic approach. If `FALSE`, the existing model 
#'   structure and parameters are applied to the new data without modification.
#' @param ... Additional arguments passed to the training function.
#'
#' @return A refitted FASSTER model.
#'
#' @examples
#' library(tsibble)
#' 
#' # Fit model to male deaths
#' fit_male <- as_tsibble(mdeaths) %>%
#'   model(FASSTER(value ~ trend(1) + fourier(12)))
#' 
#' # Refit to female deaths without re-estimating parameters
#' refit(fit_male, as_tsibble(fdeaths), reestimate = FALSE)
#' 
#' # Refit to female deaths with re-estimated parameters
#' refit(fit_male, as_tsibble(fdeaths), reestimate = TRUE)
#'
#' @export
refit.FASSTER <- function(object, new_data, specials = NULL, reestimate = FALSE, ...) {
  if (reestimate) {
    # Re-estimate the model with new data
    return(train_fasster(new_data, specials = specials, ...))
  }
  
  # Apply existing model to new data without re-estimation
  response <- new_data[[measured_vars(new_data)]]
  
  # Use the existing DLM structure
  dlmModel <- object$dlm
  
  # Filter new data with existing model
  filtered <- dlmFilter(response, dlmModel)
  
  if(!is.matrix(filtered$a)){
    filtered$a <- matrix(filtered$a)
  }
  
  # Calculate residuals
  resid <- filtered$y - filtered$f
  
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
         est = new_data %>% mutate(.fitted = filtered$f, .resid = resid),
         states = filtered$a),
    class = "FASSTER")
}
