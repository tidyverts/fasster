#' Extract Components from a FASSTER Model
#'
#' Decomposes a FASSTER model into its individual components, allowing you to
#' examine the contribution of each term to the fitted values. This is useful
#' for understanding which components drive the model's predictions and how
#' well different aspects of the model fit the data.
#'
#' @param object A FASSTER model object.
#' @param ... Additional arguments (currently unused).
#'
#' @return A dable (decomposition table) containing the response variable and
#'   each model component as separate columns. The components sum to the
#'   response variable.
#'
#' @examples
#' \dontrun{
#' # Fit a FASSTER model and extract components
#' library(tsibble)
#' library(dplyr)
#' fit <- tsibbledata::aus_retail |>
#'   filter(
#'     State == "Victoria",
#'     Industry == "Cafes, restaurants and catering services"
#'   ) |>
#'   model(fasster = FASSTER(Turnover ~ trend(1) + season("year")))
#'
#' # Extract and view components
#' components(fit)
#' }
#'
#' @importFrom purrr map_dfc
#' @export
components.FASSTER <- function(object, ...) {
  groups <- vctrs::vec_group_loc(colnames(object$dlm$FF))
  num_terms <- nrow(groups)
  num_states <- ncol(object$dlm$FF)
  for(i in seq_along(groups$loc)){
    groups$loc[[i]] <- groups$loc[[i]] + (i-1)*num_states
  }
  states <- integer(num_terms * num_states)
  states[vctrs::vec_c(!!!groups$loc)] <- 1L
  states <- matrix(states, nrow = num_terms, ncol = num_states,
                   byrow = TRUE, dimnames = list(groups$key))

  combined <- as_tibble(object$states %*% t(states * object$dlm$FF[rep(1,NROW(states)),]))
  response <- measured_vars(object$est)[[1]]

  aliases <- list2(
    !!response := reduce(syms(colnames(combined)), function(x, y, fn) call2(fn, x, y), "+")
  )

  as_dable(
    transmute(object$est, !!sym(response), !!!combined),
    resp = !!sym(response), method = "FASSTER", aliases = aliases
  )
}
