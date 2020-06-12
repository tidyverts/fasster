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

  transmute(object$est, !!sym(response), !!!combined) %>%
    as_dable(resp = !!sym(response), method = "FASSTER", aliases = aliases)
}
