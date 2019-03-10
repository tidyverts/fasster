#' @importFrom purrr map_dfc
#' @export
components.FASSTER <- function(object, ...) {
  groups <- object$dlm$FF %>%
    colnames() %>%
    factor

  states <- levels(groups) %>%
    map_dfc(~ as.numeric(groups == .x)) %>%
    set_names(levels(groups)) %>%
    as.matrix %>%
    t

  combined <- as_tibble(object$states %*% t(states * object$dlm$FF[rep(1,NROW(states)),]))
  response <- measured_vars(object$est)[[1]]

  aliases <- list2(
    !!response := reduce(syms(colnames(combined)), function(x, y, fn) call2(fn, x, y), "+")
  )

  transmute(object$est, !!sym(response), !!!combined) %>%
    as_dable(resp = !!sym(response), method = "FASSTER", aliases = aliases)
}
