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
  transmute(object$est, !!!combined)
}
