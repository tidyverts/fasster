#' @importFrom fable components
#' @export
components.FASSTER <- function(object, ...) {
  browser()
  states <- object$dlm$FF %>%
    colnames() %>%
    factor %>%
    spread_groups() %>%
    t

  tibble(index = object$index) %>%
    bind_cols((states * object$dlm$FF[rep(1,NROW(states)),]) %*% t(object$states) %>% t %>% as_tibble) %>%
    as_tsibble(index=index)
}
