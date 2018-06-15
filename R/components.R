#' @importFrom fable components
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

  data.frame(index = object$index) %>%
    cbind((states * object$dlm$FF[rep(1,NROW(states)),]) %*% t(object$states) %>% t %>% as_tibble) %>%
    as_tsibble(index=index)
}
