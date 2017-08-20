#' @importFrom forecast seasonal trendcycle remainder
var_stl_decomp <- function(stlobj){
  list(seasonal = var(diff(stlobj[,"seasonal"], frequency(stlobj))),
    trendcycle = var(diff(stlobj[,"trend"])),
    remainder = var(stlobj[,"remainder"]),
    stlobj = stlobj)
}

#' @export
#' @import purrr
#' @importFrom magrittr extract2
fasster_stl <- function(response, groupData, s.window, stlFreqs){

  ## Rough structure
  # - Take in object, determine grouping and required variables
  # - Split by each group and perfrom stl decomposition on the response, with frequency according to each seasonal term in the switching model

  ## Estimate optimal parameters specific to model groupings
  browser()
  out <- response %>%
    split(groupData) %>%
    map(~ list(.x) %>%
          map2(stlFreqs, ~ .x %>%
            ts(frequency = .y) %>%
            stl(s.window = s.window) %>%
            extract2("time.series") %>%
            var_stl_decomp
          )
        )
  # out$`_total` <- out %>%
  #   map(~ .x %>%
  #         extract2("stlobj")) %>%
  #   invoke(rbind, .) %>%
  #   var_stl_decomp()
}
