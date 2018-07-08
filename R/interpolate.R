#' Interpolate missing values using a model
#'
#' Fills in the missing values from a model's response variable using the fitted values from the model.
#'
#' @param object a mable
#' @param ... Not used
#'
#' @return
#' A tsibble with missing values interpolated
#'
#' @export
interpolate <- function(object, ...){
  UseMethod("interpolate")
}

#' @export
interpolate.mable <- function(object, ...){
  purrr::map2(object$data, object$model, function(data, model){
    resp <- expr_text(model%@%"response")
    missingVals <- is.na(data[[resp]])
    data[[resp]][missingVals] <- fitted(model)[missingVals]
    data
  }) %>%
    invoke("rbind", .)
}
