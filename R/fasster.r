#' @keywords internal
"_PACKAGE"

#' @importFrom magrittr %>%
#' @export
dplyr::`%>%`


formula_parse_infix <- function(.formula, infix=NULL){
  fn_name <- deparse(.formula[[1]])
  if(fn_name %in% infix){
    .formula[[1]] <- as.name(paste("infix", substr(fn_name, 2, nchar(fn_name) - 1), sep="."))
  }
  for(pos in 2:length(.formula)){
    if(is_call(.formula[[pos]])){
      .formula[[pos]] <- formula_parse_infix(.formula[[pos]], infix)
    }
  }
  .formula
}

