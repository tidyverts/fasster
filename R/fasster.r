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

formula_parse_groups <- function(.formula){
  #browser()
  parse <- list()
  .formula <- formula_parse_infix(.formula, "%G%")
  mt <- terms(.formula, specials = ".infix.G", allowDotAsName=TRUE)
  groupIdx <- attr(mt, "specials")[[".infix.G"]]

  if(is.null(groupIdx)){
    ## No more groups
    parse[["formula"]] <- .formula
  }
  else{
    ## Explore groups
    sub_mt <- delete.response(mt)[-c(groupIdx-1)]
    if(length(attr(sub_mt, "variables")) > 0){
      parse[["formula"]] <- .formula
    }
    parse[["formula"]] <-
    for(pos in groupIdx){
      groupCall <- as.list(attr(mt, "variables")[[1L + pos]])
      while(class(groupCall[[3]]) == "("){
        groupCall[[3]] <- groupCall[[3]][[2]]
      }
      parse[[deparse(groupCall[[2]])]] <- formula_parse_groups(as.formula(paste("~", deparse(groupCall[[3]], width.cutoff = 500))))
    }
  }

  parse
}
