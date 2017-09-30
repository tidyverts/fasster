formula_parse_infix <- function(.formula, infix=NULL) {
  fn_name <- deparse(.formula[[1]])
  if (fn_name %in% infix) {
    .formula[[1]] <- as.name(paste(".infix", substr(fn_name, 2, nchar(fn_name) - 1), sep = "."))
  }
  for (pos in 2:length(.formula)) {
    if (is.call(.formula[[pos]])) {
      .formula[[pos]] <- formula_parse_infix(.formula[[pos]], infix)
    }
  }
  .formula
}

formula_parse_groups <- function(.formula) {
  if (length(.formula) > 2) {
    .formula[[2]] <- NULL
  }
  parse <- list()
  .formula <- formula_parse_infix(.formula, "%S%")
  mt <- terms(.formula, specials = ".infix.S", allowDotAsName = TRUE)
  groupIdx <- attr(mt, "specials")[[".infix.S"]]

  sub_mt <- if (is.null(groupIdx)) mt
  else mt[-groupIdx]

  ## Add current layer's formula
  if (length(attr(sub_mt, "variables")) > 1) {
    parse[[".model"]] <- formula(sub_mt)
  }

  ## Explore more groups
  for (pos in groupIdx) {
    groupCall <- as.list(attr(mt, "variables")[[1L + pos]])
    while (class(groupCall[[3]]) == "(") {
      groupCall[[3]] <- groupCall[[3]][[2]]
    }
    parse[[deparse(groupCall[[2]])]] <- formula_parse_groups(as.formula(paste("~", deparse(groupCall[[3]], width.cutoff = 500))))
  }

  parse
}
