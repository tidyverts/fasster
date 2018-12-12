fasster_specials <- list(
  `%S%` = function(group, expr){
    group_expr <- enexpr(group)
    lhs <- factor(group)
    groups <- levels(lhs) %>% map(~ as.numeric(lhs == .x)) %>% set_names(levels(lhs))

    rhs <- parse_model_rhs(enexpr(expr), data = .data, specials = .specials)$specials %>%
      unlist(recursive = FALSE) %>%
      reduce(`+`)

    groups %>%
      imap(function(X, groupVal){
        if(is.null(rhs$JFF)){
          rhs$JFF <- rhs$FF
          rhs$X <- matrix(X, ncol = 1)
        }
        else{
          rhs$X <- rhs$X * X
          if(any(new_X_pos <- rhs$FF!=0 & rhs$JFF==0)){
            new_X_col <- NCOL(rhs$X) + 1
            rhs$JFF[new_X_pos] <- new_X_col
            rhs$X <- cbind(rhs$X, X)
          }
        }
        colnames(rhs$X) <- paste0(expr_text(group_expr), "_", groupVal, "/", colnames(rhs$X))
        colnames(rhs$FF) <- paste0(expr_text(group_expr), "_", groupVal, "/", colnames(rhs$FF))
        rhs
      }) %>%
      reduce(`+`)
  },
  `(` = function(expr){
    parse_model_rhs(enexpr(expr), data = .data, specials = .specials)$specials %>%
      unlist(recursive = FALSE) %>%
      reduce(`+`)
  },
  poly = function(...){
    cl <- match.call()
    out <- dlmModPoly(...)
    colnames(out$FF) <- rep(deparse(cl), NCOL(out$FF))
    out
  },
  seas = function(...){
    cl <- match.call()
    out <- dlmModSeas(...)
    colnames(out$FF) <- rep(deparse(cl), NCOL(out$FF))
    out
  },
  seasonal = function(...){
    cl <- match.call()
    out <- dlmModSeas(...)
    colnames(out$FF) <- rep(deparse(cl), NCOL(out$FF))
    out
  },
  trig = function(...){
    cl <- match.call()
    out <- dlmModTrig(...)
    colnames(out$FF) <- rep(deparse(cl), NCOL(out$FF))
    out
  },
  fourier = function(...){
    cl <- match.call()
    out <- dlmModTrig(...)
    colnames(out$FF) <- rep(deparse(cl), NCOL(out$FF))
    out
  },
  ARMA = function(...){
    cl <- match.call()
    out <- dlmModARMA(...)
    colnames(out$FF) <- rep(deparse(cl), NCOL(out$FF))
    out
  },
  custom = function(...){
    cl <- match.call()
    out <- dlm(...)
    colnames(out$FF) <- rep(deparse(cl), NCOL(out$FF))
    out
  },
  xreg = function(...){
    model_formula <- new_formula(
      lhs = NULL,
      rhs = reduce(c(0, enexprs(...)), ~ call2("+", .x, .y))
    )
    mm <- eval_tidy(model.matrix(model_formula), data = .data)
    out <- dlmModReg(mm, addInt = FALSE)
    colnames(out$FF) <- colnames(mm)
    out
  }
)

.specials <- new_specials_env(
  !!!fasster_specials
)
