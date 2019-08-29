#' Linear-weighting model
#' @keywords internal
#' @import quadprog
lwei <- function(formula, nopt, nout, ref, fix = NULL, data, choicerule, weighting = c('TK1992'), value = c('TK1992')) {
  obj <- Lwei$new(formula, nopt, nout, ref, fix = fix, data, choicerule, weighting = c('TK1992'), value = c('TK1992'))
  if (length(obj$fixnames) < length(obj$par)) {
    obj$fit()
  }
  return(obj)
}



# orthonormal: weights sum to 1
Lwei <- R6Class("lwei",
  inherit = cognitiveModel,
  public = list(
    type = NULL,
    initialize = function(formula, type = c("orthonormal"), fix = NULL, data = NULL, choicerule) {
      type = match.arg(type)
      formula <- as.Formula(formula)

      parspace <- make_parspace(type, formula)     
      super$initialize(formula = formula, data = data, parspace = parspace, fixpar = fix, choicerule =  choicerule, model = paste0("Linear weighting model (", type, ")"), discount = 0)

      self$type = type
    },
    predict = function(type = c("choice", "value"), action = NULL, newdata = NULL) {
      type <- match.arg(type)

      if (!is.null(newdata)) {
        input <- model.frame(formula, newdata)[, -1, drop = F]
      } else {
        input <- self$input
      }
       
      OptionList <- lapply(seq_along(self$nout), function(i) {
        model.matrix(Formula(self$formula), input, rhs = i)[, -1, drop = FALSE]
      })

      v <- lapply(OptionList, function(i) self$cpt(
        x = i[, seq_along(self$nout)],
        p = i[, -seq_along(self$nout)]))
      v <- matrix(unlist(v), ncol = length(v))
      
      nn <- unlist(lapply(OptionList, function(x) unique(str_extract(dimnames(x)[[2]], pattern = "[a-z]+"))[1]))
      if (length(unique(nn)) < self$nopt) {
        nn <- paste0(nn[1], seq_len(self$nopt))
      }
      colnames(v) <- nn

      switch (type,
        choice = super$apply_choicerule(v),
        value = v)
    },
    fit = function(type = c("quadprog")) {
      type = match.arg(type, several.ok = TRUE)

      input <- as.matrix(self$input)
      obs <- self$obs

      if ("quadprog" %in% type) {
        # Method credits: Elvis, https://stats.stackexchange.com/questions/21565/how-do-i-fit-a-constrained-regression-in-r-so-that-coefficients-total-1
        n <- ncol(input)
        Rinv <- solve(chol(t(input) %*% input))
        # C <- cbind(rep(1,n), diag(n)) # constraints
        C <- cbind(1, rbind(diag(n), -diag(n)))
        b <- c(1, rep(0, n))
        d <- t(obs) %*% input
        fit <- solve.QP(Dmat = Rinv, factorized = TRUE, dvec = d, Amat = C, bvec = b, meq = 1)
        best_par <- fit$solution
        names(best_par) <- colnames(input)
        super$setPar(best_par)
      }     
    },
    make_parspace = function(type = self$type, formula = self$formula) {
      rn <- c(ifelse(attr(terms(formula), c("intercept"))==1, '(inter)', NULL), attr(terms(formula), c("term.labels")))

      if (type == 'orthonormal') {
        return(matrix(c(0,1,.5), nrow = length(rn), ncol = 3, byrow = TRUE, dimnames = list(rn, c('ul', 'll', "start"))))     
      }
    }
  )
)

# predict.Lwei <- function(obj, type = NULL, action = NULL, newdata = NULL) {
#   return(obj$predict(type = type, action = action, newdata = newdata))
# }
