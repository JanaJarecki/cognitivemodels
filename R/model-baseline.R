# ==========================================================================
# Package: Cognitivemodels
# File: model-baseline.R
# Author: Jana B. Jarecki
# ==========================================================================

# ==========================================================================
# Cognitive Model
# ==========================================================================


#' Baseline models
#' 
#' Fits baseline models. Baseline models are stimulus-agostic models used as sanity checks in cognitive model comparisons. Other cognitive models should beat a baseline model -- if not, the other cognitive models don't describe patterns in the responses well.
#'   * `baseline_const_c()` predicts a constant value for continuous responses.
#'   * `baseline_const_d()` predicts a constant value for discrete responses.
#'   * `baseline_mean_c()` fits the mean of the observed responses for continuous responses.
#'   * `baseline_mean_d()` fits the mean of the observed responses for discrete responses.
#' 
#' @rdname baseline
#' 
#' @template cm
#' @param formula A formula, the variable in `data` to be modeled. For example, `y ~ .` models a response variable `y` (note the `~ .` after the variable name).
#' @param const A number, the value to predict.
#' 
#' @details
#' `baseline_const_c/d` predicts the value given in `const` for all trials. For example `const = 0.50` would predict \emph{Pr=0.50} for each trial, which is a commmon baseline model for tasks with two-outcome discrete choices.
#' 
#' @section Parameter:
#'  * `baseline_const_c/d` has no free parameter
#'  * `baseline_mean_c/d` has 1 free parameter, `mu`, the mean
#'  * `baseline_mean_c`, if estimated via log likelihood, has an additional free parameter, `sigma`, the standard deviation of the normal log likelihood.
#' 
#' @examples
#' # Data D: let y hold the observed responses
#' # Make a model that predicts Pr = 0.50
#' D <- data.frame(y = c(1,1,0), x = c(1,2,3))
#' M <- baseline_const_d(y ~ ., const = 0.50, data = D)
#' predict(M)                         # predicts 0.5, 0.5, 0.5
#' npar(M)                            # 0 parameter
#' logLik(M)                          # log likelihood (binomial)
#' 
#' M <- baseline_mean_d(y ~ ., D)     # Pr = mean(observed variable)
#' predict(M)                         # predicts 0.66, 0.66, 0.66
#' coef(M)                            # mean counts as free parameter
#' npar(M)                            # 1 free parameter, the mean
#' 
#' @export
baseline_const_c <- function(formula, const, data, options, discount, ...) {
  .args <- as.list(rlang::call_standardise(match.call())[-1])
  .args$type <- "constant"
  .args$mode <- "continuous"
   return(do.call(what = Baseline$new, args = .args, envir = parent.frame()))
}

#' @rdname baseline
#' 
#' @export
baseline_const_d <- function(formula, const, data, ...) {
  .args <- as.list(rlang::call_standardise(match.call())[-1])
  .args$type <- "constant"
  .args$mode <- "discrete"
   return(do.call(what = Baseline$new, args = .args, envir = parent.frame()))
}

#' Baseline models
#' 
#' @rdname baseline
#' @export
baseline_mean_c <- function(formula, data, ...) {
  .args <- as.list(rlang::call_standardise(match.call())[-1])
  .args$mode <- "continuous"
  .args$type <- "mean"
   return(do.call(what = Baseline$new, args = .args, envir = parent.frame()))
}


#' Baseline models
#' 
#' @rdname baseline
#' @export
baseline_mean_d <- function(formula, data, ...) {
  .args <- as.list(rlang::call_standardise(match.call())[-1])
  .args$mode <- "discrete"
  .args$type <- "mean"
  return(do.call(what = Baseline$new, args = .args, envir = parent.frame()))
}

Baseline <- R6Class("baseline",
  inherit = Cm,
  public = list(
    type = NULL,
    const = NULL,
    initialize = function(formula, data = NULL, type, const, mode, options = list(), ...) {
      if (missing(data)) data <- NULL
      self$type <- match.arg(type, c("constant", "mean"))
      if (self$type == "mean") {
        res <- super$get_res(f=formula, d=data)
        ps <- make_parspace(mu = c(range(res), apply(res,2,mean)))
      } else {
        ps <- make_parspace()
        self$const <- const
      }
      super$initialize(
        title = paste("Baseline:", self$type),
        formula = update(formula, formula[1:2]),
        parspace = ps,
        data = data,
        mode = mode,
        choicerule = "none",
        options = options)
    },
    predict = function(type = "response", newdata = NULL) {
      type <- match.arg(type, "response")
      D <- if (is.null(newdata)) {
        D <- self$input
      } else {
        private$get_input(f = self$formula, d = newdata)
      }
      out <- switch(self$type, # this is the model type, not response type
        constant = self$const,
        mean = apply(self$res, 2, mean, simplify = FALSE))
      out <- matrix(out, nrow = nrow(D), ncol = self$nstim)
      colnames(out) <- self$prednames
      return(drop(out))
    }
  )
)
