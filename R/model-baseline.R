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
#' Baseline models are simple stimulus-agnostic models, \code{baseline_const_c(), baseline_const_d()} predict a constant value for continuous and discrete data (resp.), \code{baseline_mean_c(), baseline_mean_d()} predict the mean of the observed values for continuous and discrete response data (resp.).
#' 
#' @rdname baseline
#' 
#' @inheritParams Cm
#' @param formula A formula, the observed variables, with only a left-hand-side (for example: \code{response ~ .}), note the "\code{~ .}" on the right.
#' @param const (optional, \bold{required} in \code{baseline_const_}) A number, the constant value that the model should predict.
#' 
#' @details
#' Baseline models are very simple models that ignore the stimuli. Baseline models are used as sanity checks in cognitive model comparisons. Other cognitive models are usually supposed to beat the baseline model -- if not, the other cognitive models do very likely not describe the observed data well.
#' 
#' Use \bold{\code{baseline_..._d}} for discrete response variables; and use \bold{\code{baseline_..._c}} for continuous response variables.
#' 
#' \bold{\code{baseline_const_}} predicts the numeric value \code{const} for all trials. A common baseline model in binary choice tasks is, for instance, a model predicting \emph{Pr=0.50} for each trial, for which you set \code{const = 0.50}. The model \bold{baseline_mean_} predicts the mean of the observed choices for each trial. This model has at least one free parameter (the mean).
#' 
#' @examples
#' # Here is some data, let y be the observed data
#' D <- data.frame(y = c(1,1,0), x = c(1,2,3))
#' 
#' # Baseline model that predicrs Pr = 0.50
#' M <- baseline_const_c(y ~ ., const = 0.50, data = D)
#' 
#' predict(M)                         # predicts 0.5, 0.5, 0.5
#' npar(M)                            # 0 parameter
#' logLik(M)                          # log likelihood (binomial)
#' 
#' M <- baseline_mean_d(y ~ ., D)     # Pr = mean(observed variable)
#' predict(M)                         # predicts 0.66, 0.66, 0.66
#' coef(M)                            # mean counts as free parameter
#' npar(M)                            # 1 free parameter, the mean
#' 
#' @family Cognitive models
#' @export
baseline_const_c <- function(formula, consr, data = NULL, ...) {
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
baseline_mean_c <- function(formula, data = NULL, ...) {
  .args <- as.list(rlang::call_standardise(match.call())[-1])
  .args$mode <- "continuous"
  .args$type <- "mean"
   return(do.call(what = Baseline$new, args = .args, envir = parent.frame()))
}


#' Baseline models
#' 
#' @rdname baseline
#' @export
baseline_mean_d <- function(formula, data = NULL, ...) {
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
    initialize = function(formula, data = NULL, type, const, mode, ...) {
      self$type <- match.arg(type, c("constant", "mean"))
      if (self$type == "mean") {
        ps <- make_parspace(m = range(super$get_res(f=formula, d=data)))
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
        options = list(fit = FALSE))
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
