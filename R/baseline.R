#' Baseline cognitive models
#' 
#' \code{baseline()} makes simple stimulus-agnostic models, \code{baseline_const()} predicts a fixed value, \code{baseline_mean()} predicts the mean observed value.
#' 
#' @rdname baseline
#' 
#' @inheritParams Cogscimodel
#' @param formula A formula specifying the observed variable as left hand (e.g., \code{y ~ .}). Note the "\code{~}" and "\code{.}", please.
#' @param type A string, can be \code{"constant", "mean"} specifying the type of baseline model (see details).
#' @param const (optional, \bold{required} if \code{type = "constant"}) A number with the prediction of the model, must be a probability.
#' 
#' @section Important:
#' \emph{Note}, \bold{these models are only implemented for discrete responses}.
#' 
#' @details
#' 
#' Baseline models are very simple models that ignore the stimuli. Baseline models are used as sanity checks in cognitive model comparisons. Other cognitive models should beat the baseline model -- if not, the other cognitive models do not describe the observed data well.
#' 
#' The \bold{\code{type = "constant"}} or \code{baseline_const()} predicts the fixed value \code{const} for all trials. A common baseline in binary choice tasks is \emph{Pr=0.50}, for which we would use \code{const = 0.50}. The shorthand \code{baseline_const()} calls \code{baseline()} with \code{type = "constant"}.
#' 
#' @examples
#' # Make some fake data
#' D <- data.frame(y = c(1,1,0), x = c(1,2,3))
#' 
#' M <- baseline_const(y~., D, const = 0.5) # Constant Pr = 0.5
#' predict(M)                               # predicts 0.5, 0.5, 0.5
#' M$predict()                              # -- " --
#' M$npar()                                 # 0 parameter
#' M$logLik()                               # log likelihood (binomial)
#' 
#' M <- baseline_mean(y~., D)               # Pr = mean(observed variable)
#' predict(M)                               # predicts 2/3, 2/3, 2/3
#' M$predict()                              # -- " --
#' M$npar()                                 # 1 free parameter
#' 
#' @family Cognitive models
#'  
#' @export
baseline_const <- function(formula, data, const, ...) {
  .args <- as.list(rlang::call_standardise(match.call())[-1])
  .args$type <- "constant"
   return(do.call(what = Baseline$new, args = .args, envir = parent.frame()))
}


#' Baseline cognitive models
#' 
#' @details The \bold{\code{type = "mean"}} or \code{baseline_mean()} predicts a fixed value, namely the mean of the observed responses, for all trials. It thus has one free parameter. It calls \code{baseline()} with \code{type = "mean"}.
#' 
#' @rdname baseline
#' 
#' @export
baseline_mean <- function(formula, data, ...) {
  .args <- as.list(rlang::call_standardise(match.call())[-1])
  .args$type <- "mean"
   return(do.call(what = Baseline$new, args = .args, envir = parent.frame()))
}

#' Baseline cognitive models
#' 
#' @rdname baseline
#' 
#' @export
baseline <- function(formula, data, type, const, ...) {
  .args <- as.list(rlang::call_standardise(match.call())[-1])
   return(do.call(what = Baseline$new, args = .args, envir = parent.frame()))
}

Baseline <- R6Class("baseline",
  inherit = Cogscimodel,
  public = list(
    type = NULL,
    const = NULL,
    initialize = function(formula, data, type, const, ...) {
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
        mode = "discrete",
        options = list(fit = FALSE))
    },
    predict = function(type = c("response"), newdata = NULL) {
      type <- match.arg(type)
      if (is.null(newdata) | missing(newdata)) {
        D <- self$input
      } else {
        D <- self$getInput(f = self$formula, f = newdata)
      }
      out <- switch(self$type,
        constant = self$const,
        mean = apply(self$res, 2, mean))
      `colnames<-`(
        matrix(out, nrow = nrow(D), ncol = self$nres()),
        paste0("pred_", self$get_stimnames())
        )
    }
  )
)
