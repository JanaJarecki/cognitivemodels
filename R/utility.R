#' Utility functions
#' 
#' @description Fits a utility function.
#' @param formula A formula specifying the variable names, like \code{yvar ~ xvar}
#' @param data A data.frame or matrix used to fit the model to.
#' @param type A string, currently only \code{"power"} for power utility.
#' @param fixed (optional) A list with fixed parameter(s).
#' @param choicerule (optional) A string specifying the choice rule, allowed values see the \code{priordist} argument of \link{choicerule}. *Required* if \code{response = "discrete"} and model learns > 1 options.
#' @param response (optional, default \code{"continuous"}) A string specifying if beliefs are predicted (\code{"continuous"}) or if choices are predicted (\code{"discrete"}).
#' @param discount (optional, default 0) A number or numeric vector of trial indices to discount.
#' @param ... other arguments from other functions, currently ignored.
#' @return An model object (similar to lm-objects) of class "utility". Itcan be viewed with \code{summary(mod)}, where \code{mod} is the name of the model object.
#' @examples 
#' #  No examples yet
#' @author Jana B. Jarecki, \email{jj@janajarecki.com}, Markus Steiner
#' @references None yet

#' @export
utility <- function(formula, data, type = c('power'), fixed = list("alpha"), choicerule = NULL, response = c('continuous', 'discrete'), discount = 0, ...) {
  args <- match.arg()[-1]
  obj <- do.call(Bayeslearn$new, args)
  # check(mod) #todo
  return(obj)
}

Bayeslearn <- R6Class('bayeslearn',
  inherit = Cogscimodel,
  public = list(
    priordist = NULL,
    format = NULL,
    priornames = NULL,
    initialize = function(formula, data, format = c('raw', 'count'), fixed, choicerule, response, discount, ...) {
      format = match.arg(format)
      self$format <- format
      formula <- self$correct_single_rhs(formula)
      self$priornames <- paste0('prior.', attr(terms(formula(formula, lhs=0, rhs=1)), 'term.labels'))

      allowedparm <- matrix(cbind(0.01, 1.99, rep(1, length(self$priornames)), 1), nc = 4, dimnames = list(self$priornames, c('ll', 'ul', 'init', 'na')))
      super$initialize(formula = formula,
        data = data,
        allowedparm = allowedparm,
        choicerule = choicerule,
        response = response,
        model = paste0('Bayesian Learning'),
        fixed = fixed,
        discount = discount,
        ...)
      self$priordist <- self$inferpriordist()
      self$model <- paste0(self$model, ' [', self$priordist, ']')
    },