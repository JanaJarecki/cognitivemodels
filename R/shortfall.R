#' Shortfall Risky Choice Model
#' 
#' shortfall(formula, data)
#' 
#' @importFrom stringr str_extract
#' @importFrom Rcpp sourceCpp
#' @inheritParams Cogscimodel
#' @description Fits the shortfall model, a cognitive model for risky choices. It trades off a risky option's expected value with a \eqn{\beta}-weighted measure of risk; risk being the chance of falling short of a \eqn{\delta}-weighted aspiration level (based on Andraszewicz, von Helversen, and Rieskamp). Model inputs are the risky options and the aspiration level. The value \eqn{v} of option \eqn{o}, with free parameters \eqn{\delta, \beta}, is
#' \deqn{v(o) = EV(o) - \beta R(o)}
#' \deqn{R(o) = \sum_i ( p_i ( max [ \delta asp_{o} - out_{o,i} , 0 ] ),}
#' where the parameter \eqn{\beta} represents risk aversion (\eqn{0 \le \beta \le 15}), the parameter \eqn{\delta} represents weight of the aspiration level (\eqn{0 \le \delta \le 1}).
#' @param formula A \link{formula} defining the observations and stimuli in data. Stimuli are defined by outcomes and probabilities, separated by a pipe (\code{|}), like this: \code{mode ~ x1 + x2 + px + I(1-px) | y1 + y2 + py + I(1-py)}, where x1 and x2 are the first option's outcomes, px is the probability Pr(x1), y1 and y2 are the second option's outcomes, and py is the probability Pr(y1).
#' @param asp A \link{formula} defining the aspiration level in data, e.g., (\code{~aspirlev}), strings will be reformulated.
#' @references Andraszewicz, S. (2014). Quntitative [ie Quantitative] analysis
#' of risky decision making in economic environments (Doctoral dissertation,
#'  University_of_Basel). doi: 10.5451/unibas-006268585
#' @return An object of class R6 holding the model, it has free parameters. A model object \code{M} can be viewed with \code{M}, predictions can be made with \code{M$predict()} for choice predictions, and \code{M$predict("ev")} for the expected value of the optimal choice and \code{M$predict("value", 1:2)} for the expected value of all choices.
#' @author Jana B. Jarecki, \email{jj@janajarecki.com}
#' @details Risk-sensitive foraging means you have, for instance, four choices between the same two risky lotteries and after the four choices you need to have accumulated at least 12 points to get a reward. The optimal solution to this choice problem relies on dynamic programming. The function creates all possible future states given the possible remaining trials, and predicts the optimal choice polica or the expected value of chosing either option given a certain state and a certain time horizon.
#' @examples
#' # None yet
#' @export
shortfall <- function(formula, asp, data, fix = list(), choicerule, options) {
  .args <- as.list(rlang::call_standardise(match.call())[-1])
  return(do.call(what = Shortfall$new, args = .args, envir = parent.frame()))
}


Shortfall <- R6Class("shortfall",
  inherit = Cogscimodel,
  public = list(
    asplevel = NULL,
    formulaAsp = NULL,
    initialize = function(formula, asp, fix = NULL, data = NULL, choicerule = NULL, options = NULL) {
      self$formulaAsp <- chr_as_rhs(asp) # store the formula for aspir. level
      # create the allowed parameter space
      # Two free parameter
      #    beta: from 0 - 10, initial value: 1, ignore value: 0
      #    delta: from 0 - 1, initial value: 0.5, ignore with 1
      parspace <- make_parspace(
        beta  = c(0, 10, 1, 0),
        delta = c(0, 1, .5, 1)
        )

      super$initialize(
        title = "Shortfall",
        formula = formula,
        data = data,
        parspace = parspace,
        fix = fix,
        choicerule = choicerule,
        discount = 0L,
        mode = "discrete",
        # Note: keep "grid" as first solver! (flat maximum issues)
        options = c(options, list(fit_solver = c("grid", "solnp")))
      )

      if (self$npar("free") > 0L) {
        message("Fitting free parameters ", .brackify(self$get_parnames("free")))
        self$fit()
      }
    },
    get_more_input = function(d) {
      # this retrieves the aspiration level
      # based on a formula stored in self$formulaAsp
      # which we assign in the initialize() function above
      aspiration <- super$get_input(f = self$formulaAsp, d = d) 
      if (length(aspiration) != nrow(d)*self$nstim()) {
        aspiration <- array(aspiration, dim = c(nrow(d), 1, self$nstim()))
      }
      return(aspiration)
    },
    make_prediction = function(type = "response", input, more_input, isnew, s, ...) {
      # input contains columns:
      #      x1, px1, x2, px2, ...
      # more_input contains:
      #       aspiration level
          
      ## ---- Shortfall function -----
      # Sum_i pi * xi - beta * Sum_i pi * max{delta * a - xi, 0}
      # where
      # xi := outcome numnber i of the option
      # pi :=  probability of the ith outcome
      # a := aspiration level for the option
      # beta, delta are free parameter

      # the shortfall_cpp() function is written i C++, see /src

      ## TODO: make work with unequal no of attributes/stim
      par <- self$get_par()
      na <- self$natt()[1]
      return(shortfall_cpp(
          x = input[,  seq(1, na, 2), drop = FALSE],
          p = input[, -seq(1, na, 2), drop = FALSE],
          a = as.double(more_input),
          beta = as.double(par["beta"]),
          delta = as.double(par["delta"])
          ))
    },
    check_input = function() {
      # this ensures that the input has an even number of columns
      # since we need x1 px1 x2 px2 .... columns
      if (!all(self$natt() %% 2 == 0L)) {
        stop("Formula needs an even number of right-hand elements, but has ", .brackify(self$natt()), ' elements.')
      }

      # this check ensures that the probabilitites in the
      # inpt sum to 1
      # probabilities are the 2nd, 4th, 6th ... column of the input
      if (!all((rowSums(self$input[, -seq(1, self$natt()[1], 2), , drop = FALSE]) == self$nstim()))) {
        stop('Probabilities (which are the 2nd, 4th, 6th... element of right side of "formula") must sum to 1, but the following variables in your data do NOT sum to 1: ', .brackify(attr(terms(self$formula), "term.labels")[seq(1, self$natt()[1], 2)]), '. Check the probability variables in "formula" and in "data".')
      }

      super$check_input() # Do not delete this, it runs default sanity checks
    }
  )
)
