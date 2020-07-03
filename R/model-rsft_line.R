# ==========================================================================
# Package: Cognitivemodels
# File: model-rsft_line.R
# Author: Jana B. Jarecki
# ==========================================================================

# ==========================================================================
# Cognitive Model
# ==========================================================================


#' Line risk-sensitive foraging model
#' 
#' shortfall(formula, data)
#' 
#' @importFrom stringr str_extract
#' @importFrom Rcpp sourceCpp
#' @description Fits the shortfall model, a cognitive model for risky choices. It trades off a risky option's expected value with a \eqn{\beta}-weighted measure of risk; risk being the chance of falling short of a \eqn{\delta}-weighted aspiration level (based on Andraszewicz, von Helversen, and Rieskamp). Model inputs are the risky options and the aspiration level. The value \eqn{v} of option \eqn{o}, with free parameters \eqn{\delta, \beta}, is
#' \deqn{v(o) = EV(o) - \beta R(o)}
#' \deqn{R(o) = \sum_i ( p_i ( max [ \delta asp_{o} - out_{o,i} , 0 ] ),}
#' where the parameter \eqn{\beta} represents risk aversion (\eqn{0 \le \beta \le 15}), the parameter \eqn{\delta} represents weight of the aspiration level (\eqn{0 \le \delta \le 1}).
#' @param formula A \link{formula} defining the observations and stimuli in data. Stimuli are defined by outcomes and probabilities, separated by a pipe (\code{|}), like this: \code{mode ~ x1 + x2 + px + I(1-px) | y1 + y2 + py + I(1-py)}, where x1 and x2 are the first option's outcomes, px is the probability Pr(x1), y1 and y2 are the second option's outcomes, and py is the probability Pr(y1).
#' @param budget A \link{formula} defining the budget, e.g., (\code{~goal}), strings will be reformulated.
#' @param state A \link{formula} defining the budget, e.g., (\code{~goal}), strings will be reformulated.
#' @param ntrials A \link{formula} defining the budget, e.g., (\code{~goal}), strings will be reformulated.
#' @return An object of class R6 holding the model, it has free parameters. A model object \code{M} can be viewed with \code{M}, predictions can be made with \code{M$predict()} for choice predictions, and \code{M$predict("ev")} for the expected value of the optimal choice and \code{M$predict("value", 1:2)} for the expected value of all choices.
#' @author Jana B. Jarecki, \email{jj@janajarecki.com}
#' @examples
#' # None yet
#' @noRd
#' @export
rsft_line <- function(formula, trials, states, budget, ntrials, data, fix = list(), choicerule, options) {
  .args <- as.list(rlang::call_standardise(match.call())[-1])
  return(do.call(what = Rsft_line$new, args = .args, envir = parent.frame()))
}


Rsft_line <- R6Class("rsft_line",
  inherit = Cm,
  public = list(
    formulaTrial = NULL,
    formulaState = NULL,
    formulaNtrials = NULL,
    formulaBudget = NULL,
    initialize = function(formula, trials, states, budget, ntrials, fix = NULL, data = NULL, choicerule = NULL, options = NULL) {
      self$formulaTrial <- chr_as_rhs(trials)
      self$formulaState <- chr_as_rhs(states)
      self$formulaNtrials <- chr_as_rhs(ntrials)
      self$formulaBudget <- chr_as_rhs(budget)
      parspace = make_parspace(alpha = c(0,2,0.5,1), beta = c(0,2,0.5,1))

      super$initialize(
        title = "RSFT line",
        formula = formula,
        data = data,
        parspace = parspace,
        fix = fix,
        choicerule = choicerule,
        discount = 0L,
        mode = "discrete",
        options = c(options, list(solver = c("solnp")))
      )
    },
    predict = function(type = "response", ...) {
      par <- self$get_par()
      alpha <- par["alpha"]
      beta <- par["beta"]
      # input contains columns:
      #      x1, px1, x2, px2, ...
      # more_input contains:
      #       budget level
      # 
      # Line Formula:
      # 
      #      a + a * (Time horizon) - state  > 0 => safe; <= 0 => risky
      #      
      #      where a is the highest value of the low variance option
      na <- self$natt[1]
      a  <- apply(self$input[, seq(1, na, 2), , drop = FALSE], 1, function(x) x[(rank(-x, ties.method = "random")) == 2])
      t_left <- self$more_input$timehorizon
      x_left <- self$more_input$need
      return(super$apply_choicerule(matrix(x_left - (alpha * a + beta * a * t_left))))
    }
  ),
  private = list(
    get_more_input = function(d) {
      # this retrieves the time horizon and the states
      # based on a formula stored in self$formulaAsp
      # which we assign in the initialize() function above
      trials <- super$get_input(f = self$formulaTrial, d = d)
      states <- super$get_input(f = self$formulaState, d = d)
      ntrials <- super$get_input(f = self$formulaNtrials, d = d)
      budget <- super$get_input(f = self$formulaBudget, d = d)
      res <- list(timehorizon = c(ntrials - trials), need = c(budget) - c(states))
      return(res)
    },
    check_input = function() {
      # this ensures that the input has an even number of columns
      # since we need x1 px1 x2 px2 .... columns
      if (!all(self$natt %% 2 == 0L)) {
        stop("Formula needs an even number of right-hand elements, but has ", .brackify(self$natt), ' elements.')
      }

      # this check ensures that the probabilitites in the
      # inpt sum to 1
      # probabilities are the 2nd, 4th, 6th ... column of the input
      if (!all((rowSums(self$input[, -seq(1, self$natt[1], 2), , drop = FALSE]) == self$nstim))) {
        stop('Probabilities (which are the 2nd, 4th, 6th... element of right side of "formula") must sum to 1, but the following variables in your data do NOT sum to 1: ', .brackify(attr(terms(self$formula), "term.labels")[seq(1, self$natt[1], 2)]), '. Check the probability variables in "formula" and in "data".')
      }

      super$check_input() # Do not delete this, it runs default sanity checks
    }
  )
)
