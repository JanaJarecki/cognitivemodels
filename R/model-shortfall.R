# ==========================================================================
# Package: Cognitivemodels
# File: model-shortfall.R
# Author: Jana B. Jarecki
# ==========================================================================

# ==========================================================================
# Cognitive Model
# ==========================================================================


#' Shortfall Risky Choice Model
#' @name shortfall
#' @description
#' Fits the shortfall model for risky choices and judgments (Andraszewicz, 2014).
#' * `shortfall_d()` fits the shortfall model for discrete responses (select option).
#' * `shortfall_c()` fits the shortfall model for continuous responses (judge options). 
#' 
#' @importFrom stringr str_extract
#' @importFrom Rcpp sourceCpp
#' 
#' @eval .param_formula(c(4,4), risky = TRUE)
#' @eval .param_fix("shortfall_d")
#' @param asp A [formula][stats::formula] or a string, the variable in `data` with the aspiration level. For example, `~ x9` or `"x9"`. Can be stimulus-specific: for example `~ x9 | x10` sets x9, x10 as aspiration levels for the 1. and 2. stimulus (respectively).
#'
#' @references
#' {Andraszewicz, S. (2014). Quantitative analysis of risky decision making in economic environments \(Doctoral dissertation, University of Basel\). doi:10.5451/unibas-006268585 }
#'
#'  
#' @details
#' The model trades off the expected value of a risky option with a \eqn{\beta}-weighted measure of the risk of the option. Risk is defined as the chance of falling short of a \eqn{\delta}-weighted aspiration level (see Andraszewicz, 2014). Model inputs are the risky options and the aspiration level. The subjective value \eqn{v} of option \eqn{o} given parameters \eqn{\delta, \beta} is modeled by
#' \deqn{v(o) = EV(o) - \beta R(o)}
#' \deqn{R(o) = \sum_i ( p_i ( max [ \delta asp_{o} - x_{o,i} , 0 ] )}.
#' 
#' ## Model Parameters
#' * _**`beta`**_: the weight of the risk, risk aversion (\eqn{0 \le \beta \le 10}).
#' * _**`delta`**_: the weight of the aspiration level (\eqn{0 \le \delta \le 1}).
#' * In `shortfall_d()`: `r .rd_choicerules()`
#' 
#' @template cm
#' 
#' @examples
#' # Make some data -----------------------------------
#' dt <- data.frame(
#'    x1 = rep(1,3), x2 = rep(2,3), px = rep(.5,3), 
#'    y1 = 0:2, y2 = rep(3,3), py = rep(.5,3),
#'    aspiration = rep(1,3), choice = c(1,1,0))
#' 
#' # Make model ----------------------------------------
#' # 1. Continuous model - normal log likelihood
#' m <- shortfall_c(
#'    formula = choice ~ x1 + px + x2,
#'    asp = "aspiration",
#'    data = dt)
#' 
#' m            # view model
#' predict(m)   # predict values/ratings
#' parspace(m)  # view parameter space
#' 
#' # 2. Discrete model - binomial log likelihood
#' m <- shortfall_d(
#'    formula = choice ~ x1 + px + x2 | y1 + py + y2,
#'    asp = "aspiration",
#'    data = dt,
#'    choicerule = "softmax")
#' 
#' m            # View model
#' predict(m)   # predict choice, Pr(select "x")
#' parspace(m)  # View parameter space

#' @rdname shortfall
#' @export
shortfall_d <- function(formula, asp, data, choicerule, fix = list(), options = NULL) {
  .args <- as.list(rlang::call_standardise(match.call())[-1])
  .args$mode <- "discrete"
  return(do.call(what = Shortfall$new, args = .args, envir = parent.frame()))
}

#' @rdname shortfall
#' @export
shortfall_c <- function(formula, asp, data, fix = list(), options = NULL) {
  .args <- as.list(rlang::call_standardise(match.call())[-1])
  .args$mode <- "continuous"
  return(do.call(what = Shortfall$new, args = .args, envir = parent.frame()))
}

Shortfall <- R6Class("shortfall",
  inherit = Cm,
  public = list(
    asplevel = NULL,
    formulaAsp = NULL,
    initialize = function(formula, asp, fix = NULL, data = NULL, choicerule = NULL, mode, options = NULL) {
      self$formulaAsp <- chr_as_rhs(asp) # store the formula for aspir. level

      # Parameter space
      # Two free parameter
      #    beta: from 0 - 10, initial value: 1, ignore: 0
      #    delta: from 0 - 1, initial value: 0.5, ignore: 1
      parspace <- make_parspace(
        beta  = c(0, 10, 1, 0),
        delta = c(0, 1, .5, 1)
        )

      super$initialize(
        title = "Shortfall",
        formula = .add_missing_prob(formula),
        data = data,
        parspace = parspace,
        fix = fix,
        choicerule = choicerule,
        discount = 0L,
        mode = mode,
        # Note: keep "grid" as first solver! (flat maximum issues)
        options = c(options, list(solver = c("grid", "solnp")))
      )
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
      na <- self$natt[1]
      return(shortfall_cpp(
          x = input[,  seq(1, na, 2), drop = FALSE],
          p = input[, -seq(1, na, 2), drop = FALSE],
          a = as.double(more_input),
          beta = as.double(par["beta"]),
          delta = as.double(par["delta"])
          ))
    }
  ),
  private = list(
    get_more_input = function(d) {
      # this retrieves the aspiration level
      # based on a formula stored in self$formulaAsp
      # which we assign in the initialize() function above
      aspiration <- super$get_input(f = self$formulaAsp, d = d) 
      if (length(aspiration) != nrow(d)*self$nstim) {
        aspiration <- array(aspiration, dim = c(nrow(d), 1, self$nstim))
      }
      return(aspiration)
    },
    check_input = function() {
      .check_probabilities(self = self)
      super$check_input() # Do not delete this, it runs default sanity checks
    },
    make_prednames = function() {
      return(self$stimnames)
    }
  )
)
