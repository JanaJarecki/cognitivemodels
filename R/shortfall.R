#' Shortfall Risky Choice Model
#' 
#' @importFrom stringr str_extract
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
# #' # Tversky, A., & Kahneman, D. (1992). Advances in prospect theory: cumulative representation of uncertainty. Journal of Risk and Uncertainty, 5, 297–323. doi:10.1007/BF00122574
# p. 313
#' dt <- data.frame(
#'   x1 = c(100, -100),
#'   px = 1,
#'   x2 = 0,
#'   y1 = c(200, -200),
#'   py = c(.71,.64),
#'   y2 = 0,
#'   rp = 1)
#' 
#' # Make the model, add fix parameters (don't fit)
#' M <- cpt(rp ~ x1 + x2 + px + I(1-px) | y1 + y2 + py + I(1-py),
#'          nopt = 2, nout = 2, ref = 0, choicerule = "softmax", data = dt,
#'          fix = list(alpha = 0.88, beta = 0.88, lambda = 2.25,
#'          gammap = 0.61, gamman = 0.69, tau = 1))
#' # View the model
#' M
#' M$predict("value") # predict values
#' M$predict("mode") # predict choices after soft-max
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
      self$formulaAsp <- chr_as_rhs(asp)
      self$asplevel <- self$get_input(f = self$formulaAsp, d = data)
      parspace <- make_parspace(
        beta  = c(0, 15, 1, 0),
        delta = c(0, 1, .5, 1)
        )

      super$initialize(
        title = "Shortfall",
        formula = formula,
        data = data,
        parspace = parspace,
        fix = fix,
        choicerule = choicerule,
        discount = 0,
        mode = "discrete",
        options = c(options, list(fit_solver = c("grid", "solnp"))))

      if (self$npar("free") > 0L) {
        message("Fitting free parameters ", .brackify(self$get_parnames("free")))
        self$fit()
      }
    },
    predict = function(type = c("response", "values"), action = NULL, newdata = NULL) {
      type <- match.arg(type)
      parameters <- self$get_par()
      beta  <- parameters["beta"]
      delta <- parameters["delta"]

      if (is.null(newdata)) {
        input <- self$input
        asplevel <- self$asplevel
      } else {
        input <- self$get_input(f = self$formula, d = newdata)
        asplevel <- self$get_input(f = self$formulaAsp, d = newdata)
      }

      
      #Todo: make this flexible to handle different stimuli with feature lengths
      X <- input[,  seq(1, self$natt()[1], 2), , drop = FALSE]
      P <- input[, -seq(1, self$natt()[1], 2), , drop = FALSE]

      ns <- self$nstim()
      EV <- sapply(1:ns, function(s) {
        rowSums(X[, , s] * P[, , s])
      })
      R <- sapply(1:ns, function(s, asplevel, iter) {
        X[] <- delta * asplevel[, , iter[s]] - X[, , s]
        X[X < 0L] <- 0L
        rowSums(X[, , s] * P[, , s])
      },
      asplevel = asplevel,
      iter = if (dim(asplevel)[3] == 1) { rep(1L,ns) } else { seq.int(ns) })
      # if there is only one aspiration level for all options use the iterator

      v <- EV - beta * R
      colnames(v) <- self$get_stimnames()

      return(switch(type,
        response = super$apply_choicerule(v),
        values = v))
    },
    check_input = function() {
      if (!all(self$natt() %% 2 == 0L)) {
        stop("Formula needs an even number of right-hand elements, but has ", .brackify(self$natt()), ' elements.')
      }
      if (!all((rowSums(self$input[, -seq(1, self$natt()[1], 2), ]) == self$nstim()))) {
        stop('Probabilities (which are the 2nd, 4th, 6th... element of right side of "formula") must sum to 1, but the following variables in your data do NOT sum to 1: ', .brackify(attr(terms(self$formula), "term.labels")[seq(1, self$natt()[1], 2)]), '. Check the probability variables in "formula" and in "data".')
      }

      super$check_input() # Do not delete this, it runs default sanity checks
    }
  )
)
