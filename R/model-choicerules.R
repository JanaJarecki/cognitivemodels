# ==========================================================================
# Package: Cognitivemodels
# File: model-choicerules.R
# Author: Jana B. Jarecki
# ==========================================================================

# ==========================================================================
# Cognitive Model only for Choicerules
# ==========================================================================

#' Choicerule Models (action-selection rules)
#' 
#' @name choicerules
#' @description
#' Models discrete action selection: applies a choce rule/decision rule/action selection rule to select among values.
#' * `softmax()` fits a soft-maximum = soft approximation to argmax (Sutton & Barto, 2018).
#' * `epsilon_greedy()` fits epsilon-greedy.
#' * `epsilon()` fits probabilistic-epsilon.
#' * `argmax()` maximizes deterministically.
#' * `luce()` selects proportionally (Luce's rule aka Luce's axiom, Luce, 1959).
#' 
#' @importFrom cognitiveutils cr_softmax
#' @importFrom cognitiveutils cr_epsilon
#' @importFrom cognitiveutils cr_argmax
#' @importFrom cognitiveutils cr_luce
#' 
#' @eval .param_formula(c(1,1))
#' @eval .param_fix("softmax")
#' 
#' @details
#' This is how the model predicts and treats observations:
#' * For `formula = y ~ x1` and `y ~ x1 | x2` it predicts the probability to **select x1**, thus `y = 1` must mean select x1.
#' * For `formula = y ~ x1 | x2 | x3` it predicts three columns, the probabilities to select x1, x2, and x3, respectively.
#' 
#' ## Model Parameters
#' Most models have no free parameters, except softmax and epsilon greedy which have 1 free parameter each:
#'  * In `softmax()`: _**`tau`**_: the softness of the choices, high values cause more equiprobable choices.
#'  * In `epsilon()` and `epsilon_greedy()`: _**`eps`**_: the error proportion of the choices, high values cause more errors.
#' 
#' ## Background
#' `epsilon()` picks action \eqn{i} with probability \eqn{(1 - \epsilon)*p(i)} and with \eqn{\epsilon} it picks randomly from all actions. For \eqn{\epsilon = 0} it gives \eqn{p(i)}, that is the  original probabilistic policy.
#' 
#' `epsilon_greedy()` picks the best action with probability \eqn{1 - \epsilon}, and with \eqn{\epsilon} it picks randomly from all actions, including the best.
#' 
#' `argmax()` picks the highest-valued action with probability 1, and in case of ties it picks equiprobable.
#' 
#' `luce()` picks action \eqn{i} with probability \eqn{v(i) / \sum v}.
#' 
#' 
#' @references 
#' Sutton, R. S., & Barto, A. G. (2018). \emph{Reinforcement Learning: An Introduction (2nd Ed.)}. MIT Press, Cambridge, MA. (http://incompleteideas.net/book/the-book-2nd.html)
#' 
#' Luce, R. D. (1959). On the possible psychophysical laws. \emph{Psychological Review, 66(2)}, 81-95. doi:[10.1037/h0043178](https://doi.org/10.1037/h0043178)
#' 
#' @template cm
#' 
#' @examples
#' # Make some fake data
#' D <- data.frame(a = c(.3,.8,.5),       # value of option A
#'                 b = c(.7,.2,.5),       # value of option B
#'                 y = c(0,1,1))          # respondent's choice (0=A, 1=B)
#' 
#' M <- softmax(y ~ a | b, D, c(tau=1))   # creates soft-max model w tau=1
#' 
#' predict(M)                             # predict action selection
#' M$predict()                            # -- (same) --
#' summary(M)                             # summarize
#' anova(M)                               # anova-like table
#' coef(M)                                # free parameter (NULL)
#' M$get_par()                            # fixed parameter (tau = 1)
#' M$npar()                               # 1 parameter
#' M$MSE()                                # mean-squared error
#' logLik(M)                              # log likelihood
#' 
#' 
#' ### Parameter specification and fitting ---------------------------------
#' softmax(y ~ a | b, D, fix="start")     # fix 'tau' to its start value
#' softmax(y ~ a | b, D, fix=c(tau=0.2))  # fix 'tau' to 0.2
#' softmax(y ~ a | b, D)                  # fit 'tau' to data y in D
#' 
#' 
#' ### The different choice rules ------------------------------------------
#' softmax(y ~ a | b, D,  fix=c(tau=0.5)) # fix 'tau' to 0.5
#' softmax(y ~ a | b, D)                  # fit 'tau' to y
#' epsilon_greedy(y~a | b, D, c(eps=0.1)) # fix 'eps' to 10 %
#' epsilon_greedy(y~a | b, D )            # fit 'eps' to y
#' epsilon(y ~ a | b, D, c(eps=0.1))      # fix 'eps' to 0.1
#' epsilon(y ~ a | b, D)                  # fit 'eps' to y
#' luce(y ~ a | b, D)                     # Luce's choice rule, 0 parameter
#' argmax(y ~ a | b, D)                   # Argmax choice rule, 0 parameter
NULL

#' @name choicerules
#' @export
softmax <- function(formula, data, fix = list(), options = NULL) {
  .args <- as.list(match.call()[-1])
  .args[["type"]] <- "softmax"
   return(do.call(what = Choicerule$new, args = .args, envir = parent.frame()))
}


#' @name choicerules
#' @export
epsilon_greedy <- function(formula, data, fix = list(), options = NULL) {
  .args <- as.list(match.call()[-1])
  .args[["type"]] <- "epsilongreedy"
   return(do.call(what = Choicerule$new, args = .args, envir = parent.frame()))
}


#' @name choicerules
#' @export
epsilon <- function(formula, data, fix = list(), options = NULL) {
  .args <- as.list(match.call()[-1])
  .args[["type"]] <- "epsilon"
   return(do.call(what = Choicerule$new, args = .args, envir = parent.frame()))
}

#' @name choicerules
#' @export
luce <- function(formula, data, ...) {
  .args <- as.list(match.call()[-1])
  .args[["type"]] <- "luce"
   return(do.call(what = Choicerule$new, args = .args, envir = parent.frame()))
}



#' @name choicerules
#' @export
argmax <- function(formula, data, ...) {
  .args <- as.list(match.call()[-1])
  .args[["type"]] <- "argmax"
   return(do.call(what = Choicerule$new, args = .args, envir = parent.frame()))
}



#' @noRd
Choicerule <- R6Class("choicerule",
  inherit = Cm,
  public = list(
    type = NULL,
    initialize = function(formula, data = NULL, type, fix = NULL, options=list()) {
      type <- match.arg(type, c("softmax", "argmax", "luce", "epsilon", "epsilongreedy"))
      self$type <- type
      parspace <- switch(type,
        epsilon =       make_parspace(eps = c(0,1,0.5,0)),
        epsilongreedy = make_parspace(eps = c(0,1,0.5,0)),
        softmax =       make_parspace(tau = c(0.0001,10,2,NA)),
        make_parspace()
      )
      super$initialize(
        title = type,
        formula = formula,
        data = data,
        parspace = parspace,
        mode = "discrete",
        fix = fix,
        options = options,
        choicerule = "none"
      )
    },
    predict = function(type = c("response"), newdata = NULL) {
      if (is.null(newdata) | missing(newdata)) {
        D <- self$input
      } else {
        D <- private$get_input(d = newdata)
      }
      fun <- switch(self$type,
        softmax = cognitiveutils::cr_softmax,
        argmax = cognitiveutils::cr_argmax,
        luce = cognitiveutils::cr_luce,
        epsilon = cognitiveutils::cr_epsilon,
        epsilongreedy = function(x, eps) {
          cognitiveutils::cr_epsilon(
            x = matrix(
              data = cognitiveutils::cr_argmax(x),
              nrow = self$nobs),
            eps = self$get_par())
        }
      )
      .args <- list(x = abind::adrop(aperm(D, c(1,3,2)), 3))
      .args <- c(.args, self$par)

      res <- do.call(what = fun, args = .args, envir = parent.frame())
      res <- matrix(res, nrow = dim(D)[1])
      colnames(res) <- paste0("pr_", private$get_stimnames())
      nn <- ifelse(ncol(res) <= 2, 1, ncol(res))
      return(res[, 1:nn])
    }
  )
)