#' Houston & McNamara's (1988) dynamic optimization for risk-sensitive foraging problems with discrete time
#' 
#' @import reshape2
#' @param env object of class \link{rsenvironment}, see example. It specifies the risky and safe option, the number of trials, the state, and requirement (aka budget).
#' @param formula (optional) object of class \link[stats]{formula}, e.g. \code{choice ~ timehorizon + state}, defines how the variables are called in the data.

#' @param data (optional) data frame
#' @inheritParams Cogscimodel
#' @references Houston, A. I., & McNamara, J. M. (1988). A framework for the functional analysis of behaviour. Behavioural and Brain Science, 11, 117–163.
#' @return An object of class R6 holding the model, it has no free parameters. A model object \code{M} can be viewed with \code{M}, predictions can be made with \code{M$predict()} for choice predictions, and \code{M$predict("ev")} for the expected value of the optimal choice and \code{M$predict("value", 1:2)} for the expected value of all choices.
#' @author Jana B. Jarecki, \email{jj@janajarecki.com}
#' @details Risk-sensitive foraging means you have, for instance, four choices between the same two risky lotteries and after the four choices you need to have accumulated at least 12 points to get a reward. The optimal solution to this choice problem relies on dynamic programming. The function creates all possible future states given the possible remaining trials, and predicts the optimal choice polica or the expected value of chosing either option given a certain state and a certain time horizon.
#' @examples
#' # define the the environment like this:
#' ###    requirement: budget = 12
#' ###    number of choices: n.trials = 4
#' ###    initial number of points: initial.state = 10
#' ###    risky option: a1, with three outcomes
#' ###    safe option: a2, with three outcomes
#' env <- rsenvironment(budget = 12,
#'                      n.trials = 4,
#'                      initial.state = 10,
#'                      a1 = matrix(c(0.1, 0.8, 0.1, 0, 1, 2), nc = 2),
#'                      a2 = matrix(c(0.4, 0.2, 0.4, 0, 1, 2), nc = 2))
#' # check the environment
#' env
#' # > env
#' # Reach 12 in 4 trials with options:
#' #     a1 (0, 0.1) or (1, 0.8) or (2, 0.1) 
#' #     a2 (0, 0.4) or (1, 0.2) or (2, 0.4) 
#' #
#' # Initial state of 10
#' 
#' # Run the optimal model
#' M <- hm1988(env = env, ~ timehorizon + state, choicerule = 'arg')
#' 
#' # Results:
#' M # view model
#' M$input # view all possible state + trial combinations
#' M$predict() # optimal response for all possible state x trial combinations
#' cbind(M$input, M$predict())
#' M$predict('response') # optimal response
#' M$predict("ev") # expected value of optimal choice for all possible states and trials
#' M$predict("value") # expected value of choice a1
#' M$predict("value", 1:2) # expected value of a1 or a2
#'
#' # Predict specific state x trial scenarios for 1 ... 4 trials left and a state of 11
#' newdt <-  data.frame(timehorizon = 1:4, state = 11)
#' M$predict('v', newdata = newdt, 1:2)
#' M$predict('r', newdata = newdt)
#' @export
hm1988 <- function(env, formula, data, choicerule, fixed = NULL, fit.options = NULL) {
  obj <- Hm1988$new(env = env, data = data, formula = formula, choicerule = choicerule, fixed = fixed, fit.options = fit.options)
  if (length(obj$getparm('free')) > 0) {
      message('Fitting free parameter', .brackify(obj$freenames))
      obj$fit(c('grid', 'solnp'))
   }
  return(obj)
}


# Define base class abd generator functions for model objects
Hm1988 <- R6Class('hm1988',
  inherit = Cogscimodel,
  public = list(
    data = NULL,
    formula = NULL,
    EV = NULL,
    V = NULL,
    env = 'rsenvironment',
    choicerule = NULL,
    initialize = function(env = NA, data, formula, choicerule, fixed, fit.options) {
      if (missing(formula)) {
        formula <- ~ timehorizon + state
      }
      if (missing(data)) {
        time_state_names <- attr(terms(formula), 'term.labels')
        data <- env$makeData(time_state_names)
      }
      allowedparm <- matrix(numeric(0), 0, 4, dimnames = list(NULL, c('ll', 'ul', 'init', 'na')))
      super$initialize(formula = formula, data = data, allowedparm = allowedparm, fixed = fixed, choicerule =  choicerule, model = 'Optimal RSFT Model (Houston & McNamara, 1988)', discount = 0, response = 'discrete', fit.options = fit.options)
      self$env <- env
      self$makeValueMat()
      self$makeEVMat()
      },
      predict = function(type = c('response', 'values', 'ev', 'pstates'), action = NULL, newdata = NULL) {
        type = match.arg(type)
        action <- if ( is.null(action) & self$env$n.actions == 2) { 1 } else { self$env$actions }
        input <- if ( is.null(newdata) ) { self$input } else { self$getinput(self$formula, newdata) }
        trials <- input[, 1, ]
        states <- input[, 2, ]
        rows <- match(states, self$env$states)
        cols <- match(trials, rev(self$env$trials))
        acts <- rep(seq_len(self$env$n.actions), each = length(trials))

        if (type == 'response' | type == 'values') {
          v <- self$V[cbind(rows, cols, acts)]
          v <- matrix(v, ncol = self$env$n.actions, dimnames = list(NULL, self$env$actions))
          if (type == 'values') {
            return(v[, action])
          } else {
            return(self$applychoicerule(v)[, action])
          }
        } else if (type == 'ev') {
          return(self$EV[cbind(rows, cols, 1)])
        } else if (type == 'pstates') {
          ps <- self$V
          ps[] <- super$applychoicerule(matrix(ps, nc = self$env$n.actions))
          return(self$env$policyPS(ps)[cbind(rows, cols)])
        }
      },
      makeValueMat = function() {
        e <- self$env
        ns <- e$n.states
        nt <- e$n.trials
        # fitness <- array(NA, dim = c(ns, nt), dimnames = list(env$states, env$trials))
        # fitness <- cbind(fitness, env$terminal.fitness(state = env$states, budget = env$budget))
        F <- e$terminal.fitness(state = e$states, budget = e$budget)
        V <- e$makeStateTrialActionMat()
        for(t in rev(e$trials)) {
          T <- e$T[, , t, , drop = FALSE]
          F <- array(rep(F, each = ns), dim = dim(T))
          TxF <- T * F
          V[, t, ] <- colSums(aperm(TxF, c(2,1,3,4)))
          F <- apply(V[, t, , drop = FALSE], 1, max)
        }
        self$V <- V
      },
      makeEVMat = function() {
        e <- self$env
        EV <- e$makeStateTrialActionMat()
        P <- t(apply(self$V, 1, super$applychoicerule))
        P <- array(P, dim = dim(self$V), dimnames = dimnames(self$V))
        P[is.na(P)] <- 0
        for (s in rownames(EV)) {
          for (tr in e$trials) {
            EV[s, tr, ] <- e$policyEV(P, s0 = as.numeric(s), t0 = as.numeric(tr))
          }
        }
        dimnames(EV)[[3]] <- NULL
        self$EV <- EV
      }
  )
)


predict.hm1988 <- function(obj, type = NULL, action = NULL, newdata = NULL) {
  obj$predict(type = type, action = action, newdata = newdata)
}