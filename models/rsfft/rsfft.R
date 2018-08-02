require(R6)
library(formula.tools)
library(reshape2)
source("../../utils/classes/cognitiveModel.R", chdir = TRUE)
source("../../utils/classes/rsenvironment.R", chdir = TRUE)

Rsfft <- R6Class("rsfft",
  inherit = cognitiveModel,
  public = list(
    environment = "rsftEnvironment",
    V = NULL,
    EV = NULL,
    initialize = function(environment, formula = NULL, fixed = NULL, data = NULL) {
      allowedparm <- matrix(numeric(0), 0, 3, dimnames = list(NULL, c("ll", "ul", "init")))
      if (is.null(formula)) {
        formula <- ~ timehorizon + state
      } 
      if (is.null(data)) {
        data <- melt(environment$stateTrialMat[, environment$trials], na.rm = TRUE, varnames = c(NA, rhs.vars(formula)[1]), value.name = rhs.vars(formula)[2])
      } else {
        data <- get_all_vars(formula, data)
      }

      super$initialize(formula = formula, data = data, allowedparm = allowedparm, fixedparm = NULL, choicerule =  "softmax", model = "Risk sensitivity FFT", discount = 0)

      self$environment <- environment
      self$makeValueMat()
      self$makeEVMat()

      },
    predict = function(type = c("choice", "value", "ev"), action = NULL, newdata = NULL) {
      type <- match.arg(type)
      if (is.null(action)) {
        action <- ifelse(self$environment$n.actions == 2, 1, self$environment$actions)
      }
      if (!is.null(newdata)) {
        input <- newdata[, rhs.vars(self$formula)]
      } else {
        input <- self$input
      }

      trials <- input[, 1]
      states <- input[, 2]
      rows <- match(states, na.omit(unique(c(self$environment$stateTrialMat))))
      cols <- match(trials, rev(self$environment$trials))

      X <- switch(type,
        ev = self$EV,
        choice = self$V,
        value = self$V)

      ids <- cbind(rows, cols, rep(seq_len(dim(X)[3]), length(trials)))

      v <- matrix(X[ids], ncol = dim(X)[3],dimnames = dimnames(X), byrow = FALSE)

      switch(type,
        choice = super$applychoicerule(v)[, action],
        value = v[, action],
        ev = v)
    },
    fft = function(state, timehorizon, budget = self$environment$budget, outcomes = self$environment$environment[, 2, , drop = FALSE], probabilities = self$environment$environment[, 1, , drop = FALSE], bReachedFun = function(environment, state) {  environment$vars == min(environment$vars) }, ...) {

      tf <- function(state) {
        self$environment$terminal.fitness(budget = self$environment$budget, state = state)
      }
      # FFT
      # 1. Reached the budget?
      # - YES
      #   | Decide by a default function
      # - NO:
      #   | 2. Is the max outcome x t >= budget for only one option?
      #     - YES:
      #       | Chose it
      #     - NO:
      #       | 3. Is the min outcome x t >= budget for one option?
      #             - YES:
      #               | Chose the sure option
      #             - NO (both unsure):
      #               | Chose higher probability option with beta
      #    
      # Problem: ignores pr(x)
      #


      # if the safe reaches the budget for sur ... why do you not take it????????
      if (state > budget) {
        return(bReachedFun(self$environment, state))
      } else {
        finalB <- state + outcomes * timehorizon
        reachB <- finalB >= budget
        anyReachB <- apply(reachB, 2, any)
        if (sum(anyReachB) == 1) {
          return(as.numeric(anyReachB))
        } else {
          sureReachB <- apply(reachB, 2, all)
          if (sum(sureReachB) == 1) {
            return(as.numeric(sureReachB))
          } else {
            probabilities[!reachB] <- 0
            prReachB <- colSums(probabilities)
            return(as.numeric(prReachB))
          }
        }
      }
    },
    makeValueMat = function() {
      V <- self$environment$makeStateTrialActionMat()
      b <- self$environment$budget
      o <- self$environment$environment[, 2, ]
      p <- self$environment$environment[, 1, ]
      colTrials <- dimnames(V)[[2]]
      rowStates <- dimnames(V)[[1]]

      for (s in rowStates) {
        for (t in colTrials) {
          V[s, t, ] <- self$fft(state = as.numeric(s), timehorizon = as.numeric(t), budget = b, outcomes = o, probabilities = p)
        }
      }
      self$V <- V
    },
    makeEVMat = function() {
      EV <- array(NA, dim = dim(self$V)[1:2], dimnames = dimnames(self$V)[1:2])
      P <- array(t(apply(self$V, 1, cogsciutils::choicerule, type = "arg")), dim = dim(self$V), dimnames = dimnames(self$V))
      for (s in rownames(EV)) {
        for (t in self$environment$trials) {
          EV[s,t] <- self$environment$policyEV(P, s0 = as.numeric(s), t0 = as.numeric(t))
        }
      }
      dim(EV) <- c(dim(EV), 1)
      self$EV <- EV
    }
  )
)


rsfft <- function(environment, formula = NULL, fixed = NULL, data = NULL) {

    obj <- Rsfft$new(environment = environment, data = data, formula = formula, fixed = fixed)

    return(obj)
}

predict.rsfft <- function(obj, type = NULL, action = NULL, newdata = NULL) {
  return(obj$predict(type = type, action = action, newdata = newdata))
}


# bReachedFun <- function(env, state) { env$terminal.fitness(state = state + env$evs - 4 * env$vars, budget = env$budget) }