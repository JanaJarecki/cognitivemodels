##############################################################################
# Optimal Algorithm for RSFT Choice problems
# Author: Jana B. Jarecki, jarecki.jana@gmail.com
##############################################################################
library(reshape2)
library(R6)
source("../../utils/classes/cognitiveModel.R", chdir = TRUE)
source("../../utils/classes/rsenvironment.R", chdir = TRUE)
source("../../utils/functions/varG.R")

# Define base class abd generator functions for model objects
Hm1988 <- R6Class("hm1988",
  inherit = cognitiveModel,
  public = list(
  data = NULL,
  formula = NULL,
  EV = NULL,
  environment = "rsenvironment",
  initialize = function(environment = NA, data, formula) {
    if (missing(formula)) {
      formula <- y ~ timehorizon + state
    } 
    if (missing(data)) {
      self$data <- melt(environment$stateTrialMat[, environment$trials], na.rm = TRUE, varnames = c(NA, rhs.vars(formula)[1]), value.name = rhs.vars(formula)[2])[, -1]
    } else {
      self$data <- get_all_vars(formula, data)
    }
    self$formula <- formula
    self$environment <- environment

    # Make state action space
    stateTrialMat <- environment$stateTrialMat
    fitness <- array(NA, dim = c(environment$n.states, environment$n.trials), dimnames = list(environment$states, environment$trials))
    fitness <- cbind(fitness, environment$terminal.fitness(state = environment$states, budget = environment$budget))
    EV <- array(NA, dim = c(environment$n.states, environment$n.trials, environment$n.actions), dimnames = list(environment$states, environment$trials, environment$actions))

    for(t in rev(environment$trials)) {
      T <- environment$T[, , t, , drop = FALSE]
      F <- array(rep(fitness[, t + 1], each = dim(T)[1]), dim = dim(T))      
      TxF <- T * F
      EV[, t, ] <- colSums(aperm(TxF, c(2,1,3,4)))
      fitness[, t] <- apply(EV[, t, , drop = FALSE], 1, max)
     }

     self$EV <- EV
    },
    predict = function(type = c("choice", "ev"), action = NULL, newdata = NULL) {
      type = match.arg(type)

      if (is.null(action)) {
        action <- ifelse(self$environment$n.actions == 2, 1, self$environment$actions)
      }
      if (!is.null(newdata)) {
        data <- get_all_vars(self$formula, newdata)
      } else {
        data <- self$data
      }

      trials <- data[, 1]
      states <- data[, 2]

      rows <- match(states, na.omit(unique(c(self$environment$stateTrialMat))))
      cols <- match(trials, rev(self$environment$trials))
      ids <- cbind(rows, cols, rep(seq_len(self$environment$n.actions), each = length(trials)))

      evs <- matrix(self$EV[ids], ncol = self$environment$n.actions, dimnames = list(NULL, self$environment$actions))

      switch(type,
        choice = choicerule(evs, "argmax")[, action],
        ev = evs[, action])
    }
  )
)

hm1988 <- function(environment, formula, data) {
  obj <- Hm1988$new(environment = environment, data = data, formula = formula)
  return(obj)
}

predict.hm1988 <- function(obj, type = c("choice", "ev"), action = NULL, newdata = NULL) {
  obj$predict(type = type, action = action, newdata = newdata)
}


print.optimal <- function(obj) {
  cat("<optimal>",
    "\nModel for risk-sensitive foraging",
    "\ntimehorizon:", obj$environment$n.trials, "budget:", obj$environment$budget, "\n")
}

sse.optimal <- function(obj, ...) {
  obj$sse(...)
}