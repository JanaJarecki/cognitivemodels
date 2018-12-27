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
    V = NULL,
    env = "rsenvironment",
    choicerule = NULL,
    initialize = function(env = NA, data, formula, choicerule, fixed) {
      if (missing(formula)) {
        formula <- ~ timehorizon + state
      }
      if (missing(data)) {
        time_state_names <- attr(terms(formula), 'term.labels')
        data <- env$makeData(time_state_names)
      }

      allowedparm <- matrix(numeric(0), 0, 3, dimnames = list(NULL, c("ll", "ul", "init")))
      super$initialize(formula = formula, data = data, allowedparm = allowedparm, fixedparm = fixed, choicerule =  choicerule, model = "Optimal RSFT Model (Houston & McNamara, 1988)", discount = 0)

      self$formula <- formula
      self$env <- env
      self$makeValueMat()
      self$makeEVMat()
      },
      predict = function(type = c("choices", "values", "ev", "pstates"), action = NULL, newdata = NULL) {
        type = match.arg(type)

        if (is.null(action)) {
          action <- ifelse(self$env$n.actions == 2, 1, self$env$actions)
        }
        if (!is.null(newdata)) {
          data <- get_all_vars(self$formula, newdata)
        } else {
          data <- self$input
        }

        trials <- data[, 1]
        states <- data[, 2]
        rows <- match(states, self$env$states)
        cols <- match(trials, rev(self$env$trials))
        acts <- rep(seq_len(self$env$n.actions), each = length(trials))

        v <- self$V[cbind(rows, cols, acts)]
        v <- matrix(v, ncol = self$env$n.actions, dimnames = list(NULL, self$env$actions))

        if (type == "choices") {
          cc <- self$applychoicerule(v)
        } else if (type == "ev") {
          ev <- self$EV[cbind(rows, cols, 1)]
        } else if (type == "pstates") {
          ps <- self$V
          ps[] <- super$applychoicerule(matrix(ps, nc = self$env$n.actions))
          ps <- self$env$policyPS(ps)[cbind(rows, cols)]
        }

        switch(type,
          choices = cc[, action],
          values = v[, action],
          pstates = ps,
          ev = ev)
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
        EV <- e$makeStateTrialActionMat()[,,1,drop=F]
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

hm1988 <- function(env, formula, data, choicerule, fixed = NULL) {
  obj <- Hm1988$new(env = env, data = data, formula = formula, choicerule = choicerule, fixed = fixed)
  return(obj)
}

predict.hm1988 <- function(obj, type = NULL, action = NULL, newdata = NULL) {
  obj$predict(type = type, action = action, newdata = newdata)
}