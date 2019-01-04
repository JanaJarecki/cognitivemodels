require(R6)
library(Formula)
library(reshape2)
library(arrangements)
library(Rcpp)
sourceCpp("rsfft.cpp")
source("../../utils/classes/cognitiveModel.R", chdir = TRUE)
source("../../utils/classes/rsenvironment.R", chdir = TRUE)

Rsfft <- R6Class("rsfft",
  inherit = cognitiveModel,
  public = list(
    env = "rsftenvironment",
    V = NULL,
    EV = NULL,
    state = NULL,
    budget = NULL,
    timehorizon = NULL,
    nout = NULL,
    nopt = NULL,
    terminal.fitness = NULL,
    initialize = function(env = NULL, formula = NULL, sbt = NULL, nout = NULL, nopt = NULL, fixed = NULL, data = NULL, choicerule, terminal.fitness.fun) {
      if (is.null(formula)) {
        formula <- ~ timehorizon + state
      } 
      if (is.null(data)) {
        time_state_names <- attr(terms(formula), 'term.labels')
        data <- env$makeData(time_state_names)
        self$makeValueMat()
        self$makeEVMat()
      }

      n_node_orders <- nrow(permutations(3))
      allowedparm <- matrix(c(1, n_node_orders, 0), 1, 3, dimnames = list("alpha", c("ll", "ul", "init")))
      super$initialize(formula = formula, data = data, allowedparm = allowedparm, fixedparm = fixed, choicerule = choicerule, model = "Risk sensitivity FFT", discount = 0)

      self$env <- env
      self$nout <- nout
      self$nopt <- nopt
      self$state <- model.frame(sbt, data)[, 1]
      self$budget <- model.frame(sbt, data)[, 2]
      self$timehorizon <- model.frame(sbt, data)[, 3]   
      self$terminal.fitness <- terminal.fitness.fun

      },
    predict = function(type = c("choice", "node", "value", "ev", "pstate"), action = NULL, newdata = NULL) {
      type <- match.arg(type)
      nout <- self$nout

      if (is.null(action)) {
        action <- ifelse(self$nopt == 2, 1, seq_len(self$nopt))
      }
      if (!is.null(newdata)) {
        input <- newdata[, rhs.vars(self$formula)]
      } else {
        input <- self$input
      }

      x_cols <- seq_len(nout)
      nopt <- self$nopt
      ntrial <- length(self$state)

      outcomes_arr <- array(0L, dim = c(ntrial, nout, nopt))
      for (i in seq_len(nout)) {
         outcomes_arr[,,i] <- model.matrix(Formula(self$formula), input, rhs = i)[, -1, drop = FALSE][, x_cols, drop = FALSE]
      }
      probabilities_arr <- array(0L, dim = c(ntrial, nout, nopt))
      for (i in seq_len(nout)) {
         probabilities_arr[,,i] <-model.matrix(Formula(self$formula), input, rhs = i)[, -1, drop = FALSE][, -x_cols, drop = FALSE]
      }

      # test change

      # outcomes_list <- lapply(seq_len(nout), function(i) {
       
      # })
      # probabilities_list <- lapply(seq_len(nout), function(i) {
      #   model.matrix(Formula(self$formula), input, rhs = i)[, -1, drop = FALSE][, -x_cols, drop = FALSE]
      # })

      # FFT
      # 1. Reached the budget?
      # - YES
      #   | Decide by a default function
      # - NO:
      #   | 2. Can only one of the options reach the budget: Is max outcome x t >= budget for only one option?
      #     - YES:
      #       | Chose it, proportionally to how much it exceeds 0
      #     - NO:
      #       | 3. Can one option reach it for sure? Is the min outcome x t >= budget for one option?
      #             - YES:
      #               | Chose the sure option, proportionally to how much it exceeds the budet
      #             - NO (both unsure):
      #               | Chose higher EV option

      nopt <- self$nopt
      timehorizon <- self$timehorizon
      state <- self$state
      budget <- self$budget
      col_order <- permutations(3)[self$parm['alpha'],]
      # values to return for each node of the tree
      values <- lapply(seq_len(nopt), function(i) nodevalues(col_order-1, probabilities_arr[,,i], outcomes_arr[,,i], timehorizon, budget, state, self$terminal.fitness))

      # values_reached_b <- lapply(seq_len(nopt), function(i) self$reachedBudgetFun(probabilities_list[[i]], outcomes_list[[i]]))
      # for (i in seq_len(nopt)) {
      #   values[[i]][,1] <- - values_reached_b[[i]]
      # }

      # Indicator matrix for the tree
      I <- indicators(col_order, outcomes_arr, probabilities_arr, timehorizon, budget, state)
      # return values at the nodes specified by the indicator
      values <- matrix( unlist ( lapply(seq_len(nopt), function(x) rowSums(values[[x]] * I)) ), ncol = nopt, byrow = FALSE)
      
      if (type == "ev") {
        trials <- input[, 1]
        states <- input[, 2]
        rows <- match(states, self$env$states)
        cols <- match(trials, rev(self$env$trials))
        ev <- self$EV[cbind(rows, cols, 1)]
      } else if (type == "choice" | type == "value") {
        # acts <- rep(seq_len(nout), each = length(trials))
        # v <- matrix(self$V[cbind(rows, cols, acts)], ncol = self$env$n.actions, dimnames = list(NULL, self$env$actions))
        if (type == "choice") {
          choice <- super$applychoicerule(values)
        }
      } else if (type == "pstate") {
          trials <- input[, 1]
          states <- input[, 2]
          rows <- match(states, self$env$states)
          cols <- match(trials, rev(self$env$trials))
          ps <- self$V
          ps[] <- self$applychoicerule(matrix(ps, nc = self$env$n.actions))
          ps <- self$env$policyPS(ps)[cbind(rows, cols)]
      } else if (type == 'node') {
        node <- apply(I, 1, function(x) colnames(I)[which(x==1)])
        node <- factor(node, levels = colnames(I), ordered = TRUE)
      }

      switch (type,
        choice = choice[, action],
        node = node,
        value = values[, action],
        ev = ev,
        pstate = ps)
    },
    fit = function(type = 'grid') {
      fun <- function(parm, self) {
        self$setparm(parm)
        -cogsciutils::gof(obs = self$obs, pred = self$predict(), type = 'log', response = 'discrete')
      }
      free_parm <- self$freenames
      LB <- self$allowedparm[, 'll'][free_parm]
      UB <- self$allowedparm[, 'ul'][free_parm]
      STEP <- c(alpha = 1, tau = .5)[free_parm]
      if (type == 'grid') {
        parGrid <- expand.grid(sapply(free_parm, function(i) seq(LB[i], UB[i], STEP[i]), simplify = FALSE))
        ll <- sapply(1:nrow(parGrid), function(i) fun(parm = unlist(parGrid[i,,drop=FALSE]), self = self))
        self$setparm(unlist(parGrid[which.min(ll), ]))
      }
      else {
       par0 <- self$allowedparm[, 'init']
        fit <- solnp(pars = par0, fun = fun, LB = LB, UB = UB, self = self)
        self$setparm(fit$pars)
      }
    },
    reachedBudgetFun = function(x, p) {
      x <- as.matrix(x)
      p <- as.matrix(p)
      return(sapply(1:nrow(x), function(i) -varG(x = x[i,], p = p[i,])))
    },
    makeValueMat = function() {
      V <- self$env$makeStateTrialActionMat()
      b <- self$env$budget
      o <- self$env$environment[, 2, ]
      p <- self$env$environment[, 1, ]
      colTrials <- dimnames(V)[[2]]
      rowStates <- dimnames(V)[[1]]

      for (s in rowStates) {
        for (tr in colTrials) {
          V[s, tr, ] <- self$fft(state = as.numeric(s), timehorizon = as.numeric(tr), budget = b, outcomes = o, probabilities = p)
        }
      }
      self$V <- V
    },
    makeEVMat = function() {
      EV <- array(NA, dim = dim(self$V)[1:2], dimnames = dimnames(self$V)[1:2])
      P <- t(apply(self$V, 1, self$applychoicerule))
      P <- array(P, dim = dim(self$V), dimnames = dimnames(self$V))
      P[is.na(P)] <- 0

      for (s in rownames(EV)) {
        for (tr in self$env$trials) {
          EV[s,tr] <- self$env$policyEV(P, s0 = as.numeric(s), t0 = as.numeric(tr))
        }
      }
      dim(EV) <- c(dim(EV), 1)
      self$EV <- EV
    }
  )
)


rsfft <- function(formula = NULL, sbt = NULL, nopt = NULL, nout = NULL, fixed = NULL, data = NULL, env = NULL, choicerule, terminal.fitness.fun) {
    obj <- Rsfft$new(env = env, formula = formula, sbt = sbt, nopt = nopt, nout = nout, data = data, fixed = fixed, terminal.fitness.fun = terminal.fitness.fun, choicerule)
  if (length(obj$fixednames) < length(obj$parm)) {
    obj$fit()
  }
  return(obj)
}