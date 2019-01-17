require(R6)
library(Formula)
library(reshape2)
library(arrangements)
library(Rcpp)
library(Rsolnp)
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
    features = NULL,
    terminal.fitness = NULL,
    sbt = NULL,
    initialize = function(env = NULL, formula = NULL, sbt = NULL, nout = NULL, nopt = NULL, fixed = NULL, data = NULL, choicerule, terminal.fitness.fun) {
      if (is.null(formula)) {
        formula <- ~ timehorizon + state
      } 
      if (is.null(data)) {
        stop("not yet implemented.")
        # Check the implementation here
        # time_state_names <- attr(terms(formula), 'term.labels')
        # data <- env$makeData(time_state_names)
        # self$makeValueMat()
        # self$makeEVMat()
      }

      allowedparm <- matrix(0, 4, 3, dimn = list(
        c("maxlv", "minhv", "pmaxlv", "order"),
        c("ll", "ul", "init")))
      super$initialize(formula = formula, data = data, allowedparm = allowedparm, fixedparm = fixed, choicerule = choicerule, model = "Risk sensitivity FFT", discount = 0)

      self$env <- env
      self$nout <- nout
      self$nopt <- nopt
      self$state <- model.frame(sbt, data)[, 1]
      self$budget <- model.frame(sbt, data)[, 2]
      self$timehorizon <- model.frame(sbt, data)[, 3]   
      self$terminal.fitness <- terminal.fitness.fun
      self$sbt <- sbt

      # n_node_orders <- nrow(permutations(3))
      features <- self$makeFeatureMat()
      self$features <- features

      self$allowedparm[1:3, 'ul'] <- apply(features, 2, max)
      self$allowedparm[1:3, 'init'] <- rowMeans(self$allowedparm[1:3,])
      self$allowedparm['order', ] <- c(1, 6, 1)
    },
    makeFeatureMat = function(input = self$input, budget = self$budget, state =self$state, th = self$timehorizon) {
      fml <- Formula(self$formula)
      nout <- self$nout
      nopt <- self$nopt
      ntrial <- nrow(input)
      x_cols <- seq_len(nout)
      
      outcomes_arr <- array(0L, dim = c(ntrial, nout, nopt))
      probabilities_arr <- array(0L, dim = c(ntrial, nout, nopt))

      for (i in seq_len(nout)) {
         outcomes_arr[, , i] <- model.matrix(fml, input, rhs = i)[, -1, drop = FALSE][, x_cols, drop = FALSE]
      }     
      for (i in seq_len(nout)) {
         probabilities_arr[, , i] <- model.matrix(fml, input, rhs = i)[, -1, drop = FALSE][, -x_cols, drop = FALSE]
         checkSumTo1 <- all.equal(rowSums(probabilities_arr[,,i]), rep(1, ntrial))
         if (!isTRUE(checkSumTo1)) {
            stop('Some probabilities in your data do not sum to 1.\nProbabilities are specified in your formula by "', paste(attr(terms(formula(Formula(self$formula), rhs = i, lhs = 0)), 'term.labels')[-x_cols], collapse = ', '), '".\nCheck if the formula refers to the right columns?')
         }
       }

      variances <- sapply(1:nopt, function(i) varG(probabilities_arr[,,i], outcomes_arr[,,i]))
      which_hv <- cbind(rep(1:ntrial, nout), rep(1:nout, each=ntrial), apply(variances, 1, which.max))
      which_lv <- cbind(rep(1:ntrial, nout), rep(1:nout, each=ntrial), apply(variances, 1, which.min))

      hv_outcomes <- array(outcomes_arr[as.matrix(which_hv)], dim(variances))
      hv_probabilities <- array(probabilities_arr[as.matrix(which_hv)], dim(variances))      
      lv_outcomes <- array(outcomes_arr[as.matrix(which_lv)], dim(variances))

      max_lv_outcome <- apply(lv_outcomes, 1, max)
      max_hv_outcome <- apply(hv_outcomes, 1, max)
      min_hv_outcome <- apply(hv_outcomes, 1, min)
      need <- budget - state

      features <- cbind(
        need / max_lv_outcome / th,
        need / min_hv_outcome / th,
        hv_probabilities[hv_outcomes == max_hv_outcome])

      # Correct for NaN and Inf values
      features[need == 0, 1:2] <- 0
      for (i in 1:2) {
        infrows <- is.infinite(features[, i])
        features[infrows, i] <- max(features[!infrows, i]) + quantile(features[!infrows, i], .01)
      }
      return(features)
    },
    predict = function(type = c("choice", "node", "value", "ev", "pstate"), action = NULL, newdata = NULL) {
      type <- match.arg(type)
      
      if (is.null(action)) {
        action <- ifelse(self$nopt == 2, 1, seq_len(self$nopt))
      }

      if (!is.null(newdata)) {
        input <- newdata[, rhs.vars(self$formula)]
        state <- newdata[, rhs.vars(self$sbt)][,1]
        budget <- newdata[, rhs.vars(self$sbt)][,2]
        timehorizon <- newdata[, rhs.vars(self$sbt)][,3]
        features <- self$makeFeatureMat(
          input = input,
          budget = budget,
          state = state,
          th = timehorizon)
      } else {
        input <- self$input
        features <- self$features
      }

      splitCriteria <- self$parm[1:3]
      order <- permutations(3,3)[self$parm['order'], ]
      exits <- matrix(c(1, 0, 1)[order], nrow = 1)[rep(1, nrow(input)), ]
      exits <- cbind(exits, 1L - exits[, 3])
      
      I <- indicators(features[, order], splitCriteria[order])   

      values <- t(exits)[t(I)==1]
      values <- cbind(values, 1-values)
      
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
    fit = function(type = c('grid', 'rsolnp')) {
      if ('order' %in% self$freenames) {
        self$freenames <- setdiff(self$freenames, 'order')
        self$fixednames <- c(self$fixednames, 'order')
        possibleOrders <- seq_len(self$allowedparm['order', 'ul'])
        gofvalues <- sapply(possibleOrders, function(o) {
          self$setparm(c(order = o))
          super$fit(type)
          return(self$gofvalue)
        } )
        self$setparm(c(order = possibleOrders[which.min(gofvalues)]))
        self$fixednames <- setdiff(self$fixednames, 'order')
        self$freenames <- c(self$freenames, 'order')
      } else {
        super$fit(type)
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