#' Houston & McNamara's (1988) dynamic optimization model for risk-sensitive foraging problems in discrete time
#' 
#' @import data.table
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
#' M <- hm1988(env = env, ~ timehorizon + state, choicerule = "arg")
#' 
#' # Results:
#' M # view model
#' M$input # view all possible state + trial combinations
#' M$predict() # optimal mode for all possible state x trial combinations
#' cbind(M$input, M$predict())
#' M$predict("mode") # optimal mode
#' M$predict("ev") # expected value of optimal choice for all possible states and trials
#' M$predict("value") # expected value of choice a1
#' M$predict("value", 1:2) # expected value of a1 or a2
#'
#' # Predict specific state x trial scenarios for 1 ... 4 trials left and a state of 11
#' newdt <-  data.frame(timehorizon = 1:4, state = 11)
#' M$predict("v", newdata = newdt, 1:2)
#' M$predict("r", newdata = newdt)
#' 
#' @export
hm1988 <- function(formula, state, budget, trial, ntrials, initstate, data, choicerule, fix = list(), options = NULL, fitnessfun = NULL) {
  .args <- as.list(rlang::call_standardise(match.call())[-1])
  return(do.call(what = Hm1988$new, args = .args, envir = parent.frame()))
}

Hm1988 <- R6Class("hm1988",
  inherit = Cogscimodel,
  public = list(
    s_t_b_s0_T_var = NULL,
    environments = NULL,
    fitnessfun = NULL,
    envid = NULL,
    EV = NULL,
    V = NULL,
    env = "rsenvironment",
    initialize = function(formula, state, budget, trial, ntrials, initstate = 0, data, fix = list(), choicerule = NULL, options = list(), fitnessfun = function(state, budget) { as.numeric(state >= budget) }) {
      if (missing(formula)) {
        formula <- ~ timehorizon + state
      }
      if (missing(data)) {
        time_state_names <- attr(terms(formula), "term.labels")
        data <- env$makeData(time_state_names)
      }
      self$fitnessfun <- fitnessfun
      data <- as.data.frame(data)
      self$s_t_b_s0_T_var <- list(state, trial, budget, initstate, ntrials)
      self$init_environments(f = formula, d = data)
      self$V <- lapply(unique(self$envid), function(i)
        self$make_value_mat(e = self$environments[[i]]))
      self$EV <- lapply(unique(self$envid), function(i)
        self$make_ev_mat(e = self$environments[[i]], V = self$V[[i]]))
      super$initialize(
        formula = formula,
        data = data,
        parspace = make_parspace(),
        fix = fix,
        choicerule =  choicerule,
        title = "Optimal RSFT Model (Houston & McNamara, 1988)",
        discount = 0,
        mode = "discrete",
        options = options)  
      },
      predict = function(type = c("response", "values", "ev", "pstates"), action = NULL, newdata = NULL) {
        if (!is.null(newdata)) {
          stop("New data is not (yet) implemented.")
        }
        type = match.arg(type)
        pred <- data.table(envid = self$envid)
        pred[, order := .I]
        out <- pred[, c(list(order = order), as.list(as.data.frame(self$make_prediction(envid = envid[1], type = ..type, action = ..action)))), by = envid][, -1]
        return(as.matrix(out[order(order), -1]))
        
        # if (is.null(newdata)) {
        #   input <- self$input
        #   t_s_b_nt <- self$more_input
        # } else {
        #   input <- self$get_input(f=self$formula, d=newdata)
        #   t_s_b_nt <- self$get_more_input(d=newdata)
        # }
        # input <- if ( is.null(newdata) ) { self$input } else { self$get_input(self$formula, newdata) }
      },
      make_prediction = function(envid, type, action = NULL) {
        env <- self$environments[[envid]]
        V <- self$V[[envid]]
        EV <- self$EV[[envid]]
        action <- if ( is.null(action) & env$n.actions == 2) { 1 } else { self$stimnames }
        s_t_b_s0_T <- self$more_input[self$envid == envid, ]
        # Store the separate variables
        states <- s_t_b_s0_T[, 1, drop = FALSE]
        timehorizon <- s_t_b_s0_T[, 5] - s_t_b_s0_T[, 2] + 1
        rows <- match(states, env$states)
        cols <- match(timehorizon, rev(env$trials))
        acts <- rep(seq_len(env$n.actions), each = length(timehorizon))

        if (type == "response" | type == "values") {
          v <- V[cbind(rows, cols, acts)]
          v <- matrix(v, ncol = env$n.actions, dimnames = list(NULL, self$stimnames))
          if (type == "values") {
            return(v[, action, drop = FALSE])
          } else {
            return(super$apply_choicerule(v)[, action, drop = FALSE])
          }
        } else if (type == "ev") {
          return(EV[cbind(rows, cols, 1)])
        } else if (type == "pstates") {
          ps <- V
          ps[] <- super$apply_choicerule(matrix(ps, nc = env$n.actions))
          return(env$policyPS(ps)[cbind(rows, cols)])
        }
      },
      init_environments = function(f, d) {
        # Number of risky choice options
        n <- length(all.vars(formula(as.Formula(f), lhs=0, rhs=1)))
        d <- cbind(
          private$get_more_input(d = d),
          get_all_vars(formula = f, data = d)
        )
        # variable name of state, trial, budget, init state, num trials
        vars <- names(d)[1:5]
        # Names of the budget, n(trials), initial state
        # b_name  <- all.vars(self$formula_bstntis)[1]
        # nt_name <- all.vars(self$formula_bstntis)[4] 
        # is_name <- all.vars(self$formula_bstntis)[5]
        # # Formula of them
        # f_bsnt <- as.formula(paste("~", paste(all.vars(self$formula_bstntis)[c(1,4,5)], collapse="+")))
        # f2 <- update(f, as.formula(paste("NULL ~ . +", f_bsnt[2])))
        # Store the respective data
        d[, envid := match(apply(.SD, 1, paste, collapse=""), unique(apply(.SD, 1, paste, collapse="")))]
        unique_d <- unique(d)
        unique_input <- super$get_input(f = f, d = as.data.frame(unique_d))
        # Store an environment in the list
        no <- dim(unique_input)[3]
        self$environments <- lapply(unique_d$envid, function(i, vars) {
          .row <- which(unique_d$envid == i)
          .args <- list(
            budget = unlist(unique_d[.row, vars[3], with = FALSE]),
            n.trials = unlist(unique_d[.row, vars[5], with = FALSE]),
            initial.state = unlist(unique_d[.row, vars[4], with = FALSE]),
            terminal.fitness = self$fitnessfun)
          .oargs <- lapply(1:no, function(o, .row, input) {
              t(matrix(unique_input[.row, , o], nr = 2))[, 2:1, drop = FALSE]
              }, .row = .row, input = input)
          names(.oargs) <- paste0("o", 1:no)
          .args <- c(.args, .oargs)
          do.call(rsenvironment, .args)
        }, vars = vars)
        # store environment id to match the order in the data from different environments later
        self$envid <- d$envid
      },
      make_value_mat = function(e) {
        #e <- self$env
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
        # self$V <- V
        return(V)
      },
      make_ev_mat = function(e, V) {
        #e <- self$env
        EV <- e$makeStateTrialActionMat()
        P <- t(apply(V, 1, super$apply_choicerule))
        P <- array(P, dim = dim(V), dimnames = dimnames(V))
        P[is.na(P)] <- 0
        for (s in rownames(EV)) {
          for (tr in e$trials) {
            EV[s, tr, ] <- e$policyEV(
              policy = P,
              s0 = as.numeric(s),
              t0 = as.numeric(tr))
          }
        }
        dimnames(EV)[[3]] <- NULL
        return(EV)
        #self$EV <- EV
      }
  ),
  # private functions
  private = list(
    get_more_input = function(d) {
      input <- lapply(self$s_t_b_s0_T_var, function(v, d) {
          if (is.character(v)) v <- chr_to_rhs(v)
          if (is.numeric(v)) {
            return(v[seq.int(nrow(d))])
          } else if (inherits(v, "formula")) {
            return(get_all_vars(v, data = d)[,1])
          }
      }, d = d)
      return(as.data.table(input))
    }
  )
)