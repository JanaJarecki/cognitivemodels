#' Houston & McNamara's (1988) dynamic optimization model for risk-sensitive foraging problems in discrete time
#' 
#' @import data.table
#' @inheritParams Cm
#' @param formula An oject of class \link[stats]{formula}, e.g. \code{choice ~ x1 + p1 + x2 + p2 | y1 + q1 + y2 + q2}. Defines the risky options by alternating outcome1 + probability1 + outcome2 + probability2 + .... Different gambles are separated by a \code{"|"}. Leeft-hand-side is optional.
#' @param data A data frame holding the variables of the gambles in \code{formula}.
#' @param trials (defauls \code{"all"}) A numeric vector of length \code{nrow(data)}; the current decision trial. Can also be a single number, a formula (e.g., \code{~t} in which case the model takes \code{data$t} as trial), or \code{"all"} (in which case the model predicts for all trials).
#' @param states (defauls \code{"all"}) A numeric vector of length \code{nrow(data)}; the resource states or point total. Can also be a single number or a formula (e.g., \code{~s} in which case the model takes \code{data$s} as states), or \code{"all"} in which case the model predicts for all possible states.
#' @param budget A number; the critical state/goal/requirement, that matters in the terminal payout function. Can also be a numeric vector (of length \code{nrow(data)}), or a formula (e.g., \code{~b} in which case the model takes \code{data$b} as budget).
#' @param ntrials A number; the total number of trials available, must be greater than \code{trial}. Can also be a numeric vector (of length \code{nrow(data)}) or a formula (e.g., \code{~total}) in which case the model takes \code{data$total}).
#' @param initstate (default 0) A number; the starting state in the first trial. Can also be a numeric vector (of length \code{nrow(data)}) or a formula (e.g., \code{~state0}) in which case the model takes \code{data$state0}).
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
hm1988 <- function(formula, trials, states, budget, ntrials, initstate = 0, data = NULL, choicerule = NULL, fix = list(), options = NULL, fitnessfun = NULL) {
  .args <- as.list(rlang::call_standardise(match.call())[-1])
  return(do.call(what = Hm1988$new, args = .args, envir = parent.frame()))
}

Hm1988 <- R6Class("hm1988",
  inherit = Cm,
  public = list(
    s_t_b_s0_T_var = NULL,
    unique_d = NULL,
    environments = NULL,
    fitnessfun = NULL,
    envid = NULL,
    EV = NULL,
    V = NULL,
    env = "rsenvironment",
    initialize = function(formula, trials = "all", states = "all", budget, ntrials, initstate = 0, data = NULL, fix = list(), choicerule = "argmax", options = list(), fitnessfun = function(state, budget) { as.numeric(state >= budget) }) {
      self$fitnessfun <- fitnessfun   
      self$s_t_b_s0_T_var <- list(states, trials, budget, initstate, ntrials)
      self$init_environments(f = formula, d = data)
      self$V <- lapply(unique(self$envid), function(i)
        self$make_value_mat(e = self$environments[[i]]))
      self$EV <- lapply(unique(self$envid), function(i)
        self$make_ev_mat(e = self$environments[[i]], V = self$V[[i]]))
      formula <- 
      super$initialize(
        formula =  .add_missing_prob(formula),
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
        type <- match.arg(type)
        if (is.null(newdata)) {
          envid <- self$envid
          more_input <- self$more_input
        }
        if (!is.null(newdata)) {
          stop("'newdata' is not yet implemented (sorry).")
          more_input <- private$get_more_input(d = newdata)
          d <- cbind(
            as.data.table(more_input)[, 3:5],
            get_all_vars(formula = self$formula, data = newdata)
          )
          envid <- self$unique_d[d, on = .NATURAL]$envid
        }
        ids <- unique(envid)
        out <- lapply(
          ids,
          self$make_prediction,
          type = type,
          action = action)
        out <- do.call(rbind, out)

        if (sum(more_input[c("states", "trials")] == "all") == 1) {
          stop("If 'states' = 'all', then 'trials' must also be 'all'.")
        } else if (all(more_input[c("states", "trials")] == "all")) {
          order <- seq.int(sum(sapply(self$environments, function(x) nrow(x$makeData(c("t","s"))))))
        } else {
          order <- unlist(sapply(ids, function(x) which(envid == x)))
        }

        return(cogsciutils:::drop2(out[order(order), , drop = FALSE]))
      },
      make_prediction = function(envid, type, action = NULL, more_input = self$more_input) {
        env <- self$environments[[envid]]
        V <- self$V[[envid]]
        EV <- self$EV[[envid]]
        states <- self$get_states(e = env, id = envid, x = more_input$states)
        timehorizon <- self$get_timehorizons(e = env, id = envid, x = more_input$trials, nt = more_input$ntrials)
        action <- if (is.null(action) & env$n.actions == 2) { 1 } else { self$stimnames }
        
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
        d <- cbind(
          as.data.table(private$get_more_input(d = d))[, 3:5],
          get_all_vars(formula = f, data = d)
        )
        d[, envid := match(apply(.SD, 1, paste, collapse=""), unique(apply(.SD, 1, paste, collapse="")))]
        unique_d <- unique(d)
        self$unique_d <- unique_d
        unique_input <- super$get_input(f = f, d = as.data.frame(unique_d))
        # Store an environment in the list
        no <- dim(unique_input)[3]
        self$environments <- lapply(unique_d$envid, function(i, vars) {
          .row <- which(unique_d$envid == i)
          .args <- list(
            budget        = unlist(unique_d[.row, 1]),
            n.trials      = unlist(unique_d[.row, 3]),
            initial.state = unlist(unique_d[.row, 2]),
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
      get_states = function(e, x = self$more_input$states, id = self$envid) {
        if (missing(e)) {
          return(do.call(c, lapply(self$environments, self$get_states)))
        }
        if (x[1] == "all") {
          return(e$makeData(c("", ""))[, 2])
        } else {
          return(x[self$envid == id])
        }
      },
      get_timehorizons = function(e, x = self$more_input$trials, nt = self$more_input$ntrials, id = self$envid) {
        if (missing(e)) {
          return(do.call(c, lapply(self$environments, self$get_timehorizons)))
        }
        if (x[1] == "all") {
          return(e$makeData(c("", ""))[, 1])
        } else {
          return((nt - x + 1L)[self$envid == id])
        }
      },
      make_value_mat = function(e) {
        ns <- e$n.states
        F <- e$terminal.fitness(state = e$states, budget = e$budget)
        V <- e$makeStateTrialActionMat()
        for(t in rev(e$trials)) {
          T <- e$T[, , t, , drop = FALSE]
          F <- array(rep(F, each = ns), dim = dim(T))
          TxF <- T * F
          V[, t, ] <- colSums(aperm(TxF, c(2,1,3,4)))
          F <- apply(V[, t, , drop = FALSE], 1, max)
        }
        return(V)
      },
      make_ev_mat = function(e, V) {
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
      out <- lapply(self$s_t_b_s0_T_var, function(v, d) {
          if (is.character(v)) {
            if (v[1] == "all") {
              return(v[1])
            } else {
              v <- chr_to_rhs(v)
            }
          }
          if (is.numeric(v)) {
            return(rep(v, nrow(d)/length(v)))
          } else if (inherits(v, "formula")) {
            return(get_all_vars(v, data = d)[, 1])
          }
      }, d = d)
      names(out) <- c("states", "trials", "budget", "initstate", "ntrials")
      private$check_more_input(x = out)
      return(out)
    },
    check_input = function() {
      .check_probabilities(self = self)
      super$check_input() # Do not delete this, it runs default sanity checks
    },
    check_more_input = function(x) {
      if (is.numeric(x$trials)) {
        if (any(x$trials > x$ntrials)) {
        stop("All 'ntrials' must be > 'trials'. Check the values in rows:  ", .brackify(which(x$trials > x$ntrials)), ".")
        }
        if (any(x$trials <= 0)) {
          stop("All 'trials' must be > 0. Check the 'trials' in rows:  ", .brackify(which(x$trials <= 0)), ".")
        }
        if (any(x$ntrials <= 0)) {
          stop("All 'ntrials' must be > 0. Check the 'ntrials' in rows:  ", .brackify(which(x$ntrials <= 0)), ".")
        }
      }     
    }
  )
)