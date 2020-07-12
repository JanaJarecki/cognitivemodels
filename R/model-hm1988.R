# ==========================================================================
# Package: Cognitivemodels
# File: model-hm1988.R
# Author: Jana B. Jarecki
# ==========================================================================

# ==========================================================================
# Cognitive Model (maybe not really cognitive but more ecological)
# ==========================================================================


#' Dynamic optimization model for risk-sensitive foraging problems in discrete time
#' @description
#' `hm1988()` generates Houston & McNamara's (1988) optimal model for risk-sensitive foraging with discrete choices. 
#' 
#' @import data.table
#' 
#' @eval .param_formula(c(4,4), risky = TRUE)
#' @eval .param_fix(onlycr = TRUE)
#' @param trials The variable in `data` with the decision trial, can be a number, numeric vector, string, formula, or `".ALL"`. Vectors must have length `nrow(data)`. If `".ALL"` the model predicts for all possible trials and states.
#' @param states The variable in `data` with the states/accumulated resources, can be a number, numeric vector, string, formula, or `".ALL"`. Vectors must have length `nrow(data)`. If `".ALL"` the model predicts for all possible trials and states.
#' @param budget A number; the goal/requirement/critical state, that matters in the terminal payout function. Can also be a numeric vector of length `nrow(data)`, or a right-side formula that refers to a variable in `data`.
#' @param ntrials A number; the total number of trials available. Can also be a numeric vector of length `nrow(data)`, or a right-side formula that refers to a variable in `data`.
#' @param initstate (default 0) A number; the starting state in the first trial. Can also be a numeric vector of length `nrow(data)`, or a right-side formula that refers to a variable in `data`.
#' @param fitnessfun (optional) A function, the terminal fitness function, needs two arguments, `budget` and `state`.
#' @template param-choicerule
#' 
#' @references Houston, A. I., & McNamara, J. M. (1988). A framework for the functional analysis of behaviour. Behavioural and Brain Science, 11, 117-163. [doi:10.1017/S0140525X00053061](https://doi.org/10.1017/S0140525X00053061)
#' 
#' @details Risk-sensitive foraging means you have, for instance, four choices between the same two risky lotteries and after the four choices you need to have accumulated at least 12 points to get a reward. The optimal solution to this choice problem relies on dynamic programming. The function creates all possible future states given the possible remaining trials, and predicts the optimal choice polica or the expected value of chosing either option given a certain state and a certain time horizon.
#' 
#' ## Model Parameters
#' The model has no free parameters. If `choicerule` is specified, it can estimate 1 free parameter: `r .rd_choicerules()`
#' 
#' @template cm
#' 
#' @examples
#' ## Make fake data -----------------------------------------------------
#' D <- data.frame(
#'   x1 = 0, x2 = 1, x3 = 2,
#'   px11 = 0.1, px12 = 0.8, px13 = 0.1,
#'   px21 = 0.4, px22 = 0.2, px23 = 0.4,
#'   s = rep(9:11, each = 4),
#'   init = rep(9:11, each = 4), t = 4:1)
#' 
#' ## Setup the model --------------------------------------------------
#' M <- hm1988(~ x1+px11+x2+px12+x3+px13 | x1+px21+x2+px22+x3+px23,
#'              trials = ~t, states = ~s, budget = 12, ntrials = 4,
#'              initstate = ~init, data = D, choicerule = "argmax")
#' 
#' M                          # View model
#' predict(M)                 # Predict choice probability of 1st option (arg-max)
#' predict(M, type="values")  # Predict expected values
#' @export
hm1988 <- function(formula, trials, states, budget, ntrials, initstate = 0, data = NULL, choicerule = "argmax", fix = list(), options = NULL, fitnessfun = NULL) {
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
    initialize = function(formula, trials = ".ALL", states = ".ALL", budget, ntrials, initstate = 0, data = NULL, fix = list(), choicerule = "argmax", options = list(), fitnessfun = function(state, budget) { as.numeric(state >= budget) }) {
      if (sum(grepl(".ALL", c(trials, states))) == 1L) {
        states <- trials <- ".ALL"
        warning("Setting both, 'trials' and 'states' = '.ALL'.\n  * Both, trials and states, must be set to '.ALL', rather than one.")

      }
      self$fitnessfun <- fitnessfun
      self$choicerule <- .check_and_match_choicerule(choicerule, mode="discrete")
      super$init_par(parspace = make_parspace(), fix = fix, options = options, mode = "discrete")

      self$s_t_b_s0_T_var <- list(states, trials, budget, initstate, ntrials)
      self$init_environments(f = formula, d = data)
      self$V <- lapply(unique(self$envid), function(i)
        self$make_value_mat(e = self$environments[[i]]))
      self$EV <- lapply(unique(self$envid), function(i)
        self$make_ev_mat(e = self$environments[[i]], V = self$V[[i]]))
      super$initialize(
        formula = .add_missing_prob(formula),
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
        out <- lapply(ids,
          self$make_prediction,
          type = type,
          action = action)
        out <- lapply(out, as.matrix)
        out <- do.call(rbind, out)

        if (is.character(more_input$states) && more_input$states == ".ALL") {
          order <- seq.int(sum(sapply(self$environments, function(x) nrow(x$makeData(c("t","s"))))))
        } else {
          order <- unlist(sapply(ids, function(x) which(envid == x)))
        }

        return(cognitiveutils:::drop2(out[order(order), , drop = ncol(out) == 1]))
      },
      make_prediction = function(envid, type, action = NULL, more_input = self$more_input) {
        env <- self$environments[[envid]]
        V   <- self$V[[envid]]
        EV  <- self$EV[[envid]]
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
            return(v[, , drop = FALSE])
          } else {
            return(super$apply_choicerule(v))
          }
        } else if (type == "ev") {
          return(EV[cbind(rows, cols, 1)])
        } else if (type == "pstates") {
          ps <- V
          ps[] <- super$apply_choicerule(matrix(ps, nc = env$n.actions))
          y <- env$policyPS(ps)[cbind(rows, cols)]
          return(as.matrix(y))
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
        if (x[1] == ".ALL") {
          return(e$makeData(c("", ""))[, 2])
        } else {
          return(x[self$envid == id])
        }
      },
      get_timehorizons = function(e, x = self$more_input$trials, nt = self$more_input$ntrials, id = self$envid) {
        if (missing(e)) {
          return(do.call(c, lapply(self$environments, self$get_timehorizons)))
        }
        if (x[1] == ".ALL") {
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
      }
  ),
  # private functions
  private = list(
    get_more_input = function(d) {
      out <- lapply(self$s_t_b_s0_T_var, function(v, d) {
          if (is.character(v)) {
            if (v[1] == ".ALL") {
              return(v[1])
            } else {
              v <- .as_rhs(v)
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