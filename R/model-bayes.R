# ==========================================================================
# Package: Cognitivemodels
# File: model-bayes.R
# Author: Jana B. Jarecki
# ==========================================================================

# ==========================================================================
# Cognitive Model
# ==========================================================================

#' Bayesian Inference Cognitive Model
#' @name bayes
#' @description
#' `bayes()` fits a Bayesian cognitive model, updating beliefs about the probability of discrete event outcomes based on the frequencies of outcomes.
#'   * `bayes_beta_c()` fits a model for 2 outcomes (beta-binomial) for continuous responses
#'   * `bayes_beta_d()` fits a model for 2 outcomes (beta-binomial) for discrete responses
#'   * `bayes_dirichlet_c()` fits a model for _n > 2_ outcomes (dirichlet-categorical/multinomial) for continuous responses
#'   * `bayes_dirichlet_d()` fits a model for _n > 2_ outcomes (dirichlet-categorical/multinomial) for discrete responses
#' 
#' @useDynLib cognitivemodels, .registration = TRUE
#' @importFrom gtools rdirichlet
#' 
#' @eval .param_formula(2)
#' @eval .param_fix("bayes_beta_d", dyn_args = "formula", which = 2)
#' @param format (optional) A string, the format the data to be modeled, can be abbreviated, default is `"raw"`; allowed values:
#'  * `"raw"` means that the data are trial-by-trial binary occurrence indicators: 1, 0, 1, ... means the event happened in trial with a value of 1.
#'  * `"cumulative"` means the data are trial-by-trial cumulative counts of events: 0, 1, 1, 2, ... counts how often the event happened up to the trial.
#'  * `"count"` means the data are total events counts, ignoring the trial-by-trial order of events: 2, 10, ... means the event happened 2 times, then (starting from zero!) it happened 10 times.
#' @param type (optional) A string, the type of inference, `"beta-binomial"` or `"dirichlet-multinomial"`. Can be abbreviated. Will be inferred, if missing.
#' @param prior_sum (optional) A number; the prior hyperparameter will be constrained to sum to this number; defaults to the number of prior parameters; if `prior_sum = NA` no sum constraint is placed.
#' 
#' @details
#' The model models -- as response -- the belief about the occurrence of the first event in the `formula` as follows:
#' * `y ~ x1` models the beliefe about event **x1 occurring** versus it not occurring.
#' * `y ~ x1 + x2` models beliefs about **x1 versus x2** occurring.
#' * `y ~ x1 + x2 + x3` models beliefs about x1, x2, and x3 occurring.
#' 
#' ## Model Parameters
#' The model has _n + 1_ (_n_ = number of events) free parameters, which are:
#' * `delta` is the learning rate, it weights the observation during learning, value < 1 causes conservatism, > 1 causes liberal learning, and 1 is optimal Bayesian.
#' * `x1, x2` (dynamic names) are the prior parameter, their names correspond to the right side of `formula`. Also known as the hyperparameter of the prior belief distribution before trial 1. If they are constrainted to sum to _n_ and _n_ - 1 parameter are estimated.
#' * In `bayes_beta_d()` or `bayes_dirichlet_d()`: `r .rd_choicerules()`
#' 
#' @author Markus Steiner
#' @template cm
#' 
#' @references 
#' {Griffiths, T. L., & Yuille, A. (2008). Technical Introduction: A primer on probabilistic inference. In N. Chater & M. Oaksford (Eds.), \emph{The Probabilistic Mind: Prospects for Bayesian Cognitive Science (pp. 1 - 2)}. Oxford University Press. \url{https://doi.org/10.1093/acprof:oso/9780199216093.003.0002}}
#' 
#' {Tauber, S., Navarro, D. J., Perfors, A., & Steyvers, M. (2017). Bayesian models of cognition revisited: Setting optimality aside and letting data drive psychological theory. \emph{Psychological Review, 124(4)}, 410 - 441. \url{http://dx.doi.org/10.1037/rev0000052}}
#' 
#' 
#' @examples 
#' D <- data.frame(
#'   a = c(0,0,1,1,1),              # event A, e.g. coin toss "heads"
#'   b = c(1,1,0,0,0),              # event B, complement of A
#'   y = c(0.5,0.3,0.2,0.3,0.5))    # participants' beliefs about A
#' 
#' M <- bayes_beta_c(
#'      formula = y ~ a + b,
#'      data = D)   # fit all parameters
#' predict(M)                        # predict posterior means
#' summary(M)                        # summarize model
#' parspace(M)                       # view parameter space
#' anova(M)                          # anova-like table
#' logLik(M)                         # loglikelihood
#' MSE(M)                            # mean-squared error   
#' 
#' 
#' # Predictions ----------------------------------------------
#' predict(M, type = "mean")                  # posterior mean
#' predict(M, type = "max")                   # maximum posterior
#' predict(M, type = "sd")                    # posterior SD
#' predict(M, type = "posteriorpar")          # posterior hyper-par.
#' predict(M, type = "draws", ndraws = 3)     #  --"--  3 draws
#' 
#' 
#' # Fix parameter ---------------------------------------------
#' bayes_beta_c(~a+b, D, list(delta=1, priorpar=c(1, 1)))  # delta=1, uniform prior
#' bayes_beta_c(~a+b, D, list(delta=1, a=1, b=1))          # -- (same) --
#' bayes_beta_c(~a+b, D, fix = "start")                    # fix to start values
#' 
#' 
#' # Parameter fitting ----------------------------------------
#' # Use a response variable, y, to which we fit parameter
#' bayes(y ~ a + b, D, fix = "start")              # "start" fixes all par., fit none 
#' bayes(y ~ a + b, D, fix = list(delta=1))         # fix delta, fit priors 
#' bayes(y ~ a + b, D, fix = list(a=1, b=1))        # fix priors, fit delta 
#' bayes(y ~ a + b, D, fix = list(delta=1, a=1))    # fix delta & prior on "a"
#' bayes(y ~ a + b, D, list(delta=1, b=1))          # fix delta & prior on "b"
#' 
#' 
#' ### Parameter meanings
#' # ---------------------------------------
#' # delta parameter: the learning rate or evidence weight
#' bayes(y ~ a + b, D, c(delta = 0))             # 0   -> no learning
#' bayes(y ~ a + b, D, c(delta = 0.1))           # 0.1 -> slow learning
#' bayes(y ~ a + b, D, c(delta = 9))             # 9   -> fast learning
#' bayes(y ~ a + b, D, c(a=1.5, b=0.5))                # prior: a more likely
#' bayes(y ~ a + b, D, list(priorpar=c(1.5, 0.5)))     # -- (same) --
#' bayes(y ~ a + b, D, c(a = 0.1, b=1.9))              # prior: b more likely
#' bayes(y ~ a + b, D, list(priorpar = c(0.1, 1.9)))   # -- (same) --
NULL


#' @rdname bayes
#' @export
bayes_beta_c <- function(formula, data, fix = NULL, format = c("raw", "count", "cumulative"), prior_sum = NULL, ...) {
  .args <- as.list(rlang::call_standardise(match.call())[-1])
  .args[["mode"]] <- "continuous"
  .args[["options"]] <- list(fit_args = list(pdf = "truncnorm", a = 0, b = 1))
  return(do.call(what = Bayes$new, args = .args, envir = parent.frame()))
}


#' @rdname bayes
#' @export
bayes_beta_d <- function(formula, data, fix = NULL, format = NULL, prior_sum = NULL, ...) {
  .args <- as.list(rlang::call_standardise(match.call())[-1])
  .args[["mode"]] <- "discrete"
  return(do.call(what = Bayes$new, args = .args, envir = parent.frame()))
}


#' @rdname bayes
#' @export
bayes_dirichlet_d <- function(formula, data, fix = NULL, format = NULL, prior_sum = NULL, ...) {
  .args <- as.list(rlang::call_standardise(match.call())[-1])
  .args[["mode"]] <- "discrete"
  return(do.call(what = Bayes$new, args = .args, envir = parent.frame()))
}


#' @rdname bayes
#' @export
bayes_dirichlet_c <- function(formula, data, fix = NULL, format = NULL, prior_sum = NULL,  ...) {
  .args <- as.list(rlang::call_standardise(match.call())[-1])
  .args[["mode"]] <- "continuous"
  return(do.call(what = Bayes$new, args = .args, envir = parent.frame()))
}

#' @rdname bayes
#' @export
bayes <- function(formula, data = data.frame(), fix = list(), format = c("raw", "count", "cumulative"), type = NULL, discount = 0L, options = list(), prior_sum = NULL, ...) {
  .args <- as.list(rlang::call_standardise(match.call())[-1])
  return(do.call(what = Bayes$new, args = .args, envir = parent.frame()))
}


#' @noRd
Bayes <- R6Class("Bayes",
  inherit = Cm,
  public = list(
    priordist = NULL,
    format = NULL,
    priornames = NULL,
    npred = NULL,
    prior_sum = FALSE,
    initialize = function(formula, data = data.frame(), type = NULL, format = c("raw", "cumulative", "count"), fix = list(), choicerule = NULL, mode = NULL, discount = 0, options = list(),  prior_sum = NULL, ...) {
      format <- match.arg(format)
      formula <- private$sanitize_formula(f = formula, format = format)
      self$format <- format
      self$priordist <- self$infer_priordist(type = type, f=formula)
      self$prior_sum <- c(.rhs_length(formula)[1], prior_sum)[1L + !is.null(prior_sum)]
      self$init_npred(f=formula)
      fix <- self$reformulate_fix(fix = fix, f = formula)
      if (is.null(options$fit_measure)) options$fit_measure <- "loglikelihood"
      super$initialize(
        title = "Bayesian model",
        formula = formula,
        data = data,
        parspace = self$make_parspace(f=formula),
        choicerule = choicerule,
        mode = mode,
        fix = fix,
        discount = discount,
        options = c(options, list(solver = c("solnp"))),
        ...)
    },
    #' @export
    make_prediction = function(type, input, s, ndraws = 3, ...) {
      type <- match.arg(type, c("response", "mean", "max", "sd", "draws", "posteriorpar"))
      if (type == "response") { type <- "mean" }
      par <- self$get_par()
      na <- self$natt[1]
      no <- self$nobs    
      if (self$format != "count") { # shift input by 1 lag
        input <- rbind(0L, input[-nrow(input), , drop = FALSE])
      }
      # Compute posterior parameter
      posteriorpar <- self$update_priorpar(data = input, priorpar = par[-1L][(1:na) + na * (s-1L)], delta = par["delta"])

      if (type == "draws") {
        self$prednames <- paste0(rep(self$prednames, each=ndraws), 1:ndraws)
        return(
          t(sapply(1:no, function(i) { self$posterior_draw(i, par = posteriorpar[i, , drop=FALSE], ndraws = ndraws) }))
        )
      } else if (type ==  "posteriorpar") {
        return(posteriorpar)
      } else {
        cols <- if (type == "sd") { 1 } else { 1:self$npred[s] }
        return(switch(type,
          mean = self$posterior_mean(posteriorpar),
          max = self$posterior_max(posteriorpar),
          sd = self$posterior_sd(posteriorpar))[, cols, drop = FALSE])
      }
    },
    init_npred = function(f) {
      self$npred <- ifelse(.rhs_length(f) == 2, 1, .rhs_length(f))
    },
    reformulate_fix = function(fix, f) {
      if (!is.list(fix) & any(grepl("^priorpar[0-9]", names(fix)))) {
          stop("'fix' must be a list, not a ", class(fix), ".", call. = FALSE)
        }
      if(any(grepl("^priorpar$", names(fix)))) {
        return(c(
          fix[-which(grepl("^priorpar$", names(fix)))],
          setNames(as.list(fix[["priorpar"]]), rownames(self$make_parspace(f=f))[-1])))
      } else {
        return(fix)
      }
    },
    update_priorpar = function(data, delta, priorpar) {
      dist = self$priordist
      priorpar <- matrix(priorpar, ncol = ncol(data), nrow = nrow(data), byrow = TRUE)
      if (dist == "beta-binomial" | dist == "dirichlet-multinomial") {
        return(priorpar + delta * data)
      }
    },
    posterior_draw = function(t, par, ndraws) {
      dist <- self$priordist
      if (dist == "dirichlet-multinomial" | dist == "beta-binomial") {
        return(matrix(gtools::rdirichlet(ndraws, par), nrow = 1L))
      }  
    },
    posterior_max = function(par, ...) {
      t <- self$priordist
      if (t == 'beta-binomial' | t == 'dirichlet-multinomial') {
        par[] <- round(par, 10)
        (par - 1) / rowSums(par - 1)
      }
    },
    posterior_mean = function(par, ...) {
      t <- self$priordist
      if (t == 'beta-binomial' | t == 'dirichlet-multinomial') {
        par / rowSums(par)
      }
    },
    posterior_sd = function(par, ...) {
      if (self$priordist == "beta-binomial") {
        a <- par[, 1, drop = FALSE]
        b <- par[, 2, drop = FALSE]
        return(sqrt((a * b) / ((a + b)^2 * (a + b + 1))))
      } else {
        message("Sorry, type='sd' is not (yet) implemented for conjugate prior: ", self$priordist, ".")
      }
    },
    infer_priordist = function(type = c("beta-binomial", "dirichlet-multinomial"), f) {
      if (length(type) > 0L) {
        return(match.arg(type))
      }
      f <- as.Formula(f)
      if (length(attr(terms(formula(f, lhs=0, rhs=1)), "term.labels")) < 3) {
        return("beta-binomial")
      } else {
        return("dirichlet-multinomial")
      }
    },
    count_raw_data = function(data) {
      if (!length(data)) return(NULL)
      counts <- apply(data, 3,
        function(p) {
          apply(p, 2, cumsum)
        }
      )
      return(array(counts, dim(data), dimnames = dimnames(data)))
    },
    get_parnames = function(x = "all") {
      if (x == "priors") {
        return(super$get_parnames()[-1])
      }
      return(super$get_parnames(x))
    },
    make_parspace = function(f = self$formula) {
      d_par <- list(delta = c(0L, 10, 1, 1))
      p_ul <- c(.rhs_length(f), self$prior_sum)[2L - (is.na(self$prior_sum))]
      p_par <- setNames(lapply(1:sum(.rhs_length(f)), function(.) c(0.00001, p_ul, 1L, 1L)), unlist(.rhs_varnames(f)))
      # Note: don't change the order, the d_par needs to come first
      return(do.call(make_parspace, c(d_par, p_par)))
    }
  ),

  # Private methods
  private = list(
    get_input = function(f = self$formula, d) {
      if (self$format == "raw") {
        return(self$count_raw_data(super$get_input(f=f,d=d)))
      } else {
        return(super$get_input(f=f,d=d))
      }
    },
    sanitize_formula = function(f, format = self$format) {
      if (format == "raw") {
        return(.add_missing_prob(f, 1, 1))
      } else {
        if (any(.rhs_length(f) == 1)) {
            stop("'formula' needs > 1 right-hand side variables (unless format = 'raw'), but has only 1 RHS (", .abbrDeparse(f), ").\n  * Did you forget to write the second option in 'formula'? (e.g., y ~ a + b)\n  * Do you wan tto use 'format = raw'?")
          }
        return(f)
      }
    },
    make_prednames = function() {
      nn <- private$get_parnames()[-1]
      natt <- self$natt[1]
      npred <- self$npred
      tmp <- unlist(lapply(1:self$nstim, function(i) {
        if (npred[i] == 1) { c(TRUE, FALSE) } else { rep(TRUE, npred[i]) } }))
      return(nn[tmp])
    },
    make_constraints = function() {
      if (is.na(self$prior_sum)) return(NULL)
      # Priors depend on the type of prior distribution
      parnames <- names(self$par)
      C <- NULL
      for (p in .rhs_varnames(self$formula)) {
        C <- .combine_constraints(C,
          L_constraint(
            L = as.integer(parnames %in% p),
            dir = "==",
            rhs = self$prior_sum,
            names = parnames)
        )
      }
      return(C)
    },
    check_input = function() {
      # This function is called automatically at the end of initialize()
      # Write code for checking inputs (= data of the RHS of formula) below
      # -----------------
      super$check_input() # leave this at the end, it runs background checks
    }
  )
)
