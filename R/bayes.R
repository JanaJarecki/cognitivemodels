#' Bayesian Inference Cognitive Model
#' 
#' \code{bayes()} fits a Bayesian cognitive model, updating beliefs about discrete-event probabilities from  event frequencies, \code{bayes_beta()} is for binomial events, \code{bayes_dirichlet()} is for categorical/multinomial events.
#' 
#' @useDynLib cogscimodels, .registration = TRUE
#' @importFrom gtools rdirichlet
#' 
#' @inheritParams Cogscimodel
#' @param formula A formula specifying reported beliefs ~ event + event ... (e.g., \code{y ~ coin_heads + coin_tails}).
#' @param format A string (default \code{"raw"}) with the data format. Can be \code{"raw"}, \code{"cummulative"}, \code{"count"}, where \code{"raw"} means data are occurrence indicators (1=event, 0=no event); \code{"cumulative"} means data are cumulative event frequencies (1,2,2,...), and code{"count"} means data are non-ordered event frequencies (10,2,5,...).
#' @param type (optional) A string, \code{"beta-binomial"} or \code{"dirichlet-multinomial"}, specifying the type of inference. Can be abbreviated. Will be inferred if missing.
#' @param ... other arguments from other functions, currently ignored.
#' 
#' @return An model object (similar to lm-objects) of class "bayes". It can be viewed with \code{summary(mod)}, where \code{mod} is the name of the model object.
#' 
#' @section Parameter Space:
#' \tabular{lrcllr}{\verb{   }\strong{Name} \tab \verb{    }\strong{LB} \tab  \strong{-} \tab \strong{UB}\verb{    } \tab \strong{Description} \tab \strong{Start Value}\cr
#' \verb{   }\code{delta} \tab  0 \tab  - \tab  10 \tab  Weight of observation during learning, < 1 yields conservatism, > 1 yields liberal learning, 1 is optimal Bayesian \tab  1\cr
#' \verb{   }\code{priorpar} \tab  0.001 \tab  - \tab   n events \tab  Hyperparameter of the prior belief distribution before trial 1, sum to n events. Note: parameter names will be the RHS of the \code{formula} \tab  1}
#' 
#' @author Jana B. Jarecki, Markus Steiner
#' @references {Griffiths, T. L., & Yuille, A. (2008). Technical Introduction: A primer on probabilistic inference. In N. Chater & M. Oaksford (Eds.), \emph{The Probabilistic Mind: Prospects for Bayesian Cognitive Science (pp. 1 - 2)}. Oxford University Press. \url{https://doi.org/10.1093/acprof:oso/9780199216093.003.0002}}
#' @references {Tauber, S., Navarro, D. J., Perfors, A., & Steyvers, M. (2017). Bayesian models of cognition revisited: Setting optimality aside and letting data drive psychological theory. \emph{Psychological Review, 124(4)}, 410 - 441. \url{http://dx.doi.org/10.1037/rev0000052}}
#' 
#' @details Given the formula \code{y ~ a} the model predicts beliefs about event "a" occurring, but given \code{y ~ a + b} it predicts beliefs about events "a" and "b" occurring. If "a" and "b" are complementary the predictions will be complements, the difference is that
#' \itemize{
#'  \item{For \code{y~a} predictions have 1 column, \code{pred_a}}
#'  \item{For \code{y~a+b} predictions have 2 columns, \code{pred_a, pred_b} (with \code{pred_a} = 1 - \code{pred_b})}
#'  \item{For \code{y~a+b+c} predictions have 3 columns, \code{pred_a, pred_b, pred_c}}
#'  \item{etc.}
#' }
#' 
#' \emph{Note}, during parameter fitting the model treats the response variable \code{y} as beliefs about the \emph{first} event. In other words, \code{y} represents observed beliefs about \code{a} for the formula \code{y ~ a + b} (not \code{b}).
#' 
#' 
#' @examples 
#' D <- data.frame(
#'   a = c(0,0,1,1,1),              # event A, e.g. coin toss "heads"
#'   b = c(1,1,0,0,0),              # event B, complementary to A
#'   y = c(0.5,0.6,0.6,0.6,0.5))    # participants' reported beliefs
#' 
#' M <- bayes(y ~ a + b, D, fix="start")          # fixed par. to start values
#' predict(M)                                     # predict posterior means
#' anova(M)                                       # anova-like table
#' summary(M)                                     # summarize
#' M$MSE()                                        # mean-squared error   
#' 
#' ### Different predictions
#' # ---------------------------------------
#' predict(M, type = "mean")                      # predict posterior mean
#' predict(M, type = "max")                       #  --"--  maximum posterior
#' predict(M, type = "sd")                        #  --"--  posterior SD
#' predict(M, type = "posteriorpar")              #  --"--  posterior hyper-par.
#' predict(M, type = "draws", ndraws = 3)         #  --"--  3 draws from posterior
       
#' 
#' ### Ways to formulate the model parameter
#' # ---------------------------------------
#' bayes(~a+b, D, list(delta=1, priorpar=c(1, 1)))  # delta=1, uniform prior
#' bayes(~a,   D, list(delta=1, priorpar=c(1, 1)))  # -- (same) --
#' bayes(~a+b, D, list(delta=1, a=1, b=1))          # -- (same) --
#' bayes(~a+b, D, fix = "start")                    # fix par. to start values
#' bayes(~a,   D, fix = "start")                    # -- (same) --

#' 
#' 
#' ### Parameter fitting
#' ---------------------
#' # Use a response variable, y, to which we fit parameter
#' bayes(y ~ a + b, D, fix = "start")               # "start" fixes all par., fit none 
#' bayes(y ~ a + b, D, fix = list(delta=1))         # fix delta, fit priors 
#' bayes(y ~ a + b, D, fix = list(a=1, b=1))        # fix priors, fit delta 
#' bayes(y ~ a + b, D, fix = list(delta=1, a=1))    # fix delta & prior on "a"
#' bayes(y ~ a + b, D, list(delta=1, b=1))          # fix delta & prior on "b"
#' 
#' 
#' ### Parameter meanings
#' ------------------------
#' # delta parameter
#' bayes(y ~ a, D, c(delta = 0))                    # delta=0 -> no learning
#' bayes(y ~ a, D, c(delta = 0.1))                  # 0.1 -> slow learning
#' bayes(y ~ a, D, c(delta = 9))                    # 9   -> fast learning
#' 
#' # prior parameter
#' bayes(y ~ a, D, c(a=1.5, b=0.5))                 # prior belief: "a" more likely
#' bayes(y ~ a, D, c(priorpar = c(1.5, 0.5)))       # -- (same) --
#' bayes(y ~ a, D, c(a = 0.1, b=0.9))               # prior belief: "b" more likely
#' bayes(y ~ a, D, c(priorpar = c(0.1, 1.9)))       # -- (same) --
#' 
#' @export
bayes <- function(formula, data, fix = list(), format = c("raw", "count", "cumulative"), type = NULL, discount = 0L, options = list(), ...) {
  .args <- as.list(rlang::call_standardise(match.call())[-1])
  return(do.call(what = Bayes$new, args = .args, envir = parent.frame()))
}

#' Bayesian Inference Cognitive Model
#' @inheritParams bayes
#' @rdname bayes
#' @details \code{bayes_beta()} calls \link{bayes} with \code{type = "beta-binomial"}.
#' @export
bayes_beta <- function(formula, data, fix = NULL, format = NULL, ...) {
  .args <- as.list(rlang::call_standardise(match.call())[-1])
  return(do.call(what = Bayes$new, args = .args, envir = parent.frame()))
}

#' Bayesian Inference Cognitive Model
#' @inheritParams bayes
#' @rdname bayes
#' @details \code{bayes_dirichlet()} calls \link{bayes} with \code{type = "dirichlet-multinomial"}.
#' @export
bayes_dirichlet <- function(formula, data, fix = NULL, format = NULL, ...) {
  .args <- as.list(rlang::call_standardise(match.call())[-1])
  return(do.call(what = Bayes$new, args = .args, envir = parent.frame()))
}



Bayes <- R6Class("Bayes",
  inherit = Cogscimodel,
  public = list(
    priordist = NULL,
    format = NULL,
    priornames = NULL,
    npred = NULL,
    initialize = function(formula, data, type = NULL, format = c("raw", "cumulative", "count"), fix = list(), choicerule = NULL, mode = "continuous", discount = 0, options = list(), ...) {
      self$format <- match.arg(format)
      self$priordist <- self$infer_priordist(type = type, f=formula)
      self$init_npred(f=formula)
      fix <- self$reformulate_fix(fix = fix, f = formula)
      if (is.null(options$fit_measure)) options$fit_measure <- "mse"
      if (grepl("^l", options$fit_measure) & options$fit==TRUE) {
        stop("Sorry, for bayes(), log likelihood is not yet implemented as fit measure.")
      }

      super$initialize(
        title = "Bayesian model",
        formula = formula,
        data = data,
        parspace = self$make_parspace(f=formula),
        choicerule = choicerule,
        mode = mode,
        fix = fix,
        discount = discount,
        options = c(options, list(fit_solver = c("solnp"))),
        ...)
    },
    make_prediction = function(type, input, s, ndraws = 3, ...) {
      type <- match.arg(type, c("response", "mean", "max", "sd", "draws", "posteriorpar"))
      if (type=="response") type <- "mean"
      par <- self$get_par()
      na <- self$natt()[1]
      no <- self$nobs()     
      if (self$format != "count") {
        # shift input by 1 lag
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
      f <- as.Formula(f)
      self$npred <- vapply(1:length(f)[2], function(i) length(attr(terms(formula(f, lhs=0, rhs=i)), "term.labels")), 1L)
    },
    make_prednames = function() {
      nn <- self$get_parnames()[-1]
      natt <- self$natt()
      npred <- self$npred
      tmp <- unlist(lapply(1:self$nstim(), function(i) {
        if (npred[i] == 1) { c(TRUE, FALSE) } else { rep(TRUE, npred[i]) } }))
      return(nn[tmp])
    },
    reformulate_fix = function(fix, f) {
      if(any(grepl("^priorpar$", names(fix)))) {
        return(c(
          fix[-which(grepl("^priorpar$", names(fix)))],
          setNames(as.list(fix[["priorpar"]]), rownames(self$make_parspace(f=f))[-1])))
      } else {
        return(fix)
      }
    },
    get_input = function(f = self$formula, d) {
      if (self$format == "raw") {
        return(self$count_raw_data(super$get_input(f=f,d=d)))
      } else {
        return(super$get_input(f=f,d=d))
      }
    },
    update_priorpar = function(data, delta, priorpar) {
      dist = self$priordist
      priorpar <- matrix(priorpar, ncol = ncol(data), nrow = nrow(data), byrow = TRUE)
      if (dist == "beta-binomial" | dist == "dirichlet-multinomial") {
        return(priorpar + delta * data)
      }
    },
    set_formula = function(f) {
      f <- as.Formula(f)
      fs <- lapply(seq.int(length(f)[2]), function(x) formula(f, lhs=0, rhs=x, drop = FALSE))
      trms <- lapply(fs, function(x) attr(terms(x), "term.labels"))
      fs <- lapply(1:length(trms), function(i) {
        x <- trms[[i]]
        if (length(x) == 1) {
          if (self$format != "raw") {
            stop('"formula" needs > 1 right-hand side variables (unless format is "raw"), but "formula" has only 1 RHS (', .abbrDeparse(f), "). -> Add another variable to the formula to specify the counts of the other possible event(s), like: y ~ count_0 + count_1.")
          }
          update(fs[[i]], as.formula(paste0("~ . + I(1-", x, ")")))
          } else {
            fs[[i]]
          }
        })
      self$formula <- update(f, as.formula(paste("", paste(fs, collapse = " | "))))
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
    make_parspace = function(f) {
      self$set_formula(f)
      f <- self$formula
      # Learning rate parameter
      d_par <- list(delta = c(0L, 10, 1, 1)) # NOT dynamic
      
      # Dynamic parameters: prior names are dynamically generated from
      # RHS of formula. If formula is ... ~ xx + yy we name priors:
      # "prior.xx" and "prior.yy"
      p_names <- sapply(seq.int(length(f)[2]), function(i) attr(terms(formula(f, rhs=i)), "term.labels"))
      p_names <- p_names
      p_par <- setNames(lapply(1:length(p_names), function(.) c(0.001, self$natt()[1], 1L, 1L)), p_names)

      return(do.call(make_parspace, c(d_par, p_par))) #Note: don't change order
    },
    make_constraints = function() {
      # make constraint for the priors
      # note: priors depend on the type of prior distribution
      # (beta distribution, dirichlet distribution, etc.)
      # therefore we make prior parameter dynamically
      parnames <- self$get_parnames()
      parnames_fix <- self$get_parnames("fix")
      parnames_priors <- self$get_parnames("priors")
      a <- cumsum(self$natt())
      a0 <- cumsum(self$natt())-1
      id <- lapply(1:self$nstim(), function(i) a0[i]:a[i])
      C <- lapply(id, function(i) {
        if (!all(parnames_priors[i] %in% parnames_fix)) {
          L_constraint(
            L = as.integer(parnames %in% parnames_priors[i]),
            dir = "==",
            rhs = length(parnames_priors[i]))
        }
      })
      return(do.call(.combine_constraints, c(C, list(super$make_constraints()))))
    },
    check_input = function() {
      # This function is called automatically at the end of initialize()
      # Write code for checking inputs (= data of the RHS of formula) below

      # -----------------
      super$check_input() # leave this at the end, it runs background checks
    }
  )
)
