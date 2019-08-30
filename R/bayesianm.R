#' Bayesian Updating/Learning Model
#' 
#' @useDynLib cogscimodels, .registration = TRUE
#' @import rje
#' @description Fits a Bayesian cognitive learning model. According to this model, agents update_priorpar beliefs about probabilities of events in a Bayesian way given a series of observed events and prior beliefs. The model can make predictions from raw observations or relative frequencies.
#' @param formula A formula specifying the variable names of response and observed events, e.g. \code{reponse ~ eventA + eventB + eventC}. Use a pipe (\code{|}) to indicate learning about more than one options, e.g. \code{reponse ~ option1_eventA + option1_eventB | option2_eventX + option2_eventY}.
#' @param data A data.frame or matrix containing the variables in \code{formula}; ensure it is ordered in the order in which data was seen.
#' @param format A string (default \code{"raw"}), specifying if data are binary indicators for observed/not observed (1=event seen, 0=event not seen) or cummulative counts of how often an outcome was observed. Allowed are \code{"raw", "count"}.
#' @param fix (optional) specifies fixed parameter, to set priors for eventA and eventB use \code{fixed=list(prior.eventA=1, prior.eventB=4)}.
#' @param choicerule (optional) A string specifying the choice rule, allowed values see the \code{priordist} argument of \link{choicerule}. *Required* if \code{response = "discrete"} and model learns > 1 options.
#' @param mode (optional, default \code{"continuous"}) A string specifying if beliefs are predicted (\code{"continuous"}) or if choices are predicted (\code{"discrete"}).
#' @param discount (optional, default 0) A number or numeric vector of trial indices to discount.
#' @param ... other arguments from other functions, currently ignored.
#' @return An model object (similar to lm-objects) of class "bayeslearn". Itcan be viewed with \code{summary(mod)}, where \code{mod} is the name of the model object.
#' @examples 
#' #  No examples yet
#' @author Jana B. Jarecki, \email{jj@janajarecki.com}, Markus Steiner
#' @references {Griffiths, T. L., & Yuille, A. (2008). Technical Introduction: A primer on probabilistic inference. In N. Chater & M. Oaksford (Eds.), The Probabilistic Mind: Prospects for Bayesian Cognitive Science (pp. 1–2). Oxford University Press. https://doi.org/10.1093/acprof:oso/9780199216093.003.0002}

#' @export
bayesianm <- function(formula, data, fix = list(), format = c("raw", "count"), choicerule = NULL, mode = c("continuous", "discrete"), discount = 0, options, ...) {
  .args <- as.list(rlang::call_standardise(match.call())[-1])
  return(do.call(what = Bayesianm$new, args = .args, envir = parent.frame()))
}

Bayesianm <- R6Class('bayesianm',
  inherit = Cogscimodel,
  public = list(
    priordist = NULL,
    format = NULL,
    priornames = NULL,
    initialize = function(formula, data, format = c("raw", "count"), fix = list(), choicerule = NULL, mode = NULL, discount = 0, options = list(), ...) {
      self$format <- match.arg(format)
      self$priordist <- self$infer_priordist(f=formula)

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
      if (self$npar("free") > 0L) {
        message("Fitting free parameters ", .brackify(self$get_parnames("free")))
        self$fit()
      }
    },
    predict = function(type = c("pmean", "map", "psd", "draws", "hyperparm"), newdata = NULL, ndraws = 1000) {
      type <- match.arg(type)
      par <- self$get_par()
      D <- self$input
      D <- abind::abind(array(0L, dim=c(1, dim(D)[2:3])), D, along=1)
      prior_par <- par[self$get_parnames("priors")]
      posterior_par <- self$update_priorpar(
        prior_par = prior_par,
        data = D,
        delta = par["delta"]
        )
      no <- self$nobs()
      
      if (type == "draws") {
        return(lapply(1:no, function(t) { self$rdraw(t, rpar = posterior_par[t,, , drop=FALSE], ndraws = ndraws) }))
      } else if (type ==  "hyperparm") {
        return(array(posterior_par, dim=dim(D), dimnames=dimnames(D)))
      } else {
        R <- apply(posterior_par, 3L, switch(type,
          pmean = self$pmean,
          map = self$map,
          psd = self$psd))
        R <- array(R, dim = dim(D), dimnames = dimnames(D))
        R <- R[-dim(R)[1], , , drop = FALSE]
        R <- abind::adrop(R, c(FALSE, FALSE, dim(R)[3] == 1L))
        return(R)
      }         
    },
    reformulate_fix = function() {

    },
    get_input = function(f = self$formula, d) {
      if (self$format == "raw") {
        return(self$count_raw_data(super$get_input(f=f,d=d)))
      } else {
        return(super$get_input(f=f,d=d))
      }
    },
    update_priorpar = function(data, delta, prior_par) {
      dist = self$priordist
      posterior <- data
      for (o in 1:dim(data)[3]) {
        for (x in 1:dim(data)[2]) {
            if (dist == 'binomial' | dist == 'multinomial') {
              posterior[, x, o] <- prior_par[x] + data[,x,o] * delta
          }
        }       
      }
      return(posterior)
    },
    set_formula = function(f) {
      f <- as.Formula(f)
      fs <- lapply(seq.int(length(f)[2]), function(x) formula(f, lhs=0, rhs=x, drop=FALSE))
      trms <- lapply(fs, function(x) attr(terms(x), "term.labels"))
      # If any RHS-segnemts of formula have uneven number of variables
      # This means that the last probability was not supplied. In this case,
      # add the last probability as I(1-p1-p2-p3-...)
      fs <- lapply(1:length(trms), function(i) {
        x <- trms[[i]]
        if (length(x) == 1) {
          if (self$format == "count") {
            stop('If format="count", the RHS of "formula" needs more than 1 element. You must specify counts of all possible events (~ nA + nB +...). Did you only specify the count of one event? Check the RHS of your formula:\n    ', .abbrDeparse(f), ".")
          }
          update(fs[[i]], as.formula(paste0("~ . + I(1-", x, ")")))
          } else {
            fs[[i]]
          }
        })
      self$formula <- update(f, as.formula(paste("", paste(fs, collapse = " | "))))
    },
    rdraw = function(t, rpar, ndraws) {
      d <- c(ndraws, dim(self$input)[2:3])
      dn <- c(list(NULL), dimnames(self$input)[2:3])
      p_array <- array(NA, d, dn) # initialize
      dist <- self$priordist
      for (o in 1:d[3]) {
        p_array[,,o]  <- if (dist == 'multinomial') {
                            rje::rdirichlet(ndraws, rpar[,,o])
                          } else if (dist == 'binomial') {
                            rbeta(ndraws, rpar[1,1,o], rpar[1,2,o])
                          }
      }
      if(d[3] == 1) {
        return(p_array[,,1])
      } else {
        return(p_array)
      }     
    },
    map = function(par, ...) {
      t <- self$priordist
      if (t == 'binomial' | t == 'multinomial') {
        (par - 1) / rowSums(par - 1)
      }
    },
    pmean = function(par, ...) {
      t <- self$priordist
      if (t == 'binomial' | t == 'multinomial') {
        par / rowSums(par)
      }
    },
    psd = function(par, ...) {
      if (self$priordist == "binomial") {
        a <- par[,1]
        b <- par[,2]
        return(sqrt((a * b) / ((a + b)^2 * (a + b + 1))))
      }
    },
    infer_priordist = function(f) {
      f <- as.Formula(f)
      if (length(attr(terms(formula(f, lhs=0, rhs=1)), "term.labels")) < 3) {
        return("binomial")
      } else {
        return("multinomial")
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
      p_names <- paste0("prior.", p_names)
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
      con <- NULL
      if (!all(parnames_priors %in% parnames_fix)) {
        con <- L_constraint(
          L = as.integer(parnames %in% parnames_priors),
          dir = "==",
          rhs = length(parnames_priors))
      }
      return(.combine_constraints(con, super$make_constraints()))
    },
    check_input = function() {
      # This function is called automatically at the end of initialize()
      # Write code for checking inputs (= data of the RHS of formula) below

      # -----------------
      super$check_input() # leave this at the end, it runs background checks
    }
  )
)
