#' Exemplar-based cognitive model
#' 
#' @useDynLib cogscimodels, .registration = TRUE
#' @import rje
#' @description Fits a Bayesian cognitive learning model. According to this model, agents update beliefs about probabilities of events in a Bayesian way given a series of observed events and prior beliefs. The model can make predictions from raw observations or relative frequencies.
#' @param formula A formula specifying the variable names of response and observed events, e.g. \code{reponse ~ eventA + eventB + eventC}. Use a pipe (\code{|}) to indicate learning about more than one options, e.g. \code{reponse ~ option1_eventA + option1_eventB | option2_eventX + option2_eventY}.
#' @param data A data.frame or matrix containing the variables in \code{formula}; ensure it is ordered in the order in which data was seen.
#' @param format A string (default \code{"raw"}), specifying if data are binary indicators for observed/not observed (1=event seen, 0=event not seen) or cummulative counts of how often an outcome was observed. Allowed are \code{"raw", "count"}.
#' @param fixed (optional) specifies fixed parameter, to set priors for eventA and eventB use \code{fixed=list(prior.eventA=1, prior.eventB=4)}.
#' @param choicerule (optional) A string specifying the choice rule, allowed values see the \code{priordist} argument of \link{choicerule}. *Required* if \code{response = "discrete"} and model learns > 1 options.
#' @param response (optional, default \code{"continuous"}) A string specifying if beliefs are predicted (\code{"continuous"}) or if choices are predicted (\code{"discrete"}).
#' @param discount (optional, default 0) A number or numeric vector of trial indices to discount.
#' @param ... other arguments from other functions, currently ignored.
#' @return An model object (similar to lm-objects) of class "bayeslearn". Itcan be viewed with \code{summary(mod)}, where \code{mod} is the name of the model object.
#' @examples 
#' #  No examples yet
#' @author Jana B. Jarecki, \email{jj@janajarecki.com}, Markus Steiner
#' @references {Griffiths, T. L., & Yuille, A. (2008). Technical Introduction: A primer on probabilistic inference. In N. Chater & M. Oaksford (Eds.), The Probabilistic Mind: Prospects for Bayesian Cognitive Science (pp. 1–2). Oxford University Press. https://doi.org/10.1093/acprof:oso/9780199216093.003.0002}

#' @export
bayeslearn <- function(formula, data, format = c('raw', 'count'), fixed = list(), choicerule = NULL, response = c('continuous', 'discrete'), discount = 0, ...) {
  args <- c(list(formula = formula, data = data, format = format, fixed = fixed, choicerule = choicerule, response = match.arg(response), discount = discount), list(...))
  obj <- do.call(Bayeslearn$new, args)
  # check(mod) #todo
  if (length(obj$getparm('free')) > 0) {
    message('Fitting free parameter ', .brackify(obj$freenames))
    obj$fit(c('grid', 'solnp'))
  }
  return(obj)
}

Bayeslearn <- R6Class('bayeslearn',
  inherit = Cogscimodel,
  public = list(
    priordist = NULL,
    format = NULL,
    priornames = NULL,
    initialize = function(formula, data, format = c('raw', 'count'), fixed, choicerule, response, discount, ...) {
      format = match.arg(format)
      self$format <- format
      formula <- self$correct_single_rhs(formula)
      self$priornames <- paste0('prior.', attr(terms(formula(formula, lhs=0, rhs=1)), 'term.labels'))

      allowedparm <- matrix(cbind(0.01, 1.99, rep(1, length(self$priornames)), 1), nc = 4, dimnames = list(self$priornames, c('ll', 'ul', 'init', 'na')))
      super$initialize(formula = formula,
        data = data,
        allowedparm = allowedparm,
        choicerule = choicerule,
        response = response,
        model = paste0('Bayesian Learning'),
        fixed = fixed,
        discount = discount,
        ...)
      self$priordist <- self$inferpriordist()
      self$model <- paste0(self$model, ' [', self$priordist, ']')
    },
    getinput = function(f = self$formula, d) {
      input <- super$getinput(f, d)
      input <- self$formatinput(input)
      if (self$format == 'raw') {
        input <- self$count_raw_data(input)
      }
      return(input)
    },
    setpriors = function(x) {
      x <- unlist(x)
      priors <- self$priornames
      nx <- length(x)
      npriors <- length(priors)
      if (nx == 1) {
        x <- rep(x, npriors)
        names(x) <- priors
      } else if (nx != npriors) {
        stop('Fixed parameters "priors.xx" must be ', npriors, ' long, but has ', nx, ' values ', .brackify(x), '. Use "fixed=list(', paste0(priors, '=1', collapse=', '), ')" to set a uniform prior.')
      }
      return(x)
    },
    predict = function(type = c('pmean', 'map', 'draws', 'hyperparm'), newdata, ndraws = 1000) {
      input <- self$input
      d <- dim(input)
      dn <- dimnames(input)
      posterior <- self$update(
        prior = unlist(self$parm[self$priornames]),
        data = input)
      type <- match.arg(type)
      out <- if (type == 'draws') {
              lapply(1:d[1], function(t) { self$rdraw(t, rpar = posterior[t,,,drop=FALSE], draws = ndraws) })
              } else if (type == 'map') {
                array(apply(posterior, 3, self$map), d, dn)
              } else if (type == 'pmean') {
                array(apply(posterior, 3, self$pmean), d, dn)
              } else if (type == 'hyperparm') {
                array(posterior, d, dn)
              }
      
      if (!is.list(out)) {
        if (d[3] == 1) {
          return(out[,,1])
        }
      }
      return(out)   
    },
    update = function(prior, data, dist = self$priordist) {
      posterior <- data
      for (o in 1:dim(data)[3]) {
        for (x in 1:dim(data)[2]) {
            if (dist == 'binomial' | dist == 'multinomial') {
              posterior[,x,o] <- prior[x] + data[,x,o]
          }
        }       
      }
      return(posterior)
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
    map = function(posterior) {
      t <- self$priordist
      if (t == 'binomial' | t == 'multinomial') {
        (posterior - 1) / rowSums(posterior - 1)
      }
    },
    pmean = function(posterior) {
      t <- self$priordist
      if (t == 'binomial' | t == 'multinomial') {
        posterior/rowSums(posterior)
      }
    },
    formatinput = function(x) {
      if (dim(x)[2] == 1) {
        tmp <- x
        x <- array(NA, dim(x) + c(0,1,0))
        x[,-1,] <- tmp
        x[,1,] <- if (self$format == 'raw') {
          1 - tmp[,1,]
        } else {
          if (!all(x[1,1,] %in% c(0,1))) {
            stop('The RHS of "formula" must contain 2 or more counts of events, like "y ~ n0 + n1" or for multiple options "y ~ n0 + n1 | n0_2 + n1_2". But your formula=', as.character(self$formula), '.\nPlease check if you forgot to add a RHS-variable with counts of observations to "formula". You said format="count", this means your data are cummulative counts -- check if the data really contains "counts". Maybe you want format="raw"?')
          }
          1:dim(x)[1] - tmp[,1,]
        }
        
      }
      return(x)
    },
    inferpriordist = function(x = self$input) {
      return(ifelse(dim(x)[2] <= 2, 'binomial', 'multinomial'))
    },
    correct_single_rhs = function(formula) {
      f <- as.Formula(formula)
      rhslist <- lapply(1:length(f)[2], function(i) attr(terms(formula(f, lhs=0, rhs=i)), 'term.labels'))
      for (i in seq_along(rhslist)) {
        if (length(rhslist[[i]]) == 1) {
          if (self$format == 'count') {
           stop('Right-hand side of "formula" needs > 1 variables per option, but has ~', v, '. Add the variable counting the second event to the RHS of "formula", e.g. y ~ count1a + count1b | count2a + count2b.')
          } else {
            rhslist[[i]] <- paste(rhslist[[i]], '+ I(1 - ', rhslist[[i]], ')')
          }
        } else {
          rhslist[[i]] <- paste(rhslist[[i]], collapse = ' + ')
        }
      }
      new <- as.formula(paste('~', paste(rhslist, collapse = ' | ')))
      return(update(f, new))
    },
    count_raw_data = function(data) {
      if (any(!(unlist(data) %in% c(0, 1)))) {
        stop('raw data must only contain 0 or 1, but contains ',
          cogscimodels:::.brackify(data[which(!(data %in% c(0, 1)))]))
      }
      counts <- apply(data, 3,
        function(p) {
          apply(p, 2, cumsum)
        }
      )
      counts <- array(counts, dim(data), dimnames = dimnames(data))
      return(counts)
    },
    eqfun = function() {
      if (any(self$priornames %in% self$freenames)) {
        eqfun <- function(pars, self) {
          return(sum(pars[self$priornames]))
        }
        b <- max(rowSums(self$allowedparm[self$priornames, c('ul', 'll')]))
        return(list(eqfun = eqfun, eqb = b))
      } else {
        return(NULL)
      }
    }
  )
)
