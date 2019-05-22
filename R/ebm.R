#' Exemplar-based cognitive model
#' 
#' @import Rcpp
#' @import combinat
#' @useDynLib cogscimodels, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @description Fits an exemplar-based model (also known as generalized context model, GCM, Medin & Schaffer, 1978; Nosofsky, 1986). The model predicts the value of a current stimulus from the values of previous stimuli, weighted by the similarity between the current and the previous stimuli's features. This model can be used to fit judgments (1,2,...,100) or categorization (0,1) responses.
#' @param formula a formula specifying the variables (\code{reponse ~ feature1 + feature2 + ... | criterion}), which needs a pipe (\code{|}) before the criterion variable. Examples:
#' \itemize{
#'   \item Categorization model: \code{category ~ f1 + f2 + f3 | true_class}
#'   \item Judgment model: \code{judgment ~ f1 + f2 + f3 | value}
#'   \item Memory-based preference model: \code{purchased ~ f1 + f2 + f3 | value | price} (see details)
#' }
#' @param data a data.frame or matrix containing the variables in \code{formula}; make sure it is ordered in the order in which respondents saw the stimuli.
#' @param type (optional) String specifying the type of model: \code{"categorization", "judgment", "valuebasedchoice"}, will be inferred if omitted.
#' @return An exemplar-based model; it can be viewed with \code{summary(mod)}, where \code{mod} is the name of the model object.
#' @examples 
#' #  No examples yet
#' @author Jana B. Jarecki, \email{jj@janajarecki.com}
#' @references {Medin, D. L., & Schaffer, M. M. (1978). Context theory of classification learning. Psychological Review, 85, 207-238. doi:10.1037//0033-295X.85.3.207}
#' @references {Nosofsky, R. M. (1986). Attention, similarity, and the identification-categorization relationship. Journal of Experimental Psychology: General, 115, 39-57. doi:10.1037/0096-3445.115.1.39}
#' @references {Juslin, P., Olsson, H., & Olsson, A.-C. (2003). Exemplar effects in categorization and multiple-cue judgment. Journal of Experimental Psychology: General, 132, 133–156. doi:10.1037/0096-3445.132.1.133}
#' @details You may add a 'cost' term to the formula separated by a second pipe:  \code{reponse ~ feature1 + feature2 + ... | criterion | cost}. The model then predicts a value, compares this value to cost, and if the value is greater than the cost returns \code{1}, otherwise \code{0} (chose the stimulus, reject the stimulus). This is implemented with a softmax choice rule.
#' @export
ebm <- function(formula, data, type, ...) {
   args <- c(list(formula = formula, data = data, type = type), list(...))
   obj <- do.call(Ebm$new, args)
   # check(mod) #todo
   if (length(obj$freenames) > 0) {
      message('Fitting free parameter', .brackify(obj$freenames))
      obj$fit(c('grid', 'solnp'))
   }
   return(obj)
}


Ebm <- R6Class('ebm',
  inherit = Cogscimodel,
  public = list(
    type = NULL,
    criterion = NULL,
    cost = NULL,
    learntrials = NULL,
    ndim = NULL,
    biasnames = NULL,
    weightnames = NULL,
    initialize = function(formula, data, type, fixed = NULL, learntrials = seq_len(nrow(data)), initialparm = NULL, discount = NULL, choicerule = NULL, fit.options = list()) {
      if ( !any(grepl('\\|', formula)) ) {
        stop('"formula" in ebm misses a pipe ("|") before the criterion variable,\n\te.g., formula = y ~ x1 + x2 | cr.', call. = FALSE)
      }
      f <- as.Formula(formula)
      d <- data.frame(data)
      self$learntrials <- learntrials
      self$criterion <- get_all_vars(formula(f, lhs=0, rhs=2), d)[,1]
      if ( length(f)[2] > 2) {
        self$cost <- get_all_vars(formula(f, lhs=0, rhs=3), d)[,1]
      }
      d <- get_all_vars(formula(f, lhs=1, rhs=1), d) 
      self$weightnames <- attr(terms(formula(f, lhs=0, rhs=1)), 'term.labels')
      self$ndim <- length(self$weightnames)
      allowedparm <- matrix(c(rep(c(0.001, 1, 1/self$ndim, 1), self$ndim),
                        lambda = c(0.001, 10, 0.5, 1),
                        r = c(1, 2, 1.5, 1),
                        q = c(1, 2, 1.5, 1)
                        ),
        ncol = 4, byrow = TRUE, dimnames= list(c(self$weightnames,'lambda','r','q'), c('ll','ul','init','na')))
      if ( missing(type) ) {
        self$inferType(d[, 1])
      } else {
        self$type <- match.arg(type, c('judgment', 'choice', 'valuebasedchoice'))
      }
      if (self$type == 'valuebasedchoice' & is.null(choicerule)) {
        choicerule <- 'softmax'
      }
      if ( self$type == "choice" ) {
        labels <- if (self$type == 'choice') {
          sort(unique(self$criterion))
          }
        biasparm <- matrix(rep(c(0, 1, 1/length(labels), 1), length(labels)), nc = 4, byrow =TRUE, dimnames = list(paste0('b', labels)))
        self$biasnames <- rownames(biasparm)
        allowedparm <- rbind(allowedparm, biasparm)
      }
      discount <- self$inferdiscount(self$criterion, discount, showmsg = all(names(allowedparm) %in% names(fixed)))
      super$initialize(formula = formula, data = d, fixed = fixed, allowedparm = allowedparm, choicerule = choicerule, discount = discount, model = paste0('Exemplar-based model [type: ', self$type, ']'), response = ifelse(self$type == 'judgment', 'continuous', 'discrete'), fit.options = fit.options)
      self$formula <- formula
      if (!is.null(fit.options$newdata)) {
        self$fit.options$newdata <- get_all_vars(self$formula, fit.options$newdata)
      }

      self$allowedparm[names(initialparm), ] <- initialparm #todo: move this into class cognitivemodel
    },
    inferType = function(response) {
      self$type <- if ( !is.null(self$cost) ) {
        'valuebasedchoice'
      } else if ( !is.factor(response) & ( any(floor(response) != response) | any(floor(self$criterion) != self$criterion) | length(unique(response)) > 2 | length(unique(self$criterion)) > 2 ) ) {
        'judgment'
      } else {
        'choice'
      }
    },
    inferdiscount = function(criterion, discount, showmsg) {
      if ( !is.null(discount) ) {
        return(discount)
      }  else if (self$type != 'judgment') {
        discount <- which(!duplicated(criterion))[2] + 1
        if ( showmsg ) {
          message('Parameter estimates improve if first few trials are ignored. Discounting trial 1 to ', discount, '.\nTo avoid, set "discount=0"')
        }
        return(discount)
      }    
    },
    settype = function(x) {
      x <- match.arg(x, c('judgment', 'choice', 'valuebasedchoice'))
      self$type <- x
      self$pred <- NULL
    },
    parGrid = function(offset) {
      return(MakeGridList(
        names = self$freenames,
        ll = self$allowedparm[, 'll'] + offset,
        ul = self$allowedparm[, 'ul'] - offset,
        nsteps = list(w = 4, b = 4, r = 3, q = 3, lambda = 4, tau = 4, sigma = 4),
        sumto = list('w' = self$weightnames, 'b' = self$biasnames),
        regular = TRUE,
        offset = offset))
    },
    eqfun = function() {
      allowed <- rownames(self$allowedparm)
      free_weights <- any(self$weightnames %in% self$freenames)
      free_bias <- any(self$biasnames %in% self$freenames)
      weight_eqfun <- function(pars, self) {
        return(sum(pars[self$weightnames]))
      }
      bias_eqfun <- function(pars, self) {
        return(sum(pars[self$biasnames]))
      }
      both_eqfun <- function(pars, self) {
        return(c(sum(pars[self$weightnames]), sum(pars[self$biasnames])))
      }
      if ( free_weights & !free_bias ) {
        return( list(eqfun = weight_eqfun, eqb = 1) )
      } else if ( !free_weights & free_bias ) {
        return( list(eqfun = bias_eqfun, eqb = 1) )
      } else if ( free_weights & free_bias ) {
        return( list(eqfun = both_eqfun, eqb = c(1,1)) )
      } else {
        return(NULL)
      }
    },
    getinput = function(f = self$formula, d) {
      f <- delete.response(terms(as.Formula(f)))
      f <- update(f, new = drop.terms(f, length(attr(f, 'term.labels'))))
      f <- if (self$type == 'choice') {
        return(super$getinput(f, d))
      } else {
        f <- update(f, new = drop.terms(f, length(attr(f, 'term.labels'))))
        return(super$getinput(f, d))
      }
    },
    predict = function(newdata, learnto = max(self$learntrials), firstout = if (missing(newdata)) { 1 } else { (nrow(self$input) + 1) } ) {
      if ( missing(newdata) & !is.null(self$pred) ) {
        return(self$pred)
      }
      parameter <- self$parm
      features <- self$input
      criterion <- self$criterion
      cost <- self$cost
      lastlearntrial <- learnto

      if ( !missing(newdata) ) {
        d <- as.data.frame(newdata)
        f <- as.Formula(self$formula)
        if ( learnto > max(self$learntrials) ) {
          criterion <- c(criterion, get_all_vars(formula(f, lhs=0, rhs=2), d)[,1])
        } else {
          criterion <- c(criterion, rep(0, nrow(newdata)))
        }
        if ( length(f)[2] > 2 ) {
          cost <- c(cost, get_all_vars(formula(f, lhs=0, rhs=3), d)[,1])
          if (firstout > 1) {
            cost <- tail(cost, -(firstout-1))
          }
        }
        features <- abind(features, self$getinput(self$formula, newdata), along = 1)
      }
      parameter <- ifelse(is.na(parameter), self$allowedparm[names(parameter), 'na'], parameter)
      parameter <- unlist(parameter)
      .a <- list(
        values = as.double(criterion),
        features = matrix(features, ncol = dim(features)[2]),
        w = as.double(parameter[self$weightnames]),
        r = as.double(parameter['r']),
        q = as.double(parameter['q']),
        b = if (self$type != 'choice' ) { NA } else { as.double(parameter[self$biasnames]) },
        lambda = as.double(parameter['lambda']),
        lastLearnTrial = lastlearntrial,
        firstOutTrial = firstout)
      out <- do.call(ebm_cpp, .a)

      if (self$type == 'valuebasedchoice') {
        out <- cbind(out,cost) #todo JJ: change to handle > 1 option
      }
      out <- as.matrix(super$applychoicerule(out))[,1]
      if ( missing(newdata) ) {
        self$pred <- out
      }
      return(out)
    },
    print = function(digits = 2) {
      super$print(digits = digits)
    })
)