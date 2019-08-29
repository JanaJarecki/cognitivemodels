#' Exemplar-based cognitive model
#' 
#' @import Rcpp
#' @import combinat
#' @useDynLib cogscimodels, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @importFrom abind abind
#' @inheritParams Cogscimodel
#' @description Fits an exemplar-based model, which is also known as generalized context model (GCM, Medin & Schaffer, 1978; Nosofsky, 1986). The model predicts the value of a current stimulus from the values of previous stimuli, weighted by the similarity between the current and the previous stimuli's features. This model can be used to fit judgments (continuous) or categorization (discrete) modes.
#' @param formula A formula (e.g., \code{class ~ f1 + f2}, or probably \code{judgment ~ dim1 + dim2 + dim3}) specifying the observed data and the feature variables.
#' @param criterion A formula (e.g., \code{~ true_class} or \code{~ true_value}) specifying the true class or criterion, note the "\code{~}".
#' @param ... other arguments from other functions, currently ignored.
#' @return An exemplar-based model; a model \code{M} can be viewed with \code{summary(M)}, or \code{anova(M)}.
#' @author Jana B. Jarecki, \email{jj@janajarecki.com}
#' @references {Medin, D. L., & Schaffer, M. M. (1978). Context theory of classification learning. Psychological Review, 85, 207-238. doi:10.1037//0033-295X.85.3.207}
#' @references {Nosofsky, R. M. (1986). Attention, similarity, and the identification-categorization relationship. Journal of Experimental Psychology: General, 115, 39-57. doi:10.1037/0096-3445.115.1.39}
#' @references {Juslin, P., Olsson, H., & Olsson, A.-C. (2003). Exemplar effects in categorization and multiple-cue judgment. Journal of Experimental Psychology: General, 132, 133–156. doi:10.1037/0096-3445.132.1.133}
#' @details If \code{type} is missing it will be inferred.
#' @section Parameter Space of the Model:
#' \itemize{
  #' \item{n weights}{n attribute weights (range: 0-1, sum to 1), the parameter names equal the RHS of \code{formula}.}
  #' \item{lambda}{Sensitivity (range: 0.001-10), higher values mean more discriminability within the psychological space.}
  #' \item{r}{Exponent in the decay function (range: 1-2), a value of 1 yields exponential decay, 2 yields Gaussian decay.}
  #' \item{q}{}
  #' \item{m biases}{Bias towards categories (range: 0-1, sums to 1), the parameter names are \code{bx, by} were x,y are the categories}
#' }
#' @examples 
#' No examples yet
#' @export
ebm <- function(formula, data, criterion, fix, mode, ...) {
   .args <- as.list(rlang::call_standardise(match.call())[-1])
   return(do.call(what = Ebm$new, args = .args, envir = parent.frame()))
}


Ebm <- R6Class('ebm',
  inherit = Cogscimodel,
  public = list(
    type = NULL,
    criterion = NULL,
    formulaCriterion = NULL,
    learntrials = NULL,
    parnamesBias = NULL,
    parnamesWeights = NULL,
    initialize = function(formula, data, criterion, mode = NULL, fix = NULL, learntrials = NULL, discount = NULL, choicerule = NULL, options = list()) {

      self$learntrials <- if ( is.null(learntrials) ) { seq_len(nrow(data)) } else { learntrials }
      self$formulaCriterion <- criterion
      self$criterion <- super$get_input(f = criterion, d = data)
      mode <- if (is.null(mode)) { super$infer_mode(self$criterion) } else { mode }
      parspace <- self$make_parspace(
          formula = formula,
          criterion = self$criterion, 
          mode = mode)
      self$infer_discount(
          discount = discount,
          criterion = self$criterion,
          mode = mode,
          showmsg = all(rownames(parspace) %in% names(fix))
        )

      super$initialize(
        formula = formula,
        data = data,
        fix = fix,
        parspace = parspace,
        choicerule = choicerule,
        discount = discount,
        title = paste0('Exemplar-based model'),
        mode = mode,
        options = c(options, list(fit_solver = "solnp"))
      )

      if ( self$npar('free') > 0 ) {
        message('Fitting free parameter ', .brackify(self$get_parnames('free')))
        self$fit()
      }
    },
    predict = function(newdata, learnto = max(self$learntrials), firstout = if (missing(newdata)) { 1 } else { (self$nobs() + 1) } ) {
      input <- self$input
      criterion <- self$criterion
      lastlearntrial <- learnto

      if (!missing(newdata)) {
        input <- abind::abind(input, self$get_input(f = self$formula, d = newdata), along = 1)
        if ( learnto > max(self$learntrials) ) {
          criterion <- abind::abind(criterion, super$get_input(f = self$formulaCriterion, d = newdata), along = 1)
        } else {
          criterion <- abind::abind(criterion, array(0, dim = c(nrow(newdata), dim(criterion)[2:3])), along = 1)
        }
      }

      parameter <- self$get_par()

      .args <- list(
        values = as.double(criterion),
        features = matrix(input, ncol = self$natt()),
        w = as.double(parameter[seq_len(self$natt())]),
        r = as.double(parameter['r']),
        q = as.double(parameter['q']),
        b = if (self$mode == "continuous") {
          NA
          } else {
            as.double(tail(parameter, -(self$natt() + 3)))
          },
        lambda = as.double(parameter['lambda']),
        lastLearnTrial = lastlearntrial,
        firstOutTrial = firstout)
      out <- do.call(ebm_cpp, args = .args, envir = parent.frame())
      out <- round(out, 1e9)
      out <- as.matrix(super$apply_choicerule(out))[,1]

      return(out)
    },
    make_constraints = function() {
      parnames <- self$get_parnames()
      parnames_fix <- self$get_parnames("fix")
      con <- super$make_constraints()

      if (!all(self$get_parnames("weights") %in% parnames_fix)) {
        con_w <- L_constraint(
          L = as.integer(parnames %in% self$get_parnames("weights")),
          dir = "==",
          rhs = 1L,
          names = parnames)
        con <- rbind(con, con_w)
      }
      if (!all(self$get_parnames("biases") %in% parnames_fix)) {
        con_b <- L_constraint(
          L = as.integer(parnames %in% self$get_parnames("biases")),
          dir = "==",
          rhs = 1L,
          names = parnames)
        con <- rbind(con, con_b)
      }
      return(con)
    },
    make_parspace = function(formula, criterion, mode) {
      # parameter that are NOT dynamic
      lrq_par <- list(
        lambda = c(0.001, 10, 0.5, 1),
         r = c(1, 2, 1.5, 1),
         q = c(1, 2, 1.5, 1))
      # dynamic weight parameter names
      w_names <- attr(terms(as.Formula(formula)), "term.labels")
      nw <- length(w_names)
      w_par <- setNames(lapply(1:nw, function(.) c(0.001,1,1/nw,1)), w_names)
      # dynamic bias parameter names
      if (mode == "discrete") {
        b_names <- paste0("b", sort(unique(criterion)))
        nb <- length(b_names)
        b_par <- setNames(lapply(1:nb, function(.) c(0.001,1,1/nb,1)), b_names)
      }
      # Note:
      # Do not change the order (1. weights, 2. lrq, 3. bias)!
      return(do.call(make_parspace, c(w_par, lrq_par, b_par)))
    },
    get_parnames = function(x = "all") {
      if (x == "weights") {
        return(head(self$get_parnames(), self$natt()))
      } else if (x == "biases") {
        return(tail(self$get_parnames(), -(self$natt() + 3L)))
      } else {
        return(super$get_parnames(x))
      }
    },
    infer_discount = function(discount, criterion, mode, showmsg) {
      if ( length(discount) == 0 ) {
        return(discount)
      }  else if (mode != "continuous") {
        discount <- which(!duplicated(criterion))[2] + 1
        if (showmsg == TRUE) {
          message('Parameter estimates improve if first few trials are ignored. Discounting trial 1 to ', discount, '.\nTo avoid, set "discount=0"')
        }
        return(discount)
      }    
    },
    parGrid = function(offset) {
      parspace <- self$parspace
      offset <- c(offset, w = .1, b = .1)
      return(MakeGridList(
        names = self$freenames,
        ll = setNames(parspace[, 'll'], rownames(parspace)),
        ul = setNames(parspace[, 'ul'], rownames(parspace)),
        nsteps = list(w = 4, b = 4, r = 3, q = 3, lambda = 4, tau = 4, sigma = 4),
        sumto = list('w' = self$parnamesWeights, 'b' = self$parnamesBias),
        regular = TRUE,
        offset = offset))
    },
    eqfun = function() {
      allowed <- rownames(self$parspace)
      free_weights <- any(self$parnamesWeights %in% self$get_parnames('free'))
      free_bias <- any(self$parnamesBias %in% self$get_parnames('free'))
      return(super$makeSumConstraints(list(free_weights, free_bias), c(1,1)))
      # weight_eqfun <- function(pars, self) {
      #   return(sum(pars[self$parnamesWeights]))
      # }
      # bias_eqfun <- function(pars, self) {
      #   return(sum(pars[self$parnamesBias]))
      # }
      # both_eqfun <- function(pars, self) {
      #   return(c(sum(pars[self$parnamesWeights]), sum(pars[self$parnamesBias])))
      # }
      # if ( free_weights & !free_bias ) {
      #   return( list(eqfun = weight_eqfun, eqb = 1) )
      # } else if ( !free_weights & free_bias ) {
      #   return( list(eqfun = bias_eqfun, eqb = 1) )
      # } else if ( free_weights & free_bias ) {
      #   return( list(eqfun = both_eqfun, eqb = c(1,1)) )
      # } else {
      #   return(NULL)
      # }
    },    
    print = function(digits = 2) {
      super$print(digits = digits)
    })
)

