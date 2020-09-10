# ==========================================================================
# Package: Cognitivemodels
# File: model-ebm.R
# Author: Jana B. Jarecki
# ==========================================================================

# ==========================================================================
# Cognitive Model
# ==========================================================================

#' Exemplar-based Cognitive Models
#' @description
#' `ebm()` fits an exemplar-based model.
#'   * `gcm()` fits a generalized context model (aka. exemplar model) for discrete responses (Medin & Schaffer, 1978; Nosofsky, 1986)
#'   * `ebm_j()` fits an exemplar-based judgment model for continuous responses (Juslin et al., 2003)
#' @name ebm
#' 
#' @import Rcpp
#' @import combinat
#' @useDynLib cognitivemodels, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @importFrom abind abind
#' 
#' @eval .param_formula(c(2,2))
#' @template param-choicerule
#' @param class A [formula][stats::formula], the variable in `data` with the feedback about the true class/category. For example `~ cat`. `NA`s are interpreted as trials without feedback (partial feedback, see details).
#' @param criterion A [formula][stats::formula], the variable in `data` with the feedback about the continous criterion. For example, `~ val` `NA`s are interpreted as trials without feedback (partial feedback, see details).
#' @param similarity (optional) A string, similarity function, currently `"minkowski"` (default) and `"mahalanobis"`.
#' @param mode (optional) A string, the response mode, can be `"discrete"` or `"continuous"`, can be abbreviated. If missing, will be inferred from `criterion`.
#' @eval .param_fix("gcm", dyn_args = c("formula", "class"), which = 4)
#' 
#' @details
#' The model can predict new data - `predict(m, newdata = ...)` - and this is how it works:
#'  * If `newdata`s `criterion` or `class` variable has only `NA`s, the model predicts using the originally supplied `data` as exemplar-memory. Parameters are not re-fit.
#'  * If `newdata`'s' `criterion` or `class` variable has values other than `NA`, the model predicts the first row in `newdata` using the originally-supplied `data` as exemplars in memory, but predictions for subsequent rows of `newdata` use also the criterion values in new data. In other words, exemplar memory is \emph{extended} by exemplars in new data for which a criterion exists. Parameters are not re-fit.
#' 
#' ## Model Parameters
#' The model has the following free parameters, depending on the model specification (see [npar()]). A model with formula `~ x1 + x2` has parameters:
#' * _**`x1, x2`**_ (dynamic names) are attention weights, their names correspond to the right side of `formula`.
#' * _**`lambda`**_ is the sensitivity, larger values make the similarity decrease more steeply with higher distance metric.
#' * _**`r`**_ is the order of the Minkowski distance metric (2 is an Euclidean metric, 1 is a city-block metric).
#' * _**`q`**_ is the shape of the relation between similarity and distance, usually equal to _`r`_.
#' * In `gcm()`: 
#'   * _**`b0, b1`**_ (dynamic names) is the bias towards categories, their names are `b` plus the unique values of `class`. For example `b0` is the bias for class = 0.
#'   *  `r .rd_choicerules()`
#' 
#' ## Partial Feedback
#' Regarding `NA` values in `class` or `criterion`: The model takes `NA` values in the class/criterion variable as trials without feedback, in which a stimulus was shown but no feedback about the class or criterion was given (partial feedback paradigm). The model predicts the class or criterion for such trials without feedback based on the previous exemplar(s) for which feedback was shown. The model ignores the trials without feedback in the prediction of the subsequent trials.
#' 
#' @references
#' {Medin, D. L., & Schaffer, M. M. (1978). Context theory of classification learning. \emph{Psychological Review, 85}, 207-238. \url{http://dx.doi.org/10.1037//0033-295X.85.3.207}}
#' 
#' {Nosofsky, R. M. (1986). Attention, similarity, and the identification-categorization relationship. \emph{Journal of Experimental Psychology: General, 115}, 39-57. \url{http://dx.doi.org/10.1037/0096-3445.115.1.39}}
#' 
#' {Juslin, P., Olsson, H., & Olsson, A.-C. (2003). Exemplar effects in categorization and multiple-cue judgment. \emph{Journal of Experimental Psychology: General, 132}, 133-156. \url{http://dx.doi.org/10.1037/0096-3445.132.1.133}}
#' 
#' @template cm
#' @template param-choicerule
#' 
#' @examples 
#' # Make some fake data
#' D <- data.frame(f1 = c(0,0,1,1,2,2,0,1,2),     # feature 1
#'                 f2 = c(0,1,2,0,1,2,0,1,2),     # feature 2
#'                 cl = c(0,1,0,0,1,0,NA,NA,NA),  # criterion/class
#'                  y = c(0,0,0,1,1,1,0,1,1))     # participant's responses
#' 
#' M <- gcm(y ~ f1+f2, class= ~cl, D, fix="start",
#'          choicerule = "none")                  # GCM, par. fixed to start val.
#' 
#' predict(M)                                     # predict 'pred_f', pr(cl=1 | features, trial)
#' M$predict()                                    # -- (same) --
#' summary(M)                                     # summary
#' anova(M)                                       # anova-like table
#' logLik(M)                                      # Log likelihood
#' M$logLik()                                     # -- (same) --
#' M$MSE()                                        # mean-squared error
#' M$npar()                                       # 7 parameters
#' M$get_par()                                    # parameter values
#' M$coef()                                       # 0 free parameters
#' 
#' 
#' ### Specify models
#' # -------------------------------
#' gcm(y ~ f1 + f2, class = ~cl, D, 
#'     choicerule = "none")                          # GCM (has bias parameter)
#' ebm(y~f1+f2, criterion=~cl, D, mode="discrete",
#'     choicerule = "none")                          # -- (same) --
#' ebm_j(y ~ f1 + f2, criterion = ~cl, D)              # Judgment EBM  (no bias par.)
#' ebm(y~f1+f2, criterion=~cl, D, mode="continuous")   # -- (same) --
#' 
#' 
#' ### Specify parameter estimation
#' # -------------------------------
#' gcm(y~f1+f2, ~cl, D, fix=list(b0=0.5, b1=0.5),
#'      choicerule = "none")                       # fix 'bias' par. to 0.5, fit 5 par
#' gcm(y~f1+f2, ~cl, D, fix=list(f1=0.9,f2=0.1),
#'      choicerule = "none")                       # fix attention 'f1' to 90 %  f1 & fit 5 par
#' gcm(y~f1+f2, ~cl, D, fix=list(q=2, r=2),
#'      choicerule = "none")                      # fix 'q', 'q' to 2 & fit 5 par
#' gcm(y~f1+f2, ~cl, D, fix=list(q=1, r=1),
#'      choicerule = "none")                      # fix 'q', 'r' to 1 & fit 5 par
#' gcm(y~f1+f2, ~cl, D, fix=list(lambda=2),
#'      choicerule = "none")                      # fix 'lambda' to 2 & fit 6 par
#' gcm(y~f1+f2, ~cl, D, fix="start", 
#'     choicerule = "none")                        # fix all par to start val. 
NULL


#' @name ebm
#' @export
gcm <- function(formula, class, data, choicerule, fix = NULL, options = NULL, similarity = "minkowski", ...) {
   .args <- as.list(rlang::call_standardise(match.call())[-1])
   names(.args)[which(names(.args) == "class")] <- "criterion"
   .args[["mode"]] <- "discrete"
   .args[["title"]] <- "GCM"
   return(do.call(what = Ebm$new, args = .args, envir = parent.frame()))
} 


#' @name ebm
#' @export
ebm_j <- function(formula, criterion, data, fix = NULL, options = NULL, similarity = "minkowski", ...) {
   .args <- as.list(rlang::call_standardise(match.call())[-1])
   .args[["mode"]] <- "continuous"
   .args[["title"]] <- "Exemplar-based judgment"
   return(do.call(what = Ebm$new, args = .args, envir = parent.frame()))
}


#' @name ebm
#' @export
mem <- function(formula, criterion, data, choicerule, options = NULL, ...) {
   .args <- as.list(rlang::call_standardise(match.call())[-1])
   weights <- lapply(.rhs_var(formula), function(x) setNames(rep(1/length(x), length(x)), x))
   .args[["fix"]] <- c(list(r = 1L, q = 1L), unlist(weights))
   .args[["mode"]] <- "continuous"
   return(do.call(what = ebm_j, args = .args, envir = parent.frame()))
}

#' @name ebm
#' @export
ebm <- function(formula, criterion, data, mode, fix = NULL, options = NULL, ...) {
   .args <- as.list(rlang::call_standardise(match.call())[-1])
   return(do.call(what = Ebm$new, args = .args, envir = parent.frame()))
}




Ebm <- R6Class('ebm',
  inherit = Cm,
  public = list(
    type = NULL,
    criterion = NULL,
    formulaCriterion = NULL,
    learntrials = NULL,
    parnamesBias = NULL,
    parnamesWeights = NULL,
    similarity = NULL,
    multiplicative = NULL,
    initialize = function(formula, data = NULL, criterion, mode = NULL, fix = NULL, learntrials = NULL, discount = NULL, choicerule = NULL, similarity = c("minkowski", "mahalanobis"), multiplicative = TRUE, options = list(), title = "Exemplar model") {
      if (is.null(data)) data <- data.frame()
      data <- as.data.frame(data)
      self$similarity <- match.arg(similarity)
      self$multiplicative = as.numeric(multiplicative)
      self$formulaCriterion <- .as_rhs(criterion)
      self$learntrials <- if ( is.null(learntrials) ) { seq_len(nrow(data)) } else { learntrials }
      criterion <- private$get_more_input(d=data)
      if (is.null(mode)) {
        mode <- super$infer_mode(criterion)
      }
      parspace <- private$make_parspace(
        formula = formula,
        criterion = criterion,
        mode = mode)
      if (is.null(discount)) {
        discount <- private$infer_discount(discount = discount, criterion = criterion, mode = mode, showmsg = !all(rownames(parspace) %in% names(fix)))
      }
      if (mode == "continuous") {
        options <- c(options, list(fit_measure = "mse"))
      }

      super$initialize(
        formula = formula,
        data = data,
        fix = fix,
        parspace = parspace,
        choicerule = choicerule,
        discount = discount,
        title = paste(title, ifelse(multiplicative, "multiplicative", "additive"), similarity, sep = " - "),
        mode = mode,
        options = c(options, list(solver = "solnp"))
      )
    },
    make_prediction = function(type, input, more_input, isnew = FALSE, s = NULL, ...) {
      self$pred_types <- c("response", "value")
      par <- self$get_par()
      criterion <- more_input
      na <- self$natt[1]
      learnto <- max(self$learntrials)
      firstout <- 1L

      if (isnew == TRUE) {
        firstout <- self$nobs + firstout
        criterion <- c(self$more_input[, , s], c(more_input))
        input <- rbind(self$input[,,s], input)
      }

      b <- if (self$mode == "continuous") {
            NA
          } else {
            as.double(tail(par, -(na + 3))[1:na])
          }
      exemplar_w <- as.numeric(!is.na(criterion)) # exemplar weights
      # Initial prediction until the first feedback is shown
      init <- switch(self$mode,
        continuous = sum(range(criterion, na.rm=TRUE)) / 2,
        discrete = 1/length(unique(criterion[!is.na(criterion)])))
      has_criterion <- !is.na(criterion)
      criterion[is.na(criterion) & cumsum(!is.na(criterion)) > 0] <- 0
      return(ebm_cpp(
            criterion = as.double(criterion),
            features = matrix(input, ncol = na),
            w = as.double(par[seq.int(na)]),
            wf = as.double(exemplar_w),
            lambda = as.double(par["lambda"]),
            r = as.double(par["r"]),
            q = as.double(par["q"]),
            b = b,
            lastLearnTrial = learnto,
            firstOutTrial = firstout,
            init = as.double(init),
            has_criterion = as.double(has_criterion),
            similarity = self$similarity,
            ismultiplicative = self$multiplicative))
    }
  ),
  private = list(
    get_more_input = function(d) {
      return(private$get_input(f = self$formulaCriterion, d = d, na.action = NULL))
    },
    make_prednames = function() {
      sn <- paste0(all.vars(self$formulaCriterion[[2]]))
      if (self$mode == "discrete") {
        sn <- paste0(sn, unique(self$more_input))
      }
      return(sn)
    },
    make_constraints = function() {
      parnames <- private$get_parnames()
      parnames_fix <- private$get_parnames("fix")
      # initialize constraintss with NULL
      con_w <- con_b <- NULL
      if (!all(private$get_parnames("weights") %in% parnames_fix)) {
        con_w <- L_constraint(
          L = as.integer(parnames %in% private$get_parnames("weights")),
          dir = "==",
          rhs = 1L,
          names = parnames)
      }
      if (!all(private$get_parnames("biases") %in% parnames_fix)) {
        con_b <- L_constraint(
          L = as.integer(parnames %in% private$get_parnames("biases")),
          dir = "==",
          rhs = 1L,
          names = parnames)
      }
      return(.combine_constraints(con_w, con_b, super$make_constraints()))
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
      b_par <- NULL
      if (mode == "discrete") {
        b_names <- paste0("b", sort(unique(criterion)))
        nb <- length(b_names)
        b_par <- setNames(lapply(1:nb, function(.) c(0,1,1/nb,1)), b_names)
      }
      # Note:
      # Do not change the order (1. weights, 2. lrq, 3. bias)!
      return(do.call(make_parspace, c(w_par, lrq_par, b_par)))
    },
    get_parnames = function(x = "all") {
      if (x == "weights") {
        return(head(super$get_parnames(), self$natt))
      } else if (x == "biases") {
        return(setdiff(tail(super$get_parnames(), -(self$natt + 3L)), c("tau", "eps")))
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
    make_pargrid = function(offset, nsteps = self$options$fit_control$nsteps) {
      parspace <- self$parspace
      offset <- c(offset, w = .1, b = .1)
      return(make_grid_id_list(
        names = private$get_parnames("free"),
        lb = private$get_lb("free"),
        ub = private$get_ub("free"),
        nsteps = c(list(w = 4, b = 4), r = 3, q = 3, lambda = 4, tau = 4, sigma = 4),
        sumto = list("w" = private$get_parnames("weights"), "b" = private$get_parnames("biases")),
        regular = TRUE,
        offset = offset))
    })
)