# ==========================================================================
# Package: Cognitivemodels
# File: utils-formula.R
# Author: Jana B. Jarecki
# ==========================================================================

# ==========================================================================
# Cognitive Model
# ==========================================================================

#' Exemplar-based Cognitive Models
#' 
#' \code{ebm()} fits exemplar-based models, \code{gcm()} fits a generalized context model (Medin & Schaffer, 1978; Nosofsky, 1986), \code{ebmcj()} fits an exemplar-based judgment model (Juslin et al., 2003).
#' 
#' @rdname ebm
#' 
#' @import Rcpp
#' @import combinat
#' @useDynLib cognitivemodels, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @importFrom abind abind
#' 
#' @inheritParams Cm
#' @param formula A formula specifying  participants' responses ~ stimulus 
#'    features (e.g., \code{y ~ f1 + f2}).
#' @param criterion A formula specifying the class, criterion, or feedback (e.g., \code{~ class}).
#' @param ... other arguments from other functions, currently ignored.
#' @return An exemplar-based model; a model \code{M} can be viewed with
#'   \code{summary(M)}, or \code{anova(M)}.
#' 
#' @references {Medin, D. L., & Schaffer, M. M. (1978). Context theory of classification learning. \emph{Psychological Review, 85}, 207-238. \url{http://dx.doi.org/10.1037//0033-295X.85.3.207}}
#' @references {Nosofsky, R. M. (1986). Attention, similarity, and the identification-categorization relationship. \emph{Journal of Experimental Psychology: General, 115}, 39-57. \url{http://dx.doi.org/10.1037/0096-3445.115.1.39}}
#' @references {Juslin, P., Olsson, H., & Olsson, A.-C. (2003). Exemplar effects in categorization and multiple-cue judgment. \emph{Journal of Experimental Psychology: General, 132}, 133-156. \url{http://dx.doi.org/10.1037/0096-3445.132.1.133}}
#' 
#' @details
#' If \code{mode} is missing it will be inferred from the RHS of \code{criterion} or \code{class}.
#' 
#' The model can predict new data (\code{predict(M, newdata = ....)}), and this is how it works:
#' \itemize{
#'  \item{If \code{newdata}'s \code{criterion} has only \code{NA}s, the model predicts using the old data (the originally-supplied \code{data} argument) as exemplar-memory. Parameters are not re-fit.}
#'  \item{If \code{newdata}'s' \code{criterion} has also non-\code{NA}s, the model predicts the first new data row using the old data, but predictions for subsequent new data use also the criterion in new data. In other words, exemplar memory is \emph{extended} by exemplars in new data for which a criterion exists. Parameters are not re-fit.}
#' }
#' 
#' @section Parameter Space:
  #'  \tabular{lrcllr}{\verb{   }
  #'  \strong{Name} \tab \verb{   }\strong{LB} \tab  \strong{-} \tab \strong{UB}\verb{   } \tab \strong{Description} \tab \strong{Start Value} \cr\verb{   }
  #' \code{f1, f2} \tab 0 \tab-\tab 1 \tab Attention weights, sum to 1. Note: parameter names are equal to the LHS of \code{formula} \tab 1 / n features \cr\verb{   }
  #' \code{lambda} \tab 0.001 \tab-\tab 10 \tab Sensitivity, higher values increase the discriminability in the psychological space \tab 0.5 \cr\verb{   }
  #' \code{q} \tab 0 \tab-\tab 2 \tab Exponent in the distance metric, 1 yields city-block, 2 yields Euclidean metric \tab 1.5 \cr\verb{   }
  #' \code{r} \tab 0 \tab-\tab 2 \tab Exponent in the decay functio, 1 yieldes = exponential decay, 2 yields gaussian decay \tab 1.5
  #'}
#' \verb{   }where \emph{LB, UB} = inclusive bounds and \emph{Start Value} = starting value for fitting.
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
#' gcm(y~f1+f2, ~cl, D, fix="start", 
#'     choicerule = "none")                        # fix all par to start val. 
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


#' @export
gcm <- function(formula, class, data, fix = NULL, options = NULL, ...) {
   .args <- as.list(rlang::call_standardise(match.call())[-1])
   names(.args)[which(names(.args) == "class")] <- "criterion" # rename class to criterion
   .args[["mode"]] <- "discrete"
   .args[["title"]] <- "GCM"
   return(do.call(what = Ebm$new, args = .args, envir = parent.frame()))
} 


#' Exemplar-based Cognitive Models 
#' 
#' @inheritParams ebm
#' @rdname ebm
#' 
#' @details \code{ebm_j()} calls \link{ebm} with \code{mode = "continuous"}.
#' 
#' @examples
#' ebm_j(y ~ f1 + f2, D)
#' 
#' @export
ebm_j <- function(formula, criterion, data, fix = NULL, options = NULL, ...) {
   .args <- as.list(rlang::call_standardise(match.call())[-1])
   .args[["mode"]] <- "continuous"
   .args[["title"]] <- "Exemplar-based judgment"
   return(do.call(what = Ebm$new, args = .args, envir = parent.frame()))
}

#' Exemplar-based Cognitive Models 
#' 
#' @inheritParams ebm
#' @rdname ebm
#' 
#' @details \code{mem()} calls \link{ebm_j} with weights fixed to be equal and \code{r=2, p=2}.
#' 
#' @examples
#' mem(y ~ f1 + f2, D)
#' 
#' @export
mem <- function(formula, criterion, data, options = NULL, ...) {
   .args <- as.list(rlang::call_standardise(match.call())[-1])
   weights <- lapply(.rhs_var(formula), function(x) setNames(rep(1/length(x), length(x)), x))
   .args[["fix"]] <- c(list(r = 1L, q = 1L), unlist(weights))
   .args[["mode"]] <- "continuous"
   return(do.call(what = ebm_j, args = .args, envir = parent.frame()))
}

#' Exemplar-based Cognitive Models 
#' 
#' @inheritParams ebm
#' @rdname ebm
#' 
#' @param class A formula specifying the class feedback (e.g., \code{~ cl}), note the "\code{~}". \code{NA} values are retained, assuming a partial feedback paradigm.
#' 
#' @details \code{gcm()} \link{ebm} with \code{mode = "discrete"}.
#' 
#' @section Parameter Space:
#' The \code{gcm()} and \code{ebm(mode = "discrete")} have as many response bias parameters as there are category labels. For example if the categories are 0 or 1:
  #' \verb{   }\tabular{lrcllr}{\verb{   }
  #'  \strong{Name} \tab \verb{   }\strong{LB} \tab  \strong{-} \tab \strong{UB}\verb{   } \tab \strong{Description} \tab \strong{Start Value} \cr\verb{   }
  #' \code{b0, b1} \tab 0 \tab-\tab 1 \tab Response bias towards each category, sum to 1. Note: names are \code{b}+unique \code{class} labels.\tab 1 / n classes
  #' }
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
    initialize = function(formula, data = NULL, criterion, mode = NULL, fix = NULL, learntrials = NULL, discount = NULL, choicerule = NULL, options = list(), title = "Exemplar-based Model") {
      if (is.null(data)) data <- data.frame()
      data <- as.data.frame(data)
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
        title = title,
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
            has_criterion = as.double(has_criterion)))
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
        return(tail(super$get_parnames(), -(self$natt + 3L)))
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