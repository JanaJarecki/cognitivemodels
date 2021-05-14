# ==========================================================================
# Package: Cognitivemodels
# File: model-cpt.R
# Author: Jana B. Jarecki
# ==========================================================================

# ==========================================================================
# Cognitive Model
# ==========================================================================


#' Cumulative Prospect Theory Models
#' @name cpt
#' @description 
#' Fits cumulative prospect theory, CPT (Tversky & Kahneman, 1992).
#' * `cpt_d()` fits CPT for discrete responses = choices.
#' * `cpt_c()` fits CPT for continuous responses = utility values.
#' * `cpt_mem_d()` fits CPT with an editing step based on memory for discrete responses = choices (Thaler & Johnson, 1990).
#' * `cpt_mem_c()` fits CPT with an editing step based on memory for continuous responses = utility values (Thaler & Johnson, 1990).
#' 
#' @eval .param_formula(c(4,4), risky = TRUE)
#' @eval .param_fix("cpt_d")
#' 
#' @importFrom stringr str_extract
#' @importFrom abind abind
#' @importFrom data.table %between%
#' 
#' @eval .param_formula(c(4,4), risky = TRUE)
#' @param ref (optional, default: 0) A number, string, or RHS [formula][stats::formula], the reference point or the variable in `data` that holds the reference point. For example `~ ref`.
#' @param weighting (optional) A string, name of the probability weighting function, allowed are `"KT1992"` for the weighting in Kahneman & Tversky (1992) and `NA` for no weighting.
#' @param value (optional) A string, the name of the value function. Allowed is only `"KT1992"` for the value function in Kahneman & Tversky (1992) and `NA` for no value transformation.
#' @param mem (optional, default: 0) A number, string, or RHS [formula][stats::formula()], the prior gains or losses in memory. Formula and string refer to variables in `data`, for example `~ xoutc`.
#' @param editing (optional) A string, the editing rule to use (see Thaler & Johnson, 1999, pp. 645), currently only `"hedonic"`.
#' 
#' @references Tversky, A., & Kahneman, D. (1992). Advances in prospect theory: cumulative representation of uncertainty. Journal of Risk and Uncertainty, 5, 297–-323. doi:[10.1007/BF00122574](https://doi.org/10.1007/BF00122574)
#' 
#' Thaler, R. H., & Johnson, E. J. (1990). Gambling with the House Money and Trying to Break Even: The Effects of Prior Outcomes on Risky Choice. Management Science, 36(6), 643--660. doi:[10.1287/mnsc.36.6.643](https://doi.org/10.1287/mnsc.36.6.643)
#' 
#' @details
#' Fits cumulative prospect theory.
#' 
#' ## Parameter Space
#' The model has the following free parameters:
#'  * _**`alpha`**_ the utility exponent for positive outcomes.
#'  * _**`beta`**_ the utility exponent for negative outcomes.
#'  * _**`gammap`**_ the probability distortion for positive outcomes.
#'  * _**`gamman`**_ the utility exponent for negative outcomes.
#'  * _**`lambda`**_ the loss aversion.
#'  * In `cpt_d()` and `cpt_mem_d()`: `r .rd_choicerules()`.
#' 
#' @template cm
#' @template param-choicerule
#' 
#' @examples
#' ## From Tversky, A., & Kahneman, D. (1992).
# p. 313
#' dt <- data.frame(
#'   x1 = c(100, -100),
#'   px = 1,
#'   x2 = 0,
#'   y1 = c(200, -200),
#'   py = c(.71,.64),
#'   y2 = 0,
#'   rp = 1)
#' 
#' # Make the model -------------------------------------------
#' # add fix parameters (don't fit)
#' # using the Parameter from the paper
#' 
#' # Discrete responses with choicerule
#' M <- cpt_d(rp ~ x1 + px + x2 | y1 + py + y2, ref = 0,
#'          choicerule = "softmax", data = dt,
#'          fix = list(alpha = 0.88, beta = 0.88, lambda = 2.25,
#'          gammap = 0.61, gamman = 0.69, tau = 1))
#' # View the model
#' M        # has a parameter `tau`
#' 
#' # Continuous responses/utility
#' M <- cpt_c(rp ~ x1 + px + x2 | y1 + py + y2, ref = 0,
#'          data = dt,
#'          fix = list(alpha = 0.88, beta = 0.88, lambda = 2.25,
#'          gammap = 0.61, gamman = 0.69))
#' # View the model
#' M        # No parameter `tau`
#' 
#' # Methods ---------------------------------------------------
#' predict(M, "value") # predict values, also: M$predict("value")
#' predict(M, "mode") # predict choice probability after softmax
#' summary(M)
#' anova(M)
NULL


#' @name cpt
#' @export
cpt_d <- function(formula, data, choicerule, ref = 0L, fix = list(), weighting = c("TK1992"), value = c("TK1992"), options = NULL) {
  .args <- as.list(rlang::call_standardise(match.call())[-1])
  .args["editing"] <- "none"
  .args["mode"] <- "discrete"
  return(do.call(what = Cpt$new, args = .args, envir = parent.frame()))
}

#' @name cpt
#' @export
cpt_c <- function(formula, data, ref = 0L, fix = list(), weighting = c("TK1992"), value = c("TK1992"), options = NULL) {
  .args <- as.list(rlang::call_standardise(match.call())[-1])
  .args["editing"] <- "none"
  .args["mode"] <- "continuous"
  return(do.call(what = Cpt$new, args = .args, envir = parent.frame()))
}

#' @name cpt
#' @export
cpt_mem_d <- function(formula, mem, data, fix = list(), choicerule, editing = "hedonic",  weighting = c("TK1992"), value = c("TK1992"), options = NULL) {
  #warning("Model is under active development an not tested! Don't use.")
  .args <- as.list(rlang::call_standardise(match.call())[-1])
  .args["mode"] <- "discrete"
  return(do.call(what = Cpt$new, args = .args, envir = parent.frame()))
}

#' @name cpt
#' @export
cpt_mem_c <- function(formula, mem, data, fix = list(), editing = "hedonic",  weighting = c("TK1992"), value = c("TK1992"), options = NULL) {
  #warning("Model is under active development an not tested! Don't use.")
  .args <- as.list(rlang::call_standardise(match.call())[-1])
  .args["mode"] <- "continuous"
  return(do.call(what = Cpt$new, args = .args, envir = parent.frame()))
}

Cpt <- R6Class("cpt",
  inherit = Cm,
  public = list(
    formulaRef = NULL,
    formulaMem = NULL,
    wfun = NULL,
    vfun = NULL,
    efun = NULL,
    initialize = function(formula, ref = 0L, mem = 0L, fix = NULL, data = NULL, mode, choicerule = NULL, weighting = c("TK1992"), value = c("TK1992"), editing = c("hedonic"), options = list()) {
      # Ranges of parameters following these studiese
      # 1. International comparison study
      # Rieger, M. O., Wang, M., & Hens, T. (2017). Estimating cumulative prospect theory parameters from an international survey. Theory and Decision, 82, 567–596. doi:10.1007/s11238-016-9582-8
      # and
      # 2. parameter stability study
      # Glöckner, A., & Pachur, T. (2012). Cognitive models of risky choice: parameter stability and predictive accuracy of prospect theory. Cognition, 123, 21–32. doi:10.1016/j.cognition.2011.12.002

      self$formulaRef <- if (is.numeric(ref)) { ref } else { .as_rhs(ref) }
      self$formulaMem <- if (is.numeric(mem)) { mem } else { .as_rhs(mem) }
      self$set_weightingfun(weighting)
      self$set_valuefun(value)
      self$set_editingfun(editing)

      parspace <- make_parspace(
        alpha   = c(0.001,  2,  .8, 1L),
        beta    = c(0.001,  2,  .8, 1L),
        gammap  = c(0.001,  2, 1,   1L), 
        gamman  = c(0.001,  2, 1,   1L),
        lambda  = c(0.001, 10, 1, 1L)
        )
      if (is.na(weighting) == TRUE) {
        parspace <- parspace[!grepl("gamma",rownames(parspace)),]
      }
      if (is.na(value) == TRUE) {
        parspace <- parspace[!grepl("alpha|beta|lambda",rownames(parspace)), ]
      }
      formula <- .add_missing_prob(formula)
      super$initialize(
        title = paste0("CPT (", unique(c(weighting, value)), ")"),
        formula = formula,
        data = data,
        fix = fix[intersect(names(fix), rownames(parspace))],
        choicerule = choicerule,
        discount = 0L,
        mode = mode,
        options = c(options, list(solver = c("grid", "solnp"))),
        parspace = parspace       
        )
    },
    make_prediction = function(type, input, more_input, ...) {
      par <- self$get_par()
      na <- self$natt[1]
      ref <- more_input[,   1:(na/2),  drop = FALSE] # reference point
      mem <- more_input[, -(1:(na/2)), drop = FALSE] # prior outcomes/memory
      input <- self$efun(input = input, mem = mem)
      X <- input[,  seq(1, ncol(input), 2), drop = FALSE] #1., 3., 5. input
      P <- input[, -seq(1, ncol(input), 2), drop = FALSE] #2., 4., 6. input
      X <- X - ref[, rep(1L, ncol(X)), drop = FALSE]
      # Cumulative prospect theory
      #    sum(w(.) * v(.))
      return(rowSums(
        self$wfun(x=X, p=P) * 
        self$vfun(x=X))
      )
    },
    set_weightingfun = function(type) {
      if (is.na(type)) {
        wfun <- function(x, p, ...) return(p)
      } else if (type == "TK1992") {
        # One-parameter specification of cumulative prospect theory (?)
        wfun <- function(x, p, gammap=self$get_par()["gammap"], gamman=self$get_par()["gamman"]) {
        id <- apply(cbind(p, x), 1, paste, collapse = "")
        id <- match(id, unique(id))
        px <- cbind(p, x, id)
        p_unique <- p[!duplicated(id), , drop = F]
        x_unique <- x[!duplicated(id), , drop = F]
        out <- t(sapply(1:nrow(p_unique), function(.rowid, x, p) {
          x <- x[.rowid, ]
          p <- p[.rowid, ]
          pn <- p[x < 0]
          pp <- p[x >= 0]
          # Store the order
          norder <- order(x[x <  0])
          porder <- order(x[x >= 0], decreasing = TRUE)
          # Cummulative sum of the orderes probabilities
          pnord <- cumsum(pn[norder])
          ppord <- cumsum(pp[porder])
          # Weight the cummulative probabilities
          wpord <- (ppord^gammap / (rowSums(cbind(ppord, 1-ppord)^gammap) )^(1/gammap))
          wnord <- (pnord^gamman / (rowSums(cbind(pnord, 1-pnord)^gamman) )^(1/gamman))
          # Subtract the weights
          if (sum(x < 0) > 1) {
            wnord <- wnord - c(0, head(wnord, -1))
          }
          if (sum(x >= 0) > 1) {
            wpord <- wpord - c(0, head(wpord, -1))
          }
          # Reorder weights to the original order
          wp <- wpord[order(porder)]
          wn <- wnord[order(norder)]
          out <- x
          out[x >= 0] <- wp
          out[x <  0] <- wn
          return(out)
          }, x = x_unique, p = p_unique))
        out[p_unique == 0L] <- 0L
        out[p_unique == 1L] <- 1L
        return(out[id,])
        }
      }
      self$wfun <- wfun
    },
    set_valuefun = function(type) {
      if (is.na(type)) {
        vfun <- function(x, ...) { return(x) }
      } else  if (type == "TK1992") {
        vfun <- function(x, alpha = self$get_par()["alpha"], beta = self$get_par()["beta"], lambda = self$get_par()["lambda"]) {
          x <- replace(x^alpha, x < 0, (-lambda * (-x[x < 0])^beta))
          return(x)
        }
      } 
      self$vfun <- vfun
    },
  set_editingfun = function(type) {
    if (type == "hedonic") {
      efun <- function(input, mem = NULL) {
        X <- input[,  seq(1, ncol(input), 2), drop = FALSE] #1., 3., 5. input
        P <- input[, -seq(1, ncol(input), 2), drop = FALSE] #2., 4., 6. input
        Xmin <- apply(X, 1, min, na.rm = TRUE)
        X <- X - Xmin
        X <- cbind(X, mem + Xmin)
        P <- cbind(P, 1L)        
        input <- cbind(X, P)[,  rep(1:ncol(X), each = 2) + rep(c(0, ncol(X)), ncol(X)), drop = F]
        return(input)
      }
    } else {
      efun <- function(input, ...) return(input)
    }
    self$efun <- efun
  }
  ),
  private = list(
    get_more_input = function(d = data.frame()) {
      fr <- self$formulaRef
      if (is.numeric(fr)) {
        ref <- array(fr, dim = c(nrow(d), self$natt[1]/2, self$nstim))
      } else {
        ref <- super$get_input(f = chr_as_rhs(fr), d = d)
        ref <- array(ref, dim = c(nrow(d), self$natt[1]/2, self$nstim))
      }
      fr <- self$formulaMem
      if (is.numeric(fr)) {
        mem <- array(fr, dim = c(nrow(d), self$natt[1]/2, self$nstim))
      } else {
        mem <- super$get_input(f = chr_as_rhs(fr), d = d)
        mem <- array(mem, dim = c(nrow(d), 1L, self$nstim))
      }
      return(abind::abind(ref, mem, along = 2L))
    },
    check_input = function() {
      .check_probabilities(self = self)
      super$check_input() # don"t change this, it runs default checks!
    },
    make_prednames = function() {
      return(self$stimnames)
    }
  )
)