# ==========================================================================
# Package: Cognitivemodels
# File: model-cpt.R
# Author: Jana B. Jarecki
# ==========================================================================

# ==========================================================================
# Cognitive Model
# ==========================================================================

#' Cumulative Prospect Theory Models
#' 
#' @description 
#' cpt_* fit a cumulative prospect theory.
#' 
#' * `cpt()` fits cumulative prospect theory (Tversky & Kahneman, 1992)
#' * `cpt_mem()` fits cumulative prospect theory with memory (Thaler & Johnson, 1990)
#' 
#' @importFrom stringr str_extract
#' @importFrom abind abind
#' @importFrom data.table %between%
#' 
#' @inheritParams Cm
#' @templateVar parameter formula
#' @param formula A \link{formula} such as \code{y ~ x1 + p1 + x2 | y1 + py + y2} specifying the columns in \code{data} that contain outcomes, probabilities and (optional) the observations. Lines (\code{|}) separate different gambles. The formula must alternate outcomes and probabilities (x + p + x2 + p2), the last probability can be omitted.
#' @templateVar parameter choicerule
#' @param ref (optional, default: 0) A number, string, or RHS \link{formula}. The reference point. Formula and string set the reference point to variables in data, and `ref = "myvar"` means `data$myvar`, and so does `ref = ~ myvar`.
#' @param weighting (optional) A string, the weighting function. Currently only \code{"KT1992"}, the value function in Kahneman & Tversky (1992), is implemented.
#' @param value (optional) A string, the value function. Currently, only \code{"KT1992"} the value function in Kahneman & Tversky (1992) is implemented.
#' @references Tversky, A., & Kahneman, D. (1992). Advances in prospect theory: cumulative representation of uncertainty. Journal of Risk and Uncertainty, 5, 297–-323. doi:[10.1007/BF00122574](https://doi.org/10.1007/BF00122574)
#' @return An object of class R6 holding the model, it has free parameters. A model object `M` can be viewed with `M`, choice or utility predictions can be made by `predict(M)`, also for new data by `predict(M, newdata = ...`).
#' @author Jana B. Jarecki, \email{jj@janajarecki.com}
#' @details Fits cumulative prospect theory.
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
#' # Make the model, add fix parameters (don"t fit)
#' # using the Parameter from the paper
#' M <- cpt(rp ~ x1 + px + x2 | y1 + py + y2, ref = 0,
#'          choicerule = "softmax", data = dt,
#'          fix = list(alpha = 0.88, beta = 0.88, lambda = 2.25,
#'          gammap = 0.61, gamman = 0.69, tau = 1))
#' 
#' # View the model
#' M
#' 
#' # Methods
#' predict(M, "value") # predict values, also: M$predict("value")
#' predict(M, "mode") # predict choice probability after softmax
#' summary(M)
#' anova(M)
#' @export
cpt <- function(formula, data, choicerule, ref = 0L, fix = list(), weighting = c("TK1992"), value = c("TK1992"), options = NULL) {
  .args <- as.list(rlang::call_standardise(match.call())[-1])
  .args["editing"] <- "none"
  return(do.call(what = Cpt$new, args = .args, envir = parent.frame()))
}

#' Prospect Theory Model with an Editing Step
#' @describeIn cpt Prospect Theory with Memory
#' 
#' @param mem (optional, default: 0) A number, string, or RHS \link{formula}. Thr prior gains or losses in memory. Formula and string refer to variables in the data, and `mem = "myvar"`means `data$myvar` and so does `mem = ~myvar`.
#' @param editing (optional, default: \code{"hedonic"}) A string, the editing rule to use (see Thaler & Johnson, 1999, pp. 645). Currently only \code{"hedonic"}.
#' @references  Thaler, R. H., & Johnson, E. J. (1990). Gambling with the House Money and Trying to Break Even: The Effects of Prior Outcomes on Risky Choice. Management Science, 36(6), 643–-660. doi:[10.1287/mnsc.36.6.643](https://doi.org/10.1287/mnsc.36.6.643)
#' @export
cpt_mem <- function(formula, mem, data, choicerule, editing = "hedonic", ... ) {
  .args <- as.list(rlang::call_standardise(match.call())[-1])
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
    initialize = function(formula, ref = 0L, mem = 0L, fix = NULL, data = NULL, choicerule = NULL, weighting = c("TK1992"), value = c("TK1992"), editing = c("hedonic"), options = list()) {
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
      formula <- .add_missing_prob(formula)
      super$initialize(
        title = paste0("CPT (", unique(c(weighting, value)), ")"),
        formula = formula,
        data = data,
        fix = fix,
        choicerule =  choicerule,
        discount = 0L,
        mode = "discrete",
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
      X <- input[,  seq(1, na, 2), drop = FALSE] #1., 3., 5. input
      P <- input[, -seq(1, na, 2), drop = FALSE] #2., 4., 6. input
      X <- X - ref
      # Cumulative prospect theory
      #    sum(w(.) * v(.))
      return(rowSums(
        self$wfun(x=X, p=P, gammap=par["gammap"], gamman=par["gamman"]) * 
        self$vfun(x=X, alpha = par["alpha"], beta = par["beta"], lambda = par["lambda"]))
      )
    },
    set_weightingfun = function(type) {
      if (type == "TK1992") {
        # One-parameter specification of cumulative prospect theory (?)
        wfun <- function(x, p, gammap, gamman, ...) {
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
      if (type == "TK1992") {
        vfun <- function(x, alpha, beta, lambda, ...) {
          x <- replace(x^alpha, x < 0, (-lambda * (-x[x < 0])^beta))
          return(x)
        }
      } 
      self$vfun <- vfun
    },
  set_editingfun = function(type) {
    if (type == "hedonic") {
      efun <- function(input, mem = NULL) {
        na <- self$natt[1]
        X <- input[,  seq(1, na, 2), drop = FALSE] #1., 3., 5. input
        P <- input[, -seq(1, na, 2), drop = FALSE] #2., 4., 6. input
        X[P==0L] <- NA
        Xmin <- apply(X, 1, min, na.rm = TRUE)
        P[X==Xmin & P>0L] <- 1L
        X[] <- t(sapply(1:nrow(X), function(i) ifelse(X[i,] == Xmin[i],
          X[i,] + mem[i,], X[i,] - Xmin[i])))
        X[P==0L] <- 0L
        input[,  seq(1, na, 2)] <- X
        input[, -seq(1, na, 2)] <- P
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
        mem <- array(ref, dim = c(nrow(d), self$natt[1]/2, self$nstim))
      }
      return(abind::abind(ref, mem, along = 2L))
    },
    check_input = function() {
      .check_probabilities(self = self)
      super$check_input() # don"t change this, it runs default checks!
    }
  )
)