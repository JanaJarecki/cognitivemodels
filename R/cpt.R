#' Kahneman & Tversky's (1992) cumulative prospect theory model
#' @importFrom stringr str_extract
#' @inheritParams Cogscimodel
#' @description Fits the cumulative prospect theory model.
#' @param formula Object of class formula, for instance \code{response ~ x1 + x2 + px + I(1-px) | y1 + y2 + py + I(1-py)}, defines the response, outcome, and probability variables; choice options must be separated by a pipe (\code{|}). 
#' @param nopt Integer spedifying the number of options.
#' @param nout Integer specifying the number of outcomes.
#' @param ref Integer or numeric vector, reference point.
#' @param choicerule (optional) choierule, for instance \code{"softmax"}, allowed values are listed in \code{type} under \code{\link[cogsciutils]{choicerule}}.
#' @param weighting (optional) weighting function. Currently the one used in Kahneman & Tversky (1992), \code{"KT1992"}, is possible.
#' @param value (optional) value function. Currently, only the one used by Kahneman & Tversky (1992), \code{"KT1992"}, is possible.
#' @references Tversky, A., & Kahneman, D. (1992). Advances in prospect theory: cumulative representation of uncertainty. Journal of Risk and Uncertainty, 5, 297–323. doi:10.1007/BF00122574
#' @return An object of class R6 holding the model, it has free parameters. A model object \code{M} can be viewed with \code{M}, predictions can be made with \code{M$predict()} for choice predictions, and \code{M$predict("ev")} for the expected value of the optimal choice and \code{M$predict("value", 1:2)} for the expected value of all choices.
#' @author Jana B. Jarecki, \email{jj@janajarecki.com}
#' @details Risk-sensitive foraging means you have, for instance, four choices between the same two risky lotteries and after the four choices you need to have accumulated at least 12 points to get a reward. The optimal solution to this choice problem relies on dynamic programming. The function creates all possible future states given the possible remaining trials, and predicts the optimal choice polica or the expected value of chosing either option given a certain state and a certain time horizon.
#' @examples
# #' # Tversky, A., & Kahneman, D. (1992). Advances in prospect theory: cumulative representation of uncertainty. Journal of Risk and Uncertainty, 5, 297–323. doi:10.1007/BF00122574
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
#' # Make the model, add fixed parameters (don't fit)
#' M <- cpt(rp ~ x1 + x2 + px + I(1-px) | y1 + y2 + py + I(1-py),
#'          nopt = 2, nout = 2, ref = 0, choicerule = "softmax", data = dt,
#'          fixed = list(alpha = 0.88, beta = 0.88, lambda = 2.25,
#'          gammap = 0.61, gamman = 0.69, tau = 1))
#' # View the model
#' M
#' M$predict("value") # predict values
#' M$predict("response") # predict choices after soft-max
#' @export
cpt <- function(formula, nopt, nout, ref, fixed = list(), data, choicerule, weighting = c('TK1992'), value = c('TK1992')) {
  message("This function is experimental and still under development.")
  obj <- Cpt$new(formula = formula, nopt = nopt, nout = nout, ref = ref, fixed = fixed, data = data, choicerule = choicerule, weighting = c('TK1992'), value = c('TK1992'))
  if ( length(obj$fixednames) < length(obj$parm) ) {
    obj$fit(c('solnp'))
  }
  return(obj)
}


Cpt <- R6Class("cpt",
  inherit = Cogscimodel,
  public = list(
    nopt = NULL,
    nout = NULL,
    ref = NULL,
    wfun = NULL,
    vfun = NULL,
    initialize = function(formula, nopt, nout, ref, fixed = NULl, data = NULL, choicerule = NULL, weighting = c('TK1992'), value = c('TK1992'), fit.options = list(nbest = 1)) {
      # Ranges of parameters following these studiese
      # 1. International comparison study
      # Rieger, M. O., Wang, M., & Hens, T. (2017). Estimating cumulative prospect theory parameters from an international survey. Theory and Decision, 82, 567–596. doi:10.1007/s11238-016-9582-8
      # and
      # 2. parameter stability study
      # Glöckner, A., & Pachur, T. (2012). Cognitive models of risky choice: parameter stability and predictive accuracy of prospect theory. Cognition, 123, 21–32. doi:10.1016/j.cognition.2011.12.002
      if ( any(is.na(fixed)) ) {
        stop('n cpt() ignoring parameters is not (yet) implemented.', call.=FALSE)
      }
      allowedparm <- list( #todo: check if the 'ignore' values are correct
        alpha   = c(0, 2, .8, 1),
        beta    = c(0, 2, .8, 1),
        gammap  = c(0, 2, 1, 1),
        gamman  = c(0, 2, 1, 1),
        lambda  = c(0, 10, 1.5, 1)
        )
      allowedparm <- matrix(unlist(allowedparm), ncol = 4, byrow = TRUE, dimnames= list(names(allowedparm), c('ll', 'ul', 'init', 'na')))
     
      super$initialize(formula = formula, data = data, allowedparm = allowedparm, fixed = fixed, choicerule =  choicerule, model = paste0("Cumulative prospect theory [", unique(c(weighting, value)), "]"), discount = 0, response = 'discrete', fit.options = fit.options)

      if (length(ref) == 1) {
        ref <- rep(ref, nrow(data))
      }      
      if (length(nout) == 1) {
        nout <- rep(nout, each = nopt)
      }

      self$ref  <- ref
      self$nout <- nout
      self$nopt <- nopt
      self$setWeightingFun(weighting)
      self$setValueFun(value)
    },
    predict = function(type = c("response", "value"), action = NULL, newdata = NULL, newref = NULL) {
      type <- match.arg(type)

      if ( !is.null(newdata) ) {
        input <- self$getinput(self$formula, newdata)
        # input <- model.frame(self$formula, newdata)[, -1, drop = F]
        ref <- if(length(newref) == 1) { rep(newref, nrow(input)) } else { newref }
      } else {
        input <- self$input
        ref <- self$ref
      }
     v <- apply(input, 3, function(i) {
      self$cpt(x = i[,1:2,drop=FALSE], p = i[,3:4,drop=FALSE], ref = ref)})
      colnames(v) <- self$getoptionlabel()

      switch (type,
        response = super$applychoicerule(v),
        value = v)
    },
    cpt = function(x, p, ref) {
      if (!is.matrix(p)) {
        p <- t(as.matrix(p))
      }
      if (!is.matrix(x)) {
        x <- t(as.matrix(x))
      }

      x <- x - ref

      args <- c(as.list(self$parm), list(p = p, x = x))      
      out <- rowSums(do.call(self$wfun, args) * do.call(self$vfun, args))
      
      return(out)
    },
    setWeightingFun = function(type) {
      if (type == "TK1992") {
        # One-parameter specification of cumulative prospect theory (?)
        wfun <- function(p, x, gammap, gamman, ...) {
          if(dim(p)[2] != 2) {
            stop("p in the cpt-weighting fun needs 2 columns,\nbut has ", ncol(p), ", which is: ", head(p))
        }
        px <- cbind(p, x)
        id <- as.numeric(factor(apply(px, 1, paste, collapse=''), ordered = TRUE))
        px <- cbind(px, id)
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
          wpord <- (ppord^gammap / ( rowSums(cbind(ppord, 1-ppord)^gammap) )^(1/gammap))
          wnord <- (pnord^gamman / ( rowSums(cbind(pnord, 1-pnord)^gamman) )^(1/gamman))
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
        return(out[id,])
        }
      }
      self$wfun <- wfun
    },
    setValueFun = function(type) {
        if (type == "TK1992") {
          vfun <- function(x, alpha, beta, lambda, ...) {
            out <- x^alpha
            out <- replace(out, x < 0, (-lambda * (-x)^beta )[x<0])
            return(out)
          }
        } 
        self$vfun <- vfun
    }
  )
)