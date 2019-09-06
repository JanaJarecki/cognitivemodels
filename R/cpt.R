#' Kahneman & Tversky's (1992) cumulative prospect theory model
#' @importFrom stringr str_extract
#' @importFrom abind abind
#' @importFrom data.table %between%
#' @inheritParams Cogscimodel
#' @description Fits the cumulative prospect theory model.
#' @param formula Object of class \link{formula} (e.g, \code{y ~ x1 + p1 + x2 | y1 + py + y2}) specifying the observed variables and the gambles. Gambles must be in the form X+P+X+P+..., the last P can be omitted (X+P+X) and will be inferred (e.g. \code{y ~ x1 + p1 + x2 + I(1-p1)}). A pipe (\code{|}) separates multiple gambles (e.g., two gambles: \code{y ~ x1 + p1 + x2 | z1 + pz + z2}).
#' @param ref A Integer, numeric matrix, or formula defining the reference point. If it is a formula (e.g, \code{~z}) the reference point is a variable in \code{data}.
#' @param choicerule (optional) choierule, for instance \code{"softmax"}, allowed values are listed in \code{type} under \code{\link[cogsciutils]{choicerule}}.
#' @param weighting (optional) weighting function. Currently the one used in Kahneman & Tversky (1992), \code{"KT1992"}, is possible.
#' @param value (optional) value function. Currently, only the one used by Kahneman & Tversky (1992), \code{"KT1992"}, is possible.
#' @references Tversky, A., & Kahneman, D. (1992). Advances in prospect theory: cumulative representation of uncertainty. Journal of Risk and Uncertainty, 5, 297–323. doi:10.1007/BF00122574
#' @return An object of class R6 holding the model, it has free parameters. A model object \code{M} can be viewed with \code{M}, predictions can be made with \code{M$predict()} for choice predictions, and \code{M$predict("ev")} for the expected value of the optimal choice and \code{M$predict("value", 1:2)} for the expected value of all choices.
#' @author Jana B. Jarecki, \email{jj@janajarecki.com}
#' @details Fits cumulative prospect theory.
#' @examples
#' ## Tversky, A., & Kahneman, D. (1992). Advances in prospect theory: cumulative representation of uncertainty. Journal of Risk and Uncertainty, 5, 297–323. doi:10.1007/BF00122574
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
#' # Make the model, add fix parameters (don't fit)
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
cpt <- function(formula, data = NULL, ref, fix = list(), choicerule = NULL, weighting = c('TK1992'), value = c('TK1992'), options = NULL) {
  message("This function is experimental and still under development.")
  .args <- as.list(rlang::call_standardise(match.call())[-1])
  return(do.call(what = Cpt$new, args = .args, envir = parent.frame()))
}

Cpt <- R6Class("cpt",
  inherit = Cogscimodel,
  public = list(
    formulaRef = NULL,
    wfun = NULL,
    vfun = NULL,
    initialize = function(formula, ref, fix = NULL, data = NULL, choicerule = NULL, weighting = c('TK1992'), value = c('TK1992'), options = list()) {
      # Ranges of parameters following these studiese
      # 1. International comparison study
      # Rieger, M. O., Wang, M., & Hens, T. (2017). Estimating cumulative prospect theory parameters from an international survey. Theory and Decision, 82, 567–596. doi:10.1007/s11238-016-9582-8
      # and
      # 2. parameter stability study
      # Glöckner, A., & Pachur, T. (2012). Cognitive models of risky choice: parameter stability and predictive accuracy of prospect theory. Cognition, 123, 21–32. doi:10.1016/j.cognition.2011.12.002

      if ( any(is.na(fix)) ) {
        stop('Ignoring parameters by setting them NA is not (yet) implemented.', call.=FALSE)
      }
      self$formulaRef <- if (is.numeric(ref)) { ref } else { chr_as_rhs(ref) }
      self$set_weightingfun(weighting)
      self$set_valuefun(value)

      # todo: check if 'na' (ignore) values are correct
      parspace <- make_parspace(
        alpha   = c(0,  2,  .8, 1L),
        beta    = c(0,  2,  .8, 1L),
        gammap  = c(0,  2, 1,   1L), 
        gamman  = c(0,  2, 1,   1L),
        lambda  = c(0, 10, 1.5, 1L)
        )

      super$initialize(
        title = paste0("Cumulative prospect theory (", unique(c(weighting, value)), ")"),
        formula = formula,
        data = data,
        fix = fix,
        choicerule =  choicerule,        
        discount = 0L,
        mode = "discrete",
        options = c(options, list(fit_solver = "grid")),
        parspace = parspace       
        )
    },
    get_more_input = function(d) {
      fr <- self$formulaRef
      if (is.numeric(fr)) {
        ref <- array(fr, dim = c(nrow(d), self$natt()[1]/2, self$nstim()))
      } else {
        ref <- super$get_input(f = chr_as_rhs(fr), d = d)
      }
      return(ref)
    },
    make_prediction = function(type, input, more_input, ...) {
      par <- self$get_par()
      no <- self$nobs()
      ns <- self$nstim()
      na <- self$natt()[1]
      X <- input[,  seq(1, na, 2), drop = FALSE] #1., 3., 5. input
      P <- input[, -seq(1, na, 2), drop = FALSE] #2., 4., 6. input
      X <- X - more_input # more_input = reference point
      # Cumulative prospect theory
      #    sum(w(...) * v(...))
      return(rowSums(
        self$wfun(x=X, p=P, gammap=par["gammap"], gamman=par["gamman"]) * 
        self$vfun(x=X, alpha = par["alpha"], beta = par["beta"], lambda = par["lambda"]))
      )
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
        if ( (length(x) %% 2) != 0 ) {
            update(fs[[i]], as.formula(paste0("~ . + I(1-", paste0(x[seq(2,length(x),2)], collapse = "-"), ")")))
          } else {
            fs[[i]]
          }
        })
      self$formula <- update(f, as.formula(paste(fs, collapse = " | ")))
    },
    set_weightingfun = function(type) {
      if (type == "TK1992") {
        # One-parameter specification of cumulative prospect theory (?)
        wfun <- function(x, p, gammap, gamman, ...) {
          if(dim(p)[2] != 2) {
            stop('Probabilities ("p" in the cpt weighting function) need 2 columns, but has ', ncol(p), ".")
        }
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
    set_valuefun = function(type) {
      if (type == "TK1992") {
        vfun <- function(x, alpha, beta, lambda, ...) {
          x <- replace(x^alpha, x < 0, (-lambda * (-x[x < 0])^beta))
          return(x)
        }
      } 
      self$vfun <- vfun
    },
    check_input = function() {
      P <- self$input[, seq(2, self$natt()[1], 2), , drop = FALSE]
      f <- self$formula
      fs <- lapply(seq.int(length(f)[2]), function(x) attr(terms(formula(f, lhs=0, rhs=x, drop=FALSE)), "term.labels"))
      pvars <- lapply(fs, function(x) x[seq(2, length(x), 2)] )
      if (!all(apply(P, 3, rowSums) == 1L)) {
        stop('In "data" the probability variables must sum to 1 (2nd, 4th... RHS variables in the "formula"). This is not true for:\n  ', paste(lapply(pvars[which(apply(P, 3, function(x) !all(rowSums(x) == 1L)))], .brackify), "does not sum to 1.\n  "), "Make sure outcomes and probabilities alternate in 'formula', like:  ~ x1+p1+x2+p2.")
      }
      pvars <- lapply(pvars, function(x) x[grep("I", x, invert=TRUE)] )
      if (!all(P %between% list(0L,1L))) {
        stop('In "data" the probability variables (2nd, 4th... RHS variables in the "formula") must lie between 0 - 1. This is not true for:\n  ', .brackify(unlist(pvars[which(apply(P, 3, function(x) !all(x %between% list(0L,1L))))])), ".", '\n  Check "data" or "formula"; in RHS of "formula" do outcomes and probabilities alternate (like: y ~ x1 + p1 + x2 + p2)?')
      }
      super$check_input() # don't change this, it runs default checks!
    }
  )
)