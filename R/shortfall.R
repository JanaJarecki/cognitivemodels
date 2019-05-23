#' Kahneman & Tversky's (1992) cumulative prospect theory model
#' @importFrom stringr str_extract
#' @inheritParams Cogscimodel
#' @description Fits the cumulative prospect theory model.
#' @param formula Object of class formula, for instance \code{response ~ x1 + x2 + px + I(1-px) | y1 + y2 + py + I(1-py)}, defines the response, outcome, and probability variables; choice options must be separated by a pipe (\code{|}). 
#' @param asp Object of class formula defining the aspiration level variable (\code{aso=~var}), strings will be reformulated.
#' @param choicerule (optional) choierule, for instance \code{"softmax"}, allowed values are listed in \code{type} under \code{\link[cogsciutils]{choicerule}}.
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
shortfall <- function(formula, asp, fixed = list(), data, choicerule) {
  obj <- Shortfall$new(formula = formula, asp = asp, fixed = fixed, data = data, choicerule = choicerule)
  if ( length(obj$fixednames) < length(obj$parm) ) {
    obj$fit(c('grid', 'solnp'))
  }
  return(obj)
}


Shortfall <- R6Class("shortfall",
  inherit = Cogscimodel,
  public = list(
    aspirationlevel = NULL,
    asp = NULL,
    initialize = function(formula, asp, fixed = NULL, data = NULL, choicerule = NULL, fit.options = list()) {
      # if ( any(is.na(fixed)) ) {
      #   stop('n cpt() ignoring parameters is not (yet) implemented.', call.=FALSE)
      # }
      allowedparm <- list(
        beta = c(0, 15, 1, 0),
        delta   = c(0, 1, .5, 1)
      )
      allowedparm <- matrix(unlist(allowedparm), ncol = 4, byrow = TRUE, dimnames= list(names(allowedparm), c('ll', 'ul', 'init', 'na'))) 
      super$initialize(formula = formula, data = data, allowedparm = allowedparm, fixed = fixed, choicerule =  choicerule, model = paste0('Shortfall'), discount = 0, response = 'discrete', fit.options = fit.options)

      self$asp <- if (is.character(asp)) { reformulate(asp) } else { asp }
      self$aspirationlevel <- self$getinput(self$asp, data)
      
      if(dim(self$input)[2] %% 2 != 0) {
        stop('Formula needs an even number of right-hand elements, but has ', dim(self$input)[2], ' elements.')
      }
    },
    predict = function(type = c("response", "value"), action = NULL, newdata = NULL) {
      type <- match.arg(type)
      beta <- self$parm[['beta']]
      delta <- self$parm[['delta']]

      if ( is.null(newdata) ) {
        input <- self$input
        al <- self$aspirationlevel
      } else {
        input <- self$getinput(self$formula, newdata)
        al <- self$getinput(self$asp, newdata)
      }

      no <- dim(input)[3]
      nh <- dim(input)[2]/2
      X <- input[,   1:nh , , drop=FALSE]
      P <- input[, -(1:nh), ,drop=FALSE]      

      EV <- sapply(1:no, function(o) {
        rowSums(X[,,o] * P[,,o])
      })
      R <- sapply(1:no, function(o, al) {
        X[] <- delta * al[,,o] - X[,,o]
        X[X<0] <- 0
        rowSums(X[,,o] * P[,,o])
      }, al = al)
      
      v <- EV - beta * R
      colnames(v) <- self$getoptionlabel()

      return(switch(type,
        response = super$applychoicerule(v),
        value = v))
    }
  )
)
