#' Utility functions
#' 
#' @description Fits a utility function.
#' @param formula A formula specifying the variable names, like \code{yvar ~ xvar}
#' @param data A data.frame or matrix used to fit the model to.
#' @param type A string, currently only \code{"power"} for power utility.
#' @param fixed (optional) A list with fixed parameter(s).
#' @param choicerule (optional) A string specifying the choice rule, allowed values see the \code{priordist} argument of \link{choicerule}. *Required* if \code{response = "discrete"} and model learns > 1 options.
#' @param response (optional, default \code{"continuous"}) A string specifying if beliefs are predicted (\code{"continuous"}) or if choices are predicted (\code{"discrete"}).
#' @param discount (optional, default 0) A number or numeric vector of trial indices to discount.
#' @param ... other arguments from other functions, currently ignored.
#' @return An model object (similar to lm-objects) of class "utility". Itcan be viewed with \code{summary(mod)}, where \code{mod} is the name of the model object.
#' @examples 
#' #  No examples yet
#' @author Jana B. Jarecki, \email{jj@janajarecki.com}, Markus Steiner
#' @references None yet

#' @export
utility <- function(formula, data, type = c('power'), fixed = list(), choicerule = NULL, response = c('continuous', 'discrete'), discount = 0, ...) {
  args <- as.list(match.call()[-1L])
  obj <- do.call(Utility$new, args)
  # check(model) #todo
  #todo move this into cogscimodel-class if possible
  if (length(obj$getparm('free')) > 0) {
    message('Fitting free parameter ', .brackify(obj$freenames))
    obj$fit(c('grid', 'solnp'))
  }
  return(obj)
}

Utility <- R6Class('utility',
  inherit = Cogscimodel,
  public = list(
    type = NULL,
    initialize = function(formula, data, type = c('power'), fixed = list(1), choicerule = NULL, response = c('continuous', 'discrete'), discount = 0, ...) {
      self$type <- match.arg(type)
      allowedparm <- rbind(alpha = c(ll=0, ul=2, init=1, na=1))
      super$initialize(formula = formula,
        data = data,
        allowedparm = allowedparm,
        choicerule = choicerule,
        response = match.arg(response),
        model = 'Utility',
        fixed = fixed,
        discount = discount,
        ...)
    },
    predict = function(newdata, response = c("response")) {
      x <- if (!missing(newdata)) {
        self$getinput(f = self$formula, d = newdata)
      } else {
        self$input
      }
      return(x^(self$getparm()['alpha']))
    }
    )
)