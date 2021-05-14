# ==========================================================================
# Package: Cognitivemodels
# File: model-shift.R
# Author: Jana B. Jarecki
# ==========================================================================

# ==========================================================================
# Cognitive Model
# ==========================================================================

#' Shifting Cognitive Model
#' @name shift
#' @description
#' `shift()` fits a change point model that shifts over time between two values
#'   * `shift_d()` fits the change point for discrete responses.
#' 
#' @useDynLib cognitivemodels, .registration = TRUE
#' 
#' @eval .param_formula(2, new = "shifting from `x1` to `x2`.")
#' @eval .param_fix("shift_d")
#' @param time (optional) Variable with the decision time or trial across which the shift occurs; can be a numeric vector, string, or formula. If missing will be set to range from 1 to the number of rows in data.
#' 
#' @details
#' The model models a shift from one value to another over time. 
#' 
#' ## Model Parameters
#' The model has 1 free parameter:
#' * _**`c`**_ the change point when the shift occurs in `time`
#' * Additoinal parameters in `shift_c()`: `r .rd_choicerules()`
#' 
#' @template cm
#' 
#' 
#' @examples 
#' D <- data.frame(a = rep(0L,4), b = 1L, y = c(0,0,1,1))
#' M <- shift_d(~ a + b, D, fix = c(c=1.5))
#' predict(M)
NULL


#' @rdname shift
#' @export
shift_d <- function(formula, data, time, fix = NULL, ...) {
  .args <- as.list(rlang::call_standardise(match.call())[-1])
  .args[["mode"]] <- "discrete"
  return(do.call(what = Shift$new, args = .args, envir = parent.frame()))
}


#' @noRd
Shift <- R6Class("shift",
  inherit = Cm,
  public = list(
    time = NULL,
    initialize = function(formula, data = data.frame(), time = 1:nrow(data), choicerule = NULL, fix = list(), mode = NULL, discount = 0, options = list(), ...) {
      if (is.null(options$fit_measure)) options$fit_measure <- "loglikelihood"
      self$time <- if(!is.numeric(time)) { get_all_vars(.as_rhs(time), data = data)[, 1] } else { time }
      ps <- make_parspace(c = c(0, max(self$time) + 1))

      super$initialize(
        title = "Shifting model",
        formula = formula,
        data = data,
        parspace = ps,
        choicerule = choicerule,
        mode = mode,
        fix = fix,
        discount = discount,
        options = c(options, list(solver = c("solnp"))),
        ...)

      if (any(self$natt != 2)) {
        stop("Argument 'formula' specifies too many values, shift() can shift between two values, but 'formula' specifies ", .brackify(self$natt), "values.")
      }
    },
    #' @export
    make_prediction = function(type, input, more_input, ...) {
      type <- match.arg(type, c("response", "weight"))
      w <- 1L - private$logistic(x = more_input, xmid = self$get_par()["c"])
      if (type == "weight") return(w)
      return(rowSums(cbind(w,1-w) * input))
    }
  ),

  # Private methods
  private = list(
    get_more_input = function(d) {
      return(array(self$time, dim(self$input))[,1,,drop=F])
    },
    logistic = function(x, xmid, ymax = 1, steep = 20) {
      return( ymax / (1 + exp(-steep * (x - xmid))) )
    },
    check_input = function() {
      super$check_input() # leave this at the end, it runs background checks
    }
  )
)
