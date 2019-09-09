#' Utility Models
#' 
#' \code{utility()} fits utility models, \code{utility_pow()} fits a power utility
#' 
#' @rdname utility
#' 
#' @inheritParams Cogscimodel
#' @param formula A formula specifying responses ~ values (e.g, \code{y ~ x1 | x2}).
#' @param type A string. Currently only \code{"power"} for power utility.
#' 
#' @return A model object (similar to lm-objects) of class "utility", which can be viewed with \code{summary(M)} or \code{anova(M)} and predictions can be made with \code{predict(M)}.
#' 
#' @section Parameter Space:
#' \tabular{lcrcllr}{\verb{   } \tab \strong{Name} \tab \verb{    }\strong{LB} \tab  \strong{-} \tab \strong{UB}\verb{    } \tab \strong{Description} \tab \strong{Start Value}\cr
#' 
#' \verb{   } Power \tab \code{rp} \tab  \eqn{-}20* \tab  - \tab  20 \tab  Exponent for positive \eqn{x \ge 0}. Values < 0 yield concavity, 0 logarithm, > 0 convexity \tab  1\cr
#' 
#' \verb{   } \tab \code{rn} \tab  \eqn{-}20* \tab  - \tab 20 \tab  Exponent for negative \eqn{x < 0;}\tab  1
#' }
#' \verb{   }*\emph{Note}, the lower bound is 0.001 if \eqn{x} contains positive \emph{and} negative values (see Wakker, 2008).
#' @details  
#' \emph{Power Utility [(\code{utility(type="power"), utility_pow()}].} The utility \eqn{U(x)} for positive outcomes \eqn{x} is \eqn{x^r if r > 0}, and is \eqn{log(x) if r = 0}, and is \eqn{-x^r if r < 0}. The utility for negative outcomes \eqn{x} equals \eqn{-U(-x)} with a separate exponent \code{rn}.
#' 
#' @references {Wakker, P. P. (2008). Explaining the characteristics of the power (CRRA) utility family. \emph{Health Economics, 17(12), 1329-1344. } \url{htrps://doi.org/10.1002/hec.1331}}
#' 
#' {Tversky, A. (1967). Utility theory and additivity analysis of risky choices. \emph{Journal of Experimental Psychology, 75(1)}, 27-36. \url{htrp://dx.doi.org/10.1037/h0024915}}
#' 
#' @examples 
#' #  No examples yet
#' 
#' @export
utility_pow <- function(formula, data, fix = list(), choicerule = NULL, mode = NULL, discount = 0, options = list(), ...) {
  .args <- as.list(rlang::call_standardise(match.call())[-1])
  .args[["type"]] <- "power"
  return(do.call(what = Utility$new, args = .args, envir = parent.frame()))
}



#' Utility Models
#' 
#' @noRd
utility_exp <- function(formula, data, fix = list(), choicerule = NULL, mode = NULL, discount = 0, options = list(), ...) {
  .args <- as.list(rlang::call_standardise(match.call())[-1])
  .args[["type"]] <- "exponential"
  return(do.call(what = Utility$new, args = .args, envir = parent.frame()))
}


#' Utility Models
#' 
#' @rdname utility
#' 
#' @export
utility <- function(formula, data, type = c("power"), fix = list(), choicerule = NULL, mode = NULL, discount = 0, options = list(), ...) {
  .args <- as.list(rlang::call_standardise(match.call())[-1])
  return(do.call(what = Shortfall$new, args = .args, envir = parent.frame()))
}

Utility <- R6Class("utility",
  inherit = Cogscimodel,
  public = list(
    type = NULL,
    initialize = function(formula, data, type = c("power", "exponential"), fix = list(1), choicerule = NULL, mode = c("continuous", "discrete"), discount = 0, options = list(), ...) {
      self$type <- match.arg(type)
      parspace <- self$make_parspace(
        type = self$type,
        input = super$get_input(f=formula, d=data))
      super$initialize(
        formula = formula,
        data = data,
        title = paste("Utility:", self$type),
        parspace = parspace,
        choicerule = choicerule,
        mode = match.arg(mode),      
        fix = fix,
        discount = discount,
        options = options,
        ...)
    },
    make_parspace = function(type, input) {
      if (type == "power") {
        parspace <- make_parspace(rp = c(-20, 20, 1, 1),
                                  rn = c(-20, 20, 1, 1))
        if (min(input) < 0L & max(input) > 0L) {
          parspace[, "lb"] <- 0.0001
        }
        return(parspace)
      } else if (type == "exponential") {
        return(make_parspace(r = c(0.001, 20, 1, 1)))
      }
    },
    make_prediction = function(response = c("response"), input, ...) {
      par <- self$get_par()
      type <- self$type
      if (type == "power") {
        rp <- par["rp"]
        rn <- par["rn"]
        # Utility:
        # -------------------------------------
        #                   x^rp      if rp > 0
        # u(x | x >= 0) =  log(x)     if rp = 0
        #                   -x^rp     if rp < 0
        #
        #                   -x^rp     if rn > 0
        # u(x | x <  0) =  log(x)     if rn = 0
        #                   x^rp      if rn < 0
        #
        # Note: we're using the fact that x^0  = 1 below
        # Note: use sign(r) because  -x^r returns NA for very small r < 0
        return(
          replace(
            x = (sign(rp) * input^rp)^(rp != 0L) * (log(input)^(rp == 0L)),
            list = (input < 0),
            values = -((-1)^(rn < 0L) * ((-1L*input[input < 0L])^rn)^(rn != 0L) * log(input)^(rn == 0L))
            ))
      } else if (type == "exponential") {
        r <- par["r"]
        # Utility:
        # -------------------------------------
        #                   1 - exp(-rx)  if r > 0
        # u(x | x >= 0) =   x             if = 0 
        #                   exp(-rx)      if r < 0
        # Note: we're using the fact that x^0  = 1 below
        # Note: use sign(rp) because  -x^a returns NA for very small a < 0
        return(
          (sign(rp) + sign(rp) * -exp(-r * input))^(rp != 0L) * input^(rp==0L)
        )
      }
    }
    )
)
