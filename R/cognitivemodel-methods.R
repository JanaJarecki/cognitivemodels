# ==========================================================================
# Package: Cognitivemodels
# File: cognitivemodel-methods.R
# Author: Jana B. Jarecki
# ==========================================================================

# ==========================================================================
# Methods for the Lego-style Cognitive Model
# ==========================================================================


# Define S3 methods for the class 'cognitivemodel'


#' Adds a component to a cognitivemodel via `+`
#' 
#' @export
`+` <- function (e1, e2) { UseMethod("+") }
#' @export
`+.default` <- function (e1, e2) { .Primitive("+")(e1, e2) }
#' @export
#' @method + csm
`+.csm` <- function(e1, e2) {
  x_name <- .abbrDeparse(substitute(e2))
  if (inherits(e2, "cm")) {
    return(e1$add_model(e2, x_name = x_name))
  }
  if (inherits(e2, "function")) {
    return(e1$add_fun(e2))
  }
}


#' Ends building a cognitivemodel via `+`
#' 
#' @param obj a model object of class 'cognitivemodel'
#' @param ... other arguments (ignored)
#' @export
end <- function(obj, ...) { UseMethod("end") }
#' @export
end.csm = function(obj, ...) {
  return(obj$end())
}


#' Adds a function to a cognitivemodel via `+`
#' 
#' @param obj a model object of class 'cognitivemodel'
#' @param ... other arguments (ignored)
#' @export
fun <- function(obj, ...) { UseMethod("fun") }
#' @export
fun.csm = function(obj, ...) {
  return(obj$add_fun(...))
}


#' Estimates free parameters of a cognitvemodel generated via `+`
#' 
#' @param obj a model object of class 'cognitivemodel'
#' @param ... other arguments (ignored)
#' @export
fit <- function(obj, ...) { UseMethod("fit") }
#' @export
fit.csm <- function(obj, ...) {
  return(obj$fit(...))
}