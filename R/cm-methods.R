# Define S3 methods for cm

#' @title Number of Parameters, Attributes, Stimuli
#' @name npar
#' 
#' @description
#' \code{npar(m)} counts the parameters in a cm, \code{nstim(m)} counts stimuli, \code{natt(m)} counts attributes by stimulus, \code{nobs(m)} counts observations in the data of a model.
#' 
#' @usage \code{npar(x)}
#' @param x a model object
#' @param ... other arguments (ignored)
#' @examples 
#' D <- data.frame(x = 1, y = 1, z = 1)
#' M <- bayes_beta(y ~ x + z, D, fix = "start")
#' npar(M) # 3
#' 
#' # Print the model
#' M 
#' 
#'# Bayesian model
#'# Call:  
#'# bayes_beta(formula = y ~ x + z, data = D, fix = "start")  
#'#
#'# Constrained parameter:
#'# delta      x      z
#'#     1      1      1
#' 
#' 
#' @export
npar <- function(x, ...) {
  UseMethod("npar")
}
npar.default <- function(x, ...) {
  error("'npar' method is not defined for object of class ", class(x))
}
#' @name npar
#' @param type A string: \code{"all"} counts all parameters, \code{"free"} counts the free parameters, \code{"fix"} countes the fixed parameters, \code{"constrained"} counts constrained parameters.
#' @export
#' @method npar cm
npar.cm <- function(x, type = "all") {
  return(x$npar(type))
}

#' @export
nobs <- function(x, ...) {
  UseMethod("nobs")
}
#' @name npar
#' @usage \code{nobs(x)}
#' @param x a model object
#' @examples 
#' nobs(M)
nobs.cm <- function(x) {
  return(x$nobs)
}

#' @name npar
#' @usage \code{nstim(x)}
#' @param x a model object
#' @examples 
#' nstim(M)
#' @export
nstim <- function(x) {
  UseMethod("nstim")
}
nstim.default <- function(x) {
  error("'npar' method is not defined for object of class ", class(x))
}
#' @export
#' @method nstim cm
nstim.cm <- function(x) {
  return(x$nstim)
}

#' @name npar
#' @usage \code{natt(x)}
#' @param x a model object
#' @examples 
#' natt(M)
#' @export
natt <- function(x) {
  UseMethod("natt")
}
natt.default <- function(x) {
  error("'npar' method is not defined for object of class ", class(x))
}
#' @export
#' @method natt cm
natt.cm <- function(x) {
  return(x$natt)
}





#' @export
coef.cm <- function(x, ...) {
  do.call(x$coef, list(...))
}




#' @name gof
#' @family {fit measures for cognitive models}
#' @export
logLik.cm <- function(x, ...) {
  x$logLik(...)
}

#' @name gof
#' @family {fit measures for cognitive models}
#' @usage \code{MSE(x)}
#' @examples 
#' MSE(M)     # 0.1805
#' 
#' @export
MSE <- function(x, ...) {
  UseMethod("MSE")
}
#' @family {fit measures for cognitive models}
#' @export
MSE.cm <- function(x, ...) {
  x$MSE(...)
}

#' @family {fit measures for cognitive models}
#' @export
AIC.cm <- function(x, ...) {
  x$AIC(...)
}
#' @family {fit measures for cognitive models}
#' @export
AICc.cm <- function(x) {
  x$AICc()
}
#' @export
BIC.cm <- function(x) {
  return(x$BIC())
}
#' @export
RMSE.cm <- function(x) {
  return(x$RMSE())
}



#' @export
summary.cm <- function(x) {
  return(x$summary())
}
#' @export
nstim.cm <- function(x, ...) {
  return(x$nstim)
}
nres.cm <- function(x, ...) {
  return(x$nres)
}
natt.cm <- function(x, ...) {
  return(x$natt)
}
ncon.cm <- function(x, ...) {
  return(x$ncon)
}
getCall.cm <- function(x, ...) {
  return(x$getCall())
}


print.summary.cm = function(x, digits = max(3L, (getOption("digits") - 3L)), ...) {
  cat("\nModel:\n", trimws(x$model),
    if (is.null(x$choicerule)) {
      'with no choice rule'
    } else {
      paste('with', x$choicerule, 'choicerule')
    })
  cat("\nCall:\n",
    paste(deparse(x$call), sep="\n", collapse="\n"), "\n", sep="")
  if (length(x$freenames) == 0L) {
    cat("\nNo Free Parameters\n")
  } else {
    cat("\n(Constrained) Parameters:\n")
    print(x$coefficients, digits = digits, ...)
  }
  cat("\nFit Measures:\n")
  cat(paste(c("MSE:", "LL:", "AIC:", "BIC:"), format(x[c("mse", "logLik", "aic", "bic")], digits = 2L), collapse = ", "))
  cat(
    "\n\n"
    )
  invisible(x)
}


#' @title Computes Various Model Fit Measures
#' @name gof
#' @export
SSE <- function(x) {
  UseMethod("SSE")
}
#' @name gof
#' @family {fit measures for cognitive models}
#' 
#' @description
#' \code{logLik(m)} computes the log likelihood of a cm object, \code{SSE(m)} computes the sum of squared errors, \code{MSE(m)} computes the mean squared error.
#' 
#' 
#' @usage \code{SSE(x)}
#' @param x a cm object
#' @param ... other arguments (ignored)
#' @return A number measuring the goodness of fit between predictions and observed data.
#' @details If a model predicts several values the error measures use the first column of predictions to compute the errors. For example, if the predictions are pr(x) and pr(z), the sum of squared errors is based on the data - pr(x).
#' @examples 
#' D <- data.frame(x = 1, y = 1:1, z = 0:1)
#' M <- bayes_beta(y ~ x + z, D, fix = "start")
#' # If you want, look at the predictions
#' # predict(M)
#' 
#' SSE(M)     # 0.361
#' 
#' @export
SSE.cm <- function(x) {
  x$SSE()
}