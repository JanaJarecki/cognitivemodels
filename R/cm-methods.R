# ==========================================================================
# Package: Cognitivemodels
# File: cm-methods.R
# Author: Jana B. Jarecki
# ==========================================================================

# ==========================================================================
#  Define S3 methods for the class 'cm'
# ==========================================================================


#' @title Number of Parameters, Attributes, and Stimuli
#' @name npar
#' 
#' @description
#' \code{npar(m)} counts the parameters in a cm, \code{nstim(m)} counts stimuli, \code{natt(m)} counts attributes by stimulus, \code{nobs(m)} counts observations in the data of a model.
#' 
#' @usage npar(x)
#' @param x a model object
#' @param ... other arguments (ignored)
#' @examples 
#' D <- data.frame(x = 1, y = 1, z = 1)
#' M <- bayes_beta(y ~ x + z, D, fix = "start")
#' npar(M) # 3
#' @export
npar <- function(x, ...) {
  UseMethod("npar")
}
npar.default <- function(x, ...) {
  stop("'npar' method is not defined for object of class ", class(x), ".")
}
#' @name npar
#' @param type A string: \code{"all"} counts all parameters, \code{"free"} counts the free parameters, \code{"fix"} countes the fixed parameters, \code{"constrained"} counts constrained parameters.
#' @export
#' @method npar cm
#' @examples
#' npar(M)
npar.cm <- function(x, type = "all", ...) {
  return(x$npar(type))
}

#' @title Number of Parameters, Attributes, and Stimuli
#' 
#' @export
nobs <- function(x, ...) {
  UseMethod("nobs")
}
#' @name nobs
#' @usage nobs(x)
#' @param x a model object
#' @examples 
#' nobs(M)
#' @export
nobs.cm <- function(x, ...) {
  return(x$nobs)
}

#

#' @title Number of Parameters, Attributes, and Stimuli
#' 
#'  @name nstim
#' @usage nstim(x)
#' @param x a model object
#' @examples 
#' nstim(M)
#' @export
nstim <- function(x) {
  UseMethod("nstim")
}
nstim.default <- function(x) {
  stop("'nstim' method is not defined for object of class ", class(x), ".")
}
#' @export
#' @method nstim cm
nstim.cm <- function(x) {
  return(x$nstim)
}

#' Number of Attributes of options
#' 
#' @name natt
#' @usage natt(x)
#' @param x a model object
#' @examples 
#' natt(M)
#' @export
natt <- function(x) {
  UseMethod("natt")
}
natt.default <- function(x) {
  stop("'natt' method is not defined for object of class ", class(x), ".")
}
#' @export
#' @method natt cm
natt.cm <- function(x) {
  return(x$natt)
}





#' Print the (only free) model parameters
#' 
#' @export
#' @examples
#' coef(M)
coef.cm <- function(object, ...) {
  do.call(object$coef, list(...))
}




#' @name gof
#' @family {fit measures for cognitive models}
#' @export
logLik.cm <- function(object, ...) {
  object$logLik(...)
}

#' @name gof
#' @family {fit measures for cognitive models}
#' @usage MSE(x)
#' @examples 
#' MSE(M)     # 0.1805
#' 
#' @export
MSE <- function(x, ...) { UseMethod("MSE") }
#' @family {fit measures for cognitive models}
#' @export
MSE.cm <- function(x, ...) {
  x$MSE(...)
}
#' @family {fit measures for cognitive models}
#' @export
AICc.cm <- function(object, ..., k) {
  object$AICc()
}
#' @family {fit measures for cognitive models}
#' @export
RMSE.cm <- function(x) {
  return(x$RMSE())
}



#' @export
summary.cm <- function(object, ...) {
  return(object$summary())
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

#' Get the Call to a cognitive model object
#' 
#' @param x An existing fit from a cognitive model function such as \link{cm}
#' @export
getCall.cm <- function(x, ...) {
  return(x$getCall())
}

#' @export
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
SSE <- function(x, ...) {
  UseMethod("SSE")
}
#' @name gof
#' @family {fit measures for cognitive models}
#' 
#' @description
#' \code{logLik(m)} computes the log likelihood of a cm object, \code{SSE(m)} computes the sum of squared errors, \code{MSE(m)} computes the mean squared error.
#' 
#' 
#' @usage SSE(x)
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
SSE.cm <- function(x, ...) {
  x$SSE()
}



#' Predictions from Cognitive Models (class cm)
#' 
#' @param object A cognitive model object of class 'cm'
#' @param ... Additional cognitive models
#' @param type A string, what to predict, usually \code{"response"}, but other values may be possible, see the help pages of the model you use.
#' @param newdata A data.frame with new data to predict
#' 
#' @return A vector or matrix of predictions, if multiple models are supplied using \code{...}, returns a list containing the predictions for each model
#' 
#' @export
predict.cm <- function(object, ..., type = "response", newdata = NULL) {
  dotargs <- list(...)
  if (missing(object) & length(dotargs)) {
    object <- dotargs
  }
  if (is.list(object) & length(object) > 1) {
    dotargs <- c(object[-1], dotargs)
  }  
  named <- if (is.null(names(dotargs))) {
              rep_len(FALSE, length(dotargs)) 
            }  else {
              (names(dotargs) != "")
            }
  if (any(named)) {
    warning("These arguments to 'predict.cogscimodel' are invalid and dropped: ",
      paste(deparse(dotargs[named]), collapse=", "))
  }
  dotargs <- dotargs[!named]

  # Allowed models are cogscimodel, lm, merMod
  class_allowed <- vapply(dotargs, function(x) inherits(x, "cogscimodel"), NA) | vapply(dotargs, function(x) inherits(x, "lm"), NA) | vapply(dotargs, function(x) inherits(x, "merMod"), NA)
  dotargs <- dotargs[class_allowed]

  ## If multiple objects, predict for all of them
  if (length(dotargs)) {
    return(lapply(c(object, dotargs), predict, type = type, newdata = newdata))
  }
  return(object$predict(type = type, newdata = newdata))
}



#' Show the paramter space of a cognitive model
#' 
#' @description
#' `parspace(m)` shows the parameter names, the upper and lower bounds of parameters in a cognitive model stored as `m`.
#' 
#' @usage parspace(x)
#' @param x a model object of class cm
#' @param ... other arguments (ignored)
#' @examples 
#' D <- data.frame(x = 1, y = 1, z = 1)
#' M <- bayes_beta(y ~ x + z, D, fix = "start")
#' parspace(M)    # view the parspace
#' @export
parspace <- function(x, ...) {
  UseMethod("parspace")
}
parspace.default <- function(x, ...) {
  stop("'parspace' method is not defined for object of class ", class(x), ".")
}
#' @name parspace
#' @export
#' @method parspace cm
parspace.cm <- function(x, ...) {
  title <- ifelse(length(x$title), x$title, class(x)[1])
  cat("\nParameter space of the cognitive model '", title, "':\n", sep = "")
  print(format(x$parspace, digits = 2L, scientific = 10, width = 6), quote = FALSE, right = TRUE)
  cat("---\nNote. lb = lower bound, ub = upper bound, start = start value.\n")
}
#' @name parspace
#' @export
#' @method parspace character
parspace.character <- function(x, ...) {
  x <- do.call(.cm_dummy_model, args = c(x, list(...)))
  cat("\nParameter in model '", class(x)[1], "'", sep = "")
  cat(", assuming formula '", paste(as.character(x$formula), sep = "", collapse = ""), "':\n", sep="")
  print(format(x$parspace, digits = 2L, scientific = 10, width = 6), quote = FALSE, right = TRUE)
  cat("---\nNote. lb/ub = lower/upper bound, start = start value.\n")
}

#' Returns a cognitive model without data
#' 
#' @param x A string, the model call. For example `"gcm"`, `"cpt"`, `"bayes_beta_c"`.
#' @param formula (optional) A formula.
#' 
#' @noRd
.cm_dummy_model <- function(x, formula = ~ x1, ...) {
  args <- c(list(...), list(formula = formula, options = list(fit = FALSE), choicerule = "none"))
  if (grepl("ebm|mem", x)) {
    args <- c(args, criterion = ~c)
    args[["formula"]] <- ~ f1 + f2
  } else if (grepl("gcm", x)) {
    args <- c(args, class = ~c)
    args[["formula"]] <- ~ f1 + f2
  }
  return(
    do.call(x, args)
  )
}

#' Show the constraints of a cognitive model
#' 
#' @description
#' \code{constraints(x)} shows the parameter constraints  of a cognitive model named \code{x} nicely formatted
#' 
#' @usage constraints(x)
#' @param x a model object of class cm
#' @param ... other arguments (ignored)
#' @examples 
#' D <- data.frame(x = 1, y = 1, z = 1)
#' M <- bayes_beta(y ~ x + z, D, fix = "start")
#' constraints(M)    # view the parspace
#' @export
constraints <- function(x, ...) {
  UseMethod("constraints")
}
constraints.default <- function(x, ...) {
  stop("'constraints' method is not defined for object of class ", class(x), ".")
}
#' @name constraints
#' @method constraints cm
#' @export
constraints.cm <- function(x, ...) {
  cat("\nParameter constraints of the cognitive model '", class(x)[1], "':\n", sep = "")
  return(x$constraints)
}