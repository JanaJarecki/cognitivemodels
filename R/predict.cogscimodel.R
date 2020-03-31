#' Predictions from Cognitive Models
#' 
#' @param obj A cognitive models with class cogscimodel
#' @param ... Additional cognitive models
#' @param type A string specifying what to predict, usually it is the \code{"response"}, but other values may be possible, see the help pages of the model being used
#' @param newdata A data.frame with new data to predict
#' 
#' @return A vector or matrix of predictions, if multiple models are supplied using \code{...}, returns a list containing the predictions for each model
#' 
#' @export
predict.cm <- function(obj, ..., type = "response", newdata = NULL) {
  dotargs <- list(...)
  if (missing(obj) & length(dotargs)) {
    obj <- dotargs
  }
  if (is.list(obj) & length(obj) > 1) {
    dotargs <- c(obj[-1], dotargs)
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
    return(lapply(c(obj, dotargs), predict, type = type, newdata = newdata))
  }

  return(obj$predict(type = type, newdata = newdata))
}