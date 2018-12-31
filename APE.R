#' Calculate root-mean-squared error RMSE (aka root-mean-square deviation, RMSD) and root-weighted-mean-square ereror between two variables
#' @import stats
#' @param obs A vector of responses or a binary matrix with a column for each response option
#' @param pred A vector of predictions or a numeric matrix with predictions, in the same order as obs
#' @param method "vector" (default), "mean", or "median", if "vector" the function returns the vector of absolute percentage errors, if "mean" the function returns the mean absolute percentage error, if "median" the function returns the median absolute percentage error
#' @param discount A number or integer vector to ignore observations. If it is a number n, the first \code{discount} observations will be ignored. If it is a vector all observations at the positions specified in \code{discount} will be ignored. For example, \code{discount = c(1, 10)} ignores the 1st and 10th observations.
#' @param ... other arguments are ignored
#' @return Absolute percentage error between \code{obs} and \code{pred}
#' @examples
#' # None so far

#' @export
APE <- function(obs, pred, method = "vector", discount = 0, ...) {
  if (is.matrix(pred)) {
    if (is.vector(obs)) {
      pred <- pred[1:nrow(pred) + nrow(pred) * (obs-1)]
    }
    if (is.matrix(obs)) {
      pred <- pred[as.logical(obs)]
    }
    obs <- rep(1, times = length(pred))
  }
  if (is.matrix(obs)) {
    obs <- rep(1, times = length(pred))
  }
  
  if (discount[1] > 0) {
    pred <- pred[-discount]
    obs <- obs[-discount]
  }

  # Error measures
  out <- abs((pred - obs)/obs)
  if(method == "mean") {
    out <- mean(out)
  }
  if(method == "median") {
    out <- median(out)
  }
  
  return (out)
}
