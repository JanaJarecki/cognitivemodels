# ==========================================================================
# Package: Cognitivemodels
# File: utils-cm_options.R
# Author: Jana B. Jarecki
# ==========================================================================

# ==========================================================================
# Cognitivemodel options
# ==========================================================================

#' Advanced Options for Cognitive Models
#' 
#' @param lb A named numeric vector, minimum values that a parameter may take. E.g., `c(k = -10)` lets a parameter _k_ start at -10.
#' @param ub A named numeric vector, maximum value that parameters may take. E.g., `c(k = 10)` lets a parameter _k_ go until 10.
#' @param fit Logical (default `TRUE`), `FALSE` disables parameter fitting. Useful for testing models.
#' @param fit_measure A string (default `"loglikelihood"`), the goodnes of fit measure that is optimized during parameter estimation. Can be one of the `types` in the function \link[cognitiveutils]{gof}:
#' * `"loglikelihood"` is log likelihood which uses a
#'   * ... binomial PDF in discrete-data models.
#'   * ... normal PDF in continuous-data models, this adds a free parameter `"sigma"`, the standard deviation of the PDF, estimated from the data.
#' * `"mse"` is mean-squared error
#' * `"accuracy"` is percent accuracy
#' @param fit_n An integer, the number of data points underlying aggregated data. If the data that is being predicted is aggregated data, `n` is the number observations underlying each data point in the aggregated data.
#' @param fit_data A data frame, the data to estimate the model parameters from. Needed if the data for the parameter estimation differs from the data in the main `data` argument in a model.
#' @param solver A string, the alorithm used for parameter estimation,  \link{cm_solvers()} lists the options. Changing this may cause warnings about ignored options and may cause parameter bounds to be ignored and the model to fail.
#' * `"grid"` uses a grid-search
#' * `"solnp"` uses \link[Rsolnp]{solnp}
#' * ... and 21 other solvers such as `"optimx"`, `"nloptr"`, and `"nlminb"` from \link{ROI} - R optimization infrastructure
#' * `c("grid", "abcde")`, where abcde is a solver, performs a grid search, followed by an optimization with the solver. The solver optimizes n times, using the n best parameter sets that resulted from the grid search as start values. The overal best parameter result wins.
#' @param fit_grid_offset A small number, amount to offset the parameter values in a grid search from the parameter boundaries.
#' @param fit_args A list, additional arguments passed **directly** to the solver function, see the respective pages of the solver that you want to use, to see which arguments the solver function has.
#' @param fit_control A list, arguments passed to the **`control`** argument in the solver function, allowed values depend on the `solver` that you want to use to for the control options.
#' @export
cm_options = function(
  lb = NULL,
  ub = NULL,
  start = NULL,
  fit = TRUE,
  fit_measure = "loglikelihood",
  fit_n = 1L,
  fit_data = NULL,
  solver = "auto",
  fit_grid_offset = 0L,
  fit_control = list(),
  fit_args = list()) {
  fit_control <- c(fit_control, list(trace=0L, nbest=1L, nsteps=20))
  fit_control <- fit_control[!duplicated(names(fit_control))]
  fit_measure <- match.arg(fit_measure, c("loglikelihood", "mse", "wmse", "rmse", "sse", "wsse", "mape", "mdape", "accuracy"))

  out <- .named_list(
    lb,
    ub,
    start,
    fit,
    fit_measure,
    fit_n,
    fit_data,
    solver,
    fit_grid_offset,
    fit_control,
    fit_args)
  class(out) <- "cm_options"
  return(out)
}

#' Show the Choicerules for Discrete Cognitive Models
#' 
#' @usage cm_choicerules()
#' @examples
#' cm_choicerules()
#' @export
cm_choicerules <- function() {
  cat("\nAvailable choice rules:\n")
  return(sort(c("softmax", "luce", "epsilon", "argmax")))
}

#' Show the Optimization Solvers for Cognitive Models
#' 
#' @usage cm_solvers()
#' @examples
#' cm_solvers()
#' @export
cm_solvers <- function() {
  cat("\nAvailable optimization solvers:\n")
  return(solvers())
}