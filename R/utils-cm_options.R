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
#' @param lb A named numeric vector, minimum parameter values; e.g., to let a parameter _k_ take values from -10 set `lb = c(k = -10)`.
#' @param ub A named numeric vector, maximum parameter values; e.g., to let a parameter _k_ take values up to 10, set  `ub = c(k = 10)`.
#' @param fit Logical (default `TRUE`), `FALSE` omits parameter fitting. Useful for testing models.
#' @param fit_measure A string (default `"loglikelihood"`), the fit measure to use during parameter estimation, can be one of the `types` in the function \code{\link[cognitiveutils]{gof}}, e.g. \code{"mse"}.
#' @param fit_n An integer, if the data that is being predicted is aggregated data, supply the number subjects underlying each data point.
#' @param fit_data A data frame, the data based on which the parameters are being estimated. Useful if the data used in estimation differs from the data in the main `data` argument in a model.
#' @param solver A string, the algorithm for optimizing the parameters during estimation, use `solvers()` to list the available options. `"grid"` uses a grid-search, `"solnp"` uses \code{\link[Rsolnp]{solnp}}, other solvers contained in the R optimization infrastructure can be named (see \link{ROI}). This may cause warnings about ignored options and may cause parameter bounds to be ignored and the model to fail. Can also be `c("grid", "xxx")` which uses a grid search followed by a second solver xxx, which is executed repeatedly with the best parameters from the grid search as start values; xxx mus be an allowed solver.
#' @param fit_grid_offset A small number, amount to offset the parameter values in a grid-search from the parameter boundaries.
#' @param fit_args A list, additional arguments to be passed to the fit solver specified in `solver`, see the respective pages of the solver to see which arguments are allowed.
#'  @param fit_control A list, the argument `control` passed to a the solver, allowed values depend on the `solver` that is being used, see the respective pages of the solvers for details.
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