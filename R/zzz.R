# ==========================================================================
# Package: Cogscimodels
# File zzz.R
# Author: Jana B. Jarecki
# Changed: 2019-12-13
# ==========================================================================

.onUnload <- function (libpath) {
  # Whenever you use C++ code in your package, you need to clean up after yourself when your package is unloaded
  # see http://r-pkgs.had.co.nz/src.html#cpp
  library.dynam.unload("cogscimodels", libpath)
}


# ==========================================================================
# Cogscimodel options
# ==========================================================================
 #' This is the list of options that can be used in cogscimodels. Pass them as list to the argument options in cogscimodels.
#' 
#' @param fit Logical (default \code{TRUE}), \code{FALSE} omits parameter fitting. Useful for testing models.
#' @param fit_measure A string (default \code{"loglikelihood"}), fit measure to use, can be one of \code{types} in the function \code{\link[cogsciutils]{gof}}, e.g. \code{"mse"}, \code{"sse"}.
#' @param fit_n An integer, if the data that is being predicted is aggregated data, supply the number subjects underlying each data point.
#' @param fit_data A data frame with data other than the main \code{"data"} argument to be used for fitting.
#' @param fit_solver Algorithm used to do the parameter estimation. \code{"grid"} uses a grid-search, \code{"solnp"} uses \code{\link[Rsolnp]{solnp}}, other solvers contained in the R optimization infrastructure can be named (see \code{\link{ROI}}), for example \code{"optimx"} or \code{"nlminb"}, see the page of ROI. This may cause warnings about ignored options. THis may cause parameter bounds to be ignored and the model to fail.
#' @param fit_grid_offset A number to offset the grid-search from the boundaries.
#' @param fit_control A list of control arguments, depends on the \code{"fit_solver"} being used, see the respective pages of the solvers.
#' @param fit_args A list of arguments to be passed to the fit solver, \code{"fit_solver"} being used, see the respective pages of the solvers.
#' @export
cogscimodel_options = function(
  lb = NULL,
  ub = NULL,
  start = NULL,
  fit = TRUE,
  fit_measure = "loglikelihood",
  fit_n = 1L,
  fit_data = NULL,
  fit_solver = "auto",
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
    fit_solver,
    fit_grid_offset,
    fit_control,
    fit_args)
  class(out) <- "cogscimodel_options"
  return(out)
}