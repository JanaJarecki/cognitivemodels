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
#' @param lb Named numeric vector, minimum values that a parameter may take: `c(k = -10)` lets a parameter _k_ start at -10.
#' @param ub Named numeric vector, maximum value that parameters may take: `c(k = 10)` lets a parameter _k_ go until 10.
#' @param start Named numeric vector, start value for parameters: `c(k = 5)` lets a parameter _k_  start at 5 in the optimiuation.
#' @param fit Logical (default `TRUE`), `FALSE` disables parameter fitting. Useful for testing models.
#' @param fit_measure A string (default `"loglikelihood"`), the goodnes of fit measure that is optimized during parameter estimation. Can be one of the `types` in the function \link[cognitiveutils]{gof}:
#' * `"loglikelihood"`. Uses a binomial PDF in models with discrete data. Uses 
#'    a normal PDF \eqn{N(\mu, \sigma)} in models with continuous data:
#'    \eqn{\mu}=predictions, \eqn{\sigma}=constant, estimated as additional free paramter. To change the PDF set `fit_args = list(pdf = "xxx")`
#' * `"mse"` is mean-squared error
#' * `"accuracy"` is percent accuracy
#' @param fit_args A named list, additional arguments for fitting, can be arguments to the function [cognitiveutils::gof], such as
#'   * `list(n = 30)` assumes each row in `data` is the mean of 30 observations. Useful to fit aggregated data.
#'   * `list(pdf = "multinom")` uses a multinomial PDF in the log-likelihood
#'   * `list(pdf = "truncnorm", a = 0, b = 1)` uses a truncated normal PDF in the log-likelihood
#' @param fit_data A data frame, the data to estimate the model parameters from. Needed if the data for the parameter estimation differs from the data in the main `data` argument in a model.
#' @param solver A string, the alorithm to optinize the free parameters. Run \link{cm_solvers()} to list the options. Changing this may cause warnings about ignored options and may cause parameter bounds to be ignored and the model to fail. Examples:
#' * `"grid"` uses a grid search with a regular grid
#' * `"solnp"` uses \link[Rsolnp]{solnp}
#' * ... and 21 other solvers such as `"optimx"`, `"nloptr"`, and `"nlminb"` from \link{ROI} - R optimization infrastructure
#' * `c("grid", "xxx")` uses a grid-plus-optimization, where xxx is a solver: A grid search, followed by an optimization with xxx using the _n_ best solutions as start values in the optimization; the overal best parameter result wins; _n_ can be set by changing `solver_args$nbest`.
#' @param solver_args (optional) A list, additional arguments that are passed **directly** to the optimization solver
#'   * `list(offset = )` A small number by which to offset the parameters from their boundaries when `solver = "grid"`.
#'   * `list(nsteps = )` A number, number of steps for each parameter in the regular grid, for `solver = "grid"`).
#'   * `list(nbest = )` Number of best solutions used as starting values in a grid-plus-optimization, for `solver = c("grid", "xxx")`.
#'   * `list(control = )` control arguments in the solver (solnp)[Rsolnp::solnp()] and the (ROI solvers)[https://rdrr.io/cran/ROI/man/ROI_solve.html]
#' @export
cm_options = function(
  lb = NULL,
  ub = NULL,
  start = NULL,
  fit = TRUE,
  fit_measure = "loglikelihood",
  fit_args = list(),
  fit_data = NULL,
  solver = "auto",
  solver_args = list()) {
  solver_args <- c(solver_args, list(control = list(trace=0L), nbest=1L, nsteps=20, offset = 0L))
  solver_args <- solver_args[!duplicated(names(solver_args))]
  fit_args <- c(fit_args, list(n = 1L))
  fit_args <- fit_args[!duplicated(names(fit_args))]
  fit_measure <- match.arg(fit_measure, c("loglikelihood", "mse", "wmse", "rmse", "sse", "wsse", "mape", "mdape", "accuracy"))

  out <- .named_list(
    lb,
    ub,
    start,
    fit,
    fit_measure,
    fit_args,
    fit_data,
    solver,
    solver_args)
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