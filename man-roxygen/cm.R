#' @param data A data frame.
#' @param formula A [formula](stats::formula), the variables in `data` to be modeled. For example, `y ~ x1 + x2 | z1 + z2` models the response `y` as function of one stimulus with features `x1`, `x2` and one stimulus with features `z1`, `z2`. Horizontal lines (`|`) separate different stimuli.
#' @param options (optional) A list to change the parameter estimation, see [cm_options()] or the section Options below.
#' @section Options:
#' The following can be passed in one list, e.g. `options = list(lb = c(k = -10))`.
#' 
#' \describe{
#'  \item{`lb`}{Named numeric vector, sets the lower parameter bounds;
#'        `lb = c(k = -10)` lets a parameter _k_ start at -10.}
#'   \item{`ub`}{Named numeric vector, sets the upper parameter bounds:
#'         `ub = c(k = 10)` lets a parameter _k_ go until 10.}
#'   \item{`start`}{Named numeric vector, sets the start values of parameter:
#'         `start = c(k = 5)` lets a parameter _k_ start at 5.}
#'   \item{`fit`}{Logical (default `TRUE`), `fit = FALSE` disables parameter
#'         estimation. Useful for testing models.}
#'   \item{`fit_measure`}{A string (default `"loglikelihood"`), the goodness
#'        of fit measure that is optimized during parameter estimation.
#'        Can be one of the `types` in the function [cognitiveutils::gof()`:
#'         `"loglikelihood"`. Uses a binomial PDF in models with discrete 
#'        data. Uses a normal PDF \eqn{N(\mu, \sigma)} in models with 
#'        continuous data:  \eqn{\mu}=predictions, \eqn{\sigma}=constant,
#'        estimated as additional free paramter. To change the PDF set
#'       `fit_args = list(pdf = "")}
#'   \item{`fit_args`}{Named list, options for parameter estimation. Can be
#'          arguments to the function (gof())[cognitiveutils::gof()]. 
#'         `list(pdf = "truncnorm", a = 0, b = 1)` changes the PDF
#'          in the log likelihood to a truncated normal between 0 and 1,
#'         `list(pdf = "multinom")` changes it to a multinomial PDF.
#'         `list(grid_offset = .01)` offsets the parameter in a grid search
#'         by 0.01 from the parameter boundaries, `list(nsteps = 10)` defines
#'         10 steps for each parameter in the regular grid in the grid search.}
#'   \item{`fit_data`}{A data frame, the data to estimate the model parameters
#'         from. Needed if the data for the parameter estimation differs from
#'         the data in the main `data` argument in a model.}
#'   \item{`solver`}{A string, the optimizer for the parameter estimation. Run
#'        `cm_solvers()` to list all solvers. Can be `"grid"`, `"solnp"` 
#'        `"optimx"`, `"nloptr"`, `"nlminb"` and others from [ROI]. Can be
#'       `c("grid", "xxx")`: a grid-plus-optimization: A grid search, followed
#'       by an optimization with xxx using the _n_
#'       best solutions as start values in the optimization; the overal best 
#'      parameter set wins; and _n_ can be set in `solver_args$nbest`
#'       Changing the solver may cause warnings and ignored parameter bounds.}
#'   \item{`solver_args`}{A named list, additional arguments passed directly
#'         to the solver function, see the pages of the solver to see which
#'         arguments the solver function has. For example:
#'        `list(offset = 0.01)` offset the parameters from their boundaries
#'         when `solver = "grid"`.
#'        `list(nsteps = 10)` uses 10 steps for each parameter in the regular
#'         grid, for `solver = "grid"`), `list(nbest = 3)` uses the 3 best
#'         parameter sets from the grid search as starting values in a
#'         grid-plus-optimization solver, for `solver = c("grid", "xxx")`.
#'         `list(control = )` control arguments in the solver
#'          (solnp)[Rsolnp::solnp()] and the
#'          (ROI solvers)[https://rdrr.io/cran/ROI/man/ROI_solve.html]}
#' }
#' @return Returns a cognitive model object, which is an object of class [cm](Cm). A model, that has been assigned to `m`, can be summarized with `summary(m)` or `anova(m)`. The parameter space can be viewed using `parspace(m)`, constraints can be viewed using `constraints(m)`.
#' @family cognitive models
