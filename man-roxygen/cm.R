#' @param formula A \link{formula} such as \code{y ~ x1 + p1 + x2 | y1 + py + y2} specifying the columns in \code{data} that contain outcomes, probabilities and (optional) the observations. Lines (\code{|}) separate different gambles. The formula must alternate outcomes and probabilities (x + p + x2 + p2), the last probability can be omitted.
#' @param data A data frame.
#' @param fix (optional) A list or a string, parameter constraints:
#'   * `"start"` constrains all parameters to their starting values. The model estimates no parameters. Useful for model testing.
#'   * `k = 0.50` constrains a parameter _k_ to 0.50  (parameter names, see details).
#'   * `k = "delta"` constrains a parameter _k_ to equal a parameter _delta_.}
#'   * `k = NA` tells the model to omit parameter _k_, if the model works without _k_.
#' @param options (optional) A list, options to control the parameter fitting methods, see the "Options" section of \code{\link{cm_options}}.