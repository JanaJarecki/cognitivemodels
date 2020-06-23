#' @param fix (optional) A list or a string, parameter constraints:
#'   * `"start"` constrains all parameters to their starting values. The model estimates no parameters. Useful for model testing.
#'   * `list(k = 0.50)` constrains a parameter _k_ to 0.50  (parameter names, see details).
#'   * `list(k = "delta")` constrains a parameter _k_ to equal a parameter _delta_.
#'   * `list(k = NA)` omits parameter _k_, if the model works without _k_.
