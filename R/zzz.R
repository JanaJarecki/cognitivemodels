cogscimodel_options = function(
  fit_measure = "loglikelihood",
  fit_n = 1L,
  fit_data = NULL,
  fit_solver = "auto",
  fit_grid_nbest = 1L,
  fit_grid_nsteps = 10,
  fit_grid_offset = 0L,
  fit_options = list(trace = 0)) {

  out <- .named_list(
    fit_measure,
    fit_n,
    fit_data,
    fit_solver,
    fit_grid_nsteps,
    fit_grid_nbest,
    fit_grid_offset,
    fit_options)
  class(out) <- "cogscimodel_options"
  return(out)
}