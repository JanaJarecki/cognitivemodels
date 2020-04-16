# ==========================================================================
# Package: Cognitivemodels
# File: utils-checks.R
# Author: Jana B. Jarecki
# ==========================================================================



#' Checks the choicerule
#' 
#' @param x the name of the choicerule
#' @export
#' @noRd
.check_and_match_choicerule <- function(x = NULL) {
  if (!length(x)) {
    stop("Must supply a 'choicerule'.\n  * Set choicerule to 'none' to not apply a choicerule.\n  * Allowed values are 'softmax', 'luce', 'epsilon'")
    }
  x <- match.arg(x, c("none", "softmax", "argmax", "luce", "epsilon"))
  return(x)
}





#' Checks and optionally installs missing solvers
#' 
#' @param solver_name the name of the solver
#' @export
#' @noRd
.match_and_check_solver <- function(solver) {
  roi_solvers <- gsub("ROI.plugin.", "", ROI::ROI_available_solvers()$Package)
  roi_registered <- names(ROI::ROI_registered_solvers())
  roi_solvers <- unique(c(roi_solvers, roi_registered))
  matched_solver <- try(match.arg(solver, c("grid", "solnp", "auto", roi_solvers), several.ok = TRUE), silent = TRUE)
  if (inherits(matched_solver, "try-error")) {
    stop("'solver' must be one of the following, rather than '", solver, "':\n    ", paste(sort(c("solnp", "auto", roi_solvers)), collapse = "\n    "))
  } else {
    solver <- unique(matched_solver)
  }
  if (length(solver) > 2L) {
    stop("'solver' can have 2 entries, but it has ", length(solver), ".")
  }
  if (length(solver) == 2L) {
    if (!any(grepl("grid", solver))) {
      warning("Dropped the second solver '", solver[2], "', using only '", solver[1], "'.", call. = FALSE) 
    } else if (solver[2] == "grid") {
      solver <- solver[2:1]
      warning("Using solver 'grid' first, followed by '", solver[2], "'.", call. = FALSE)
    }
  }
  
  missing <- is.na(match(solver, c("grid", "solnp", "auto", roi_registered)))
  if (any(missing)) {
    install <- menu(c("Yes", "No, stop the model."), title = paste0("The solver '", solver[missing], "' is not (yet) installed. Want to install it?"))
    if (install == 1) {
      install.packages(paste0("ROI.plugin.", solver[missing]))
      return(solver)
    } else {
      stop("Model stopped, because the ROI solver plugin was not (yet) installed. \n  * See which solvers you got: ROI::ROI_registered_solvers()\n  * Do you want to change the solver?", call. = FALSE)
    }
  } else {
    return(solver)
  }
}
