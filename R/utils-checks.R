# ==========================================================================
# Package: Cognitivemodels
# File: utils-checks.R
# Author: Jana B. Jarecki
# ==========================================================================

# ==========================================================================
# Utility functions for checking sanity of model inputs
# ==========================================================================



#' Checks the choicerule
#' 
#' @importFrom utils menu
#' @importFrom utils install.packages
#' 
#' @param x the name of the choicerule
#' @export
#' @noRd
.check_and_match_choicerule <- function(x = NULL, mode) {
  mode <- match.arg(mode, c("continuous", "discrete"))
  if (mode == "continuous") {
    if (length(x) > 0 && inherits(try(match.arg(x,"none"),silent=T),"try-error")) {
      warning("Info: dropped 'choicerule = \"", x, "\"', because model models continuous responses.", call. = FALSE)
    }
    return("none")
  }
  allowed <- cm_choicerules(msg = FALSE)
  if (!length(x)||inherits(try(match.arg(x,"none"),silent=TRUE),"try-error")) {
    stop("Argument 'choicerule' must take an allowed value, instead of \"", x, "\".\n  * Allowed values are ", .dotify(dQuote(allowed)), ".\n  * To use no choicerule, set choicerule = \"none\".", call. = FALSE)
  }
  x <- match.arg(x, c("none", allowed))
  return(x)
}

#' Checks the parameter values
#' 
#' @param x A vector or list with parameters to fix
#' @param pass Logical, whether to pass this check
#' @export
#' @noRd
.check_par <- function(x = NULL, parspace, pass = FALSE) {
  # Formal checks
  if (pass == TRUE | length(x) == 0L) { return() }
  if (is.character(x)) { if(x[1] == "start") { return() }}
  if (length(x) & all(is.numeric(x))) { x <- as.list(x) }
  if (length(x) > 1L & !is.list(x)) { 
    stop("Parameters to fix must be a list, not a ", typeof(x), ".\n  * Did you forget to supply a list to 'fix'? fix = list( ... )?", call.=FALSE)
  }
  if (length(x) != sum(sapply(x, length))) {
    stop("Parameters to fix must be a list with 1 parameter per list entry, but the ", which(lapply(x, length) > 1L), ". entry of 'fix' has multiple parameters.\n  * Do you need to change the format of 'fix'?", call. = FALSE)
  }
  if (any(duplicated(names(x)))) {
    stop("Names of fixed parameters must be unique, but 'fix' contains ", .dotify(sQuote(names(x)[duplicated(names(x))])), " ", sum(duplicated(names(x))) + 1, " times.", call. = FALSE)
  }
  # apply the check par function iteratively if par length > 1
  if (length(x) > 1L) {
    Map(function(x, i) .check_par(x = setNames(x, i), parspace), x, names(x) )
    return()
  }

  .check_parnames(x = names(x), y = rownames(parspace), pass = pass)
  .check_parvalues(x = x, y = parspace, pass = pass)
  .check_fixvalues(x = x, y = parspace, pass = pass)
}


.check_parnames <- function(x, y, pass = FALSE) {
  x <- unlist(x)
  if (pass == TRUE) { return() }
  if (!x %in% y) {
    stop("Parameter names must be ", .brackify(dQuote(y)), ", not ", dQuote(x), ".\n  * ", .didyoumean(x, y), call. = FALSE)
  }
}

.check_parvalues <- function(x, y, n = names(x), pass = FALSE) {
  x <- unlist(x)
  if (pass == TRUE | is.na(x) | is.character(x)) { return() }
  y <- y[n, ]
  tolerance <- sqrt(.Machine$double.eps)
  if (x < (y["lb"] - tolerance) | (x > y["ub"] + tolerance)) {
    stop("Parameter ", sQuote(n), " must be between ", y["lb"], " and ", y["ub"], ".\n  * Did you accidentally fix '", n, " = ", x, "'?\n  * Would you like to change the parameter range? options = list(", ifelse(x > y["ub"] + tolerance, "ub", "lb"), " = c(", n, " = ", x, ")", call.=FALSE)
    }
}


#' Checks the fixed parameter
#' 
#' @param x the fixed parameter
#' @param y the parameter space object
#' @export
#' @noRd
.check_fixvalues = function(x, y, pass = FALSE) {
  x <- unlist(x)
  if (pass == TRUE | length(x) == 0L) { return() }
  if (is.character(x)) {
    if (names(x) == x) {
      stop("Fixed  parameter (equality-constrained) must be equal to another parameter, not itself. \n  * Did you accidentally fix ", names(x), " = ", dQuote(x), "?", call. = FALSE)
    }
   if (!x %in% rownames(y)) {
      stop("Fixed parameter (equality-constrained) must be equal to one of ", .dotify(dQuote(rownames(y))), ".\n  * Did you accidentally fix ", names(x), " = ", dQuote(x), "? ", .didyoumean(x, setdiff(rownames(y), names(x))), call. = FALSE)
    }
  }
  if (is.na(x) & is.na(y[names(x), "na"])) {
      stop("Fixed parameter ", sQuote(names(x)), " can't be NA and thereby ignored, because the model needs the parameter ", sQuote(names(x)), ".\n  * Do you want to fix ", sQuote(names(x)), " to be between ", paste(y[names(x), c("lb","ub")], collapse = " and "), "?", call. = FALSE)
    }
}

#' Prints the possible optimization solvers
#' 
#' @noRd
solvers <- function() {
  warning("Function 'solvers()' is depreciated, use 'cm_solvers()' instead.")
  #   "clp"          "deoptim"   "glpk"     "lpsolve"  "neos"     "nloptr" "symphony" "cccp"     ipop"  "mosek"    "cbc"      "nlminb"

  # "nloptr.lbfgs"

  # out: "alabama", "glpk", "qpoases", "gurobi", "scs", "msbinlp", "optimx", "ecos", "quadprog", "cplex"

  roi_solvers <- suppressWarnings(try(ROI::ROI_available_solvers()$Package, silent = TRUE))
  if (!inherits(roi_solvers, "try-error")) {
      roi_solvers <- gsub("ROI.plugin.", "", roi_solvers)
  } else {
    roi_solvers <- NULL
  }
  roi_registered <- names(ROI::ROI_registered_solvers())
  roi_solvers <- unique(c(roi_solvers, roi_registered))
  # hack because not clear what the solvers are
  roi_solvers <- c(
    grep("nloptr", roi_solvers, value=T, invert=T),
    paste0("nloptr.", c("lbfgs", "mma", "auglag", "bobyqa", "cobyla", "crs2lm", "direct", "directL", "isres", "lbfgs", "neldermead", "newuoa", "sbplx", "slsqp", "stogo", "tnewton", "varmetric")))
  return(sort(c("grid", "solnp", "auto", roi_solvers)))
}



#' Checks and matches the solver name
#' 
#' @param solver the name of the solver
#' @export
#' @noRd
.check_and_match_solver <- function(solver) {
  if (is.null(solver)) solver <- "auto"
  solver <- tolower(solver)
  allowed <- cognitivemodels::cm_solvers(msg = FALSE)
  for (s in solver) {
    if (inherits(try(match.arg(s, allowed), silent = TRUE), "try-error")) {
      stop("'solver' must be a valid name, not ", dQuote(setdiff(s, allowed)), ".\n  * ", .didyoumean(s, allowed), "\n  * Would you like to see all valid names? cm_solvers()", call. = FALSE)
    }
  }
  
  solver <- unique(match.arg(solver, allowed, several.ok = TRUE))
  if (length(solver) > 2L) {
    stop("'solver' must have 2 entries, not ", length(solver), ".")
  }
  if (length(solver) == 2L) {
    if (!any(grepl("grid", solver))) {
      warning("Dropped the second solver '", solver[2], "', using only '", solver[1], "'.", call. = FALSE) 
    } else if (solver[2] == "grid") {
      solver <- solver[2:1]
      warning("Using solver 'grid' first, then '", solver[2], "'.", call. = FALSE)
    }
  }
  return(solver)
}

#' Optionally installs missing solver
#' 
#' @param solver the name of the solver
#' @export
#' @noRd
.install_solver_if_missing <- function(solver) {
  # because ROI plugins names are the first string before the "."
  missing <- is.na(match(solver, c("grid", "solnp", "auto", names(ROI::ROI_registered_solvers()))))
  print
  if (any(missing)) {
    plugin <- gsub("\\..*", "", solver)
    install <- utils::menu(c("Yes", "No, stop the model."), title = paste0("The package for solver '", plugin[missing], "' is not yet installed. Want to install it? (Type 1 or 2)"))
    if (install == 1) {
      install.packages(paste0("ROI.plugin.", plugin[missing]))
      library(paste0("ROI.plugin.", plugin[missing]), character.only=TRUE)
      return(solver)
    } else {
      stop("Model stopped, because the ROI solver plugin was not (yet) installed. \n  * Would you like to see the solvers plugins that are installed, ROI::ROI_registered_solvers()?\n  * Would you like to change the solver, options = list(solver = \"...\")?", call. = FALSE)
    }
  }
}

solver <- "nloptr.stogo"






  # if (length(fix) < nrow(parspace) & is.null(self$res) & self$options$fit == TRUE ) {
  #   stop("'formula' must have a left side to estimate parameter ", .brackify(setdiff(rownames(parspace), names(fix))), ".\n  
  #       * Did you forget to add a left-hand to the formula?\n  
  #       * Did you forget to fix the parameter ", .dotify(setdiff(rownames(parspace), names(fix))), "?", call. = FALSE)
  #   }
