# ==========================================================================
# Package: Cognitivemodels
# File: utils-constraints.R
# Author: Jana B. Jarecki
# ==========================================================================

# ==========================================================================
# Utilities for working with parameter constraints
# ==========================================================================

#' Makes constraints
#' 
#' Transforms the fixed parameter into constraints
#' 
#' @param parspace A parameter space matrix, see make_parspace()
#' @param fix A list of fixed parameters
#' @noRd
.make_constraints <- function(parspace = NULL, fix = NULL, par = NULL) {
  if (is.null(fix)) {
    return(NULL)
  }
  pnames <- rownames(parspace)
  fnames <- names(fix)
  n <- length(pnames)
  A <- matrix(0L, nrow = length(fix), ncol =  n, dimnames = NULL)
  
  # ignored parameter
  Cna <- NULL
  if (anyNA(fix)) {
    nc <- sum(is.na(fix))
    cs <- which(pnames %in% fnames[is.na(fix)])
    L <- A[1:nc, , drop=F]
    L[cbind(1:nc, cs)] <- 1L
    Cna <- ROI::L_constraint(
      L = L,
      rhs = parspace[fnames[is.na(fix)], 4L],
      dir= rep("==", nc),
      names = pnames
    )
  }
  
  # constant/fixed parameter
  Cf <- NULL
  if (any(sapply(fix, is.numeric))) {
    nc <- sum(sapply(fix, is.numeric))
    cs <- match(fnames[sapply(fix, is.numeric)], pnames)
    L <- A[1:nc, , drop = FALSE]
    L[cbind(1:nc, cs)] <- 1L
    Cf <- ROI::L_constraint(
      L = L,
      rhs = unlist(fix[sapply(fix, is.numeric)]),
      dir= rep("==", nc),
      names = pnames
    )
  }
  
  # equal to other parameter
  Ce <- NULL
  if (any(sapply(fix, is.character))) {
    nc <- sum(sapply(fix, is.character))
    cs <- which(pnames %in% fnames[sapply(fix, is.character)])
    ct <- match(fix[sapply(fix, is.character)], pnames)
    L <- A[1:nc, , drop=F]
    L[cbind(1:nc, cs)] <- 1L
    L[cbind(1:nc, ct)] <- -1L
    Ce <- ROI::L_constraint(
      L = L,
      rhs = rep(0L, nc),
      dir= rep("==", nc),
      names = pnames
    )
  }
  C <- .combine_constraints(Cna, Cf, Ce)
  return(C)
}

#' Check the consistency of two constraints
#' 
#' @param x Object of the class "constraint" from the ROI package (see \link[ROI]{L_constraint})
#' @param y Object of the class "constraint" from the ROI package (see \link[ROI]{L_constraint})
#' @noRd
.consistent_constraints <- function(x = NULL, y = NULL) {
  x <- .combine_constraints(x, y)
  if (is.null(x)) { return(TRUE) }
  A <- as.matrix(x$L)
  b <- unname(x$rhs)
  # Drop unconstrained parameter
  A <- A[, !apply(A==0L, 2, all), drop=FALSE]
  # fixme: this is a hack b/c matlib::R breaks with 1-d matrices 
  #        https://github.com/friendly/matlib/issues/34
  if (all(dim(A) == c(1,1))) { return(TRUE) }
  if (isTRUE(all.equal( matlib::R(A), matlib::R(cbind(A, b)) ))) {
    return(TRUE)
  } else {
    cat("A constraint is violated:\n")
    print(y)
    cat("\n")
    stop("Parameters in 'fix' must fulfil the constraints (above), but don't.\n  * Do you need to change the parameters in 'fix'?", call. = FALSE)
  }
}


#' Combine multiple constraints of type "constraint" from ROI
#' Combine constraints, ignore \code{NULL} constraints
#' 
#' @param ... Objects of the class "constraint" from the ROI package (see \link[ROI]{L_constraint})
#' @return An object of class "constraint" with all constraints or \code{NULL} if all \code{...} are \code{NULL}.
#' @noRd
.combine_constraints <- function(...) {
  C <- list(...)
  C <- Filter(Negate(is.null), C)
  if (length(C)) {
    #fixme this is a hack because the ROI package has a tiny bug in the rbind function
    C <- lapply(C, function(x) {
      if (class(x)[1] == "csm_constraint") class(x) <- class(x)[-1L]
      return(x)
    })
    C <- do.call(ROI:::rbind.constraint, C)
    C <- .as.csm_constraint(C)
    return(C)
  }
}


#' Simplify constraints
#' 
#' Simplify a constraint by removing the fully-determined parameter
#' 
#' @param x An object of type L_constraint from the package ROI
#' @noRd
.simplify_constraints <- function(x) {
  if (length(x) == 0) { return(x) }
  A <- as.matrix(x$L)
  rhs <- x$rhs
  # Get parameter that are under-determined by constraints x
  qrcoef <- qr.coef(qr(A), rhs)
  # candidates for parameters that HAVE _NO_ solution
  ids <- which(is.na(qrcoef))
  # which rows of A to keep (i.e. under-constraint parameters)
  .rows <- rowSums(A[, ids, drop = FALSE] != 0L) > 0L
  .cols <- colSums(A[.rows, , drop = FALSE] != 0L) > 0L | (colSums(A == 0L) == nrow(A))
  
  C <- ROI::L_constraint(
    L = A[.rows, .cols, drop = FALSE],
    dir = x$dir[.rows],
    rhs = x$rhs[.rows],
    names = x$names[.cols])
  C <- .as.csm_constraint(C)
  return(C)
}

#' Coerce to constraint object
#' 
#' @param x An object of type constraint from the package ROI
#' @noRd
.as.csm_constraint <- function(x = NULL) {
  if (!length(x)) return(x)
  if (!inherits(x, "csm_constraint")) {
    class(x) <- c("csm_constraint", class(x))
  }
  return(x)
}



#' Solve constraints
#' 
#' Gets the solution to a constraint for the fully-determined parameter
#' 
#' @param x An object of type L_constraint from the package ROI
#' @noRd
.solve_constraints <- function(x, b) {
  if (length(x) == 0) { return(x) }
  A <- as.matrix(x$L)
  colnames(A) <- x$names
  b <- x$rhs
  decomp <- qr(A)
  R <- qr.R(decomp)
  if (nrow(A) <= ncol(A)) {
    # Identify the rows with solutions:
    unsolved <- seq_len(ncol(A))
    repeat {
      solved <- unsolved[apply(R[, unsolved, drop = FALSE] != 0, 1,
                               function(row) if (sum(row) == 1) which(row) else NA)]
      if (all(is.na(solved))) break
      unsolved <- setdiff(unsolved, solved)
    }
    solved <- setdiff(seq_len(ncol(A)), unsolved)
    if (length(solved) == 0) {
      return(NULL)
    } else {
      # Find the solutions:
      Q <- qr.Q(decomp)
      Qtb <- t(Q) %*% b
      return(solve(R[solved, solved, drop = FALSE], Qtb[solved]))
    }
  } else {
    s <- qr.solve(A, b)
    if(!isTRUE(all.equal(c(A %*% s), b))) {
      stop("Parameter constraints are inconsistent:\n ", x, call. = FALSE)
    }
  }   
}

#' 
# #' 
# .solve_constraints <- function(x, b) {
#   if (length(x) == 0) { return(x) }
#   A <- as.matrix(x$L)
#   colnames(A) <- x$names
#   if (nrow(A) != ncol(A)) {
#     b <- c(x$rhs, rep(0, ncol(A)-nrow(A)))
#     A <- rbind(A, matrix(0L, nrow=ncol(A)-nrow(A), ncol=ncol(A)))
#   } else if (identical(unname(A), diag(1, ncol(A)))) {
#     return(solve(A, x$rhs))
#   } else {
#     b <- x$rhs
#   }
  

#   # fixme this is a hack
#   if (nrow(A) == 1 & sum(A != 0) == 1) {
#     return(setNames(solve(A[A != 0], b), x$names[A != 0]))
#   }
#   # Get parameter that are under-determined by constraints x
#   solvable <- svd(A)$d != 0
#   qrsol <- try(qr.solve(A[,solvable,drop=FALSE],b), silent = TRUE)
#   if (inherits(qrsol, "try-error")) {
#     stop("'fix' over-constrains the parameters.\n", if(nrow(A) == ncol(A)) {"  * Are your constraints circular (a=b, b=a)?"}, "\n  * The original error is:\n    ", qrsol, call.=FALSE)
#   } else {
#     s <- qrsol
#     return(round(s, 32))
#   }
# }
# A <- rbind(c(0,0,0), c(1,0,0), c(0,0,0))
# svd(A)

#' Prints the constraints of a cogscimodel object nicely
#' 
#' or NULL if there are no constraints
#' 
#' @param latex (optional) if \code{TRUE} formats them for LaTeX documents
#' @export
print.csm_constraint = function(x, ..., latex = FALSE) {
  ROI:::print.constraint(x)
  if (length(x) == 0) return(NULL)
  A <- as.matrix(x$L)
  b <- x$rhs
  # We use the side-effect of printing in showEqn()
  sapply(1:nrow(A), function(i) {
    cat("  ")
    matlib::showEqn(
      A = A[i, A[i, ] != 0L, drop=FALSE],
      b = b[i],
      vars = x$names[A[i, ] != 0L],
      latex = latex)
  })
  return(x)
}


#' Returns the constraints that are under-determined
#' 
#' @param x An object of type constraint from the package ROI
#' @noRd
.which_underdetermined <- function(x) {
  if (is.null(x) || missing(x)) {
    return(NULL)
  }
  A <- as.matrix(x$L)
  b <- x$rhs
  A <- matlib::echelon(A,b)
  A <- A[, -ncol(A), drop = FALSE]
  a <- which(apply(A == 0, 2, all))
  b <- which(colSums(A[which(rowSums(A) != 1), , drop = FALSE]) != 0)
  return(sort(unique(c(a,b))))
}

#' Returns the index of the parameter that is free without any constraints
#' 
#' @param x An object of type constraint from the package ROI
#' @noRd
.which_free <- function(x) {
  if (is.null(x) || missing(x)) { return(NULL) }
  A <- as.matrix(x$L)
  b <- as.matrix(x$rhs)
  A <- matlib::echelon(A,b)
  A <- A[, -ncol(A), drop = FALSE]
  # Parameter that are in no constraints or the first parameter of a constr.
  # Todo test if the .which_free functino works properly
  a <- apply(A == 0, 2, all)
  b <- apply(A != 0, 1, function(x) (sum(x) > 1) & cumsum(x) == 1)
  b <- apply(as.matrix(b), 1, any)
  return(a | b)
}

# Todo Test the .which_fixed function
#' Returns the indexes of parameters that take a fixed value
#' 
#' @param x An object of type constraint from the package ROI
#' @noRd
.which_fix <- function(x) {
  if (is.null(x) || missing(x)) { return(NULL) }
  setdiff(seq.int(x$L$ncol), .which_underdetermined(x))
}

# Todo Test the .which_constrained function
#' Returns the indexes of parameters that have constraints on them
#' 
#' @param x An object of type constraint from the package ROI
#' @noRd
.which_constrained <- function(x) {
  if (is.null(x) || missing(x)) { return(NULL) }
  setdiff(seq.int(x$L$ncol), .which_free(x))
}