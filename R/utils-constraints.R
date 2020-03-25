
# ==========================================================================
# Utilities for working with parameter constraints
# ==========================================================================

#' Makes constraints
#' 
#' Transforms the fixed parameter into constraints
#' 
#' @param parspace A parameter space matrix, see make_parspace()
#' @param fix A list of fixed parameters
#' @NoRd
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
  
  # fixed parameter
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



#' Combine multiple constraints of type "constraint" from ROI
#' Combine constraints, ignore \code{NULL} constraints
#' 
#' @param ... Objects of the class "constraint" from the ROI package (see \link[ROI]{L_constraint})
#' @return An object of class "constraint" with all constraints or \code{NULL} if all \code{...} are \code{NULL}.
#' @NoRd
.combine_constraints <- function(...) {
  C <- list(...)
  C <- Filter(Negate(is.null), C)
  if (length(C)) {
    #fixme this is a hack because the ROI package has a tiny bug in the rbind function
    C <- lapply(C, function(x) {
      if (class(x)[1] == "csm_constraint") class(x) <- class(x)[-1L]
      return(x)} )
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
#' @NoRd
.simplify_constraints <- function(x) {
  if (length(x) == 0) { return(x) }
  A <- as.matrix(x$L)
  rhs <- x$rhs
  # Get parameter that are under-determined by constraints x
  qrcoef <- qr.coef(qr(A), rhs)
  # candidates for a solution
  ids <- which(qrcoef == 0 | is.na(qrcoef))
  # which rows of A to keep
  .rows <- rowSums(A[, ids, drop = FALSE]) != 0
  .cols <- colSums(A[.rows, , drop = FALSE]) | !colSums(A)
  
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
#' @NoRd
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
#' @NoRd
.solve_constraints <- function(x) {
  if (length(x) == 0) { return(x) }
  A <- as.matrix(x$L)
  colnames(A) <- x$names
  rhs <- x$rhs
  # Get parameter that are under-determined by constraints x
  qrcoef <- qr.coef(qr(A), rhs)
  qrcoef[qrcoef == 0L] <- NA
  qr.solve(A, rhs)
  
  
  s <- setNames(qr.solve(A, rhs)[!is.na(qrcoef)], x$names[!is.na(qrcoef)])
  return(s)
}



#' Prints the constraints of a cogscimodel object nicely
#' 
#' or NULL if there are no constraints
#' 
#' @param latex (optional) if \code{TRUE} formats them for LaTeX documents
#' @export
print.csm_constraint = function(x, latex = FALSE) {
  ROI:::print.constraint(x)
  if (length(x) == 0) return(NULL)
  A <- as.matrix(x$L)
  b <- x$rhs
  # We use the side-effect of printing in showEqn()
  sapply(1:x$L$nrow, function(i) {
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
#' @NoRd
.which.underdetermined <- function(x) {
  if (is.null(x) || missing(x)) {
    return(NULL)
  }
  A <- as.matrix(x$L)
  b <- x$rhs
  #fixme: this is a hack b/c matlib:echelon cannot handle
  #       one-row matrices
  if (nrow(A) == 1 & sum(A != 0) == 1) {
    return(A == 0)
  } else {
    A <- matlib::echelon(A,b)  
  }
  A <- A[, -ncol(A), drop = FALSE]
  a <- which(colSums(A) == 0)
  b <- which(colSums(A[which(rowSums(A) != 1), , drop = FALSE]) != 0)
  return(sort(unique(c(a,b))))
}