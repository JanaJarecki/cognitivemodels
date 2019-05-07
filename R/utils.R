# Exact variance between two gambles (without N-1 correction)
varG <- function(p, x) {
  if ( is.matrix(p)) {
    return(sapply(1:dim(p)[1], function(i) varG(p[i, ], x[i, ])))
  }
  ev <- c(x %*% p)
  p %*% ((x - ev)^2)
}

# Whenever you use C++ code in your package, you need to clean up after yourself when your package is unloaded
# see http://r-pkgs.had.co.nz/src.html#cpp
.onUnload <- function (libpath) {
  library.dynam.unload("cogscimodels", libpath)
}


# convert a vector like c(1, 4, 3, 2) into a string like [1, 4, 3, 2]
#   (common aggregation method for error messages)
# From data.table
.brackify = function(x) {
  # arbitrary cutoff
  if (length(x) > 10L) x = c(x[1:10], '...')
  sprintf('[%s]', paste(x, collapse = ', '))
}

#----------------------------------------------------------------------
# Make a list with a regularly-spaced parameter grid
# For grid-based fitting and parameter recovery
#----------------------------------------------------------------------
MakeGridList <- function(names, nsteps = list(), ul = list(), ll = list(), sumto = NULL, to = 1, ...) {
  # ul and ll at the level of the individual parameter   
  ul <- as.list(ul)
  ll <- as.list(ll)
  ul <- ul[which(names(ul) %in% names)]
  ll <- ll[which(names(ll) %in% names)]
  sumto <- Filter(Negate(is.null), sumto)
  sumto <- Filter(function(z) all(z %in% names), sumto)
  nsteps <- nsteps[which(names(nsteps) %in% c(names, names(sumto)))]

  if (!is.null(sumto) & length(to) == 1) { # Default: sum to 1
    to <- as.list(rep(to, length(sumto)))
    names(to) <- names(sumto)
  }

  for (i in setdiff(names, names(ul))) {
    ul[[i]] <- 0
  }
  for (i in setdiff(names, names(ll))) {
    ll[[i]] <- 1
  }

   # Initialize result
   parameter = list()

   # Equal grid for parameters that don't sum to x  
   for (i in c(setdiff(names, names(nsteps)))) {
      nsteps[[i]] <- 3
   }
   for (i in setdiff(setdiff(names, unlist(sumto)), unlist(sumto))) {
      parameter[[i]] <- seq(ll[[i]], ul[[i]], (ul[[i]] - ll[[i]]) / (nsteps[[i]] - 1))
   }

   # Parts that sum to x
  for (i in names(sumto)) {
    mat <- .grid(rsum = to[[i]], ncol = length(sumto[[i]]), nstep = nsteps[[i]], ...)
    colnames(mat) <- sumto[[i]]
    parameter[[i]] <- mat
  }

   parameter <- Filter(Negate(is.null), parameter)
   parameter <- lapply(parameter, as.matrix)

   ids <- expand.grid(lapply(parameter, function(x) seq_along(x[,1])))

   return(list(parm = parameter, ids = ids))
}

#----------------------------------------------------------------------
# Get one element from the parameter id list
# For grid-based fitting and parameter recovery
#----------------------------------------------------------------------
GetParmFromGrid <- function(id, grid) {
  grid <- as.list(grid)
  parmids <- grid[[2]]
  parmspace <- grid[[1]]
  parmspace <- lapply(parmspace, as.matrix)
  nn <- names(parmspace)
  out <- sapply(nn, function(p) parmspace[[p]][parmids[id, p], ], simplify = FALSE)

  nn <- vector()
  
  for (i in seq_along(out)) {
    if(length(out[[i]]) == 1 ) {
      nn <- c(nn, names(out)[i])
    } else {
      nn <- c(nn, names(out[[i]]))
    }
  }
  out <- unlist(out)
  names(out) <- nn

  return(out)
}

#' Make a regular or random grid
#' @importFrom combinat xsimplex
#----------------------------------------------------------------------
# Matrix with rows that sum to something
# For participants() print function
#----------------------------------------------------------------------
#' @param rsum row sums
#' @param ncol number of columns
#' @param nrow number of rows (auto-determines)
#' @param regular if regular, \code{FALSE} means random
#' @param nstep number of steps, e.g. 4 is 0, 0.25, 0.5, 0.75, 1
#' @param offset adjust max and min minus this value
.grid <- function(rsum, ncol, nrow, regular = TRUE, nstep = NULL, offset = 0) {
  nrow <- if( missing(nrow) ) { round(sqrt(10^ncol*2)) }
  nsteps <- if ( is.null(nstep) ) { 4 } else { nstep }
  mat <- if (regular) {
    t(combinat::xsimplex(ncol, nstep, fun = function(i) i/sum(i)))
  } else {
    sweep(matrix(rbeta(ncol * nrow, 2, 2), ncol = ncol), 1, rowSums(mat), FUN = "/")
  }
  if ( offset > 0 ) {
    mat[mat == 0] <- offset
    mat[mat == 1] <- 1 - offset
    mat <- mat / rowSums(mat)
  }   
  return(mat)
}