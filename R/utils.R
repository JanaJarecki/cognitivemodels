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

# Make a list with a regularly-spaced parameter grid for grid-based fitting and parameter recovery
MakeGridList <- function(names, nsteps = list(), ul = list(), ll = list(), sumto = NULL, to = 1, offset = 0, ...) {
  # ul and ll at the level of the individual parameter   
  ul <- as.list(ul)
  ll <- as.list(ll)
  ul <- ul[which(names(ul) %in% names)]
  ll <- ll[which(names(ll) %in% names)]
  offset <- as.list(offset)
  if ( length(offset) == 1 ) {
    offset <- rep(offset, length(ul))
    names(offset) <- names(ul)
  }
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
      lli <- ll[[i]] + offset[[i]]
      uli <- ul[[i]] - offset[[i]]
      parameter[[i]] <- seq(lli, uli, (uli - lli) / (nsteps[[i]] - 1))
   }

   # Parts that sum to x
  for (i in names(sumto)) {
    mat <- .grid(rsum = to[[i]], ncol = length(sumto[[i]]), nstep = nsteps[[i]], offset = offset[[i]], ...)
    colnames(mat) <- sumto[[i]]
    parameter[[i]] <- mat
  }

   parameter <- Filter(Negate(is.null), parameter)
   parameter <- lapply(parameter, as.matrix)

   ids <- expand.grid(lapply(parameter, function(x) seq_along(x[,1])))

   return(list(parm = parameter, ids = ids))
}


# Get one element from the parameter id list, f For grid-based fitting and parameter recovery
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
    sweep(matrix(stats::rbeta(ncol * nrow, 2, 2), ncol = ncol), 1, rowSums(mat), FUN = "/")
  }
  if ( offset > 0 ) {
    mat[mat == 0] <- offset
    mat[mat == 1] <- 1 - offset
    mat <- mat / rowSums(mat)
  }   
  return(mat)
}


##
# Utilities for parsing, printing cogscimodels
# --------------------------------------------------------------------------

# deparse(.) returning \bold{one} string
# @note Protects against the possibility that results from deparse() will be
#       split after 'width.cutoff' (by default 60, maximally 500)
.safeDeparse <- function(x, collapse=" ") paste(deparse(x, 500L), collapse=collapse)

.abbrDeparse <- function(x, width=60) {
    r <- deparse(x, width)
    if (length(r) > 1) {
      return( paste(r[1], "...") )
    } else {
      return( r )
    }
  return(exp(deltas) / sum(exp(deltas)))
}

# creates a directory for a new cogscimodel
# @param path Path to the file
.safe.dir.create <- function(path) {
  if (!dir.exists(path) && !dir.create(path)) {
    stop(gettextf("Cannot create directory '%s'", path), domain = NA)
  }
}


#' Define parameter for cognitive models
#' 
#' @usage define.parameter(alpha = c(ll=0, ul=1, init=0.5, na=0))
#' @param ... 1 or more parameters and their ranges, specified as vector: E.g., to define parameters alpha and beta enter \code{alpha = c(ll=0, ul=1, init=0.5, na=0), beta = c(0,10,5,NA)})
#' \describe{
#'    \item{\code{"ll"}}{Smallest value allowed for the parameter}
#'    \item{\code{"ul"}}{Maximum value allowed for the parameter}
#'    \item{\code{"init"}}{Initial value for fitting}
#'    \item{\code{"na"}}{Value the parameter takes if it should have no effect, can be \code{NA}}
#' }
#' @return A matrix with as many rows as parameters, where rownames are paramter names and four columns ll, ul, init, na define the lower limit, upper limit, initial value and null-effect-value (optional) for each parameter.
#' @details \code{"ll"} and \code{"ul"} are the smallest and largest value allowed for the parameter, \code{"init"} is the initial value when fitting the parameter, \code{"na"} is an optional value the parameter takes if it should have no effect.
#' @examples 
#' ## Define four parameter alpha, beta, gamma, delta
#' define.parameter(alpha = c(ll=0,ul=1,init=0.5,na=0),
#'                 beta = c(1,10,5,NA),
#'                 gamma = c(ll=1,ul=2,init=0,na=1),
#'                 delta = c(2,1,0))
#' 
#' ## Dynamically define new parameter "gamma" and "delta"
#' ## with the same definition (ul, ll, init, na)
#' def <- c(ll = 0, ul = 1, init = 0.5, na = 0)
#' deflist <- list("gamma" = def, "delta" = def)
#' do.call(define.parameter, deflist)
#' @export
define.parameter <- function(...) {
  dotargs <- list(...)

  dotargs <- vapply(dotargs, function(x) {
      n <- names(x)
      if (is.null(n)) {
        x
      } else if (identical(sort(n), c("init", "ll", "na", "ul"))) {
        x[c('ll', 'ul', 'init', 'na')]
      } else {
        stop('Parameter definition must be named "init", "ll", "na", "ul", but names contain ', .brackify[x], '.')
      }
    }, c(ll=0,ul=0,init=0,na=0))

  dotargs <- t(dotargs)
  if ( any(dotargs[,'ll'] > dotargs[,'ul']) ) {
    stop('"ll" > "ul" for the following parameter:', .brackify(rownames(dotargs)[which(dotargs[,"ll"] > dotargs[,"ul"])]), ". Check define.parameter.")
  }
  return(dotargs)
}