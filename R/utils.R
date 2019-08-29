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

# Make a named list
.named_list <- function(...) {
    L <- list(...)
    snm <- sapply(substitute(list(...)), deparse)[-1]
    if (is.null(nm <- names(L))) nm <- snm
    if (any(nonames <- nm == "")) nm[nonames] <- snm[nonames]
    setNames(L,nm)
}


# convert a vector like c(1, 4, 3, 2) into a string like [1, 4, 3, 2]
#   (common aggregation method for error messages)
# From data.table
.brackify = function(x) {
  # arbitrary cutoff
  if (length(x) > 10L) x = c(x[1:10], "...")
  sprintf("[%s]", paste(x, collapse = ", "))
}

# Make a list with a regularly-spaced parameter grid for grid-based fitting and parameter recovery
make_grid_id_list <- function(names, nsteps = list(), ub = list(), lb = list(), sumto = NULL, to = 1, offset = 0, ...) {
  # ub and lb at the level of the individual parameter   
  ub <- as.list(ub)
  lb <- as.list(lb)
  offset <- as.list(offset)
  ub <- ub[which(names(ub) %in% names)]
  lb <- lb[which(names(lb) %in% names)]
  if ( length(offset) == 1 ) {
    offset <- rep(offset, length(ub))
  }
  offset <- setNames(offset, names(ub))
  sumto <- Filter(Negate(is.null), sumto)
  sumto <- Filter(function(z) all(z %in% names), sumto)
  nsteps <- nsteps[which(names(nsteps) %in% c(names, names(sumto)))]

  if (!is.null(sumto) & length(to) == 1) { # Default: sum to 1
    to <- as.list(rep(to, length(sumto)))
    names(to) <- names(sumto)
  }

  for (i in setdiff(names, names(ub))) {
    ub[[i]] <- 0
  }
  for (i in setdiff(names, names(lb))) {
    lb[[i]] <- 1
  }

   # Initialize result
   parameter = list()

   # Equal grid for parameters that don"t sum to x  
   for (i in c(setdiff(names, names(nsteps)))) {
      nsteps[[i]] <- 3
   }
   for (i in setdiff(setdiff(names, unlist(sumto)), unlist(sumto))) {
      lli <- lb[[i]] + offset[[i]]
      uli <- ub[[i]] - offset[[i]]
      parameter[[i]] <- seq(from = lli, to = uli, by = (uli - lli) / (nsteps[[i]] - 1))
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

   return(list(par = parameter, ids = ids))
}


# Get one element from the parameter id list, f For grid-based fitting and parameter recovery
get_id_in_grid <- function(id, grid) {
  grid <- as.list(grid)
  parids <- grid[[2]]
  parspace <- grid[[1]]
  parspace <- lapply(parspace, as.matrix)
  nn <- names(parspace)
  out <- sapply(nn, function(p) parspace[[p]][parids[id, p], ], simplify = FALSE)

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

#" Make a regular or random grid
#" @importFrom combinat xsimplex
#----------------------------------------------------------------------
# Matrix with rows that sum to something
# For participants() print function
#----------------------------------------------------------------------
#" @param rsum row sums
#" @param ncol number of columns
#" @param nrow number of rows (auto-determines)
#" @param regular if regular, \code{FALSE} means random
#" @param nstep number of steps, e.g. 4 is 0, 0.25, 0.5, 0.75, 1
#" @param offset adjust max and min minus this value
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
#       split after "width.cutoff" (by default 60, maximally 500)
.safeDeparse <- function(x, collapse=" ") paste(deparse(x, 500L), collapse=collapse)
# Deparse and abbreviate
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

#' Convert a character to formula
#' Checks if x is a character and if so converts it into RHS formula
#' @par x A string or RHS formula
#' @export
chr_as_rhs <- function(x) {
  if (is.character(x)) { 
    return(reformulate(x))
  } else { 
    return(x)
  }
}


#" Define parameter for cognitive models
#" 
#" @usage make_parspace(...)
#" @param ... Parameters and their ranges, e.g., \code{make_parspace(theta = c(ub=-1, lb=1, init=0, na=0), gamma = c(0,2,1,NA))}. See details and examples.#" 
#" The general schema per parameter is a named vector \code{parametername = c(ub = #, lb = #, init = #, na = #)}
#" \describe{
#"    \item{\code{"lb"}}{Lower limit, smallest allowed value}
#"    \item{\code{"ub"}}{Upper limit, highest allowed value}
#"    \item{\code{"start"} (optional)}{Initial value for fitting}
#"    \item{\code{"na"} (optional)}{Value the parameter takes if it should have no effect, can be \code{NA}}
#" }
#" @return A matrix with as many rows as parameters, where rownames are paramter names and four columns lb, ub, init, na define the lower limit, upper limit, initial value and null-effect-value (optional) for each parameter.
#" @details \code{"lb"} and \code{"ub"} are the smallest and largest value allowed for the parameter, \code{"start"} is the initial value when fitting the parameter, \code{"na"} is an optional value the parameter takes if it should have no effect.
#" 
#" To define a parameter \eqn{\alpha} with \eqn{0 \le \alpha \le 1} use one argument, \code{alpha = c(0,1)}. To define an additional parameter \eqn{\beta} with \eqn{2 \le \beta \le 9} use two arguments, \code{alpha = c(0,1), beta = c(2,9)}, and so forth. You can specify an initial value for the parameter used in fitting as third element of vectors.
#" 
#" @examples 
#" ## Define a parameter "p" that can range from 0-1
#" make_parspace(p = c(0,1))
#" makeparspapce(p = c(lb=0, ub=2)) # the same
#" 
#" # Note:
#" # Initial value will be the mean of 0 and 1.
#" 
#" 
#" ## Define a parameter "zeta" from 0-5
#" ## and set the initial value to 2
#" make_parspace(zeta = c(lb=0, ub=5, init=2))
#" make_parspace(zeta = c(0,5,2)) # the same
#" make_parspace(zeta = c(ub=5,lb=0,init=2)) #the same
#" 
#" 
#" ## Define one parameter "expon" from 0-2 with initial value 0.5
#" ## and a value of 1 making it have no effect
#" make_parspace(expon = c(0,2,1,1))
#" make_parspace(expon = c(lb=0,ub=2,init=1,na=1)) #the same
#" 
#" 
#" ## Define four parameter alpha, beta, gamma, delta
#" make_parspace(alpha = c(lb=0,ub=1,init=0.5,na=0),
#"                 beta = c(1,10,5,NA),
#"                 gamma = c(lb=1,ub=2,init=0,na=1),
#"                 delta = c(1,2,0))
#" 
#" 
#" ## Dynamically define new parameter "gamma" and "delta"
#" ## with the same definition (ub, lb, init, na)
#" def <- c(lb = 0, ub = 1, init = 0.5, na = 0)
#" deflist <- list("gamma" = def, "delta" = def)
#" do.call(make_parspace, deflist)
#" @export
make_parspace <- function(...) {
  dotargs <- list(...)

  dotargs <- vapply(dotargs, function(x) {
      n <- names(x)
      if ( length(x) == 2 | length(x) == 3 ) {
        if (is.null(n)) {
          x <- c(x, rep(NA, 4-length(x)))
        } else {
          x <- c(x, c("na" = NA, "start" = NA))[1:4]
        }
      }

      n <- names(x)
      if (is.null(n)) {
        x
      } else if (all(c("ub", "lb") %in% n)) {
        x[intersect(c("lb", "ub", "start", "na"), n)]
      } else {
        stop('Parameter definition must be named "lb", "ub", start", "na" but names contain ', .brackify[x], ".")
      }
    }, c(lb=0, ub=0, start=0, na=0))

  dotargs <- t(dotargs)
  start_nas <- which(is.na(dotargs[, "start"]))
  dotargs[start_nas, "start"] <- rowMeans(dotargs[start_nas, c("ub", "lb"), drop = FALSE])

  if ( any(dotargs[,"lb"] > dotargs[,"ub"]) ) {
    stop('Lower value limit > upper limit for the parameter: ', .brackify(dQuote(rownames(dotargs)[which(dotargs[,"lb"] > dotargs[, "ub"])])), '. Ensure "lb" < "ub", e.g. make_parspace(a = c(0, 1, 0.5)).')
  }
  if ( any(dotargs[,"start"] < dotargs[,"lb"]) ) {
    stop("Initial value < lower limit for the parameter: ", .brackify(dQuote(rownames(dotargs)[which(dotargs[,"start"] < dotargs[,"lb"])])), '. Ensure "start" is > "lb"), e.g. make_parspace(a = c(0, 1, 0.5)).' )
  }
  if ( any(dotargs[,"start"] > dotargs[,"ub"]) ) {
    stop("Initial value > upper limit for the parameter: ", .brackify(dQuote(rownames(dotargs)[which(dotargs[,"start"] > dotargs[,"ub"])])), '. Ensure "start" is < "ub", e.g. make_parspace(a = c(0, 1, 0.5)).')
  }
  return(dotargs)
}


