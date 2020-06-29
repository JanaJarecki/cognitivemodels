# ==========================================================================
# Package: Cognitivemodels
# File: utils.R
# Author: Jana B. Jarecki
# Changed: 2019-12-13
# ==========================================================================

# ==========================================================================
# Utilities and helper functions
# ==========================================================================

#' Vectorized Multinomial Draws
#' 
#' @param probs Matrix of probabilities
#' @param m number of samples for each row of \code{probs}
#' @noRd
rmultinom2 <- function (probs, m) { 
  # Courtsey: This code comes from the Hmisc package
  # Draws from multinomial distribution
  # If probs is a matrix of m probabilities of n observations 
  # where each row sums to one, the function returns the cell 
  # counts corresponding to multinomial draws
  d <- dim(probs)
  n <- d[1]
  k <- d[2]
  lev <- dimnames(probs)[[2]]
  if (!length(lev)) lev <- 1:k
  ran <- matrix(lev[1], ncol = m, nrow = n)
  z <- apply(probs, 1, sum)
  if (any(abs(z - 1) > 1e-05))
    stop("Error in multinom: probabilities do not sum to 1  ")
  U <- apply(probs, 1, cumsum)
  for (i in 1:m) {
    un <- rep(runif(n), rep(k, n))
    ran[, i] <- lev[1 + apply(un > U, 2, sum)]
  }
  return(ran)
}

#' Variance of probabilistically-described gambles (without N-1 correction)
#' 
#' @param p A numeric vector or matrix, the probabilities.
#' @param x A numeric vector or matrix, outcomes, same length as `x`.
#' 
#' @returns 
#' @examples
#' # Variance of the gamble 0 with 10% otherwise 10:
#' 
#' varG(p = c(.9,.1), x = c(0,10))
#' @export
varG <- function(p, x) {
  if (length(x) < 2) { stop("'x' must have two outcomes per row or more, but has only 1.")}
  if (length(p) < 2) { stop("'p' must have two probabilities per row or more, but has only 1.")}
  if (length(x) != length(p)) {
    stop("'p' and 'x' must have equal numbers of entries, but length of x is ", length(x), " and dim of p is ", length(p), ".")
  }
  if (typeof(x) != typeof(p)) {
    stop("'p' and 'x' must have the same type, but type of x is ", typeof(x), " and type of p is ", typeof(p), ".")
  }
  if (!is.null(dim(x)) && dim(x)[1] > 1) {
    return(sapply(1:dim(p)[1], function(i) varG(p[i, ], x[i, ])))
  }
  if (is.null(dim(x))) x <- t(as.matrix(x))
  if (is.null(dim(p))) p <- t(as.matrix(p))
  ev <- c(x %*% t(p))
  drop(p %*% t(((x - ev)^2)))
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

# convert a vector like c(1, 4, 3, 2) into a string like [1, 4, 3, 2]
#   (common aggregation method for error messages)
# From data.table
.dotify = function(x, n=6L) {
  # arbitrary cutoff
  if (length(x) > n) x = c(x[1:n], paste0("... (", length(x) - n, " more)"))
  sprintf("%s", paste(x, collapse = ", "))
}

#' Prints a nice question for strings
#' 
#' @importFrom utils adist
#' @noRd
.didyoumean = function(x, y) {
  y <- y[order(rank(utils::adist(x, y, ignore.case = TRUE), ties.method = "random"))]
  paste0("By ", dQuote(x), ", did you mean ", .dotify(dQuote(y), 3L), "?")
}

# Make a list with a regularly-spaced parameter grid for grid-based fitting and parameter recovery
make_grid_id_list <- function(names, nsteps = list(), ub = list(), lb = list(), sumto = NULL, to = 1, offset = 0, ...) {
  # ub and lb at the level of the individual parameter   
  ub <- as.list(ub)
  lb <- as.list(lb)
  offset <- as.list(offset)
  ub <- ub[which(names(ub) %in% names)]
  lb <- lb[which(names(lb) %in% names)]
  if (length(offset) == 1) {
    offset <- rep(offset, length(ub))
  }
  offset <- setNames(offset, names)
  if (length(nsteps) == 1L) {
    nsteps <- rep(nsteps, length(names))
  }
  nsteps <- setNames(nsteps, names)
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
  nstep <- if ( is.null(nstep) ) { 4 } else { nstep }
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


# ==========================================================================
# Utilities for parsing, printing model objects
# ==========================================================================

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
}

#' creates a directory for a new cogscimodel
#' 
#' @param path Path to the file
#' @noRd
.safe.dir.create <- function(path) {
  if (!dir.exists(path) && !dir.create(path)) {
    stop(gettextf("Cannot create directory '%s'", path), domain = NA)
  }
}


#' Define parameter for cognitive models
#' 
#' `make_parspace()` makes a n x 4 matrix with the parameter bounds, starting value, and no-effect value of parameters
#' 
#' @param ... Parameters and their ranges, e.g., `make_parspace(theta = c(ub=-1, lb=1, init=0, na=0), gamma = c(0,2,1,NA))`. See details and examples. The general schema per parameter is a named vector `parametername = c(ub = #, lb = #, init = #, na = #)`.
#' \describe{
#'    \item{`"lb"`}{Lower limit, smallest allowed value}
#'    \item{`"ub"`}{Upper limit, highest allowed value}
#'    \item{`"start"` (optional)}{Initial value for estimation of parameters}
#'    \item{`"na"` (optional)}{Value the parameter takes if it should have no effect, can be 'NA'}
#' }
#' @return A matrix with as many rows as parameters, in which the rownames are the parameter names, and four columns lb, ub, init, na that define the lower limit, upper limit, initial value and null-effect-value (optional) for each parameter.
#' @details `"na"` is an optional value a parameter takes such that this parameter has no effect.
#' 
#' To define a parameter \eqn{\alpha} with \eqn{0 \le \alpha \le 1} use one argument, \code{alpha = c(0,1)}. To define an additional parameter \eqn{\beta} with \eqn{2 \le \beta \le 9} use two arguments, \code{alpha = c(0,1), beta = c(2,9)}, and so forth. You can specify an initial value for the parameter used in fitting as third element of vectors.
#' 
#' @examples 
#' ## Define a parameter "p" that can range from 0-1
#' make_parspace(p = c(0,1))
#' makeparspapce(p = c(lb=0, ub=2)) # the same
#' 
#' # Note:
#' # Initial value will be the mean of 0 and 1.
#' 
#' 
#' ## Define a parameter "zeta" from 0-5
#' ## and set the initial value to 2
#' make_parspace(zeta = c(lb=0, ub=5, init=2))
#' make_parspace(zeta = c(0,5,2)) # the same
#' make_parspace(zeta = c(ub=5,lb=0,init=2)) #the same
#' 
#' 
#' ## Define one parameter "expon" from 0-2 with initial value 0.5
#' ## and a value of 1 making it have no effect
#' make_parspace(expon = c(0,2,1,1))
#' make_parspace(expon = c(lb=0,ub=2,init=1,na=1)) #the same
#' 
#' 
#' ## Define four parameter alpha, beta, gamma, delta
#' make_parspace(alpha = c(lb=0,ub=1,init=0.5,na=0),
#'                 beta = c(1,10,5,NA),
#'                 gamma = c(lb=1,ub=2,init=0,na=1),
#'                 delta = c(1,2,0))
#' 
#' ## Dynamically define new parameter "gamma" and "delta"
#' ## with the same definition (ub, lb, init, na)
#' def <- c(lb = 0, ub = 1, init = 0.5, na = 0)
#' deflist <- list("gamma" = def, "delta" = def)
#' do.call(make_parspace, deflist)
#' @export
make_parspace <- function(...) {
  dotargs <- list(...)
  dotargs <- vapply(dotargs, function(x) { 
      if (length(x) == 2 | length(x) == 3) {
          x <- c(x, rep(NA, 4-length(x)))
      }
      names(x) <- c("lb", "ub", "start", "na")[1:length(x)]

      n <- names(x)
      if (is.null(n)) {
        x
      } else if (all(c("ub", "lb") %in% n)) {
        x[intersect(c("lb", "ub", "start", "na"), n)]
      } else {
        names(x) <- c("lb", "ub", "start", "na")
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






# ==========================================================================
# Tools to handle formulas
# ==========================================================================

#' LHS of a formula
#' 
#' Returns the left-hand side variables of a formula as strings
#' or NULL if the formula has none
#' 
#' @param formula Objects of class "formula"
#' @param n Integer, the nth LHS-variable is returned, defaults to all LHS
#' @return the right-hand side of the formula or \code{NULL} is no RHS exists
#' @noRd
.lhs_var <- function(formula, n = NULL) {
  if (attr(terms(as.formula(formula)), which = "response")) {
    return(all.vars(formula(Formula::as.Formula(formula), lhs = 1, rhs = 0)))
  } else {
    return(NULL)
  }
}


#' List of RHSs of a formula
#' 
#' Returns a list with all right-hand side variables, by-separators |, of a formula as strings
#' or NULL if the formula has none
#' 
#' @param formula Objects of class "formula"
#' @param n Integer, the nth RHS-variable is returned, defaults to all RHS
#' @return the right-hand side of the formula or \code{NULL} is no RHS exists
#' @noRd
.rhs_var <- function(formula, n = NULL) {
  formula <- Formula::as.Formula(formula)
  if (length(attr(terms(formula), which = "term.labels"))) {
    return(lapply(1:length(formula)[2], function(i) all.vars(formula(formula, lhs = 0, rhs = i))))
  } else {
    return(NULL)
  }
}





# ==========================================================================
# Utilities for working with risky gambles
# ==========================================================================
#' Gets the outcomes of gambles
#' 
#' @param x a risky gamble matrix: x, p, x, p with options in dimension 3
#' @export
get_x <- function(x) {
  return(x[, seq(1, dim(x)[2], 2), , drop = FALSE])
}

#' Gets the probabilities of gambles
#' 
#' @param x a risky gamble matrix: x, p, x, p with options in dimension 3
#' @export
get_p <- function(x) {
  return(x[, seq(2, dim(x)[2], 2), , drop = FALSE])
}


#' Gets the expected value of gambles
#' 
#' @param x a risky gamble matrix: x, p, x, p with options in dimension 3
#' @export
get_ev <- function(x) {
  d <- dim(x)[2]
  apply(x, 3, function(slice) {
    rowSums(slice[, seq(1, d, 2), drop = FALSE] * slice[, seq(2, d, 2), drop = FALSE])
  })
}



#' Gets the variance of gambles
#' 
#' @param x a risky gamble matrix: x, p, x, p with options in dimension 3
#' @export
get_var <- function(x) {
  d <- dim(x)[2]
  apply(x, 3, function(slice) cognitivemodels:::varG(
    x = slice[, seq(1, d, 2), drop = FALSE],
    p = slice[, seq(2, d, 2), drop = FALSE]))
}




#' Helper function to document fix
#' @noRd
#' @export
.param_fix <- function(x) {
  set.seed(289)
  space <- .cm_dummy_model(x)$parspace
  par <- rownames(space)
  any_na <- anyNA(space[, "na"])
  txt <- "@param fix (optional) A list or the string `\"start\"`, the model parameters that are fixed and not estimated. Model parameters are called _"
  txt <- paste0(txt, paste(paste0("`", rownames(space), "`"), collapse = ", "), "_ (see details), if this argument is empty all parameters are estimated.")

  txt2 <- '* `"start"` sets all parameters equal to their initial values (estimates none). Useful for building a first test model. \n* `list(k = 1.5)` sets parameter _`k`_ equal to 1.5.\n'
  if (length(par) > 1) {
    txt2 <- paste(txt2,
      '* `list(k = "j")` sets parameter _`k`_ equal to parameter _`j`_ (estimates _`j`_).\n',
      '* `list(j = "k", k = 1.5)` sets parameter _`k`_ equal to parameter _`j`_ and sets parameter _`j`_ equal to 1.5 (estimates none of the two).\n')
  }
  if (any_na == TRUE) {
    txt2 <- paste(txt,
      "* `list(k = NA)` omits the parameter _`k`_, if possible.\n")
  }
  # Substitute k and j and 1.5 by adequate values for a model
  txt2 <- gsub("k", par[1], txt2)
  txt2 <- gsub("j", par[2], txt2)
  txt2 <- gsub("1.5", sprintf("%.2f", space[1, "start"] + runif(1) * (space[1, "ub"] - space[1, "start"])), txt2)
  return(paste(txt, "\n", txt2))
}