#' Class for cognitive models
#' 
#' @import Rsolnp
#' @import cogsciutils
#' @import R6
#' @import Formula
#' @usage Cogscimodel$new(formula, data, allowedparm)
#' @param formula Formula (e.g., \code{y ~ x1 + x2}); depends on the model.
#' @param data Data.frame or matrix holding \code{formula}'s variables.
#' @param allowedparm  Matrix. Row names define the names of the allowed parameters, excluding choicerule parameters. Needs 4 columns named \code{"ll","ul","init","na"}, defining each parameter's lower limit, upper limit, starting value for fitting, and value that makes a parameter have zero effect.
#' @param fixed (optional) List of model parameters and their fixed values, those parameters won't be estimated. Fixing a parameter to \code{NA} means ignoring it entirely (see details).
#' @param choicerule (optional, default \code{NULL}) String specifying the choice rule. Allowed values: \code{NULL} or e.g., \code{"softmax"}, values of \code{type}s in \link[cogsciutils]{choicerule}.
#' @param model (optional) String, the model's name.
#' @param discount (optional) Integer or integer vector, which or how many many trials to discount
#' @param fit.options (optional) List with parameter estimation options:
#' \describe{
#'    \item{\code{measure}}{Goodness-of-fit measure (default: \code{"loglikelihood"}); allowed are values of \link[cogsciutils]{gof}'s argument \code{type}}
#'    \item{\code{n}}{Number of observations, if data is aggregated}
#'    \item{\code{nbest}}{How many best values to select for grid+hillclimbing?}
#'    \item{\code{newdata}}{New data to fit parameters with}
#'    \item{\code{options}}{Other options, see \link[cogsciutils]{gof}}
#' }
#' @details Ignore a model parameter by setting it to \code{NA} in \code{fixed}, in this case your matrix \code{allowedparm} needs to contain a value in the column na nullifying the effect of the parameter.
#' @examples 
#' Cogscimodel$new(formula = y ~ x, data = data.frame(y=1:2, x=3:4),
#' allowedparm = rbind(alpha = c('ul'=0, 'll'=1, 'init'=.5)),
#' model = 'name of my model')
#' @export
Cogscimodel <- R6Class(
  'cogscimodel',
  #' @section
  # should be inherited, followed by method list
  public = list(
    model = 'string',
    formula = 'Formula',
    input = 'matrix',
    obs = NULL,
    pred = 'matrix',
    parm = 'vector',
    response = 'string',
    fixednames = NULL,
    freenames = NULL,
    constrainednames = NULL,
    allowedparm = 'matrix',
    choicerule = NULL, 
    optimization = NA,
    discount = 'numeric',
    gofvalue = NULL,
    fit.options = NULL,
    initialize = function(formula, data, allowedparm, fixed = NULL, choicerule =  NULL, model = NULL, discount = NULL, response = c('discrete', 'continuous'), fit.options = list()) {
      f <- Formula(formula)
      fixed <- as.list(fixed)
      response <- match.arg(response)

      self$setinput(f, data)
      self$setobs(f, data)
      self$setresponse(response)
      self$setdiscount(discount)
      self$initializeparm(allowedparm, fixed)
      self$setchoicerule(choicerule, fixed)
      self$setparm(self$substituteparm(self$switchoffparm(fixed)))
      self$setfitoptions(fit.options, f) # run this safter initializing parm
      self$model <- model
      self$formula <- f

      # Checks
      if ( length(fixed) > 0 ) {
        allowedparm <- self$allowedparm
        if ( !all(sapply(fixed[!is.na(fixed)], is.numeric)) ) {
            stop('Parameter in "fixed" must be numeric, check ', .brackify(fixed[!sapply(fixed, is.numeric)]) , '.', call. = FALSE)
        }
        if ( any(duplicated(names(fixed))) ) {
          stop('Parameter names in "fixed" must be unique, check: ', .brackify(names(fixed)[duplicated(names(fixed))])," appear/s multiple times.", call.=FALSE)
        }
        if ( !all( names(fixed) %in% rownames(allowedparm)) ) {
          stop('Check "fixed". Allowed parameter names are ', .brackify(rownames(allowedparm)), ',\n  but supplied was: ', .brackify(setdiff(names(fixed), rownames(allowedparm))), '.', call.=FALSE)
        }
        if ( any(!fixed[sapply(fixed, is.character)] %in% rownames(allowedparm) ) ) {
          stop('Check "fixed". "fixed" may be equal to the model\'s parameters ', .brackify(rownames(allowedparm)), ', but contains ', .brackify(setdiff(fixed[sapply(fixed, is.character)], rownames(allowedparm))), '.', call.=FALSE)
        }
        if ( length(fixed) < nrow(allowedparm) & is.null(self$obs) ) {
          stop('Trying to estimate parameter ', .brackify(setdiff(rownames(allowedparm), names(fixed))), ', but "formula" lacks left-hand side specifying observed data\n\tEither add parameter to "fixed" fixing all parameters, or specify observations as left-hand-side of "formula".', call. = FALSE)
        }
      }
    },
    # Initialize parameter
    # @param x Matrix with allowed parameters, except choicerule
    initializeparm = function(x, fixed) {
      parnames <- rownames(x)
      self$fixednames <- intersect(parnames, names(fixed))
      self$freenames <- setdiff(parnames, self$fixednames)
      self$constrainednames <- names(fixed)[sapply(fixed, is.character)]
      self$parm <- x[, 'init']
      names(self$parm) <- parnames
      self$allowedparm <- x
    },
    # Retrieve the stimuli or inputs to the model
    setinput = function(f, d) {
      self$input <- self$getinput(f, d)
    },
    getinput = function(f, d) {
      f <- as.Formula(f)
      nr <- nrow(d)
      nopt <- length(f)[2]
      vn <- attr(terms(formula(f, lhs=0, rhs=1)), 'term.labels')
      nf <- sapply(1:nopt, function(i) length(attr(terms(formula(f, lhs=0, rhs=i)), 'term.labels')))
      arr <- array(NA, dim = c(nr, max(nf), nopt))
      for (o in seq_len(nopt)) {
        arr[,,o][] <- as.matrix(model.frame(formula(f, lhs=0, rhs=o), d))
      }
      if ( nopt==1 ) {
        colnames(arr) <- vn
      }
      return(arr)
    },
    getoptionlabel = function(f = self$formula) {
      f <- as.Formula(f)
      ss <- sapply(1:length(f)[2], function(i) attr(terms(formula(f, lhs=0, rhs=i)), 'term.labels')[1])
      return(abbreviate(ss,minlength=1))
    },
    # Set observations
    # @param f formula
    #@ @param d data
    setobs = function(f, d) {
      f <- as.Formula(f)
      self$obs <- model.frame(formula(f, lhs=1, rhs=0), d)
    },
    #' Set choice rule
    #' @param choicerule string holding the choicerule
    setchoicerule = function(choicerule, fixed) {
      if(is.null(self$parm)) { stop('setchoicerule needs to be after setparm') }
      if ( is.null(choicerule) & (length(self$choicerule) > 0 )) {
        self$rmchoicerule()
      }
      if ( !is.null(choicerule) ) {
        choicerule <- match.arg(choicerule, c('luce', 'argmax', 'softmax', 'epsilon'))
        self$choicerule <- choicerule

        parm <- rbind(
          tau = c(0.1, 10, 0.5, NA),
          eps = c(0.001, 1L, 0.2, NA)
          )
        self$allowedparm <- rbind(
          self$allowedparm,
          switch(choicerule,
            softmax = parm['tau', , drop = FALSE],
            epsilon = parm['eps', , drop = FALSE])
        )
        self$initializeparm(self$allowedparm, fixed)
      }
    },
    #' Remove choicerule
    rmchoicerule = function() {
      keep <- setdiff(names(self$getparm()), names(self$getparm('choicerule')))
      self$allowedparm <- self$allowedparm[keep,,drop=FALSE]
      self$choicerule <- NULL
    },
    setresponse = function(response) {
      if ( response == 'continuous' ) {
        rg <- max(self$obs) - min(self$obs)
        allowedparm <- rbind(self$allowedparm, sigma = c(0, rg, rg / 2, NA))
      }
      self$response <- response
    },
    #' Set data to ignore when fitting
    #' @param x intteger or integer vector, rows to discount
    setdiscount = function(x) {
      self$discount <- if(sum(x) == 0) {
        NULL
      } else if (is.logical(x)) {
        cumsum(x)
      } else if (length(x) > 1) {
        as.numeric(x)
      } else if(length(x) == 1) {
        seq_len(x)
      }
    },
    substituteparm = function(x) {
      if (is.null(self$allowedparm)) { stop('substituteparm called before alloewdparm is defined') }
      equal_in_fixed <- x[ na.omit(match( x, names(x) )) ]
      if ( length(equal_in_fixed) > 0 ) {
        x[sapply(x, is.character)] <- equal_in_fixed 
      }
      equal_in_free <- self$allowedparm[ na.omit(match( x, rownames(self$allowedparm) )) , 'init']
      if ( length(equal_in_free) > 0) {
        x[sapply(x, is.character)] <- equal_in_free 
      }
      return(x)
    },
    switchoffparm = function(x) {
      if (is.null(self$allowedparm)) { stop('ignoreparm called before alloewdparm is defined') }
      na_parm <- names(x[is.na(x)])
      if ( length(na_parm) > 0 ) {
        x[na_parm] <- self$allowedparm[na_parm, 'na']
      }
      return(x)
    },
    #' Get parameter
    # @param x string which type of parameter to get
    getparm = function(x = 'all') {
      x <- match.arg(x, c('all', 'free', 'fixed', 'choicerule', 'constrained'))
      return(switch(x,
        all = self$parm,
        free = self$parm[self$freenames],
        fixed = self$parm[self$fixednames],
        choicerule = self$parm[switch(self$choicerule,
          softmax = 'tau',
          epsilon = 'epsilon')],
        constrained = self$parm[self$constrainednames]))
    },
    #' Set new parameter
    #' @param x named vector with parameter values, names must be one in \code{allowedparm}
    setparm = function(x) {
      if (is.matrix(x)) {
        x <- if (nrow(x) == 1) {
          setNames(x[1,], colnames(x))
          } else if (ncol(x) == 1) {
            setNames(x[1,], rownames(x))
            } else {
              stop('In setparm(), parameter must be a named vector.', call.=FALSE)
              }
      }
      x <- x[names(x) %in% names(self$parm)]
      self$parm[names(x)] <- x
      self$pred <- NULL
    },
    #' Add a new parameter
    #' @param x named vector with parameter value not in \code{allowedparm}
    addparm = function(x) {
      warning('Function is experimental.')
      if (names(x) %in% names(self$parm)) {
        self$setparm(x)
      }
      self$parm <- c(self$parm, x)
      self$pred <- NULL
    },
    #' Number of free parameter
    #' @param x String specifying which
    nparm = function(x = 'all') {
      x <- match.arg(x, c('all', 'free', 'fixed', 'choicerule', 'constrained'))
      return(length(self$getparm(x)))
    },
    #' Sets fit options
    # param x list of fitting options
    setfitoptions = function(x, f = self$formula) {
      defaultoptions <- list(
        measure = 'loglikelihood',
        n = 1,
        nbest = length(self$freenames),
        newdata = NULL,
        options = list())

      # Checks
      if ( is.null(f) ) {
        stop('formula "f" in setfitoptions() is NULL.')
      }      
     if ( !all(names(x) %in% names(defaultoptions)) ) {
        stop('"fit.options" contains ', .brackify(setdiff(names(x), names(defaultoptions))), ', which is not part of the allowed options: ', .brackify(names(defaultoptions)), '.')
      }
      # Substitute
      if ( !is.null(x$measure) ) {
        x['measure'] <- match.arg(tolower(x$measure), c('loglikelihood', 'mse', 'wmse', 'rmse', 'sse', 'wsse', 'mape', 'mdape', 'accuracy'))
      }
      if ( !is.null(x$newdata) ) {
        x$newdata <- self$getinput(f, x$newdata)
      }
      defaultoptions[names(x)] <- x
      self$fit.options <- defaultoptions
    },
    #' Transforms predictions with choicerule
    #' @param x string, choice rules, e.g. 'softmax', allowed choicerules see \link[cogsciutils]{choicerule}
    applychoicerule = function(x) {
      if ( is.null(self$choicerule) ) {
        return(x)
      } else {
        args <- c(list(x = x, type = self$choicerule), as.list(self$getparm('choicerule')))
        x[] <- do.call(cogsciutils::choicerule, args)
        return(x)
      }
    },
    coef = function() {
      return(unlist(self$parm[setdiff(self$freenames, self$constrainednames)]))
    },
    #' Compute goodness of fit
    #' @param type string, fit measure to use, e.g. 'loglikelihood', allowed types see \link[cogsciutils]{gof}
    gof = function(type, n = 1, newdata = NULL, discount = FALSE, ...) {
      if (is.null(self$obs)) {
        stop('Model has no observed variables, check your formula or data', call.=FALSE)
      }
      if ( !(missing(newdata) | is.null(newdata)) ) {
        obs <- get_all_vars(formula(self$formula, lhs=1, rhs=0), newdata)[, 1, drop = FALSE]
        pred <- as.matrix(self$predict(newdata = newdata))[, 1:ncol(obs), drop = FALSE]
      } else {
        obs <- as.matrix(self$obs)
        pred <- as.matrix(self$predict())[, 1:ncol(obs), drop = FALSE]
      }
      if (discount) {
        pred[self$discount, ] <- NA
      }
      .args <- c(list(obs = obs, pred = pred, na.rm = TRUE, n = n), list(options = c(list(...), response = self$response)))
      do.call(cogsciutils::gof, .args)
    },
    logLik = function(...) {
      self$gof(type = 'loglikelihood', ...)
    },
    BIC = function() {
      -2 * self$logLik() + dim(self$input)[1] * log(self$nparm('free'))
    },
    AIC = function() {
     -2 * self$logLik() + 2 * self$nparm('free')
    },
    MSE = function(...) {
      self$gof(type = 'mse', ...)
    },
    SSE = function(...) {
      self$gof(type = 'sse', ...)
    },
    RMSE = function(...) {
      self$gof(type = 'rmse', ...)
    },
    fit = function(type = c('grid', 'solnp'), measure = self$fit.options$measure, ...) {
      type <- match.arg(type, several.ok = TRUE)
      pars <- self$allowedparm[self$freenames,,drop=FALSE]

      if ( type[1] == 'solnp' ) {
        fit <- self$fitSolnp(par0 = pars[, 'init'], pars)
      }

      if ( type[1] == 'grid' & length(type) == 1 ) {
        fit <- self$fitGrid(1, pars)
      }

      if ( all(type == c('grid', 'solnp')) ) {
        gridFit <- self$fitGrid(nbest = self$fit.options$nbest, pars, offset = TRUE)
        fits <- apply(gridFit$parm, 1, self$fitSolnp, parspace = pars)
        fit <- fits[[which.min(lapply(fits, function(x) tail(x$val, 1)))]]
      }

      self$setparm(fit$parm)
      self$gofvalue <- fit$vals
    },
    fitGrid = function(nbest = 1, parspace, offset = FALSE) {
      np <- length(self$freenames)
      LB <- parspace[, 'll', drop = FALSE]
      UB <- parspace[, 'ul', drop = FALSE]
      OF <- as.list(    0.1/(1 + exp(-(UB-LB)))[,1]  ) # offset, scales logistically from super small to 10% of the range of each parameter
      ST <- apply(parspace, 1, function(x) round((max(x) - min(x)) / sqrt(10 * np), 2))
      OF <- if ( offset ) { as.list(    0.1/(1 + exp(-(UB-LB)))[,1]  ) } else { 0 } # offset, scales logistically from super small to 10% of the range of each parameter
      GRID <- self$pargrid( OF )
      val <- sapply(1:nrow(GRID$ids), function(i) {
          pars <- GetParmFromGrid(i, GRID)
          self$fitObjective(pars, self = self)
        })
      select <- which(rank(val, ties.method = 'random') <= nbest)
      parm <- t(sapply(select, GetParmFromGrid, grid = GRID))
      return(
        list(parm = parm[, self$freenames, drop = FALSE], val = val[select]))
    },
    pargrid = function(offset = 0, ...) {
      allowedparm <- self$allowedparm
      return(MakeGridList(
        names = self$freenames,
        ll = setNames(allowedparm[, 'll'], rownames(allowedparm)),
        ul = setNames(allowedparm[, 'ul'], rownames(allowedparm)),
        offset = offset,
        ...))
    },
    randomPar = function(parspace) {
      n <- log(5^nrow(parspace))
      par <- apply(parspace, 1, function(i) runif(n, min = i['ll'], max = i['ul']))
      colnames(par) <- rownames(parspace)
      return(par)
    },
    fitSolnp = function(par0, parspace, control = list(trace = 1)) {
      fit <- solnp(pars = par0,
        fun = self$fitObjective,
        LB = parspace[, 'll', drop = FALSE],
        UB = parspace[, 'ul', drop = FALSE],
        eqfun = self$getSolnpEq()[[1]],
        eqB = self$getSolnpEq()[[2]],
        control = control,
        self = self)
      return(list(parm = fit$pars, val = tail(fit$value, 1)))
    },
    fitObjective = function(pars, self) {
      self$setparm(x = pars)
      negate <- (-1)^any(c('loglikelihood', 'accuracy') %in% self$fit.options$measure)
      negate * self$gof(
        type = self$fit.options$measure,
        n = self$fit.options$n,
        newdata = self$fit.options$newdata,
        options = self$fit.options$options,
        discount = !is.null(self$discount))
    },
    getSolnpEq = function() {
      if ( !any(sapply(self$parm[self$fixednames], is.character)) ) {
        return(self$eqfun())
      } else {
        # Equality bouns set to 0 for all constraitn parameter
        eqB <- rep(0L, sum(sapply(self$parm, is.character)))

        if ( is.null(self$eqfun() ) ) {
          eqfun <- function(pars, self, measure) {
            # All parameter that equal characters/other parm. names
            string_pars <- pars[which(sapply(pars, is.character))]
            par_vals <- pars[ na.omit(match( pars, names(pars) )) ] # the equality to ...
            return(string_pars - par_vals)
          }          

        } else { 
          eqfun <- function(pars, self, measure) {
            string_pars <- pars[which(sapply(pars, is.character))] # see above
            par_vals <- pars[ na.omit(match( pars, names(pars) )) ] # see above
            # All parameter with constraints from the child object
            child_pars <- self$eqfun()[[1]](pars, self)
            return(c(child_pars, string_pars - par_vals))
          }
          child_eqB <- self$eqfun()[[2]] # bound defined in the child
          eqB <- c(child_eqB, eqB)
        }
        return(list(eqfun, eqB))
      }
    },
    eqfun = function(pars, self, measure) {
      return(list(NULL, NULL)) # defaultoptions: no constraints   
    },
    print = function(digits = 2) {
      cat(self$model)
      cat('\nCall:\n',
      paste(deparse(self$formula), sep = '\n', collapse = '\n'), '\n\n', sep = '')
      if(length(self$freenames) > 0) {
          cat('Free parm estimates\n')
          print.default(format(self$parm[setdiff(self$freenames, self$constrainednames)], digits = digits), print.gap = 2L, quote = FALSE)
      } else {
        cat('No free parm\n')
      }
      if(length(self$fixednames) > 0) {
        cat('\n')
        cat('Fixed parm\n')
        print.default(format(self$parm[unique(c(self$fixednames, self$constrainednames))], digits = digits), print.gap = 2L, quote = FALSE)
      } else {
        cat('No fixed parm\n')
      }
      if (length(self$constrainednames) > 0) {
        cat('  by equality constraint: ', self$constrainednames)
      }
      cat('\n\n')      
      if (is.null(self$choicerule)) {
        cat('No choice rule\n')
      } else {
        cat('Choice rule:', self$choicerule)
      }      
      cat('\n')
      invisible(self)
    }
  )
)


# Define S3 methods
logLik.cogscimodel <- function(obj, ...) {
  obj$logLik(...)
}
SSE <- function(obj, ...) {
  UseMethod('SSE')
}
SSE.cogscimodel <- function(obj, ...) {
  obj$SSE(...)
}
MSE <- function(obj, ...) {
  UseMethod('MSE')
}
MSE.cogscimodel <- function(obj, ...) {
  obj$MSE(...)
}
RMSE <- function(obj, ...) {
  UseMethod('RMSE')
}
RMSE.cogscimodel <- function(obj, ...) {
  obj$RMSE(...)
}

# showMethods('logLik')
# logLik

# setOldClass(c('cognitiveModel', 'R6'))

# setGeneric('logLik', function(object, ...) { UseMethod('') })
# setGeneric('SSE', function(obj, ...) { UseMethod('SSE') } )
# setGeneric('MSE', function(obj, ...) { UseMethod('MSE') } )

# setMethod('SSE', c('cognitiveModel'), function(obj) obj$SSE(...))
# setMethod('MSE', c('cognitiveModel'), function(obj) obj$MSE(...))
# setMethod('logLik', c('cognitiveModel'), function(object, ...) object$logLik(...))
# setMethod('print', c('cognitiveModel'), function(x) x$print())
# setMethod('predict', c('cognitiveModel'), function(object) object$predict())



# predict.cognitiveModel <- function(obj, newdata) {
#   obj$cr(obj$predict(ewdata = newdata))
# }
# logLik.cognitiveModel <- function(obj, saturated = FALSE, ...) {
#   obj$loglik(saturated = saturated)
# }
# SSE.cognitiveModel <- function(obj, ...) {
#   obj$SSE(weighted = weighted, n = n)
# }
# MSE.cognitiveModel <- function(obj, ....) {
#   obj$MSE(weighted = weighted)
# }
# print.cognitiveModel <- function(x, digits = 2) {
#   x$print(digits = digits)
# }