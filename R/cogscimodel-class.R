#' Class for cognitive models
#' 
#' @import Formula
#' @import Rsolnp
#' @import cognitiveutils
#' @import R6
#' @usage Cogscimodel$new(formula, data, allowedparm)
#' @param formula Formula, like \code{y ~ x1 + x2}, depends on the model.
#' @param data Data.frame or matrix, must contain all variables in \code{formula}.
#' @param allowedparm Matrix with 4 columns like \code{rbind(alpha=c('ul'=0,'ll'=1,'init'=0.5,'na'=1))}. Defines each parameter's lower limit, upper limit, starting value (for fitting), and value that make the parameter have zero effect. Row names define parameter names. Columns names must be \code{'ll', 'ul', 'init', 'na'} (lower limit, upper limit, inital value, no-effect value; resp.).
#' @param fixed (optional) Named vector with model parameter(s) that are predefined (will not be fitted). If a fixed parameter is \code{NULL} model parameter will be ignored. See details.
#' @param choicerule (optional, default \code{NULL}) String specifying the choice rule, e.g. \code{'softmax', 'argmax'}, see \link[cognitiveutils]{choicerule} for possible choicerules; \code{NLL} means predictions as is.
#' @param model (optional) String, the name of the model
#' @param discount (optional) Integer or integer vector, which or how many many trials to discount?
#' @details Ignore a model parameter by setting it to \code{NULL}, in this case your matrix \code{allowedparm} needs to contain
#' @examples 
#' Cogscimodel$new(formula = y ~ x, data = data.frame(y=1:2, x=3:4), allowedparm = rbind(alpha = c('ul'=0, 'll'=1, 'init'=.5)), model = 'name of my model')
#' @export
Cogscimodel <- R6Class(
  'cogscimodel',
  #' @section This should be inherited
  #' Followed by list of methods.
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
    initialize = function(formula, data, allowedparm, fixed = NULL, choicerule =  NULL, model = NULL, discount = NULL, response = c('discrete', 'continuous'), fit.options = list(control = list(outer.iter = 400, inner.iter = 2000))) {
      self$model <- model
      f <- Formula(formula)
      self$formula <- f
      self$input <- get_all_vars(formula(f, lhs=0, rhs=NULL), data)
      if ( length(f)[1] > 0 ) {
        self$obs <- get_all_vars(formula(f, lhs=1, rhs=0), data)[, 1]
      }
      self$response <- match.arg(response)
      if ( response == 'continuous' ) {
        R <- max(self$obs, na.rm = T) - min(self$obs, na.rm = T)
        allowedparm <- rbind(allowedparm, sigma = c(0, R, R / 2, NA))
      }
      self$discount <- self$setdiscount(discount) 
      if ( !is.null(choicerule) ) {
        choicerule <- match.arg(choicerule, c('luce', 'argmax', 'softmax', 'epsilon'))
        self$choicerule <- choicerule
        if ( choicerule == 'softmax' ) {
          allowedparm <- rbind(allowedparm, tau = c(0.1, 10, 0.5, NA))
        } else if ( choicerule == 'epsilon' ) {
          allowedparm <- rbind(allowedparm, eps = c(0.001, 1L, 0.2, NA))
        }
      }
      # Checks
      if (!is.null(fixed) & !is.list(fixed)) {
        if (is.numeric(fixed)) {
          fixed <- as.list(fixed)
        } else {
          stop('"fixed" must be a list, but is ', class(fixed), '.', call. = FALSE)
        }
      }
      if ( any(duplicated(names(fixed))) ) {
        stop('Names of "fixed" must be unique: ', .brackify(names(fixed)[duplicated(names(fixed))])," appear/s multiple times.", call.=FALSE)
      }
      if ( any(!names(fixed) %in% rownames(allowedparm)) ) {
        stop('Check names of "fixed" parameter; allowed are ', .brackify(rownames(allowedparm)), ',\n\t but supplied was ', .brackify(setdiff(names(fixed), rownames(allowedparm))), '.', call.=FALSE)
      }
      if ( any(!fixed[sapply(fixed, is.character)] %in% rownames(allowedparm) ) ) {
        stop('Check "fixed", "fixed" may be equal to the model\'s parameters ', .brackify(rownames(allowedparm)), ', but contains ', .brackify(setdiff(fixed[sapply(fixed, is.character)], rownames(allowedparm))), '.', call.=FALSE)
      }
      if ( length(fixed) < nrow(allowedparm) & is.null(self$obs) ) {
        stop('Trying to estimate parameter ', .brackify(setdiff(rownames(allowedparm), names(fixed))), ', but "formula" lacks left-hand side specifying observed data\n\tEither add parameter to "fixed" fixing all parameters, or specify observations as left-hand-side of "formula".', call. = FALSE)
      }
      self$fixednames <- intersect(rownames(allowedparm), names(fixed))
      self$freenames <- setdiff(rownames(allowedparm), self$fixednames)
      self$constrainednames <- names(fixed[sapply(fixed, is.character)])
      self$parm <- allowedparm[, 'init']
      names(self$parm) <- rownames(allowedparm)
      self$allowedparm <- allowedparm
      self$setparm(self$substituteequal(fixed))
      self$setdiscount(discount)
      self$fit.options <- list(measure = 'loglikelihood', n = 1, nbest = length(self$freenames), options = list())
      if (!is.null(fit.options$measure)) {
        fit.options$measure <- match.arg(tolower(fit.options$measure), c('loglikelihood', 'mse', 'wmse', 'rmse', 'sse', 'wsse', 'mape', 'mdape', 'accuracy'))
      }
      if (!is.null(fit.options$newdata)) {
        fit.options$newdata <- get_all_vars(self$formula, fit.options$newdata)
      }
      self$fit.options[names(fit.options)] <- fit.options
    },
    #' Set data to ignore when fitting
    #' @param x intteger or integer vector, rows to discount
    setdiscount = function(x) {
      if(sum(x) == 0) {
        NULL
      } else if (is.logical(x)) {
        cumsum(x)
      } else if (length(x) > 1) {
        as.numeric(x)
      } else if(length(x) == 1) {
        seq_len(x)
      }
    },
    #' Get the choicerule parameter
    which.choiceruleparm = function() {
      return(switch(self$choicerule,
                    softmax = which(names(self$parm) == 'tau'),
                    epsilon = which(names(self$parm) == 'eps')))
    },
    substituteequal = function(x) {
      subs_val_in_fixed <- x[ na.omit(match( x, names(x) )) ]
      if ( length(subs_val_in_fixed) > 0) {
        x[sapply(x, is.character)] <- subs_val_in_fixed 
      }
      subs_val_in_free <- self$allowedparm[ na.omit(match( x, rownames(self$allowedparm) )) , 'init']
      if ( length(subs_val_in_free) > 0) {
        x[sapply(x, is.character)] <- subs_val_in_free 
      }
      return(x)
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
      if (names(x) %in% names(self$parm)) {
        self$setparm(x)
      }
      self$parm <- c(self$parm, x)
      self$pred <- NULL
    },
    #' Transforms predictions with choicerule
    #' @param x string, choice rules, e.g. 'softmax', allowed choicerules see \link[cognitiveutils]{choicerule}
    applychoicerule = function(x) {
      if ( is.null(self$choicerule) ) {
        return(x)
      } else {
        args <- c(list(x = x, type = self$choicerule), as.list(self$parm[self$which.choiceruleparm()]))
        x[] <- do.call(cognitiveutils::choicerule, args)
        return(x)
      }
    },
    coef = function() {
      return(unlist(self$parm[setdiff(self$freenames, self$constrainednames)]))
    },
    #' Compute goodness of fit
    #' @param type string, fit measure to use, e.g. 'loglikelihood', allowed types see \link[cognitiveutils]{gof}
    gof = function(type, n = 1, newdata = NULL, discount = FALSE, ...) {
      if (is.null(self$obs)) {
        stop('Model has no observed variables, check your formula or data', call.=FALSE)
      }
      if ( !(missing(newdata) | is.null(newdata)) ) {
        obs <- get_all_vars(formula(self$formula, lhs=1, rhs=0), newdata)[, 1, drop = FALSE]
        pred <- as.matrix(self$predict())[, 1:ncol(obs), drop = FALSE]
      } else {
        obs <- as.matrix(self$obs)
        pred <- as.matrix(self$predict())[, 1:ncol(obs), drop = FALSE]
      }
      if (discount) {
        pred[self$discount, ] <- NA
      }
      .args <- c(list(obs = obs, pred = pred, na.rm = TRUE, n = n), list(options = c(list(...), response = self$response)))
      do.call(cognitiveutils::gof, .args)
    },
    logLik = function(...) {
      self$gof(type = 'loglikelihood', ...)
    },
    BIC = function() {
      -2 * self$logLik() + nrow(self$input) * length(self$freenames)
    },
    AIC = function() {
      -2 * self$logLik() + 2 * length(self$freenames)
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
      
      if ( type[1] == 'grid' & length(type) == 1) {
        fit <- self$fitGrid(1, pars)
      }
      
      if ( all(type == c('grid', 'solnp')) ) {
        gridFit <- self$fitGrid(nbest = self$fit.options$nbest, pars, offset = TRUE)
        fits <- apply(gridFit$parm, 1, self$fitSolnp, parspace = pars)
        fit <- fits[[which.min(lapply(fits, function(x) tail(x$val, 1)))]]
      }
      
      self$setparm(fit$parm)
      self$gofvalue <- fit$val
    },
    fitGrid = function(nbest = 1, parspace, offset = FALSE) {
      np <- length(self$freenames)
      LB <- parspace[, 'll', drop = FALSE]
      UB <- parspace[, 'ul', drop = FALSE]
      ST <- apply(parspace, 1, function(x) round((max(x) - min(x)) / sqrt(10 * np), 2))
      GRID <- self$parGrid( if( offset ) { 0.01 * min(UB-LB) } else { 0 } )
      val <- sapply(1:nrow(GRID$ids), function(i) {
          pars <- GetParmFromGrid(i, GRID)
          self$fitObjective(pars, self = self)
        })
      select <- which(rank(val, ties.method = 'random') <= nbest)
      parm <- t(sapply(select, GetParmFromGrid, grid = GRID))
      return(
        list(parm = parm[, self$freenames, drop = FALSE],val = val[select]))
    },
    parGrid = function(offset = 0) {
      return(MakeGridList(names = self$freenames,
            ll = self$allowedparm[, 'll'],
            ul = self$allowedparm[, 'ul'],
            offset = offset))
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
      return(list(NULL, NULL)) # default: no constraints   
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