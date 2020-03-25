#' The R6 class for Cognitive Model Objects
#' 
#' \code{Cogscimodel$new(formula, data, parspace)}
#' 
#' @import Rsolnp
#' @import methods
#' @import ROI
#' @import ROI.plugin.optimx
#' @import ROI.plugin.alabama
#' @import ROI.plugin.deoptim
#' @import ROI.plugin.nloptr
#' @import cogsciutils
#' @import R6
#' @import Formula
#' @importFrom matlib showEqn
#' @importFrom rlang call_standardise
#' 
#' @param formula A formula (e.g., \code{y ~ x1 + x2}).
#' @param data A data frame containing \code{formula}'s variables.
#' @param parspace  (optional, \bold{required} to add model parameters) A amtrix with the parameter space: a \code{n x 4} matrix with columns named \code{"lb","ub","start","na"}. Easy to create using \link{make_parspace}, e.g. \code{make_parspace(alpha=c(0,1))}. The matrix' row names are parameter names, columns contain the lower limit, upper limit, starting value in fitting, and (optional) a value that makes a parameter have zero effect, which can be NA. See details.
#' @param fix (optional) Parameter constraints. Can be \code{"start"} or a list with \code{parname=value}-pairs. Parameter names see below under "parameter space".
#' \itemize{
#' \item{\code{"start"} constrains all available parameters to their starting values. Useful for model testing.}
#' \item{\code{parname = 0.5} constrains a parameter to 0.5.}
#' \item{\code{parname = "p2"} constrains a parameter to another model parameter \code{p2}.}
#' \item{\code{parname = NA} tells the model to omit a parameter, if possible.}
#' }
#' @param choicerule (optional) String specifying the choice rule (default \code{NULL}), allowed are \code{NULL}, \code{"softmax"}, \code{"eps"}, and other values of the argument \code{type} to \link[cogsciutils]{choicerule}.
#' @param title (optional, default is the class name) A string, the model's name.
#' @param mode A string. Allowed are \code{"discrete"}, \code{"continuous"}, specifies the model's response mode. Discrete responses are binary (0 or 1), continuous responses are numbers with a normal error.
#' @param discount (optional) An integer or integer vector (default \code{0}), ddefining which or how many, starting from trial 1, to discount when fitting.
#' @param options (optional) Options to control the parameter fitting methods, see the "Options" section of \code{\link{cogscimodel}}.
#' @details \code{parspace}. It is optional to define a value that makes the parameter have zero effect in the column called "na" in \code{parspace}. For example, a parameter \code{b} in \code{b*x}, has no-effect  when setting \code{b=0}. The no-effect value can be \code{NA}.
#' 
#' \bold{fix}
#'  Supplying \code{list(alpha=0.5, beta="alpha", gamma=NA)} constrains the parameters alpha, beta, and gamma as follows
#' \itemize{
  #' \item{\code{alpha=0.5}}{: fix alpha to 0.5.}
  #' \item{\code{beta="alpha"}}{: fix beta to the value of alpha; alpha may be a free or fixed model parameter in \code{parspace}.}
  #' \item{\code{delta=NA}}{: ignore delta if it can be ignored by setting delta equal to the value in the column "na" of \code{parspace}, given that parspace has a value in the column "na" for delta.}
#' }
#' You can ignore a model parameter by setting it to \code{NA} in \code{fix}, in this case your \code{parspace} needs to contain a value in the column na nullifying the effect of the parameter.
#' @section Options, see \link{cogscimodel_options}, possible options:
#' \describe{
#'    \item{\code{fit}}{(default \code{TRUE}), \code{FALSE} omits fitting the free parameter.}
#'    \item{\code{fit_measure}}{(default \code{"loglikelihood"}). When fitting, which fit measure to use? See \link[cogsciutils]{gof}'s argument \code{type}.}
#'    \item{\code{fit_data}}{(default: \code{data}). When fitting, which data other than to fit the model to? Useful if you fit to other data than you predict.}
#'    \item{\code{fit_n}}{(default: \code{1}). When fitting, the number of reservations underlying each data point in \code{fit_data}. If this data is aggregated data, change this argument.}
#'    \item{\code{fit_roi_solver}}{(default: \code{"auto"}). When fitting with the \link{http://roi.r-forge.r-project.org/index.html}{ROI pakage}, which solver to use for the parameter estimation problem.}
#'    \item{\code{nbest}}{(default: \code{self$npar("free")}, no. of free parameters) When fitting with grid search followed by a solver, how many best grid-search solutions to use as starting parameters with the solver?}
#' \item{\code{fit_options}}{(default: \code{NULL}). Other options, see \link[cogsciutils]{gof}.}
#' }
#' @examples 
#' Cogscimodel$new(
#'     formula = y ~ x,
#'     data = data.frame(y=1:2, x=3:4),
#'     parspace = rbind(alpha = c("ub"=0, "lb"=1, "start"=.5)),
#'     title = 'name of my model'))
#' @export
Cogscimodel <- R6Class(
  'cogscimodel',
  public = list(
    #' @field call The call to the model
    call = NULL,
    #' @field title A string, the name of the model
    title = NULL,
    #' @field formula Calling formula, e.g., \code{y ~ x1 + x2 + x3} models one response variable (\code{y}) given one three-attribute stimulus, and \code{y ~ x1 + x2 | z1 + z2} models a response given two two-attribute stimuli where attributes are separated by a "\code{+}"" and stimuli are separated by a "\code{|}".
    formula = NULL,
    #' @field input Inputs to the model in a standardized form: three-dimensional matrix based on \code{formula} and \code{data} with dimensions n(data) x n(attributes) x n(stimuli)
    input = NULL,
    #' @field more_input Additional inputs to the model from variables that are not part of the formula, three-dimensional matrix with dimensions n(data) x n(additional inputs) x n(stimuli)
    more_input = NULL,
    #' @field res The response variable to be modeled, two-dimensional matrix with dimensions n(data) x n(response variables)
    res = NULL,
    #' @field natt List with the number of attributes per stimulus
    natt = NULL,
    #' @field nobs Number of observations in \code{data}
    nobs = NULL,
    #' @field nstim Number of stimuli in model input
    nstim = NULL,    
    #' @field nres Number of response variables
    nres = NULL,
    #' @field ncon Number of constraints on the parameter
    ncon = NULL,
    #' @field par Model parameters, a named list
    par = NULL,
    #' @field parspace Parameter space, a matrix with one parameter per row, and four columns lb (lower bound), ub (upper bound), start (starting value for fitting) and na (value of the parameter if it has no effect on the model); rownames are parameter names
    parspace = NULL,
    #' @field pred Model predictions for \code{data}
    pred = NULL,
    #' @field parnames Parameter names
    parnames = list(),    
    #' @field stimnames Stimuli names
    stimnames = character(),
    #' @field prednames Predictio names
    prednames = character(),
    #' @field mode A string the modality of the predicted responses "\code{continuous}"" or "\code{discrete}"
    mode = NULL,
    #' @field choicerule A string with the choicerule that the model uses (e.g. "\code{softmax}"")
    choicerule = NULL, 
    #' @field pred_types The types of predictios the model males
    pred_types = c("response", "value"),
    #' @field discount A numeric with trials to discount when fitting
    discount = 'numeric',
    #' @field constraints An object with the parameter constraints
    constraints = NULL,
    #' @field fitobj An object holding the results of the parameter fitting method
    fitobj = NULL,
    #' @field options A list with options
    options = list(),
    #' @field pass_checks A logical if \code{TRUE} the model passes all internal checks
    pass_checks = FALSE,

    #' @description
    #' Initializes a new cogscimodel
    initialize = function(formula, data = NULL, parspace = make_parspace(), fix = NULL, choicerule =  NULL, title = NULL, discount = NULL, mode = NULL, options = NULL) {
      if (length(data) == 0) {
        self$pass_checks <- TRUE
      }
      # get call
      self$call <- if (deparse(sys.call()[[1]]) == "super$initialize") {
        # If it is not a stacked model
        if (!inherits(self, "csm")) {
          rlang::call_standardise(sys.calls()[[sys.nframe()-4L]])
        }
      } else {
        rlang::call_standardise(sys.call(sys.nframe()-1L))
      }
      if (length(fix) > 0) {
        if (fix[1] != "start") {
          if (!is.list(fix) & all(!is.numeric(fix))) {
            stop("'fix' must be a list, but is a", class(fix), ". Solution: use list().")
          }
        }
      }
      f <- as.Formula(formula) 

      # Assign variables
      self$title <- title
      choicerule <- if (!is.null(choicerule)) match.arg(choicerule, c("softmax", "argmax", "luce", "epsilon"))
      self$choicerule <- choicerule
      self$formula <- f
      self$set_data(data = data)
      self$discount <- private$init_discount(x = discount)
      
      # Initialize model slots
      private$init_mode(mode = mode)
      private$init_parspace(p = parspace, cr = choicerule, options = options)
      fix <- private$init_fix(fix)
      private$init_parnames(
        parspace = self$parspace,
        fix = fix,
        choicerule = choicerule)
      private$init_par(
        parspace = self$parspace,
        fix = fix)
      private$init_constraints(fix = fix)
      private$init_stimnames()
      private$init_prednames()
      private$init_options(options)
      # Checks
      # ! after setting formula, parspace, choicerule, etc.
      private$check_input()
      private$check_fix(fix)
      # Fit the model
      if (length(data) > 0 & (self$options$fit == TRUE) & (self$npar("free") > 0L)) {
        message("Fitting free parameters ",
          .brackify(private$get_parnames("free")),
          " by ", ifelse(grepl("loglikelihood|accuracy", self$options$fit_measure), "maximizing ", "minimizing "), self$options$fit_measure, " with ", paste(self$options$fit_solver, collapse=", "))
        self$fit()
      }
    },

    #' @description
    #' Fits the free model parameters to the response data
    #' @param solver (optional) A string with the model solver
    #' @param measure (optional) A string with the goodness-of-fit measure that the solver optimizes (e.g. \code{"loglikelihood"}). Possible values, see the \code{type} argument in \link[cogsciutils]{gof}
    #' @param ... other arguments
    fit = function(solver = self$options$fit_solver, measure = self$options$fit_measure, ...) {
      solver <- match.arg(solver, c("grid", "solnp", "auto", names(ROI::ROI_registered_solvers())), several.ok = TRUE)
      constraints <- .simplify_constraints(self$constraints)

      if (solver[1] == "grid") {
        fit <- private$fit_grid(...)
        if (length(solver) == 2) {
          gfit <- fit
          s <- gfit$solution
          if (solver[2] == "solnp") {
            fits <- apply(s, 1, private$fit_solnp, cons = constraints, ...)
          } else {
            fits <- apply(s, 1, private$fit_roi, cons = constraints, ...)
          }
          fit <- fits[[which.min(lapply(fits, function(x) tail(x$objval, 1)))]]
          fit[["grid"]] <- gfit
        }
      } else if (solver[1] == "solnp") {
        fit <- private$fit_solnp(cons = constraints, ...)
      } else {
        fit <- private$fit_roi(cons = constraints, ...)
      }
      self$fitobj <- fit
      solution <- setNames(fit$solution, self$parnames$free)
      #! Set only the free parameter
      self$set_par(solution[self$parnames$free], check = FALSE)
    },

    #' @description
    #' Returns the free parameters of the model
    coef = function() {
      return(self$get_par("free"))
    },

    #' @description
    #' Make predictions for every row in the input data to the model
    #' @param type (optional, default \code{"response"}) Type of prediction to make. See the respective model.
    #' @param newdata (optional) A data frame with new data for which to compute predictions.
    #' @param ... other arguments (ignored)
    predict = function(type = "response", newdata = NULL, ...) {
      #' calls the child model's make_prediction function for
      #' every slice of the third dimension of the input matrix
      # If there is new data and get the input and more_input
      if (is.null(newdata) | missing(newdata)) {
        isnew <- FALSE
        input <- self$input
        more_input <- self$more_input
      } else {
        isnew <- TRUE
        input <- private$get_input(f = self$formula, d = newdata, ...)
        more_input <- private$get_more_input(d = newdata)
      }
      # Initialize the results object and run $make_prediction()
      #  where make_prediction() is a function on
      #  the child class
      #  Note: we run make_prediction() per 2-dimensional input
      #        matrix!
      ns <- self$nstim
      RES <- sapply(seq.int(ns),
        function(s) {
          self$make_prediction(
            type = type,
            input = abind::adrop(input[, , s, drop = FALSE], 3),
            more_input = abind::adrop(more_input[, , s, drop = FALSE], 3),
            isnew = isnew,
            s = s,
            ...)
        })

      # Finally we format the RES object a bit
      RES <- matrix(unlist(RES), nrow = dim(input)[1])
      colnames(RES) <- self$prednames[1:ncol(RES)]

      # And we apply the choice rule if needed
      type <- try(match.arg(type, c("response", "value")), silent = TRUE)
      # hack to allow for more types than the two
      if (inherits(type, "try-error")) {
        type <- "response"
      }
      return(drop(switch(type,
        value = RES,
        response = private$apply_choicerule(RES))))
    },
    
    #' @description
    #' Simulates observable data from the model
    #' @param newdata (optional) A data frame with inputs (e.g., stimuli) to be used in the simulation
    #' @return A matrix with simulated choices or judgment values
    simulate = function(newdata = NULL) {
      pred <- as.matrix(self$predict(newdata = newdata))
      if (self$mode == "discrete") {
        return(rmultinom2(pred))
      }
      if (self$mode == "continuous") {
        return(pred)
      }
    },

    #' @description
    #' New data input for a cogscim
    #' @param data A data frame with variables corresponding to the inputs that the model needs
    set_data = function(data = NULL) {
      if (missing(data) | is.null(data) | length(data) == 0) {
        data <- data.frame()
      }
      if (length(data) > 0 & !inherits(data, "data.frame")) {
        stop("'data' must be a data.frame, but is a ", class(data)[1], ".")
      } 
      formula <- self$formula
      self$nobs <- nrow(data)
      self$nstim <- length(self$formula)[2]
      self$natt <- vapply(1:self$nstim, function(i) length(attr(terms(formula(self$formula, lhs=0, rhs=i)), "term.labels")), 1L)
      self$input <- private$get_input(f = formula, d = data)
      self$more_input <- private$get_more_input(d = data)
      self$res <- private$get_res(f = formula, d = data)
      self$nres <- max(0, dim(self$res)[2])
      invisible(return(self))
    },

    #' @description
    #' Set model parameter
    #' @param x A named list of parameters, their names must be in \code{parspace}
    #' @param check (optional, default \code{TRUE}) A logical value, if \code{TRUE} the parameter names are checked for validity, if \code{FALSE} no check is performed.
    set_par = function(x, check = TRUE, constrain = TRUE) {
      if (length(x) == 0) {
        return(invisible(self))
      }
      if (is.matrix(x)) {
        x <- if (nrow(x) == 1) {
          setNames(x[1,], colnames(x))
          } else if (ncol(x) == 1) {
            setNames(x[1,], rownames(x))
            } else {
              stop("In set_par(), parameter must be a named list. Did you forget to name parameter in the argument 'fix'?", call.=FALSE)
              }
      }
      if (constrain == TRUE & self$ncon > 0) {
        x <- private$constrain(x)
      }
      if (check == TRUE) {
        private$check_par(x)
      }
      self$par[names(x)] <- x
      return(invisible(self))
    },


    #' @description
    #' Get the model parameter
    # @param x string which type of parameter to get, \code{"all"} returns all parameter, \code{"free"} returns the free parameters, \code{"constrained"} returns constrained parameters, \code{"equal"} returns parameters equal to another parameter
    #' @param x (optional, default \code{"all"}) A string specifying which parameters to retrieve, allowed are \code{"all", "free", "fix", "choicerule"}
    get_par = function(x = "all") {
      return(unlist(self$par[private$get_parnames(x)]))
    },

    #' @description
    #' Number of model parameters
    #' @param x String specifying which of the parameters to return, allowed are \code{"all", "free", "constrained", "equal"}
    npar = function(x = "all") {
      ans <- try(match.arg(x, "free"), silent = TRUE)
      if (class(ans) == "try-error") { # not "free"
        return(length(private$get_parnames(x)))
      } else {
        # TODO: throw error/stop if a model has more constraints than parameters
        npar <- length(private$get_parnames()) - self$ncon
        return(npar)
      }
    }, 

    #' @description
    #' Computes the goodness of model fit
    #' @param type A string, fit measure to use, e.g. \code{"loglikelihood"}, for the allowed types see \link[cogsciutils]{gof}
    #' @param n (optional) When fitting to aggregate data, supply how many raw data points underly each aggregated data point
    #' @param newdata (optional) A data frame with new data - experimental!
    #' @param ... other arguments (ignored)
    gof = function(type, n = self$options$fit_n, newdata = self$options$fit_data, discount = FALSE, ...) {
      if (length(self$res) == 0L) { stop("The model must contain observed data to calculate the goodness of fit, but observed data are ", self$res, ".\nDid you forget a left side in 'formula'?",call.=FALSE) }

      if (is.null(newdata) | missing(newdata)) {
        obs <- as.matrix(self$res)
        pred <- as.matrix(self$predict())
      } else {
        obs <- private$get_res(f = self$formula, d = newdata)
        pred <- as.matrix(self$predict(newdata = newdata))
      }
      if (all(rowSums(obs) == nrow(obs)) & nrow(pred) == (1 - nrow(obs))) {
        pred <- cbind(pred, nrow(obs) - pred)
      }
      pred <- pred[, 1:ncol(obs), drop = FALSE]
      if (discount == TRUE) {
        pred[self$discount, ] <- NA
      }
      dotargs <- list(...)
      options <- c(dotargs[["options"]], list(response = self$mode), self$options$fit_args[["options"]])
      dotargs <- c(
        dotargs[-which(names(dotargs) == "options")],
        self$options$fit_args[-which(names(self$options$fit_args)=="options")])

      .args <- c(
        list(
          obs = obs,
          pred = pred,
          type = type,
          na.rm = TRUE,
          n = n,
          options = options
          ),
        dotargs)
      if (self$mode == "continuous" & type == "loglikelihood") {
        .args[["sigma"]] <- self$get_par()["sigma"]
      }
      gof <- try(do.call(cogsciutils::gof, args = .args, envir = parent.frame()), silent = TRUE)
      if (inherits(gof, "try-error")) {
        stop("Can't compute ", type, ", because:\n  ", geterrmessage(),
          call.= FALSE)
      } else {
        return(gof)
      }
    },
    #' @description
    #' Log likelihood of the observed responses under the model predictions
    #' @param ... other arguments (ignored)
    logLik = function(...) {
      return(self$gof(type = "loglikelihood", ...))
    },
    #' @description
    #' Bayesian Information Criterion of the model predictions given the observed responses
    #' @param ... other arguments (ignored)
    BIC = function(...) {
      k <- ifelse('newdata' %in% names(list(...)), 0, self$npar('free'))
      N <- self$nres
      return( -2 * self$logLik() + log(N)*k )
    },
    #' @description
    #' Akaike Information Criterion of the model predictions given the observed response
    #' @param ... other arguments (ignored)
    AIC = function(...) {
      k <- ifelse('newdata' %in% names(list(...)), 0, self$npar('free'))
      return( -2 * self$logLik() + 2*k )
    },
    #' @description
    #' Small-sample corrected Akaike Information Criterion of the model predictions given the responses
    #' @param ... other arguments (ignored)
    AICc = function(...) {
      k <- ifelse('newdata' %in% names(list(...)), 0, self$npar('free'))
      if (k == 0) {
        return(self$AIC())
      }
      N <- self$nres
      self$AIC() + (2 * k * (k+1)) / (N - k - 1)
    },
    #' @description
    #' Mean squared error between the model predictions given the observed responses
    #' @param ... other arguments (ignored)
    MSE = function(...) {
      return(self$gof(type = 'mse', ...))
    },
    #' @description
    #' Sum of squared errors between the model predictions given the observed responses
    #' @param ... other arguments (ignored)
    SSE = function(...) {
      return(self$gof(type = 'sse', ...))
    },
    #' @description
    #' Root mean squared error between the model predictions and the observed responses
    #' @param ... other arguments (ignored)
    RMSE = function(...) {
      return(self$gof(type = 'rmse', ...))
    },

    #' @description
    #' Change the lower limit of parameter values
    #' @param ... new parameter bound, e.g. \code{alpha = 0} sets the lower bound of the parameter \code{alpha} to 0
    set_lb = function(...) {
      x <- as.list(...)
      private$check_parnames(x)
      self$parspace[, "lb"] <- replace(self$parspace[, "lb"], match(x, rownames(self$parspace)), x[i])
      return(invisible(self))
    },

    #' @description
    #' Change the upper limit of parameter values
    #' @param ... new parameter bound, e.g. \code{alpha = 0} sets the lower bound of the parameter \code{alpha} to 0
    set_ub = function(...) {
      x <- as.list(...)
      private$check_parnames(x)
      self$parspace[, "ub"] <- replace(self$parspace[, "ub"], match(x, rownames(self$parspace)), x[i])
      return(invisible(self))
    },

    #' @description
    #' Change the starting values for fitting parameters
    #' @param ... new parameter bound, e.g. \code{alpha = 0} sets the lower bound of the parameter \code{alpha} to 0
    set_start = function(...) {
      x <- as.list(...)
      private$check_parnames(x)
      self$parspace[, "start"] <- replace(self$parspace[, "start"], match(x, rownames(self$parspace)), x[i])
    },
   
    #' @description
    #' Prints the model object
    #' @param digits A number specifying the number of digits to print
    print = function(digits = 2) {
      cat(self$title)
      cat('\nCall:\n',
      paste(.abbrDeparse(self$call), sep = '\n', collapse = '\n'), '\n\n', sep = '')
      note <- NULL
      if (self$npar("free") > 0L) {
        cat('Free parameter estimates:\n')
        par <- self$get_par()[setdiff(private$get_parnames('free'), private$get_parnames('constrained'))]
        print.default(format(par, digits = digits, justify = 'centre', width = digits+2L), print.gap=2L, quote=FALSE)
      } else {
        note <- 'No free parameter.'
      }
      if (self$ncon > 0L) {
        cat('\nConstrained parameter:\n')
        par <- self$get_par()[private$get_parnames() %in% c(private$get_parnames("constrained"), private$get_parnames("constant"))]
        print.default(format(par, digits = digits, justify = 'centre', width = digits+2L), print.gap=2L, quote=FALSE)
      } else {
        note <- cbind(note, "No fixed parameter. ")
      }
      if (self$ncon  > 0L) {
        note <- cbind(note, "View constraints by 'M$constraints'.")
      }
      if (!inherits(M, "csm")) {
        if ( is.null(self$choicerule) ) {
          note <- c(note, 'No choice rule. ')
        } else {
          cat('Choice rule:', self$choicerule)
        }
      }
      
      if ( length(note) ) cat('\n---\nNote: ', note)
      cat('\n')
      invisible(self)
    },

    #' @description
    #' Summarizes the model, it's parameters, and some fit information
    summary = function() {
      coef.table <- matrix(self$par)
      dimnames(coef.table) <- list(names(self$par), c("Estimate"))
      constr.parnames <- names(self$par)[!names(self$par) %in% private$get_parnames("free")]
      rownames(coef.table) <- replace(
        names(self$par),
        names(self$par) %in% constr.parnames,
        paste0("(", constr.parnames, ")"))
      ans <- list(call = self$formula,
                  title = self$title,
                  mse = self$MSE(),
                  aic = tryCatch(self$AIC(), error = function(e) NA),
                  bic = tryCatch(self$BIC(), error = function(e) NA),
                  logLik = tryCatch(self$logLik(), error = function(e) NA),
                  freenames = private$get_parnames('free'),
                  coefficients = coef.table)
      class(ans) <- "summary.cogscimodel"
      return(ans)
    },

    #' @description
    #' Produces a parameter grid
    #' @param nsteps A list with steps for each parameter
    #' @param offset A number by which to offset the parameters from their bounds
    #' @param par (optional) A vector with parameter names for which to generate the grid.
    pargrid = function(nsteps = NULL, offset = NULL, par = NULL) {
      return(private$make_pargrid(offset=offset, nsteps=nsteps, par=par))
    },

    #' @description
    #' Retrieves the call
    getCall = function() {
      return(self$call)
    }
  ),







  # -------------------------------------------------------------------------
  # Private methods
  private = list(
    # Get the inputs to the model
    get_input = function(f = self$formula, d, ...) {
      f <- as.Formula(f)
      if (length(d) == 0) {
        return()
      }
      if (inherits(try(.get_rhs_vars(f, d), silent = TRUE), "try-error")) {
        stop("Can't find variables from 'formula' in 'data': ", .brackify(setdiff(unlist(.rhs_varnames(f)), names(d))), ".")
      }
     
      # n observations
      no <- nrow(d)
      # n stimuli
      ns <- length(f)[2] 
      # n attributes
      na <- max(.rhs_length(formula = f)) # n attributes per stimulus
      arr <- array(NA, dim = c(no, max(na), ns))
      for (s in seq_len(ns)) {
        arr[, , s][] <- as.matrix(model.frame(formula(f, lhs=0, rhs=s), data = d, ...))
      }
      if (ns == 1) {
        colnames(arr) <- attr(terms(formula(f, lhs=0, rhs=1)), "term.labels")
      }
      return(arr)
    },
    get_more_input = function(d = NULL) {
      return(NULL)
    },
    # Gets response
    get_res = function(f = NULL, d = NULL) {
      if (length(d) == 0) {
        return(self$res)
      } else {
        f <- as.Formula(f)
        if (length(f)[1] > 0) {
          return(get_all_vars(formula(f, rhs = 0), d))
        } else {
          return(NULL)
        }    
      }
    },
    # Set the mode
    set_mode = function(x) {
      x <- match.arg(x, c("discrete", "continuous"))
      self$mode <- x
    },
    get_lb = function(x = "all") {
      return(setNames(self$parspace[private$get_parnames(x), "lb"], private$get_parnames(x)))
    },
    get_ub = function(x = "all") {
      return(setNames(self$parspace[private$get_parnames(x), "ub"], private$get_parnames(x)))
    },
    get_start = function(x = "all") {
      return(setNames(self$parspace[private$get_parnames(x), "start"], private$get_parnames(x)))
    },
    get_parnames = function(x = "all") {
      x <- match.arg(x, c("all", "free", "fix", "choicerule", "constrained", "ignored", "equal", "constant"))
      if (x == "all" & is.null(self$parnames)) {
        return(names(self$parspace))
      }
      return(unlist(self$parnames[[x]]))
    },
    get_stimnames = function(f = self$formula) {
      return(self$stimnames[seq_len(self$nstim)])
    },
    # Helper functions
    init_discount = function(x) {
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
    infer_mode = function(y) {
      if (!is.factor(y) & any(floor(y) != y) | length(unique(y)) > 2) {
        message('Inferring mode from response variable ... "continuous". Change by setting mode="discrete".')
          return("continuous")
        } else { message('Inferring mode from response variable ... "discrete". Change by setting mode="continuous".')
          return("discrete")
        }
    },


    # INITIALIZE METHODS
    # -------------------------------------------------------------------------
    init_parspace = function(p, cr, options = list()) {
      if (is.null(self$mode)) { stop("init_parspace() must be called after init_mode().") }
      sigma_par <- cr_par <- NULL
      if (!is.null(cr)) {
        cr_par <- if (cr == "softmax") {
          make_parspace(tau = c(0.001, 10, 0.5, NA))
        } else if (cr == "epsilon") {
          make_parspace(eps = c(0.001, 1L, 0.2, NA))
        }
      }
      if (self$mode == "continuous" & !is.null(options)) {
        options <- do.call(cogscimodel_options, options[!duplicated(names(options))])
        if (options$fit_measure == "loglikelihood") {
          if (!is.null(self$res)) {
            rg <- max(self$res) - min(self$res)
            sigma_par <- make_parspace(sigma = c(0, rg, rg/2, NA))
          }
          if ("sigma" %in% rownames(p)) {
            if (identical(p["sigma",,drop = FALSE], sigma_par)) {
              sigma_par <- NULL
            }
          } 
        }
      }
      p <- rbind(p, cr_par, sigma_par)
      
      if (length(options$lb)) {
        p[names(options$lb), "lb"] <- options$lb
      }
      if (length(options$ub)) {
        p[names(options$ub), "ub"] <- options$ub
      }
      not_btw <- (!p[intersect(names(options$lb),names(options$ub)), "start"] %between% list(options$lb, options$ub))
      p[not_btw, "start"] <- rowMeans(p[not_btw, c("lb", "ub"), drop=FALSE])
      p[names(options$start), "start"] <- options$start

      self$parspace <- p
    },
    init_fix = function(fix) {
      if (length(fix) == 0L) {
        return(NULL)
      } else if (c(fix[1]) == "start" & length(fix) == 1L) {
        return(setNames(self$parspace[, "start"], rownames(self$parspace)))
      } else if (anyNA(fix)) {
        return(replace(fix, is.na(fix), self$parspace[names(fix)[is.na(fix)], "na"]))
      } else {
        return(fix)
      }
    },
    init_parnames = function(parspace, fix, choicerule) {
      if (is.null(self$parspace)) { stop("init_parnames() must be called after init_parspace(). --> move init_parnames after init_parspace.") }
      parnames <- rownames(parspace)
      fixednames <- names(fix)
      # parameters that are constrained without those ignored
      constrained = setdiff(fixednames, fixednames[unlist(lapply(fix, is.na))])
      pn <- list(
        all = parnames,
        # parameters that are not fixed and not constrained
        free = setdiff(setdiff(parnames, fixednames), constrained),
        fix = intersect(parnames, fixednames),
        # parameters that are constrained without those ignored
        constrained = setdiff(fixednames, fixednames[unlist(lapply(fix, is.na))]),
        # parameters equal to a value
        constant = setdiff(fixednames, fixednames[unlist(lapply(fix, is.character))]),
        # parameters equal to another parameter
        equal = fixednames[unlist(lapply(fix, is.character))],
        # parameters that have NA values
        ignored = fixednames[unlist(lapply(fix, is.na))]
      )
      if (!is.null(choicerule)) {
        pn[["choicerule"]] <- switch(choicerule,
                              softmax = "tau",
                              epsilon = "eps")
      }
      self$parnames <- pn
    },
    init_par = function(parspace, fix) {
      if (is.null(self$parspace)) { stop("init_par() must be called after init_parspace().")
        }
      self$par <- setNames(as.list(self$parspace[, "start"]), rownames(self$parspace))
    },
    init_constraints = function(fix) {
      # constraints from child model objects and the supplied 'fix' argument
      C <- .combine_constraints(private$make_constraints(),
                                .make_constraints(parspace = self$parspace, fix = fix))
      # check over-constrained problems     
      if ((length(self$par) - length(C)) < 0) {
          message("Too many constraints: ", length(self$npar), " parameter and ", length(C), " constraints. View constraints and parameter using `M$constraints` and `M$par`.")
      }
      
      # store values and constraint
      self$constraints <- C
      self$ncon <- length(C)
      if (self$ncon > 0) {
        self$set_par(.solve_constraints(C), constrain = FALSE)
        self$set_par(self$par) #fixme (this seems inefficient)
        self$parnames$free <- C$names[.which.underdetermined(C)]
        self$parnames$constrained <- names(.solve_constraints(C))
      }
    },
    init_mode = function(mode = NULL) {
      if (is.null(mode)) {
        if (is.null(self$res)) { stop("init_mode() called before data was initialized. -> move init_mode() after set_data().")}
        self$mode <- private$infer_mode(y = self$res)
      }
      private$set_mode(mode)
    },
    init_stimnames = function() {
      self$stimnames <- abbreviate(private$make_stimnames(), minlength = 1, use.classes = FALSE)
    },
    init_prednames = function() {
      self$prednames <- paste0("pr_", abbreviate(private$make_prednames(), minlength = 1))
    },
    init_options = function(...) {
      .args <- as.list(...)
      .args <- .args[!duplicated(names(.args))] # first argument is user-supplied, second time it occurs it is the default of a model
      ## IF fitting with grid followed by a solver
      # fit_solver <- .args$fit_solver
      fit_solver <- match.arg(.args$fit_solver, c("grid", "solnp", "auto", names(ROI::ROI_registered_solvers())), several.ok = TRUE)
      if (!is.null(fit_solver)) {
        if(fit_solver[1] == "grid" & length(fit_solver) > 1L) {
          npar <- self$npar("free")
          ## use the top n parameter from the grid search, n = num free par
          if (is.null(.args$fit_control$nbest)) {
            .args$fit_control$nbest <- npar
          }       
          ub <- private$get_ub("free")
          lb <- private$get_lb("free")
          ## offset, scales logistically from super small to 10% of the range of each parameter
          if (is.null(.args$fit_grid_offset))
            .args$fit_grid_offset <- as.list(0.10 / (1 + exp(-(ub - lb))))
          ## make the steps exponentially bigger with the parameter range
          if (is.null(.args$fit_control$nsteps))
            .args$fit_control$nsteps <- round(pmax(log(ub - lb) * 2, 3) * max(1, log(npar)))
        }
      }
      self$options <- do.call(cogscimodel_options, args = .args)
    },

    # FIT FUNCTIONS
    # -------------------------------------------------------------------------
    objective = function(par, self) {
      if (all(par == 0L) & is.null(names(par))) {
        par <- private$get_start() # hack because ROI solver strips names
      }
      if (any(par < private$get_lb()[names(par)] | par > private$get_ub()[names(par)])) {
        return(-1e10)
      }
      self$set_par(x = par, check = FALSE)      
      .args <- list(
        type = self$options$fit_measure,
        n = self$options$fit_n,
        newdata = self$options$fit_data,
        options = self$options$options,
        discount = !is.null(self$discount))
      maxi <- self$options$fit_measure %in% c("loglikelihood", "accuracy")
      objval <- do.call(self$gof, args = .args, envir = parent.frame()) * (-1)^maxi
      if(any(!is.finite(objval))) {
        print(par)
      }
      return(objval)
    },
    fit_roi = function(start = private$get_start("free"), cons) {
      objective <- ROI::F_objective(
        F = function(par) { private$objective(par, self = self) },
        n = self$npar("free"),
        names = private$get_parnames("free")
      )      
      bounds <- ROI::V_bound(
        li = seq_len(self$npar("free")), lb = private$get_lb("free"),
        ui = seq_len(self$npar("free")), ub = private$get_ub("free"),
        names = private$get_parnames("free")
      )
      problem <- ROI::OP(
        objective = objective,
        constraints = cons,
        bounds = bounds
      )
      sol <- do.call(
        what = ROI::ROI_solve,
        args = c(list(
          x = problem,
          solver = self$options$fit_solver,
          start = start,
          control = self$options$fit_control[grep("grid", names(self$options$fit_control))])
          ),
        envir = parent.frame())
      if (sol$status$code == 1L) {
        print(sol)
      }
      return(sol)
    },
    fit_solnp = function(start = private$get_start("free"), cons, ...) {
      .args <-  list(
        pars = start,
        fun = private$objective,
        LB = private$get_lb("free"),
        UB = private$get_ub("free"),
        eqfun = NULL,
        eqB = NULL,
        self = self,
        control = self$options$fit_control)
      if (length(cons) > 0) {
        A <- as.matrix(cons$L)
        .args$eqB <- cons$rhs
        .args$eqfun <- function(par, self) {
          return(force(A) %*% par)
        }
      }
      fit <- do.call(Rsolnp::solnp, args = force(.args), env = parent.frame())
      fit$solution <- fit$pars
      fit$objval <- tail(fit$value, 1)
      if (fit$convergence != 0) {
        message("No optimal solution found.\nThe solver did not converge.")
      }
      return(fit)
    },
    fit_grid = function(par = self$get_par("free"), ...) {
      n   <- self$options$fit_control$nbest
      G <- private$make_pargrid(which_par = par, ...)
      objvals <- sapply(1:nrow(G$ids), function(i) {
          private$objective(par = get_id_in_grid(i, G), self = self)
        })
      best_ids <- which(rank(objvals, ties.method = "random") <= n)
      best_ids <- best_ids[order(objvals[best_ids])]
      best_par <- t(sapply(best_ids, get_id_in_grid, grid = G))
      return(list(
        solution = best_par[, private$get_parnames("free"), drop = FALSE],
        objval = objvals[best_ids])
      )
    },
    make_stimnames = function() {
      f <- as.Formula(self$formula)
      sn <- lapply(1:length(f)[2], function(i) attr(terms(formula(f, lhs=0, rhs=i)), "term.labels"))
      sn <- sapply(sn, paste0, collapse = "")
      while (any(grepl(" ", sn))) {
        sn <- gsub(" ", "", sn)
      }
      return(sn)
    },
    make_prednames = function() {
      if (self$mode == "discrete") {
        return(self$stimnames)
      } else {
        return(.lhs_var(self$formula))
      }
    },
    make_pargrid = function(offset = NULL, nsteps = NULL,  par = NULL, ...) {
      if (is.null(offset)) offset <- self$options$fit_grid_offset
      if (is.null(nsteps)) nsteps <- self$options$fit_control$nsteps
      x <- if (is.null(par)) { "free" } else { "all" }
      par <- if (is.null(par)) { 1:length(private$get_parnames(x)) }
      if (length(.simplify_constraints(self$constraints)) > 0L) { warning('Note: fit_solver="grid" does not respect linear or quadratic constraints, maybe change the solver. To this end use: options = list(fit_solver = ...), e.g, "solnp" or "optimx".') }
      return(make_grid_id_list(
        names = private$get_parnames(x)[par],
        lb = private$get_lb(x)[par],
        ub = private$get_ub(x)[par],
        offset = offset,
        nsteps = nsteps,
        ...))
    },
    make_random_par = function(parspace) {
      if (!is.null(self$constraints)) { message('Note: fit_solver="grid" does NOT respect linear or quadratic constraints -> consider a differnt solver. To this end use: options = list(fit_solver = " "), e.g, "solnp" or "optimx".') }

      # TODO: check this fun, it's experimental (5^? more? see literature)
      n <- log(5^nrow(parspace))
      par <- apply(parspace, 1, function(i) runif(n, min = i["lb"], max = i["ub"]))
      colnames(par) <- rownames(parspace)
      return(par)
    },
    make_constraints = function() {
      return(NULL)
    },
    
    simplify_constraints = function(C) {
      if (length(C) > 0) {
        # Check for fully-constrained problem (e.g. a = 1 and b = "a")
        # -> n x n constraints
        A <- as.matrix(C$L)
        i_constr <- which(1:C$L$ncol %in% C$L$j)
        A <- A[, i_constr, drop = FALSE] # remove unconstrained elements    
        if (ncol(A) == nrow(A)) { # quadratic means fully-constrained
          ss <- solve(A, C$rhs) # solve linear system
          pn <- private$get_parnames()
          ss <- setNames(ss, pn[i_constr])
          new_fix <- c(as.list(ss), self$par[private$get_parnames("fix")])
          new_fix <- new_fix[!duplicated(names(new_fix))]
          private$init_parnames(parspace=self$parspace, fix=new_fix, choicerule = self$choicerule)
          private$init_par(parspace=self$parspace, fix=new_fix)
        }
      }
      return(C)
    },

    # CHECK FUNCTIONS
    # -------------------------------------------------------------------------
    check_input = function() {
    },
    check_parclass = function(x) {
    },
    check_parnames = function(x) {
      if (self$pass_checks == TRUE) return() 
      sapply(names(x), function(n) {
        if (!n %in% rownames(self$parspace)) {
          stop("The parameter ", sQuote(n), " is no model parameter. The parameter names are ", sQuote(rownames(self$parspace)), ".", call.=FALSE)
        }
      })
      if (any(duplicated(names(x)))) {
          stop("Parameter names must be unique, but the following appear multiple times: ", .brackify(sQuote(names(fix)[duplicated(names(fix))])),".", call.=FALSE)
      }
    },
    check_par = function(x) {
      if (self$pass_checks == TRUE) return()
      if (is.null(x)) {
        return()
      }
      if (all(is.numeric(x))) {
        x <- as.list(x)
      }
      if (!is.list(x)) {
        stop("Values in set_par() need to be a list, but are a ", typeof(x), ".", call.=FALSE)
      }
      parspace <- self$parspace
      private$check_parnames(x)
      sapply(names(x), function(n) {      
        if (is.na(x[[n]]) & is.na(parspace[n, 'na'])) {
          stop("Ignoring the parameter ",dQuote(n)," is not possible.\n\nBackground: Ignoring a parameter by setting it to NA only works if the model's 'parspace' has a value in the 'na' column, which is not the case for the parameter ", dQuote(n), " in this model.", call.=FALSE)
        }
        if (!is.na(x[[n]]) & !is.character(x[[n]])) {
          if (x[[n]] < parspace[n, "lb"] | x[[n]] > parspace[n, "ub"]) {
          stop("Parameter ", sQuote(n), " in 'fix' must be between ", parspace[n, "lb"]," and ", parspace[n, "ub"], ", it cannot be ", x[[n]], ".", call.=FALSE)
          }
        }

      })
    },
    check_fix = function(fix) {
      if (self$pass_checks == TRUE) return()
      if (length(fix) > 0L) {
        if (all(is.numeric(fix))) {
          fix <- as.list(fix)
        }
        if (is.list(fix) == FALSE) {
          stop("'fix' needs to be a list, but is a ", typeof(fix), ".")
        }

        private$check_parnames(fix)
        parspace <- self$parspace
        par <- self$get_par()

        if (!all( names(fix) %in% rownames(parspace))) {
          stop("In 'fix' you can set the parameter ", .brackify(dQuote(rownames(parspace))), ", but not ", .brackify(dQuote(setdiff(names(fix), rownames(parspace)))), ". Did you misspell a parameter?", call.=FALSE)
        }
        if (!all(fix[names(fix) %in% private$get_parnames("equal")] %in% private$get_parnames())) {
          stop("Check equality constraints in 'fix'. The equality constraints ", .brackify(
            apply(cbind(private$get_parnames('equal'), self$par[private$get_parnames('equal')]), 1, paste, collapse="=")[fix[names(fix) %in% private$get_parnames('equal')] %in% private$get_parnames()]
            ), " are not valid, because you can only constrain parameters to equal the model's parameter space, which contains the following parameters ", .brackify(rownames(parspace)), ".", call.=FALSE)
        }
        if ( length(fix) < nrow(parspace) & is.null(self$res) & self$options$fit == TRUE ) {
          stop("'formula' must have a left side to estimate parameter ", .brackify(setdiff(rownames(parspace), names(fix))), ".\n  
            * Did you forget to add a left-hand to the formula?\n  
            * Did you forget to fix the parameter?", call. = FALSE)
        }

        lapply(seq_len(self$npar('equal')), function(i, fix) {
            p <- unlist(fix[private$get_parnames('equal')][i])
            newp <- setNames(list(self$par[[p]]), names(p))
            tryCatch(private$check_par( newp ), error = function(e) stop("Can't set parameter ", names(p), " = ", dQuote(p),", because the value of ",dQuote(p)," (",self$get_par()[p],") lies outside of the allowed range of ", dQuote(names(p))," (",paste(parspace[names(p), c("lb","ub")], collapse=" to "),").",call.=FALSE))

          }, fix = fix)
      }
    },


    # OTHER USEFUL STUFF
    constrain = function(par, C = self$constraints) {
      if (is.null(self$parspace)) { stop('constrain() called before parspace was initialized -> move it after init_parspace().') }
      if (is.null(self$parnames) & length(self$parspace)) { stop('constrain() called before parnames was initialized -> move it after init_parnames().') }
      if (is.null(self$par)) { stop('constrain() called before par were initialized -> move it after init_par().') }
      A <- as.matrix(C$L)
      eq <- rowSums(A) == 0 & (rowSums(A != 0) == 2)
      if (length(eq)) {
        i <- C$names[apply(A[eq, , drop=FALSE]==1, 1, which)]
        j <- C$names[apply(A[eq, , drop=FALSE]==-1, 1, which)]
        par[i] <- par[j]
      }
      return(par)
    },

    # Passes the predictions through the choicerule
    apply_choicerule = function(x) {
      if (is.null(self$choicerule)) {
        return(x)
      } else {
        args <- c(list(x = x, type = self$choicerule), as.list(self$get_par('choicerule')))
        x[] <- do.call(cogsciutils::choicerule, args)[,1:ncol(x), drop = FALSE]
        return(x)
      }
    },

    #' Remove choicerule
    rm_choicerule = function() {
      keep <- setdiff(names(self$get_par()), names(self$get_par("choicerule")))
      self$parspace <- self$parspace[keep,,drop=FALSE]
      self$choicerule <- NULL
    }
  )
)
      
      