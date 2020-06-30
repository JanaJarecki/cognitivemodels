# ==========================================================================
# Package: Cognitivemodels
# File: cm-class.R
# Author: Jana B. Jarecki
# ==========================================================================

# ==========================================================================
# Main R6 Class that is the Basis of Cognitive Models
# ==========================================================================



#' The R6 class underlying all "cm" (cognitive model) objects
#' 
#' \code{Cm$new(formula, data, parspace)}
#' 
#' @import methods
#' @import stats
#' @import Rsolnp
#' @import ROI
#' @import cognitiveutils
#' @import R6
#' @import Formula
#' @importFrom matlib showEqn
#' @importFrom rlang call_standardise
#' 
#' @aliases cm-class
#' 
#' @template cm
#' 
#' @param parspace  (optional, \bold{required} to add model parameters) A n x 4 matrix, the parameter space. Use \link{make_parspace} to construct it. Column names must be `"lb","ub","start","na"`, row names must be parameter names. Columns contain the lower limit, upper limit, starting value in fitting, and (optional) a value that makes a parameter have zero effect, which can be NA. See details.
#' @template param-choicerule
#' @template param-fix
#' @param title (optional, default is the class name) A string, the model's name.
#' @param mode A string, the response mode. Allowed are `"discrete"`, `"continuous". Discrete responses are binary (0 or 1), continuous responses are numbers-
#' @param discount (optional) An integer or integer vector (default \code{0}), ddefining which or how many, starting from trial 1, to discount when fitting. 
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
#' @examples 
#' # No examples yet.
#' @export
Cm <- R6Class(
  "cm",
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
    #' @field fix Fixed model parameters, a named list
    fix = NULL,
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
    # this is for testing purposes
    # make_prediction = NA,

    #' @description
    #' Initializes a new model
    initialize = function(formula, data = NULL, parspace = make_parspace(), fix = NULL, choicerule = if (grepl("^c", mode)) { "none" } else { NULL }, title = NULL, discount = NULL, mode = NULL, options = NULL) {
      self$title        <- title
      self$formula      <- as.Formula(formula)
      self$pass_checks  <- length(data) == 0
      self$call  <- if (deparse(sys.call()[[1L]]) == "super$initialize") {
        if (!inherits(self, "csm")) { rlang::call_standardise(sys.calls()[[sys.nframe()-4L]]) }
        } else { rlang::call_standardise(sys.call(sys.nframe()-1L)) }
      self$discount     <- private$init_discount(x = discount)
      mode <- match.arg(mode, c("continuous", "discrete"))
      self$choicerule   <- .check_and_match_choicerule(x = choicerule, mode = mode)
      
      # Initialize slots of the model
      self$set_data(data = data)
      private$init_par(parspace = parspace, fix = fix, options = options, mode = mode)
      private$init_stimnames()
      private$init_prednames()
      private$init_options(options = options)

      # Checks
      # ! after setting formula, parspace, choicerule, etc.
      private$check_input()
      .check_par(fix, self$parspace, self$pass_checks)

      # Automatically fit free parameters
      if (length(data) > 0 && (self$options$fit == TRUE) && (self$npar("free") > 0L)) {
        self$fit()
      }
    },

    #' @description
    #' Fits the free model parameters to the response data
    #' @param solver (optional) A string with the model solver
    #' @param measure (optional) A string with the goodness-of-fit measure that the solver optimizes (e.g. \code{"loglikelihood"}). Possible values, see the \code{type} argument in \link[cognitiveutils]{gof}
    #' @param ... other arguments
    fit = function(solver = self$options$solver, measure = self$options$fit_measure, ...) {
      message("Fitting free parameters ",
          .brackify(self$parnames$free2),
          " by ", ifelse(grepl("loglikelihood|accuracy", self$options$fit_measure), "maximizing ", "minimizing "), self$options$fit_measure, " with ", paste(self$options$solver, collapse=", "))
      solver <- .check_and_match_solver(solver = solver)
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
      self$set_par(solution[self$parnames$free], check = FALSE, constrain = FALSE)
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
      #   where make_prediction() is a function on
      #   the child class
      #   Note: we run make_prediction() per 2-dimensional input
      #         matrix!
      ns <- self$nstim
      RES <- sapply(seq.int(ns),
        function(s) { # s = stimulus number
          self$make_prediction(
            type = type,
            input = abind::adrop(input[, , s, drop = FALSE], 3),
            more_input = abind::adrop(more_input[, , s, drop = FALSE], 3),
            isnew = isnew,
            s = s)
        })

      # Finally we format the RES object a bit
      RES <- matrix(unlist(RES), nrow = dim(input)[1])
      colnames(RES) <- self$prednames[1:ncol(RES)]

      # And we apply the choice rule if needed
      type <- try(match.arg(type, c("response", "value")), silent = TRUE)
      # fixme: ugly hack to allow for more types than the two
      if (inherits(type, "try-error")) { type <- "response" }
      RES <- switch(type,
        value = RES,
        response = private$apply_choicerule(RES))
      return(drop(RES))
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
      if (missing(data) || is.null(data) || length(data) == 0) {
        data <- data.frame()
      } else if (!inherits(data, "data.frame")) {
        stop("'data' must be a data.frame, but is a ", class(data)[1], ifelse(is.vector(data), " vector ", ""), "", .brackify(data), ".",
        "\n  * Do you need to re-format your data?",
        "\n  * Did you forget to name the argument (data = ...) in the model?")

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
    #' @param constrain (optional, default \code{TRUE}) A logical, whether to newly constrain the parameters when setting a parameter that is constrained.
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
    #' @param x (optional) A string, which set of parameters to get, \code{"all"} returns all parameter, \code{"free"} returns the free parameters, \code{"constrained"} returns constrained parameters, \code{"equal"} returns parameters equal to another parameter
    get_par = function(x = "all") {
      return(unlist(self$par[private$get_parnames(x)]))
    },

    #' @description
    #' Number of model parameters
    #' @param x  A string, which of the parameters to return, allowed are \code{"all", "free", "constrained", "equal"}
    npar = function(x = "free") {
      ans <- try(match.arg(x, c("free", "all")), silent = TRUE)
      if (class(ans) == "try-error") { # not "free"
        return(nrow(self$parspace) - self$ncon)
      } else {
        switch(x,
          "free" = nrow(self$parspace) - self$ncon,
           "all" = nrow(self$parspace))
      }
    }, 

    #' @description
    #' Computes the goodness of model fit
    #' @param type A string, which goodness of fit measure to use, e.g. \code{"loglikelihood"}; for the allowed values see \link[cognitiveutils]{gof}
    #' @param n (optional) When fitting to aggregate data, supply how many raw data points underly each aggregated data point
    #' @param newdata (optional) A data frame with new data - experimental!
    #' @param ... other arguments (ignored)
    gof = function(type = self$options$fit_measure, n = self$options$fit_args$n, newdata = self$options$fit_data, discount = FALSE, ...) {
      if (length(self$res) == 0L) { stop("Model must have response variable to calculate the goodness of fit, but response are ", self$res, ".",
        "\n  * Did you forget a left side in 'formula'?", call.=FALSE) }


      if (length(newdata) == 0L) {
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
      options <- c(list(...), self$options$fit_args)
      options <- options[!duplicated(names(options)) & !grepl("n", names(options))]

      .args <- c(
        list(
          obs = obs,
          pred = pred,
          type = type,
          na.rm = TRUE,
          n = n,
          response = self$mode,
          sigma = if (self$mode == "continuous" & type == "loglikelihood") {
                self$get_par()["sigma"] }
          ),
          options
        )
      gof <- try(do.call(cognitiveutils::gof, args = .args, envir = parent.frame()), silent = TRUE)
      if (inherits(gof, "try-error")) {
        stop("Can't compute the model fit ", type, ", because:\n  ", geterrmessage(),
          call.= FALSE)
      } else {
        return(gof)
      }
    },
    #' @description
    #' Log likelihood of the observed responses under the model predictions
    #' @param ... other arguments (ignored)
    logLik = function(...) {
      ll <- self$gof(type = "loglikelihood", ...)
      k <- ifelse("newdata" %in% names(list(...)), 0L, self$npar("free"))
      attributes(ll) <- list(df = k, nobs = self$nobs)
      class(ll) <- "logLik"
      return(ll)
    },
    #' @description
    #' Bayesian Information Criterion of the model predictions given the observed responses
    #' @param ... other arguments (ignored)
    BIC = function(...) {
      stats::BIC(self)
    },
    #' @description
    #' Akaike Information Criterion of the model predictions given the observed response
    #' @param ... other arguments (ignored)
    AIC = function(...) {
      k <- ifelse('newdata' %in% names(list(...)), 0, self$npar("free"))
      stats::AIC(self, k = k)
    },
    #' @description
    #' Small-sample corrected Akaike Information Criterion of the model predictions given the responses
    #' @param ... other arguments (ignored)
    AICc = function(...) {
      k <- ifelse('newdata' %in% names(list(...)), 0, self$npar('free'))
      if (k == 0L) {
        return(self$AIC())
      } else {
        return(self$AIC() + (2 * k * (k+1)) / (self$nobs - k - 1))
      }
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
      .check_parnames(names(x), rownames(self$parspace))
      self$parspace[, "lb"] <- replace(self$parspace[, "lb"], match(x, rownames(self$parspace)), x[i])
      return(invisible(self))
    },

    #' @description
    #' Change the upper limit of parameter values
    #' @param ... new parameter bound, e.g. \code{alpha = 0} sets the lower bound of the parameter \code{alpha} to 0
    set_ub = function(...) {
      x <- as.list(...)
      .check_parnames(names(x), rownames(self$parspace))
      self$parspace[, "ub"] <- replace(self$parspace[, "ub"], match(x, rownames(self$parspace)), x[i])
      return(invisible(self))
    },

    #' @description
    #' Change the starting values for fitting parameters
    #' @param ... new parameter bound, e.g. \code{alpha = 0} sets the lower bound of the parameter \code{alpha} to 0
    set_start = function(...) {
      x <- as.list(...)
      .check_parnames(names(x), rownames(self$parspace))
      self$parspace[, "start"] <- replace(self$parspace[, "start"], match(x, rownames(self$parspace)), x[i])
    },
   
    #' @description
    #' Prints the model object
    #' @param digits A number specifying the number of digits to print
    print = function(digits = 2) {
      title <- self$title
      if (!inherits(self, "csm")) {
        title <- paste(title, "| choice rule:", self$choicerule)
      }
      cat(title)
      cat('\nCall:\n',
      paste(.abbrDeparse(self$call), sep = '\n', collapse = '\n'), '\n\n', sep = '')
      note <- NULL
      if (self$npar("free") > 0L) {
        title <- "Free parameter:" 
        if (self$options$fit == TRUE) {
          title <- "Free parameters:"
          if (!is.null(self$fitobj)) {
             title <- paste(title, "estimates")
            if (self$fitobj$convergence != 0) {
              title <- paste(title, "(NOT CONVERGED!)")
            }
          }
        }
        cat(title, "\n")
        par <- self$get_par()[self$parnames$free2]
        print.default(format(par, digits = digits, justify = 'centre', width = digits+2L), print.gap=2L, quote=FALSE)
        cat("\n")
      } else {
        note <- 'No free parameters.'
      }
      if (self$ncon > 0L) {
        cat("Constrained and fixed parameters:\n")
        par <- self$get_par()[!.which_free(self$constraints)]
        print.default(format(par, digits = digits, justify = 'centre', width = digits+2L), print.gap=2L, quote=FALSE)
      } else {
        note <- cbind(note, "No fixed parameter. ")
      }
      if (self$ncon  > 0L) {
        note <- cbind(note, "View constraints by constraints(.), view parameter space by parspace(.)")
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
      class(ans) <- "summary.cm"
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
  # -------------------------------------------------------------------------
  # -------------------------------------------------------------------------
  # -------------------------------------------------------------------------
  # Private methods
  private = list(
    # Get the inputs to the model
    get_input = function(f = self$formula, d, ...) {
      f <- as.Formula(f)
      d <- as.data.frame(d)
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
      na <- .rhs_length(f)
      arr <- array(NA, dim = c(no, max(na), ns))
      for (s in seq_len(ns)) {
        arr[, 1:na[s], s][] <- as.matrix(model.frame(formula(f, lhs=0, rhs=s), data = d, ...))
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
        d <- as.data.frame(d)
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
    init_parspace = function(p, choicerule, options = list(), mode, addpar = TRUE) {
      private$init_mode(mode = mode)
      sigma_par <- choicerule_par <- NULL
      if (!length(choicerule) & addpar == TRUE) {
        choicerule_par <- if (choicerule == "softmax") {
          make_parspace(tau = c(0.001, 10, 0.5, NA))
        } else if (choicerule == "epsilon") {
          make_parspace(eps = c(0.001, 1L, 0.2, NA))
        }
      }
      if (self$mode == "continuous" & !length(options) & addpar == TRUE) {
        options <- do.call(cm_options, options[!duplicated(names(options))])
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
      p <- rbind(p, choicerule_par, sigma_par)
      
      if (length(options$lb)) {
        p[names(options$lb), "lb"] <- options$lb
      }
      if (length(options$ub)) {
        p[names(options$ub), "ub"] <- options$ub
      }
      not_btw <- (!p[intersect(names(options$lb),names(options$ub)), "start"] %between% list(options$lb, options$ub))
      p[not_btw, "start"] <- rowMeans(p[not_btw, c("lb", "ub"), drop=FALSE])
      if (length(options$start)) {
        p[names(options$start), "start"] <- options$start
      }
      self$parspace <- p
    },
    init_fix = function(fix) {
      if (length(fix) == 0L) {
        fix <- NULL
      } else if (c(fix[1]) == "start" & length(fix) == 1L) {
        fix <- setNames(self$parspace[, "start"], rownames(self$parspace))
      } else if (anyNA(fix)) {
        fix <- replace(fix, is.na(fix), self$parspace[names(fix)[is.na(fix)], "na"])
      }
      self$fix <- fix
    },
    init_parnames = function(parspace = self$parspace, fix = self$fix, choicerule = self$choicerule) {
      self$parnames <- list(
        all = rownames(parspace),
        free = rownames(parspace), # should be named to_fit
        free2 = rownames(parspace), # should be free
        choicerule = switch(choicerule,
                            softmax = "tau",
                            epsilon = "eps"),
        ignored = names(fix)[unlist(lapply(fix, is.na))],
        constrained = NA
      )
    },
    init_par = function(parspace, fix, options, mode, addpar=TRUE) {
      private$init_parspace(p = parspace, choicerule = self$choicerule, options = options, mode = mode, addpar = addpar)
      .check_par(fix, self$parspace, self$pass_checks)
      private$init_fix(fix)
      private$init_parnames()
      self$par <- setNames(as.list(self$parspace[, "start"]), rownames(self$parspace))
      private$init_constraints(fix = self$fix)
    },
    init_constraints = function(fix) {
      # constraints from child model objects and the supplied 'fix' argument
      C <- .make_constraints(parspace = self$parspace, fix = fix)
      C2 <- private$make_constraints()
      if (length(fix) < length(self$par) & .consistent_constraints(C, C2)) {
        C <- .combine_constraints(C, C2)
      }

      # check over-constrained problems     
      if ((length(self$par) - length(C)) < 0) {
          message("Maybe many constraints: ", length(self$par), " parameter and ", length(C), " constraints. View constraints and parameter using `M$constraints` and `M$par`.")
      }      
      # store values and constraint
      self$constraints <- C
      # fixme: the second part is an ungly hack in case of unconstrained p
      self$ncon <- length(C)
      if (self$ncon > 0L) {
        self$ncon <- min(length(C), sum(!apply(as.matrix(C$L) == 0L, 2, all)))
        parvalues <- .solve_constraints(C, b = unlist(self$par))
        self$set_par(parvalues, constrain = FALSE)
        self$set_par(self$par, constrain = TRUE) #fixme (this seems inefficient)
        self$parnames$free <- C$names[.which_underdetermined(C)]
        self$parnames$fix <- C$names[.which_fix(C)]
        self$parnames$constrained <- C$names[.which_constrained(C)]
        self$parnames$free2 <- C$names[.which_free(C)]
      }
    },
    init_mode = function(mode = NULL) {
      if (is.null(mode)) {
        if (is.null(self$res)) { stop("init_mode() called before data was initialized. -> move init_mode() after set_data().")}
        self$mode <- private$infer_mode(y = self$res)
      }
      private$set_mode(mode)
       if (self$mode == "continuous") {
        self$choicerule <- "none"
      }
    },
    init_stimnames = function() {
      self$stimnames <- abbreviate(private$make_stimnames(), minlength = 1, use.classes = FALSE)
    },
    init_prednames = function() {
      self$prednames <- paste("pr", abbreviate(private$make_prednames(), minlength = 1), sep="_")
    },
    init_options = function(options = list(), ...) {
      .args <- list(...)
      .args <- if (length(.args)) { c(options, .args) } else { options }
      .args <- .args[!duplicated(names(.args))]
      solver <- cognitivemodels:::.check_and_match_solver(.args[["solver"]])
      if (!is.null(solver)) {
        if(solver[1] == "grid" & length(solver) > 1L) {
          npar <- self$npar("free")
          if (is.null(.args$solver_args$nbest)) {
            .args$solver_args$nbest <- npar
          }       
          ub <- private$get_ub("free")
          lb <- private$get_lb("free")
          ## offset, scales logistically from super small to 10% of the range of each parameter
          if (is.null(.args$solver_args$offset))
            .args$solver_args$offset <- as.list(0.10 / (1 + exp(-(ub - lb))))
          ## make the steps exponentially bigger with the parameter range
          if (is.null(.args$solver_args$nsteps))
            .args$solver_args$nsteps <- round(pmax(log(ub - lb) * 2, 3) * max(1, log(npar)))
        }
      }
      self$options <- do.call(cm_options, args = as.list(.args))
    },

    # FIT FUNCTIONS
    # -------------------------------------------------------------------------
    objective = function(par, self) {
      # The parameter here are only the free parameter!
      # the other parameters are retrieved in get_par() in the predict function
      if (all(par == 0L) & is.null(names(par))) {
        par <- private$get_start() # hack because ROI solver strips names
      }
      if (any(par < private$get_lb()[names(par)] | par > private$get_ub()[names(par)])) {
        return(-1e10)
      }
      self$set_par(x = par, check = FALSE, constrain = FALSE) 
      maxi <- self$options$fit_measure %in% c("loglikelihood", "accuracy")
      objval <- self$gof(
        type = self$options$fit_measure,
        newdata = self$options$fit_data,
        disount = !is.null(self$discount)) * (-1)^maxi
      if(any(!is.finite(objval))) {
        message("\nInfinite goodness of fit during optimization for parameter values:\n")
        writeLines(names(par), sep = "\t")
        writeLines(sprintf("%.4f",par), sep = "\t")
        cat("\n")
      }
      return(objval)
    },
    fit_roi = function(start = private$get_start("free"), cons) {
      if (length(cons) == 0) { cons <- NULL }
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
          solver = self$options$solver,
          start = start),
          self$options$solver_args$control
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
        control = self$options$solver_args$control)
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
      G <- private$make_pargrid(which_par = par, ...)
      objvals <- sapply(1:nrow(G$ids), function(i) {
          private$objective(par = get_id_in_grid(i, G), self = self)
        })
      n   <- self$options$solver_args$nbest
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
      if (is.null(offset)) offset <- self$options$solver_args$offset
      if (is.null(nsteps)) nsteps <- self$options$solver_args$nsteps
      x <- if (is.null(par)) { "free" } else { "all" }
      par <- if (is.null(par)) { 1:length(private$get_parnames(x)) }
      if (length(.simplify_constraints(self$constraints)) > 0L) { warning('Note: solver="grid" does not respect linear or quadratic constraints, maybe change the solver. To this end use: options = list(solver = ...), e.g, "solnp" or "optimx".') }
      return(make_grid_id_list(
        names = private$get_parnames(x)[par],
        lb = private$get_lb(x)[par],
        ub = private$get_ub(x)[par],
        offset = offset,
        nsteps = nsteps,
        ...))
    },
    make_random_par = function(parspace) {
      if (!is.null(self$constraints)) { message('Note: solver="grid" does NOT respect linear or quadratic constraints -> consider a differnt solver. To this end use: options = list(solver = " "), e.g, "solnp" or "optimx".') }

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
    check_par = function(x) {
      .check_par(x = x, parspace = self$parspace, pass = self$pass_checks)
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
    apply_choicerule = function(x, par = self$get_par("choicerule")) {
      if (self$choicerule == "none") {
        return(x)
      } else {
        args <- c(list(x = x, type = self$choicerule), as.list(par))
        x[] <- do.call(cognitiveutils::choicerule, args)[,1:ncol(x), drop = FALSE]
        return(x[, 1:max(1, (self$nopt - 1))])
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
      
      