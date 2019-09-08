#' Class for cognitive models
#' 
#' Cogscimodel$new(formula, data, parspace)
#' 
#' @import Rsolnp
#' @import ROI
#' @import ROI.plugin.optimx
#' @import ROI.plugin.alabama
#' @import ROI.plugin.deoptim
#' @import ROI.plugin.nloptr
#' @import cogsciutils
#' @import R6
#' @import Formula
#' @importFrom rlang call_standardise
#' 
#' @param formula A formula (e.g., \code{y ~ x1 + x2}).
#' @param data A data.frame containing \code{formula}'s variables.
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
#' @section Options
#' See \link{cogscimodel_options}, possible options:
#' \describe{
#'    \item{\code{fit}}{(default \code{TRUE}), \code{FALSE} omits fitting the free parameter.}
#'    \item{\code{fit_measure}}{(default \code{"loglikelihood"}). When fitting, which fit measure to use? See \link[cogsciutils]{gof}'s argument \code{type}.}
#'    \item{\code{fit_data}}{(default: \code{data}). When fitting, which data other than to fit the model to? Useful if you fit to other data than you predict.}
#'    \item{\code{fit_n}}{(default: \code{1}). When fitting, the number of reservations underlying each data point in \code{fit_data}. If this data is aggregated data, change this argument.}
#'    \item{\code{fit_roi_solver}}{(default: \code{"auto"}). When fitting with the \link{http://roi.r-forge.r-project.org/index.html}{ROI pakage}, which solver to use for the parameter estimation problem.}
#'    \item{\code{fit_grid_nbest}}{(default: \code{self$npar("free")}, no. of free parameters) When fitting with grid search followed by a solver, how many best grid-search solutions to use as starting parameters with the solver?}
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
    call = NULL,
    title = 'string',
    formula = 'Formula',
    input = 'matrix',
    more_input = NULL,
    res = NULL,
    pred = 'matrix',
    par = 'vector',
    parnames = list(),
    parspace = 'matrix',
    stimnames = character(),
    prednames = character(),
    mode = 'string',
    choicerule = NULL, 
    pred_types = c("response", "value"),
    optimization = NA,
    discount = 'numeric',
    fitobj = NULL,
    options = list(),
    initialize = function(formula, data, parspace = make_parspace(), fix = NULL, choicerule =  NULL, title = NULL, discount = NULL, mode = NULL, options = NULL) {
      # get call
      self$call <- if ( deparse(sys.call()[[1]]) == "super$initialize" ) {
        if (inherits(self, "cogscimodelstack")) {
          self$last_call
        } else {
          rlang::call_standardise(sys.calls()[[sys.nframe()-4L]])
        }
      } else {
        rlang::call_standardise(sys.call(sys.nframe()-1L))
      }
      if (all(is.numeric(fix))) {
        fix <- as.list(fix)
      }
      f <- as.Formula(formula)
      choicerule <- if (!is.null(choicerule)) match.arg(choicerule, c("softmax", "argmax", "luce", "epsilon"))

      # Assign variables to object
      self$title <- title
      self$choicerule <- choicerule
      self$set_formula(f=f)
      self$set_input(f=self$formula, d=data)
      self$set_res(f=self$formula, d=data)
      self$set_discount(x=discount)
      self$set_more_input(d=data)


      # Initialize values of parameter
      self$init_parspace(parspace=parspace, choicerule=choicerule)
      fix <- self$init_fix(fix)
      self$init_parnames(parspace=self$parspace,fix=fix,choicerule=choicerule)
      self$init_par(parspace=self$parspace, fix=fix)
      self$init_constraints()
      self$init_mode(mode=mode)
      self$init_stimnames()
      self$prednames <- self$get_prednames()
      self$init_options(options)
      # Checks
      # ! after setting formula, parspace, choicerule, etc.
      self$check_input()
      self$check_fix(fix)
      # Fit the model
      if ((self$options$fit == TRUE) & (self$npar("free") > 0L)) {
        message("Fitting free parameters ",
          .brackify(self$get_parnames("free")),
          " by ", ifelse(grepl("loglikelihood|accuracy", self$options$fit_measure), "maximizing ", "minimizing "), self$options$fit_measure, " with ", self$options$fit_solver)
        self$fit()
      }
    },
    # Prediction function -> calls child model's predict_fun
    predict = function(type = "response", newdata = NULL, ...) {
      # First we check if there is new data
      #  and get the input and more_input variables
      #  from new data
      if (is.null(newdata) | missing(newdata)) {
        isnew <- FALSE
        input <- self$input
        more_input <- self$more_input
      } else {
        isnew <- TRUE
        input <- self$get_input(f = self$formula, d = newdata, ...)
        more_input <- self$get_more_input(d = newdata)
      }
      # then we initialize the results object and run $make_prediction()
      #  where make_prediction() is a function on
      #  the child class
      #  Note: we run make_prediction() per 2-dimensional input
      #        matrix!
      ns <- self$nstim()
      RES <- sapply(seq.int(ns),
        function(s) {
          self$make_prediction(
            type = type,
            input = abind::adrop(input[, , s, drop = FALSE], 3),
            more_input = abind::adrop(more_input[, , s, drop = FALSE], 3),
            isnew = isnew,
            s = s)
        })

      # Finally we format the RES object a bit
      RES <- matrix(RES, nrow = dim(input)[1])
      colnames(RES) <- paste("pred", self$prednames[1:ncol(RES)], sep="_")

      # And we apply the choice rule if needed
      type <- try(match.arg(type, c("response", "value")), silent = TRUE)
      if (inherits(type, "try-error")) {
        type <- "response"
      }
      return(switch(type,
        value = RES,
        response = self$apply_choicerule(RES)))
    },
    #' @section Methods
    # Retrieve the call to super
    getCall = function() {
      return(self$call)
    },
    # Sets the formula
    set_formula = function(f) {
      self$formula <- f
    },
    get_formula = function() {
      return(self$formula)
    },
    # Set the stimuli as inputs to the model
    set_input = function(f = self$formula, d) {
      f <- as.Formula(f)
      self$input <- self$get_input(formula(f, lhs = 0), d)
    },
    # Get the inputs to the model
    get_input = function(f = self$formula, d, ...) {
      f <- as.Formula(f)
      no <- nrow(d) # n observations
      ns <- length(f)[2] # n stimuli
      na <- sapply(1:ns, function(i) length(attr(terms(formula(f, lhs=i, rhs=i)), "term.labels"))) # n attributes per stimulus
      arr <- array(NA, dim = c(no, max(na), ns))
      for (s in seq_len(ns)) {
        try(arr[, , s][] <- as.matrix(model.frame(formula(f, lhs=0, rhs=s), data = d, ...)))
      }
      if (ns == 1) {
        colnames(arr) <- attr(terms(formula(f, lhs=0, rhs=1)), "term.labels")
      }
      return(arr)
    },
    get_more_input = function(d = NULL) {
      return(NULL)
    },
    set_more_input = function(d = NULL) {
      self$more_input <- self$get_more_input(d = d)
    },
    # Sets reservations
    # @param f formula
    # @param d data
    set_res = function(f, d) {
      self$res <- self$get_res(f = f, d = d)
    },
    # Gets reservations
    get_res = function(f, d) {
      if (missing(f) & missing(d)) {
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
    # Sets the mode
    # @param mode A string, either \code{"continuous"} (judgments) or \code{"discrete"} (choices)
    set_mode = function(x) {
      x <- match.arg(x, c("discrete", "continuous"))
      self$mode <- x
    },
    get_mode = function() {
      return(self$mode)
    },
    #' Set new parameter
    #' @param x named vector with parameter values, names must be one in \code{parspace}
    set_par = function(x, check = TRUE) {
      if (is.matrix(x)) {
        x <- if (nrow(x) == 1) {
          setNames(x[1,], colnames(x))
          } else if (ncol(x) == 1) {
            setNames(x[1,], rownames(x))
            } else {
              stop("In set_par(), parameter must be a named list. Did you forget to name parameter in the argument 'fix'?", call.=FALSE)
              }
      }
      if (check == TRUE) {
        self$check_par(x)
      }
      self$par[names(x)] <- x
      return(invisible(self))
    },
    #' Get parameter
    # @param x string which type of parameter to get
    get_par = function(x = "all") {
      return(unlist(self$constrain( self$par[self$get_parnames(x)] )))
    },
    get_parnames = function(x = "all") {
      x <- match.arg(x, c("all", "free", "fix", "choicerule", "constrained", "ignored", "equal", "constant"))
      if (x == "all" & is.null(self$parnames)) {
        return(names(self$parspace))
      } 
      return(unlist(self$parnames[[x]]))
    },
    set_lb = function(x) {
      self$check_parnames(x)
      self$parspace[, "lb"] <- replace(self$parspace[, "lb"], match(x, rownames(self$parspace)), x[i])
    },
    get_lb = function(x = "all") {
      return(setNames(self$parspace[self$get_parnames(x), "lb"], self$get_parnames(x)))
    },
    set_ub = function(x) {
      self$check_parnames(x)
      self$parspace[, "ub"] <- replace(self$parspace[, "ub"], match(x, rownames(self$parspace)), x[i])
    },
    get_ub = function(x = "all") {
      return(setNames(self$parspace[self$get_parnames(x), "ub"], self$get_parnames(x)))
    },
    get_start = function(x = "all") {
      return(setNames(self$parspace[self$get_parnames(x), "start"], self$get_parnames(x)))
    },
    set_start = function(x) {
      self$check_parnames(x)
      self$parspace[, "start"] <- replace(self$parspace[, "start"], match(x, rownames(self$parspace)), x[i])
    },
    set_parspace = function(x) {
      self$parspace <- x
    },
    set_stimnames = function(x) {
      self$stimnames <- as.character(x)[seq_len(self$nstim())]
    },
    get_stimnames = function(f = self$formula) {
      return(self$stimnames)
    },
    get_prednames = function() {
      return(self$stimnames)
    },
    #' Set data to ignore when fitting
    #' @param x intteger or integer vector, rows to discount
    set_discount = function(x) {
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
    #' Remove choicerule
    rm_choicerule = function() {
      keep <- setdiff(names(self$get_par()), names(self$get_par("choicerule")))
      self$parspace <- self$parspace[keep,,drop=FALSE]
      self$choicerule <- NULL
    },
    #
    #
    #
    #
    # INITIALIZE methods
    # -------------------------------------------------------------------------
    init_parspace = function(parspace, choicerule) {
      if (is.null(mode)) {
        stop("init_parspace() must be called after init_mode()")
      }
      
      morepar <- NULL
      if (!is.null(choicerule)) {
        morepar <- if (choicerule == "softmax") {
          rbind(tau = c(0.001, 10, 0.5, NA))
        } else if (choicerule == "epsilon") {
          rbind(eps = c(0.001, 1L, 0.2, NA))
        }
      }
      if (self$mode == "continuous") {
        rg <- max(self$res) - min(self$res)
        morepar <- rbind(morepar, sigma = c(0, rg, rg/2, NA))
      }
      self$set_parspace(rbind(parspace, morepar))
    },
    init_fix = function(fix) {
      fix <- as.list(fix)
      if (length(fix) == 0L) {
        return(NULL)
      } else if (fix[[1]] == "start" & length(fix) == 1L) {
        return(setNames(self$parspace[, "start"], rownames(self$parspace)))
      } else {
        return(fix)
      }
    },
    init_parnames = function(parspace, fix, choicerule) {
      if (is.null(self$parspace)) {
        stop("init_parnames() must be called after init_parspace(). --> move init_parnames after init_parspace.")
      }
      parnames <- rownames(parspace)
      fixednames <- names(fix)
      self$parnames[["all"]] <- parnames
      self$parnames[["fix"]] <- intersect(parnames, fixednames)
      self$parnames[["free"]] <- setdiff(parnames, fixednames)
      self$parnames[["ignored"]] <- fixednames[unlist(lapply(fix, is.na))]
      self$parnames[["equal"]] <- fixednames[unlist(lapply(fix, is.character))]
      self$parnames[["constrained"]] <- c(self$parnames[["ignored"]], self$parnames[["equal"]])
      self$parnames[["constant"]] <- c(setdiff(fixednames, self$parnames[["equal"]]))

      if( !is.null(choicerule) ) {
        self$parnames[["choicerule"]] <- switch(choicerule,
                                              softmax = "tau",
                                              epsilon = "epsilon")
      }
    },
    init_par = function(parspace, fix) {
      if ( is.null(self$parspace) ) {
        stop("init_par() must be called after init_parspace() -> move init_par() down.")
      }
      if ( is.null(self$parnames) ) {
        stop("init_par() must be called after init_parnames() -> move init_par() down.")
      }
      par <- as.list(self$parspace[, "start"])
      names(par) <- self$get_parnames(x = "all")
      self$par <- par
      self$set_par(fix)
      self$set_par(self$parspace[self$get_parnames("ignored"), "na"])
    },
    init_constraints = function() {
      C <- self$make_constraints()
      if (length(C) > 0) {
        cpn <- self$get_parnames("constrained")
        for (i in unique(C$L$i)) {
          j <- C$L$j[C$L$i == i]
          cpn <- c(cpn, tail(setdiff(self$get_parnames()[j], cpn), 1L))
        }
        self$parnames[["constrained"]] <- cpn
      }
    },
    init_mode = function(mode = NULL) {
      if (is.null(mode)) {
        mode <- self$infer_mode(y = self$get_res())
      }
      self$set_mode(mode)
    },
    init_stimnames = function() {
      f <- self$formula
      ss <- sapply(1:length(f)[2], function(i) attr(terms(formula(f, lhs=0, rhs=i)), "term.labels")[1])
      self$set_stimnames(abbreviate(ss, minlength=1))
    },
    init_options = function(...) {
      .args <- as.list(...)
      .args <- .args[!duplicated(names(.args))] # first argument is user-supplied, second time it occurs it is the default of a model
      ## IF fitting with grid followed by a solver
      fit_solver <- .args$fit_solver
      if(!is.null(fit_solver)) {
        if(fit_solver[1] == "grid" & length(fit_solver) > 1L) {
          npar <- self$npar("free")

          ## select as many best parameter from the grid as we have free parmeter
          if (is.null(.args$fit_control$nbest)) {
            .args$fit_control$nbest <- npar
          }
          
          ub <- self$get_ub("free")
          lb <- self$get_lb("free")
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
    # getfreepar = function() {
    #   self$get_par('free')
    # },
    # getfixpar = function() {
    #   self$get_par('fix')
    # },
    # getconstrpar = function() {
    #   self$get_par('constrained')
    # },
    # getchoicerulepar = function() {
    #   self$get_par('choicerule')
    # },
    #
    #
    #
    # lm-like methods
    # -------------------------------------------------------------------------
    #' Number of free parameter
    #' @param x String specifying which
    npar = function(x = "all") {
      ans <- try(match.arg(x, "free"), silent = TRUE)
      if (class(ans) == "try-error") { # not "free"
        return(length(self$get_parnames(x)))
      } else {
        npar <- length(self$get_parnames()) - self$ncon()
        if (npar < 0) {
          # TODO: throw error/stop if a model has more constraints than parameters
          message("There may be many constraints on the parameter.")
        }
        return(npar)
      }
    },
    #' Number of trials/reservations in the data
    nobs = function() {
      # todo: maybe add the options$fit_n argument in there?
      return( dim(self$input)[1] )
    },
    #' Number of alternatives available
    nstim = function() {
      return(length(self$formula)[2])
    },
    # number of modes
    nres = function() {
      return(max(0, dim(self$res)[2]))
    },
    # number of attributes
    natt = function() {
      return(vapply(1:self$nstim(), function(i) length(attr(terms(formula(self$formula, lhs=0, rhs=i)), "term.labels")), 1L))
    },
    ncon = function() {
      return(length(self$make_constraints()))
    },
    coef = function() {
      return(self$get_par("free"))
      # return(unlist(self$par[setdiff(self$get_parnames('free'), self$get_parnames('constrained'))]))
    },
    #
    #
    #
    # Other useful methods
    # -------------------------------------------------------------------------
    constrain = function(par) {
      if (is.null(self$parspace)) { stop('constrainPar() called before $parspace was initialized -> move it after init_parspace().') }
      if (is.null(self$parnames)) { stop('constrainPar() called before $parnames was initialized -> move it after init_parnames().') }
      if (is.null(self$parnames)) { stop('constrainPar() called before $par were initialized -> move it after init_par().') }

      # Ignored parameter
      i <- names(par) %in% self$get_parnames("ignored")
      if ( sum(i) > 0) {
        par[i] <- self$parspace[i, "na"]
      }

      # Ignored parameter
      i <- names(par) %in% self$get_parnames("equal")
      all_par <- self$par
      i_values <- unlist( all_par[ unlist(par[self$get_parnames("equal")]) ] )
      if ( sum(i) > 0 ) {
        par[i] <- i_values
      }

      # # Equality-constrained parameter
      # #   a) equal to a free parameter
      # i <- self$get_parnames('equal')
      # equal_in_fix <- par[ na.omit(match( x, names(x) )) ]
      # if ( length(equal_in_fix) > 0 ) {
      #   x[sapply(x, is.character)] <- equal_in_fix 
      # }

      # #   b) equal to a free parameter
      # equal_in_free <- self$parspace[ na.omit(match( x, rownames(self$parspace) )) , "start"]
      # if ( length(equal_in_free) > 0) {
      #   x[sapply(x, is.character)] <- equal_in_free 
      # }
      return(par)
    },
    #' Transforms predictions with choicerule
    #' @param x string, choice rules, see \link[cogsciutils]{choicerule}
    apply_choicerule = function(x) {
      if ( is.null(self$choicerule) ) {
        return(x)
      } else {
        args <- c(list(x = x, type = self$choicerule), as.list(self$get_par('choicerule')))
        x[] <- do.call(cogsciutils::choicerule, args)
        return(x)
      }
    },
    # Simulate data from the model (given a choice rule)
    simulate = function() {
      pred <- as.matrix(self$predict())
      if ( ncol(pred) > 2 | self$mode == 'continuous' ) {
        stop('simulation is only implemented for binary discrete choices')
      }
      return(rbinom(pred[,1], size = 1, prob=pred))
    },
    infer_mode = function(y) {
      if(is.null(y)) {
        stop('Argument "mode" is missing (what the model predicts). Please "mode" to ="discrete" or ="continuous".')
      }
      if ( !is.factor(y) & any(floor(y) != y) | length(unique(y)) > 2 ) {
        message('Inferring mode from response variable ... "continuous". Change by setting mode="discrete".')
          return("continuous")
        } else {
          message('Inferring mode from response variable ... "discrete". Change by setting mode="continuous".')
          return("discrete")
        }
    },
    #
    #
    #
    #
    # Goodness of Fit methods
    # -------------------------------------------------------------------------
    #' Compute goodness of fit
    #' @param type string, fit measure to use, e.g. 'loglikelihood', allowed types see \link[cogsciutils]{gof}
    gof = function(type, n = self$options$fit_n, newdata = self$options$fit_data, discount = FALSE, ...) {
      if (length(self$get_res()) == 0L) {
        stop("Tried computing model fit, but didn't find a response variable. ", 'Check if "formula" has a left-hand side?', call.=FALSE)
      }
      if (is.null(newdata) | missing(newdata)) {
        obs <- as.matrix(self$get_res())
        pred <- as.matrix(self$predict())
      } else {
        obs <- self$get_res(f = self$formula, d = newdata)
        pred <- as.matrix(self$predict(newdata = newdata))
      }
      if (all(rowSums(obs) == nrow(obs)) & nrow(pred) == 1 - nrow(obs)) {
        pred <- cbind(pred, nrow(obs) - pred)
      }
      pred <- pred[, 1:ncol(obs), drop = FALSE]
      if (discount == TRUE) {
        pred[self$discount, ] <- NA
      }
      .args <- c(list(obs = obs, pred = pred, type = type, na.rm = TRUE, n = n), list(options = c(list(...), response = self$mode)))
      gof <- try(do.call(cogsciutils::gof, args=.args, envir=parent.frame()))
      if (inherits(gof, "try-error")) {
        print(gof)
      } else {
        return(gof)
      }
    },
    logLik = function(...) {
      return(self$gof(type = 'loglikelihood', ...))
    },
    BIC = function(...) {
      k <- ifelse('newdata' %in% names(list(...)), 0, self$npar('free'))
      N <- self$nres()
      return( -2 * self$logLik() + log(N)*k )
    },
    AIC = function(...) {
      k <- ifelse('newdata' %in% names(list(...)), 0, self$npar('free'))
      return( -2 * self$logLik() + 2*k )
    },
    AICc = function(...) {
      k <- ifelse('newdata' %in% names(list(...)), 0, self$npar('free'))
      if (k == 0) {
        return(self$AIC())
      }
      N <- self$nres()
      self$AIC() + (2 * k * (k+1)) / (N - k - 1)
    },
    MSE = function(...) {
      return(self$gof(type = 'mse', ...))
    },
    SSE = function(...) {
      return(self$gof(type = 'sse', ...))
    },
    RMSE = function(...) {
      return(self$gof(type = 'rmse', ...))
    },
    #
    #
    #
    # Fitting Functionalities
    # -------------------------------------------------------------------------
    fit = function(solver = self$options$fit_solver, measure = self$options$fit_measure, ...) {
      solver <- match.arg(solver, c("grid", "solnp", "auto", names(ROI::ROI_registered_solvers())), several.ok = TRUE)

      C <- self$make_constraints()
      par_in_c <- self$get_parnames()[C$L$j]
      par_fix <- self$get_parnames("fix")
      if (all(par_in_c %in% par_fix) & all(C$dir == "==")) {
        which_par <- "free" # only fit free parameter
        # this is to make the (ROI) solvers more efficient if we have no
        # other constraints than the equality- or constant constraints
      } else {
        which_par <- "all"
      }

      if (all(solver == "solnp")) {
        fit <- self$fit_solnp(which_par = which_par, ...)

      } else if (all(solver == "grid")) {
        fit <- self$fit_grid(which_par = which_par, ...)

      } else  if (all(solver == c("grid", "solnp"))) {
        grid_solution <- self$fit_grid()
        fits <- apply(grid_solution$solution, 1, self$fit_solnp, which_par = which_par, ...)
        fit <- fits[[which.min(lapply(fits, function(x) tail(x$objval, 1)))]]

      } else {
        if (solver[1] == "grid") {
          grid_solution <- self$fit_grid()
          fits <- apply(grid_solution$solution, 1, self$fit_roi, which_par = which_par, ...)
          fit <- fits[[which.min(lapply(fits, function(x) tail(x$objval, 1)))]]
        } else {
          fit <- self$fit_roi(which_par = which_par, ...)
        }
      }

      self$fitobj <- fit
      solution <- setNames(fit$solution, self$get_parnames(which_par))
      self$set_par(solution[self$get_parnames("free")]) #! only change free parameter
    },
    objective = function(par, self) {
      if (all(par == 0L) & is.null(names(par))) {
        par <- self$get_start() # hack because ROI solver strips names
      }
      if (any(par < self$get_lb() | par > self$get_ub())) {
        return(-1e10)
      }
      tmp <- self$set_par(x = par, check = TRUE)
      # check = FALSE is a hack because ROI tests the `objective` function by passing a vector of 0 without names problem: we check names
      .args <- list(
        type = self$options$fit_measure,
        n = self$options$fit_n,
        newdata = self$options$fit_data,
        options = self$options$options,
        discount = !is.null(self$discount))
      maximize <- self$options$fit_measure %in% c("loglikelihood", "accuracy")
      objval <- do.call(self$gof, args = .args, envir = parent.frame()) * (-1)^maximize
      return(objval)
    },
    fit_roi = function(start, which_par = "all") {
      objective <- ROI::F_objective(
        F = function(par) { self$objective(par, self = self) },
        n = self$npar(which_par),
        names = self$get_parnames(which_par))
      
      bounds <- ROI::V_bound(
        li = seq_len(self$npar(which_par)), lb = self$get_lb(which_par),
        ui = seq_len(self$npar(which_par)), ub = self$get_ub(which_par),
        names = self$get_parnames(which_par)
        )

      C <- if (which_par == "free") { NULL } else { self$make_constraints() }
      problem <- ROI::OP(
        objective = objective,
        constraints = C,
        bounds = bounds
        )
      if (missing(start)) {
        start <- self$get_start(which_par)
      }   
      sol <- do.call(
        what = ROI::ROI_solve,
        args = c(list(
          x = problem,
          solver = self$options$fit_solver,
          start = start,
          control = self$options$fit_control[grep("grid", names(self$options$fit_control))]),
          self$options$fit_args
          ),
        envir = parent.frame())
      return(sol)
    },
    fit_solnp = function(start, which_par = "all", ...) {
      if (missing(start)) {
        start <- self$get_start(which_par)
      }
      .args <-  list(
        pars = start,
        fun = self$objective,
        LB = self$get_lb(which_par),
        UB = self$get_ub(which_par),
        eqfun = NULL,
        eqB = NULL,
        self = self,
        control = self$options$fit_control)
      if (which_par == "all") {
        C <- self$make_constraints()
        if (length(C) > 0) {
          .args[["eqB"]] <- rep(0L, force(length(C$rhs)))
          .args[["eqfun"]] <- function(par, self) {
            C <- force(C)
            L <- force(C$L)
            out <- sapply(unique(L$i), function(i) {
              par[L$j[L$i == i]] %*% L$v[L$i == i] - C$rhs[i]
            })
            return(out)
          }
        }
      }

      fit <- do.call(Rsolnp::solnp, args = .args, env = parent.frame())
      return(list(solution = fit$pars, objval = tail(fit$value, 1)))
    },
    fit_grid = function(...) {
      G <- self$make_pargrid(
        which_par = which_par,
        offset = self$options$fit_grid_offset,
        nsteps = self$options$fit_control$nsteps,
        ...
        )
      objvals <- sapply(1:nrow(G$ids), function(i) {
          self$objective(par = get_id_in_grid(i, G), self = self)
        })
      best_ids <- which(rank(objvals, ties.method = "random") <= self$options$fit_control$nbest)
      best_par <- t(sapply(best_ids, get_id_in_grid, grid = G))
      return(list(
        solution = best_par[, self$get_parnames("free"), drop = FALSE],
        objval = objvals[best_ids])
      )
    },
    make_pargrid = function(
      offset = self$options$fit_grid_offset,
      nsteps = self$options$fit_control$nsteps,  ...) {
      if (!is.null(self$make_constraints())) {
          message('Note: fit_solver = "grid" does NOT respect linear or quadratic constraints -> maybe change the solver by using options = list(fit_solver = ...) like fit_solver = "solnp" or "optimx".')
      }
      return(make_grid_id_list(
        names = self$get_parnames("free"),
        lb = self$get_lb("free"),
        ub = self$get_ub("free"),
        offset = offset,
        nsteps = nsteps,
        ...))
    },
    make_random_par = function(parspace) {
      if (!is.null(self$make_constraints())) {
          message('Note: fit_solver = "grid" does NOT respect linear or quadratic constraints -> maybe change the solver by using options = list(fit_solver = ...) like fit_solver = "solnp" or "optimx".')
      }
      # TODO: check this fun, it's experimental (5^? more? see literature)
      n <- log(5^nrow(parspace))
      par <- apply(parspace, 1, function(i) runif(n, min = i["lb"], max = i["ub"]))
      colnames(par) <- rownames(parspace)
      return(par)
    },
    make_constraints = function() {
      pn <- self$get_parnames()
      cons1 <- cons2 <- NULL
      # Constraints for par. that are set equal to a constant, numerical value
      if (self$npar("constant") > 0) {
        this_pn <- self$get_parnames("constant")
        cons1 <- L_constraint(
          L = t(sapply(this_pn, function(x) as.integer(pn %in% x))),
          rhs = self$get_par()[this_pn],
          dir = rep("==", length(this_pn))
          )
      }
      # Constraints for par. that are set equal to the value of another par.
      if (self$npar("equal") > 0) {
        p <- self$get_par()
        this_pn <- self$get_parnames("equal")
        this_p <- unlist(self$par[this_pn])
        cons2 <- L_constraint(
          L = t(sapply(seq_along(this_pn), function(i) {
                as.integer(pn %in% this_pn[i]) - as.integer(pn %in% this_p[i])
            })),
          rhs = rep(0L, length(this_pn)),
          dir = rep("==", length(this_pn))
          )
      }
      # Returns NULL if cons1 & cons2 are NULL
      # or one constraint that combines both
      return(.combine_constraints(cons1, cons2))
    },
    #
    #
    #
    #
    # Summary and Printing Functions
    # -------------------------------------------------------------------------
    print = function(digits = 2) {
      cat(self$title)
      cat('\nCall:\n',
      paste(.abbrDeparse(self$call), sep = '\n', collapse = '\n'), '\n\n', sep = '')
      note <- NULL
      if( self$npar('free') > 0 ) {
          cat('Free parameter estimates:\n')
          print.default(format(self$get_par()[setdiff(self$get_parnames('free'), self$get_parnames('constrained'))], digits = digits, justify = 'centre', width = digits+1L), print.gap=2L, quote=FALSE)
      } else {
        note <- 'No free parameter.'
      }
      if( self$npar("fix") > 0) {
        cat('\nFixed parameter:\n')
        print.default(format(self$get_par()[self$get_parnames("constrained")], digits = digits, justify = 'centre', width = digits+1L), print.gap=2L, quote=FALSE)
      } else {
        note <- cbind(note, 'No fixed parameter. ')
      }
      if ( self$npar("equal")  > 0 ) {
        cat('  By constraints:', paste(apply(cbind(self$get_parnames('equal'), self$par[self$get_parnames('constrained')]), 1, paste, collapse="="), collapse=", "))
      }
      if ( is.null(self$choicerule) ) {
        note <- c(note, 'No choice rule. ')
      } else {
        cat('Choice rule:', self$choicerule)
      }
      if ( length(note) ) cat('\n---\nNote: ', note)
      cat('\n')
      invisible(self)
    },
    summary = function() {
      coef.table <- matrix(self$par)
      dimnames(coef.table) <- list(names(self$par), c("Estimate"))
      ans <- list(call = self$formula,
                  title = self$title,
                  mse = self$MSE(),
                  aic = tryCatch(self$AIC(), error = function(e) NA),
                  bic = tryCatch(self$BIC(), error = function(e) NA),
                  logLik = tryCatch(self$logLik(), error = function(e) NA),
                  freenames = self$get_parnames('free'),
                  coefficients = coef.table)
      class(ans) <- "summary.cogscimodel"
      return(ans)
    },
    #
    #
    #
    #
    # CHECK FUNCTIONS
    # -------------------------------------------------------------------------
    check_input = function() {
    },
    check_parclass = function(x) {
    },
    check_parnames = function(x) {
      sapply(names(x), function(n) {
        if (!n %in% rownames(self$parspace)) {
          stop("The parameter ", dQuote(n), " is no model parameter, the model's parameter names are: ", dQuote(rownames(self$parspace)), ".", call.=FALSE)
        }
      })

      if ( any(duplicated(names(x))) ) {
          stop("Parameter names must be unique, but the following appear multiple times: ", .brackify(dQuote(names(fix)[duplicated(names(fix))])),".", call.=FALSE)
        }
    },
    check_par = function(x) {
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
      self$check_parnames(x)
      sapply(names(x), function(n) {      
        if (is.na(x[[n]]) & is.na(parspace[n, 'na'])) {
          stop("Tried to ignore the parameter ", dQuote(n), ", but ignoring ",dQuote(n)," is not possible.\n\nBackground: Ignoring a parameter by setting it to NA only works if the model's 'parspace' has a value in the 'na' column, which is not the case for the parameter ", dQuote(n), " in this model.", call.=FALSE)
        }
        if (!is.na(x[[n]]) & !is.character(x[[n]])) {
          if(x[[n]] < parspace[n, "lb"] | x[[n]] > parspace[n, "ub"]) {
          stop("Tried to set the parameter ", dQuote(n), " = ", x[[n]], ", but ", dQuote(n), " may only take values from ", parspace[n, "lb"]," to ", parspace[n, "ub"] ," (including the bounds) in this model.", call.=FALSE)
          }
        }

      })
    },
    check_fix = function(fix) {
      if ( length(fix) > 0 ) {
        if (all(is.numeric(fix))) {
          fix <- as.list(fix)
        }
        if (!is.list(fix)) {
          stop("'fix' needs to be a list, but is a ", typeof(fix), ".")
        }

        self$check_parnames(fix)
        parspace <- self$parspace
        par <- self$get_par()

        if ( !all( names(fix) %in% rownames(parspace)) ) {
          stop("Check the parameter names in 'fix'. Some parameters in 'fix' ", .brackify(dQuote(setdiff(names(fix), rownames(parspace)))), " are not in the model's parameter space, which contains the following parameter ", .brackify(dQuote(rownames(parspace))), ".", call.=FALSE)
        }
        if ( !all(fix[names(fix) %in% self$get_parnames('equal')] %in% self$get_parnames() ) ) {
          stop("Check equality constraints in 'fix'. The equality constraints ", .brackify(
            apply(cbind(self$get_parnames('equal'), self$par[self$get_parnames('equal')]), 1, paste, collapse="=")[fix[names(fix) %in% self$get_parnames('equal')] %in% self$get_parnames()]
            ), " are not valid, because you can only constrain parameters to equal the model's parameter space, which contains the following parameters ", .brackify(rownames(parspace)), ".", call.=FALSE)
        }
        if ( length(fix) < nrow(parspace) & is.null(self$res) & self$options$fit == TRUE ) {
          stop('Trying to estimate model parameter ', .brackify(setdiff(rownames(parspace), names(fix))), ', but "formula" has no left-hand side. -> Add a LHS to the formula that defines the observed data, or fix all free model parameter using fix = list(...).', call. = FALSE)
        }

        lapply(seq_len(self$npar('equal')), function(i, fix) {
            p <- unlist(fix[self$get_parnames('equal')][i])
            newp <- setNames(list(self$par[[p]]), names(p))
            tryCatch(self$check_par( newp ), error = function(e) stop("Check equality constraints in 'fix'. Trying to set ", names(p), " = ", dQuote(p),", but the value of ",dQuote(p)," (",self$get_par()[p],") lies outside of the allowed range of ",dQuote(names(p))," (",paste(parspace[names(p), c("lb","ub")], collapse=" to "),").",call.=FALSE))

          }, fix = fix)
      }
    }
  )
)


# Define S3 methods
coef.cogscimodel <- function(obj, ...) {
  do.call(obj$coef, list(...))
}
predict.cogscimodel <- function(obj, ...) {
  do.call(obj$predict, list(...))
}
logLik.cogscimodel <- function(obj, ...) {
  obj$logLik(...)
}
SSE.cogscimodel <- function(obj, ...) {
  ojb$SSE()
}
#' @export
MSE <- function(x) UseMethod("MSE", x)

#' @export
MSE.cogscimodel <- function(obj, ...) {
  obj$MSE()
}
AIC.cogscimodel <- function(obj, ...) {
  obj$AIC(...)
}
AICc.cogscimodel <- function(obj, ...) {
  obj$AICc(...)
}
BIC.cogscimodel <- function(obj, ...) {
  return(obj$BIC(...))
}
RMSE.cogscimodel <- function(obj, ...) {
  return(obj$RMSE())
}
summary.cogscimodel <- function(obj, ...) {
  return(obj$summary())
}
nstim.cogscimodel <- function(obj, ...) {
  return(obj$nstim())
}
nres.cogscimodel <- function(obj, ...) {
  return(obj$nres())
}
nobs.cogscimodel <- function(obj, ...) {
  return(obj$nobs())
}
natt.cogscimodel <- function(obj, ...) {
  return(obj$natt())
}
ncon.cogscimodel <- function(obj, ...) {
  return(obj$ncon())
}
getCall.cogscimodel <- function(obj, ...) {
  return(obj$getCall())
}
print.summary.cogscimodel = function(x, digits = max(3L, (getOption("digits") - 3L)), ...) {
  cat("\nModel:\n", trimws(x$model),
    if (is.null(x$choicerule)) {
      'with no choice rule'
    } else {
      paste('with', x$choicerule, 'choicerule')
    })
  cat("\nCall:\n",
    paste(deparse(x$call), sep="\n", collapse="\n"), "\n", sep="")
  if (length(x$freenames) == 0L) {
    cat("\nNo Free Parameters\n")
  } else {
    cat("\nParameters:\n")
    print(x$coefficients, digits = digits, ...)
  }
  cat("\nFit Measures:\n")
  cat(paste(c("MSE:", "LL:", "AIC:", "BIC:"), format(x[c("mse", "logLik", "aic", "bic")], digits = 2L), collapse = ", "))
  cat(
    "\n\n"
    )
  invisible(x)
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

# options
# fit_measure = "loglikelihood"
# fit_n = 1
# fit_data = ...
# fit_roi_solver
# fit_grid_nbest = self$npar('free')

#options

# fit
# data
# measure
# n
# grid_nbest
# roi_solver
