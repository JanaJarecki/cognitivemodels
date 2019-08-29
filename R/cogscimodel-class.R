#' Class for cognitive models
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
#' @usage Cogscimodel$new(formula, data, parspace)
#' @param formula A formula (e.g., \code{y ~ x1 + x2}).
#' @param data A data.frame containing \code{formula}'s variables.
#' @param parspace  (optional, \bold{required} to add model parameters) A amtrix with the parameter space: a \code{n x 4} matrix with columns named \code{"lb","ub","start","na"}. Easy to create using \link{make_parspace}, e.g. \code{make_parspace(alpha=c(0,1))}. The matrix' row names are parameter names, columns contain the lower limit, upper limit, starting value in fitting, and (optional) a value that makes a parameter have zero effect, which can be NA. See details.
#' @param fix (optional)  A list with parameter constraints (e.g., \code{list(beta = 1)}). Allowed are numbers, strings, and NA. Supplying
#' \code{list(alpha=0.5, beta="alpha", gamma=NA)} constrains the parameters alpha, beta, and gamma as follows
#' \itemize{
  #' \item{\code{alpha=0.5}}{: fix alpha to 0.5.}
  #' \item{\code{beta="alpha"}}{: fix beta to the value of alpha; alpha may be a free or fixed model parameter in \code{parspace}.}
  #' \item{\code{delta=NA}}{: ignore delta if it can be ignored by setting delta equal to the value in the column "na" of \code{parspace}, given that parspace has a value in the column "na" for delta.}
#' }
#' @param choicerule (optional, default \code{NULL}) String specifying the choice rule, allowed are \code{NULL}, \code{"softmax"}, \code{"eps"}, and other values of the argument \code{type} to \link[cogsciutils]{choicerule}.
#' @param model (optional, default is the class name) A string, the model's name.
#' @param discount (optional, default \code{0}) An integer or integer vector, which or how many many trials to discount.
#' @param options (optional)
#' An object of class "cogscimodel_options" which contains options, generate it using \link{cogscimodel_options} using one of these options
#' \describe{
#'    \item{\code{fit_measure}}{(default: \code{"loglikelihood"}). When fitting, which fit measure to use? See \link[cogsciutils]{gof}'s argument \code{type}.}
#'    \item{\code{fit_data}}{(default: \code{data}). When fitting, which data other than to fit the model to? Useful if you fit to other data than you predict.}
#'    \item{\code{fit_n}}{(default: \code{1}). When fitting, the number of reservations underlying each data point in \code{fit_data}. If this data is aggregated data, change this argument.}
#'    \item{\code{fit_roi_solver}}{(default: \code{"auto"}). When fitting with the \link{http://roi.r-forge.r-project.org/index.html}{ROI pakage}, which solver to use for the parameter estimation problem.}
#'    \item{\code{fit_grid_nbest}}{(default: \code{self$npar("free")}, no. of free parameters) When fitting with grid search followed by a solver, how many best grid-search solutions to use as starting parameters with the solver?}
#' \item{\code{fit_options}}{(default: \code{NULL}). Other options, see \link[cogsciutils]{gof}.}
#' }
#' @details \code{parspace}. It is optional to define a value that makes the parameter have zero effect in the column called "na" in \code{parspace}. For example, a parameter \code{b} in \code{b*x}, has no-effect  when setting \code{b=0}. The no-effect value can be \code{NA}.
#' 
#' You can ignore a model parameter by setting it to \code{NA} in \code{fix}, in this case your \code{parspace} needs to contain a value in the column na nullifying the effect of the parameter.
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
    res = NULL,
    pred = 'matrix',
    par = 'vector',
    parnames = list(),
    parspace = 'matrix',
    mode = 'string',
    choicerule = NULL, 
    optimization = NA,
    discount = 'numeric',
    gofval = NULL,
    stimnames = character(),
    options = list(),
    initialize = function(formula, data, parspace, fix = NULL, choicerule =  NULL, title = NULL, discount = NULL, mode = NULL, options = NULL) {
      # get call
      self$call <- if ( deparse(sys.call()[[1]]) == 'super$initialize' ) {
        rlang::call_standardise(sys.calls()[[sys.nframe()-3]])
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


      # Initialize values of parameter
      self$init_parspace(parspace=parspace, choicerule=choicerule)
      self$init_parnames(parspace=self$parspace,fix=fix,choicerule=choicerule)
      self$init_par(parspace=self$parspace, fix=fix)
      self$init_mode(mode=mode)
      self$init_stimnames()
      self$init_options(options)



      # Checks
      # ! after setting formula, parspace, choicerule, etc.
      self$check_input()
      self$check_fix(fix)
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
    set_input = function(f, d) {
      f <- as.Formula(f)
      self$input <- self$get_input(formula(f, lhs = 0), d)
    },
    # Get the inputs to the model
    get_input = function(f, d) {
      f <- as.Formula(f)
      nr <- nrow(d)
      nopt <- length(f)[2]
      nf <- sapply(1:nopt, function(i) length(attr(terms(formula(f, lhs=i, rhs=i)), "term.labels")))
      arr <- array(NA, dim = c(nr, max(nf), nopt))
      for (o in seq_len(nopt)) {
        arr[,,o][] <- as.matrix(model.frame(formula(f, lhs=0, rhs=o), d))
      }
      vn <- attr(terms(formula(f, lhs=0, rhs=1)), "term.labels")
      if (nopt == 1) {
        colnames(arr) <- vn
      }
      return(arr)
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
        return(get_all_vars(formula(f, rhs = 0), d))
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
          rbind(tau = c(0.1, 10, 0.5, NA))
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
      fit_solver <- .args[["fit_solver"]]
      if(!is.null(fit_solver)) {
        if(fit_solver[1] == "grid" & length(fit_solver) > 1L) {
          npar <- self$npar("free")

          ## select as many best parameter from the grid as we have free parmeter
          if (is.null(.args[["fit_grid_nbest"]])) {
            .args[["fit_grid_nbest"]] <- npar
          }
          
          ub <- self$get_ub("free")
          lb <- self$get_lb("free")
    
          ## offset, scales logistically from super small to 10% of the range of each parameter
          if (is.null(.args[["fit_grid_offset"]]))
            .args[["fit_grid_offset"]] <- as.list(0.10 / (1 + exp(-(ub - lb))))

          ## make the steps exponentially bigger with the parameter range
          if (is.null(.args[["fit_grid_nsteps"]]))
            .args[["fit_grid_nsteps"]] <- round(pmax(log(ub - lb) * 2, 3) * max(1, log(npar)))
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
      return( dim(self$res)[2] )
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
        message("Inferring 'mode' ... ")
        if ( !is.factor(y) & any(floor(y) != y) | length(unique(y)) > 2 ) {
          cat("  ... as 'continuous'. Change mode by setting mode = \"discrete\".)\n")
          return("continuous")
        } else {
          cat("... as 'discrete'. Change mode by setting mode = \"continuous\".\n")
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
    gof = function(type, n = self$options$fit_n, newdata = self$options$fit_newdata, discount = FALSE, ...) {
      if (is.null(self$res)) {
        stop('When trying to fit the model, there was no observed data. Does your "formula" have a right-hand side? Check "formula" or the observed variables in the "data" argument.', call.=FALSE)
      }
      if (missing(newdata) | is.null(newdata)) {
        obs <- as.matrix(self$get_res())
        pred <- as.matrix(self$predict())
      } else {
        obs <- self$get_res(f = self$formula, d = newdata)
        pred <- as.matrix(self$predict(newdata = newdata))
      }
      pred <- pred[, 1:ncol(obs), drop = FALSE]
      if (discount == TRUE) {
        pred[self$discount, ] <- NA
      }
      .args <- c(list(obs = obs, pred = pred, na.rm = TRUE, n = n), list(options = c(list(...), response = self$mode)))
      do.call(cogsciutils::gof, .args)
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
      N <- self$nres()
      self$AIC() + (2*k*(k+1)) / (N-k-1)
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
      parspace <- self$parspace[self$get_parnames(), , drop=FALSE]

      if (all(solver == "solnp")) {
        fit <- self$fit_solnp(parspace = parspace)

      } else if (all(solver == "grid")) {
        fit <- self$fit_grid(parspace = parspace, ...)
      } else  if (all(solver == c("grid", "solnp"))) {
        grid_solution <- self$fit_grid(parspace = parspace)
        fits <- apply(grid_solution$solution, 1, self$fit_solnp, parspace = parspace)
        fit <- fits[[which.min(lapply(fits, function(x) tail(x$objval, 1)))]]
      } else {
        fit <- self$fit_roi()
      }
      self$set_par(setNames(fit$solution, self$get_parnames()))
      self$gofval <- fit$objval
    },
    objective = function(par, self) {
      if (all(par == 0L) & is.null(names(par))) {
        par <- self$get_start() # hack because ROI solver strips names
      }
      self$set_par(x = par, check = FALSE)
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
    fit_roi = function() {
      objective <- ROI::F_objective(
        F = function(par) { self$objective(par, self = self) },
        n = self$npar('free'),
        names = self$get_parnames('free'))
      
      bounds <- ROI::V_bound(
        li = seq_len(self$npar('free')), lb = self$get_lb('free'),
        ui = seq_len(self$npar('free')), ub = self$get_ub('free'),
        names = self$get_parnames('free')
        )
      
      problem <- ROI::OP(
        objective = objective,
        constraints = self$make_constraints(),
        bounds = bounds
        )
      
      sol <- ROI::ROI_solve(
        problem,
        solver = self$options$fit_solver,
        start = self$get_start("free"),
        itmax = 1000)
      return(sol)
    },
    fit_solnp = function(start = self$get_start(), parspace = self$parspace, ...) {
      .args <- list(
        pars = start,
        fun = self$objective,
        LB = self$get_lb(),
        UB = self$get_ub(),
        eqfun = NULL,
        eqB = NULL,
        self = self,
        control = self$options$fit_options)

      C <- self$make_constraints()

      if (length(C) > 0){
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

      fit <- do.call(Rsolnp::solnp, args = .args, env = parent.frame())
      return(list(solution = fit$pars, objval = tail(fit$value, 1)))
    },
    fit_grid = function(nbest = self$options$fit_grid_nbest, parspace, offset = self$options$fit_grid_offset, nsteps = self$options$fit_grid_nsteps) {
      GRID <- self$make_pargrid(offset = offset, nsteps = nsteps)
      objvals <- sapply(1:nrow(GRID$ids), function(i) {
          self$objective(par = get_id_in_grid(i, GRID), self = self)
        })
      best_ids <- which(rank(objvals, ties.method = "random") <= nbest)
      best_par <- t(sapply(best_ids, get_id_in_grid, grid = GRID))
      return(list(
        solution = best_par[, self$get_parnames("free"), drop = FALSE],
        objval = objvals[best_ids])
      )
    },
    make_pargrid = function(offset = 0, ...) {
      if (!is.null(self$make_constraints())) {
          message('Note: Linear and quadratic constraints are NOT respected by solver = "grid"! Consider changing the solver for the parameter estimation by using: options = list(fit_solver = "..."), for example "solnp", or "optimx".')
      }
      return(make_grid_id_list(
        names = self$get_parnames("free"),
        lb = self$get_lb("free"),
        ub = self$get_ub("free"),
        offset = offset,
        ...))
    },
    make_random_par = function(parspace) {
      n <- log(5^nrow(parspace))
      par <- apply(parspace, 1, function(i) runif(n, min = i["lb"], max = i["ub"]))
      colnames(par) <- rownames(parspace)
      return(par)
    },
    make_constraints = function() {
      pn <- self$get_parnames()
      L <- dir <- rhs <- NULL
      if (self$npar("constant") > 0) {
        this_pn <- self$get_parnames("constant")
        L <- sapply(this_pn, function(x) as.integer(pn %in% x))
        rhs <- self$get_par()[this_pn]
        dir <- rep("==", length(this_pn))
      }

      if (self$npar("equal") > 0) {
        p <- self$get_par()
        this_pn <- self$get_parnames("equal")
        this_p <- unlist(self$par[this_pn])
        L <- cbind(L, sapply(seq_along(this_pn), function(i) {
              as.integer(pn %in% this_pn[i]) - as.integer(pn %in% this_p[i])
          }))
        rhs <- c(rhs, rep(0L, length(this_pn)))
        dir <- c(dir, rep("==", length(this_pn)))
      }

      if (!is.null(L)) {
        return(L_constraint(
          L = unname(t(L)),
          dir = dir,
          rhs = unname(rhs),
          names = pn))
      } else {
        return(NULL)
      }
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
      if( self$npar('fix') > 0) {
        cat('\nFixed parameter:\n')
        print.default(format(self$get_par()[c(self$get_parnames('fix'))], digits = digits, justify = 'centre', width = digits+1L), print.gap=2L, quote=FALSE)
      } else {
        note <- cbind(note, 'No fixed parameter. ')
      }
      if ( self$npar('equal')  > 0 ) {
        cat('  By equality constraints:', paste(apply(cbind(self$get_parnames('equal'), self$par[self$get_parnames('equal')]), 1, paste, collapse="="), collapse=", "))
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
                  aic = self$AIC(),
                  bic = self$BIC(),
                  logLik = self$logLik(),
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
          stop("The parameter ", dQuote(n), "is no model parameter, the model's parameter names are: ", .brackify(dQuote(rownames(self$parspace))), ".", call.=FALSE)
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
        if ( length(fix) < nrow(parspace) & is.null(self$res) ) {
          stop('Trying to estimate parameter ', .brackify(setdiff(rownames(parspace), names(fix))), ', but "formula" lacks left-hand side specifying reserved data\n\tEither add parameter to "fix" fixing all parameters, or specify reservations as left-hand-side of "formula".', call. = FALSE)
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
print.summary.cogscimodel = function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  cat("\nModel:\n",trimws(x$model),
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
    printparat(x$coefficients, digits = digits, ...)
  }
  cat("\nFit Measures:\n",
    "logLik: ", format(x[['logLik']], digits=max(4L, digits+1L)),
    ", AIC: ", format(x[['aic']],    digits = max(4L, digits+1L)),
    ", BIC: ", format(x[['bic']],    digits = max(4L, digits+1L)), "\n\n"
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
