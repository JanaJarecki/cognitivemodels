# ==========================================================================
# Package: Cognitivemodels
# File: cognitivemodel-class.R
# Author: Jana B. Jarecki
# ==========================================================================

# ==========================================================================
# Cognitive Model Class for Lego-type models
# ==========================================================================


#' Add components to a model
#'
#' `cogscimodel()` initializes a cogscimodel object. It can be used to
#' declare the input data being used by the model.
#' 
#' `cogscimodel()` is used to construct the initial model object,
#' and is almost always followed by `%+%` to add a component to the
#' model.
#' 
#' @param data a data.frame holding the variables that the first model uses
#' 
#' @export
cognitivemodel <- function(data, ...) {
  return(Csm$new(data = data, ...))
}

Csm <- R6Class("csm",
  inherit = Cm,
  public = list(
    titels = list(),
    models = list(),
    functions = list(),
    model_levels = list(),
    fix = list(),
    constraints = NULL,
    parspace = NULL,
    prefixes = list(),
    data = NULL,
    call = NULL,
    title = NULL,
    super_initialized = FALSE,
    nmod = 0,
    initialize = function(data, ...) {
      # pipeline_on_exit(env = parent.frame(1L))
      self$data <- as.data.frame(data)
      return(invisible(self))
    },
    getCall = function() {
      return(self$last_call)
    },
    predict = function(type = "response", newdata = NULL, ...) {
      if (length(newdata) > 0L) {
        data <- as.data.frame(newdata)
      } else {
        data <- self$data
      }
      par <- self$get_par()
      nm <- self$nmod

      for (m in 1L:nm) {
        M <- self$models[[m]]
        if (m > 1 & length(newdata) == 0L) {
          M$set_data(d = data)
        }
        pred <- M$predict(type = type, newdata = if (length(newdata) > 0) { data })
        pred <- as.matrix(pred)
        colnames(pred) <- M$prednames[seq.int(ncol(pred))]
        pred <- self$functions[[m]](
            pred = pred,
            data = data,
            par = par)
        if (nm > 1) {
          data[, colnames(pred)] <- pred
        }
        if (m == nm) {
          nn <- ifelse(self$nres == 0L, ncol(pred), self$nres)
          return(pred[, 1:nn])
        }
      }
    },
    set_par = function(x, check = FALSE, constrain = TRUE) {
      # Set the parameter of this  model
      super$set_par(x, check = check, constrain = constrain)
      # Set the parameter of the underlying models
      npar <- lapply(self$models, function(x) x$npar("all"))
      for(m in 1:self$nmod) {
        if(length(self$models[[m]]$npar("free"))) {
          self$models[[m]]$set_par(x[intersect(names(self$models[[m]]$par), names(x))], check = check)
        }
      }
    },
    set_options = function() {
      self$options <- options
    },
    add_model = function(x, x_name) {
      n <- self$nmod + 1L
      self$call <- x$call
      self$data <- self$add_data(i = n, check = TRUE)
      CALL <- self$call_to_list(x$call)
      CALL[["data"]] <- self$data
      CALL[["options"]] <- c(list(fit = FALSE), eval(CALL[["options"]]))
      CALL <- as.call(CALL)
      M <- try(eval(substitute(CALL), envir=parent.frame(2)), silent = TRUE)
      if (inherits(M, "try-error")) {
        if (n > 1) {
          note <- paste0("  (Internal data has predictions from previously added models called: ", .brackify(sapply(self$models, function(x) x$prednames)), ".)")
        } else {
          note <- NULL
        }        
        stop("Error during setup of model ", n, " '", paste0(substr(x_name, 1, 20), ifelse(nchar(x_name) > 20, "...", "")), "': ", M, note, call. = FALSE)
      }
      self$titels[[n]] <- CALL[[1]]
      self$models[[n]] <- M
      self$nmod <- n
      # inherit mode from the last model
      self$mode <- M$mode
      self$functions[[n]] <- function(pred, data, par) { return(pred) }

      # Use the settings of the model as settings in end()
      self$add_fix(M$par[names(M$get_par("fix"))])
      self$add_parspace(M$parspace)
      self$add_title(M$title)
      self$add_constraints(M$constraints)
      self$end(discount = M$discount, options = M$options)
      return(invisible(self))
    },
    add_data = function(i, check = FALSE) {
      data <- self$data
      if (i > 1L) {
        pred <- self$models[[(i - 1L)]]$predict()
        pred <- as.matrix(pred)
        if (check == TRUE) {
          self$check_varnames(colnames(pred))
        }
        if (is.null(colnames(pred))) {
          colnames(pred) <- self$models[[(i - 1L)]]$prednames[seq.int(ncol(pred))]
        }        
        data[, colnames(pred)] <- pred
      }
      return(data)
    },
    add_fun = function(fun) {
      if (!is.function(fun)) {
        stop("'fun' must be a function, but '", .abbrDeparse(fun), "'' is a ", class(fun), ".")
      }
      m <- self$nmod
      if (!identical(self$functions[[m]], function(pred, data, par) { return(pred) }, ignore.environment = TRUE)) { stop("Can't add 2 functions after another, 'fun() %>% fun()'' is impossible.\n  * Combine the functions into one.")
      }
      self$functions[[m]] <- fun
      # TODO: add checks for fun() arguments data, par
      return(invisible(self))
    },
    check_varnames = function(x) {
      if (any(x %in% colnames(self$data))) {
          stop("A variable in 'data' has the same name as the prediction of the ", i, ". model. Duplicated names are not allowed.\n  * Do you want to rename each of these variables in data? - ", .brackify(x[which(x %in% colnames(self$data))]), "?")
        }
    },
    call_to_list = function(x) {
      return(as.list(rlang::call_standardise(as.call(x))))
    },
    add_title = function(x) {
      self$title <- paste0(c(self$title, x), collapse = " > ")
    },
    add_fix = function(x) {
      self$fix <- c(self$fix, x)
    },
    add_parspace = function(x) {
      # TODO fix how cognitivemodel (the lego class) deals with initializing sigma, tau, or eps in > 1 model
      parspace <- rbind(self$parspace, x)
      parspace[!duplicated(rownames(parspace))]
      self$parspace <- parspace
    },
    init_call = function() {
      self$call[[1]] <- paste0(self$titel, collapse="+")
      self$call$fix <- self$fix
      self$call <- as.call(self$call)
    },
    end = function(discount = 0L, options = self$options) {
      # TODO: put some sanitize names code here for the model names
      # fixme: the constraints in fix are giving rise to duplicated constraints
      # idea: maybe using "fix" only from the global parameters?
      super$initialize(
        formula = self$models[[self$nmod]]$formula,
        data = self$data,
        title = self$title,
        mode = self$mode,
        fix = NULL,
        parspace = self$parspace,
        discount = discount,
        options = c(list(fit = FALSE), options),
        choicerule = self$models[[self$nmod]]$choicerule
        )
      return(invisible(self))
    },
    fit = function(discount = 0L, options = self$options) {
      self$discount <- discount
      self$options$fit <- TRUE 
      options <- c(options, self$options)
      options <- options[!duplicated(names(options))]
      super$init_par(parspace = self$parspace, fix = self$fix, options = options, mode = self$mode, addpar = FALSE)
      super$init_options(options)
      if(options$fit != FALSE) {
        super$fit()  
      }    
    },
    add_constraints = function(cons = NULL) {
      if (length(cons) == 0L) {
        return(invisible(self))
      }
      if (length(self$constraints) == 0L) {
        self$constraints <- cons
        return(invisible(self))
      }
      C <- self$constraints
      A <- array(0L, dim(C$L) + dim(cons$L))
      j <- seq.int(nrow(C$L))
      k <- seq.int(ncol(C$L))
      A[j, k] <- as.matrix(C$L)
      A[-j, -k] <- as.matrix(cons$L)
      C <- ROI::L_constraint(
        L = A,
        dir = c(C$dir, cons$dir),
        rhs = c(C$rhs, cons$rhs),
        names = c(C$names, cons$names)
      )
      class(C) <- c("csm_constraint", class(C))
      self$constraints <- C
      return(invisible(self))
    }
  ),
  private = list(
    make_constraints = function() {
      return(self$constraints)
    }
  )
)
