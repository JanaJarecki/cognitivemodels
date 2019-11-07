#' Start chain of cogscimodels
#' 
#' @param data a data.frame holding the variables that the first model uses
#' 
#' @export
start <- function(data, ...) {
  return(Cogscimodelstack$new(data = data, ...))
}


Cogscimodelstack <- R6Class("cogscimodelstack",
  inherit = Cogscimodel,
  public = list(
    titels = list(),
    models = list(),
    model_levels = list(),
    fixes = list(),
    prefixes = list(),
    data = NULL,
    last_call = NULL,
    super_initialized = FALSE,
    nmod = 0,
    functions = list(),
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
          M$set_input(d = data)
          M$set_more_input(d = data)
        }
        pred <- M$predict(type = type, newdata = if (length(newdata) > 0) { data })
        pred <- self$functions[[m]](
            pred = pred,
            data = data,
            par= par)
        if (nm > 1) {
          data[, colnames(pred)] <- pred
        }
        if (m == nm) {
          nn <- ifelse(self$nres() == 0L, ncol(pred), self$nres())
          return(pred = pred[, 1:nn, drop = FALSE])
        }
      }
    },
    set_par = function(x, check = FALSE) {
      super$set_par(x, check = check)
      nm <- self$nmod
      j <- 1L
      for (m in 1:nm) {
        M <- self$models[[m]]
        k <- length(intersect(M$get_parnames(), names(x)))
        if (M$npar("free") > 0L) {
          M$set_par(x[j:(j+k-1)], check = check)
        }
        j <- j + k
      } 
    },
    set_options = function() {
      self$options <- options
    },
    add_model = function(x, x_name) {
      if (self$super_initialized == TRUE) {
        stop("The model has been completed by end_model(), add the new sub-model before the the call to end_model().", call.=FALSE)
      }
      n <- self$nmod + 1L
      self$data <- self$add_data(i = n, check = TRUE)      
      x <- substitute(x)
      CALL <- self$call_to_list(x)
      self$last_call <- as.call(CALL)
      CALL[["data"]] <- self$data
      CALL[["options"]] <- c(list(fit = FALSE, CALL[["options"]]))
      CALL <- as.call(CALL)
      M <- try(eval(substitute(CALL), envir=parent.frame(2)), silent = TRUE)      
      if (inherits(M, "try-error")) {
        stop("Error trying to add the ", n, ". model, ", x_name, ": ", gsub(".*:", "  ", M), call. = FALSE)
      }
      self$titels[[n]] <- CALL[[1]]
      self$fixes[[n]] <- M$par[M$get_parnames("fix")]
      self$models[[n]] <- M
      self$nmod <- n
      self$functions[[n]] <- function(pred, data, par) { return(pred) }
      return(invisible(self))
    },
    add_data = function(i, check = FALSE) {
      data <- self$data
      if (i > 1L) {
        pred <- self$models[[(i - 1L)]]$predict()
        if (check == TRUE) { self$check_varnames(colnames(pred)) }
        data[, colnames(pred)] <- pred
      }
      return(data)
    },
    add_fun = function(fun) {
      if (!is.function(fun)) {
        stop("'fun' must be a function, but '", .abbrDeparse(fun), "'' is a ", class(fun), ".")
      }
      m <- self$nmod
      if (!identical(self$functions[[m]], function(pred, data, par) { return(pred) }, ignore.environment = TRUE)) { stop("Can't add 2 functions after another, fun() %>% fun() is not possible -> Combine the functions into one.")
      }
      self$functions[[m]] <- fun
      # TODO: add checks for fun() arguments data, par
      return(invisible(self))
    },
    check_varnames = function(x) {
      if (any(x %in% colnames(self$data))) {
          stop('"data" has a variable that is named like the prediction of the ', i, '. model! Duplicated colnames are not allowed; rename the following variables in "data":', .brackify(x[which(x %in% colnames(self$data))]))
        }
    },
    call_to_list = function(x) {
      return(as.list(rlang::call_standardise(as.call(x))))
    },
    end = function(discount = 0L, options = NULL) {
      nm <- self$nmod
      self$options <- options
      title <- paste0(self$titels, collapse=" > ")
      mode <- self$models[[nm]]$mode
      parspace <- do.call(rbind, lapply(self$models, function(m) m$parspace))
      fix <- as.list(unlist(Filter(length, self$fixes), recursive = FALSE))
      self$last_call[[1]] <- paste0(self$titels, collapse="+")
      self$last_call$fix <- fix
      self$last_call <- as.call(self$last_call)
      # TODO: put some sanitize names code here
      self$super_initialized <- TRUE
      super$initialize(
        formula = self$models[[length(self$models)]]$formula,
        data = self$data,
        title = title,
        mode = mode,
        fix = fix,
        parspace = parspace,
        discount = discount,
        options = c(self$options, list(fit_solver = self$models[[nm]]$options$fit_solver))
        )
      return(invisible(self))
    },
    make_constraints = function() {
      C <- lapply(self$models, function(x) x$constraints)
      C <- Filter(Negate(is.null), C)
      i <- 1
      n <- length(C)
      while (i <= n) {
        if (i < n) {
          if (length(C[[i+1]]) > 0L) {
            # shift parameter position
            C[[i+1]]$L$j <- C[[i+1]]$L$j + C[[i]]$L$ncol
          }
        }
        C[[i]]$L$dimnames <- NULL
        C[[i]]$L$ncol <- length(self$par)
        i <- i + 1L
      }
      C <- do.call(.combine_constraints, C)
      return(C)
    }
    )
  )
#' @export
add_model.cogscimodelstack = function(obj, ...) {
  return(obj$add_model(...))
}
#' @export
fun.cogscimodelstack = function(obj, ...) {
  return(obj$add_fun(...))
}
#' @export
end.cogscimodelstack = function(obj, ...) {
  return(obj$end(...))
}
#' @export
`%+%.cogscimodelstack` <- function(obj, ...) {
  x_name <- .abbrDeparse(substitute(...))
  return(obj$add_model(..., x_name = x_name))
}
# #' @export
# `+.cogscimodelstack` <- function(e1, e2) {
#   x_name <- .abbrDeparse(substitute(e2))
#   return(e1$add_model(e2, x_name = x_name))
# }
