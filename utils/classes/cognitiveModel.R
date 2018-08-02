# Work with reference classes
library(R6)
library(cogsciutils)
library(formula.tools)

cognitiveModel <- R6Class("cognitiveModel",
  public = list(
    model = "string",
    formula = "formula",
    input = "matrix",
    obs = "matrix",
    pred = 'matrix',
    parm = 'vector',
    fixednames = NULL,
    freenames = NULL,
    allowedparm = 'matrix',
    choicerule = NA, 
    optimization = NA,
    discount = NA,
    initialize = function(formula, data, allowedparm, fixedparm = NULL, choicerule =  NULL, model = NULL, discount = NULL) {
      self$model <- model
      self$formula <- formula
      self$input <- as.matrix(data[, rhs.vars(formula)])
      self$obs <- as.vector(data[, lhs.vars(formula)])
      
      if (!is.null(choicerule)) {
        choicerule <- match.arg(choicerule, c("luce", "argmax", "softmax", "epsilon"))
        if (choicerule == "softmax") {
          allowedparm <- rbind(allowedparm, tau = c(0.001, 5, 0.5))
        } else if (choicerule == "argmax") {
          allowedparm <- rbind(allowedparm, eps = c(0.001, 1, 0.2))
        }
      }

      self$fixednames <- names(fixedparm)
      self$freenames <- setdiff(rownames(allowedparm), names(fixedparm))
      self$parm <- allowedparm[, "init"]
      names(self$parm) <- rownames(allowedparm)
      self$setparm(fixedparm)
      self$allowedparm <- allowedparm
      self$choicerule <- choicerule    
      self$setdiscount(discount)
    },
    setdiscount = function(x) {
      if(sum(x) == 0) {
        x <- NULL
      } else if (is.logical(x)) {
        x <- cumsum(x)
      } else if (length(x > 1)) {
        x <- as.numeric(x)
      } else if(length(x) == 1) {
        x <- seq_len(x)
      }
      self$discount <- x
      return(invisible(self))
    },
    which.choiceruleparm = function() {
      return(switch(self$choicerule,
        softmax = which(names(self$parm) == "tau"),
        epsilon = which(names(self$parm) == "eps")))
    },
    setparm = function(x) {
      x <- x[names(x) %in% names(self$parm)]
      self$parm[names(x)] <- x
      self$pred <- NULL
    },
    addparm = function(x) {
      if (names(x) %in% names(self$parm)) {
        self$setparm(x)
      }
      self$parm <- c(self$parm, x)
      self$pred <- NULL
    },
    applychoicerule = function(x) {
      if (is.null(self$choicerule)) {
        return(x)
      } else {
        args <- c(list(x = x, type = self$choicerule), as.list(self$parm[self$which.choiceruleparm()]))
        return(do.call(cogsciutils::choicerule, args))
      }
    },
    logLik = function(...) {
      return(cogsciutils::gof(obs = self$obs, pred = self$predict(), type = "loglik", discount = self$discount, ...))
    },
    MSE = function(...) {
      return(cogsciutils::gof(obs = self$obs, pred = self$predict(), type = "mse", discount = self$discount, ...))
    },
    SSE = function(...) {
      return(cogsciutils::gof(obs = self$obs, pred = self$predict(), type = "sse", discount = self$discount, ...))
    },
    RMSE = function(...) {
      return(cogsciutils::gof(obs = self$obs, pred = self$predict(), type = "rmse", discount = self$discount, ...))
    },
    print = function(digits = 2) {
      cat(self$model)
      cat("\nCall:\n",
      paste(deparse(self$formula), sep = "\n", collapse = "\n"), "\n\n", sep = "")
      if(length(self$freenames) > 0) {
          cat("Free parm estimates\n")
          print.default(format(self$parm[self$freenames], digits = digits), print.gap = 2L, quote = FALSE)
      } else cat("No free parm\n")
      cat("\n")
      if(length(self$fixednames) > 0) {
        cat("Fixed parm\n")
        print.default(format(self$parm[self$fixednames], digits = digits), print.gap = 2L, quote = FALSE)
      } else {
        cat("No fixed parm\n")
      }

      cat("\n")
      
      if (is.null(self$choicerule)) {
        cat("No choice rule\n")
      } else {
        cat("Choice rule:", self$choicerule)
      }     
      
      cat("\n")
      invisible(self)
    }
  )
)


# Define S3 methods
logLik.cognitiveModel <- function(obj, ...) {
  obj$logLik(...)
}
SSE <- function(obj, ...) {
  UseMethod("SSE")
}
SSE.cognitiveModel <- function(obj, ...) {
  obj$SSE(...)
}
MSE <- function(obj, ...) {
  UseMethod("MSE")
}
MSE.cognitiveModel <- function(obj, ...) {
  obj$MSE(...)
}
RMSE <- function(obj, ...) {
  UseMethod("RMSE")
}
RMSE.cognitiveModel <- function(obj, ...) {
  obj$RMSE(...)
}

# showMethods("logLik")
# logLik

# setOldClass(c("cognitiveModel", "R6"))

# setGeneric("logLik", function(object, ...) { UseMethod("") })
# setGeneric("SSE", function(obj, ...) { UseMethod("SSE") } )
# setGeneric("MSE", function(obj, ...) { UseMethod("MSE") } )

# setMethod("SSE", c("cognitiveModel"), function(obj) obj$SSE(...))
# setMethod("MSE", c("cognitiveModel"), function(obj) obj$MSE(...))
# setMethod("logLik", c("cognitiveModel"), function(object, ...) object$logLik(...))
# setMethod("print", c("cognitiveModel"), function(x) x$print())
# setMethod("predict", c("cognitiveModel"), function(object) object$predict())



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