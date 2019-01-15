# Work with reference classes
library(R6)
library(cogsciutils)
library(Rsolnp)
# library(formula.tools)
library(Formula)

cognitiveModel <- R6Class(
  "cognitiveModel",
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
    gofvalue = NULL,
    initialize = function(formula, data, allowedparm, fixedparm = NULL, choicerule =  NULL, model = NULL, discount = NULL) {
      self$model <- model
      formula <- as.formula(formula)
      self$input <- get_all_vars(formula, data)[,-1]
      self$obs   <- get_all_vars(formula, data)[,1]
      self$formula <- formula
      
      if (!is.null(choicerule)) {
        choicerule <- match.arg(choicerule, c("luce", "argmax", "softmax", "epsilon"))
        if (choicerule == "softmax") {
          allowedparm <- rbind(allowedparm, tau = c(0.1, 10, 0.5))
        } else if (choicerule == "epsilon") {
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
        x[] <- do.call(cogsciutils::choicerule, args)
        return(x)
      }
    },
    logLik = function(...) {
      return(cogsciutils::gof(obs = self$obs, pred = self$predict(), type = "loglik", discount = self$discount, response = 'disc'))
    },
    AIC = function(k = 2) {
     return(-2 * self$logLik() + k * length(self$freenames))
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
    fit = function(type = c('grid', 'solnp')) {
      type <- match.arg(type, several.ok = TRUE)
      allowedfreeparm <- self$allowedparm[self$freenames,,drop=FALSE]
      nfree <- length(self$freenames)

      fun <- function(parm, self) {
        self$setparm(parm)
        -cogsciutils::gof(obs = self$obs, pred = self$predict(), type = 'log', response = 'discrete')
      }

      LB <- allowedfreeparm[, 'll', drop = FALSE]
      UB <- allowedfreeparm[, 'ul', drop = FALSE]

      # Grid-based fitting 
      if ('grid' %in% type) {
        ST <- apply(allowedfreeparm, 1, function(x) round((max(x) - min(x)) / sqrt(10 * nfree), 2))
        parGrid <- expand.grid(sapply(rownames(LB), function(i) c(LB[i, ], seq(max(LB[i, ], ST[i]), UB[i, ], ST[i])), simplify = FALSE))
        negGof <- sapply(1:nrow(parGrid), function(i) fun(parm = unlist(parGrid[i,,drop=FALSE]), self = self))
        fit <- list(values = min(negGof),
                    pars = unlist(parGrid[which.min(negGof), , drop = FALSE]))
      }

      # Controls for solnp
      solnp_control <- list(trace = 1, delta = sqrt(.Machine$double.eps))

      # Optimization-based fitting
      if (all(grepl('rsolnp', type))) {
        par0 <- self$allowedparm[, 'init']
        fit <- solnp(pars = par0, fun = fun, LB = LB, UB = UB, self = self, control = solnp_control)
      }

      # Optimization-based fitting with grid values as starting point(s)
      if (any(grepl('grid|rsolnp', type))) {
          print('in solnp')
          print(UB)
          grid5 <- which(rank(negGof, ties.method = 'random') <= 5)
          par0 <- parGrid[grid5, , drop = FALSE]
          fits <- apply(par0, 1, function(i) solnp(pars = i, fun = fun, LB = LB, UB = UB, self = self, control = solnp_control))
          fit <- fits[[which.min(lapply(fits, function(fit) tail(fit$values, 1)))]]
        }

      self$gofvalue = tail(fit$value, 1)
      self$setparm(fit$pars)
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