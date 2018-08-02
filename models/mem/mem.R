library(Rcpp)
library(combinat)
library(cogsciutils)
sourceCpp('mem.cpp')
library(R6)
library(Rsolnp)

invisible(sapply(setdiff(list.files(path="../utils/", pattern="*.R$", full.names = TRUE), list.files(path="../utils/", pattern="_test.R$", full.names = TRUE)), source, .GlobalEnv))

mem <- R6Class("mem",
  inherit = cognitiveModel,
  public = list(
    type = NULL,
    criterion = NULL,
    cost = NULL,
    learntrials = NULL,
    ndim = NULL,
    initialize = function(formula, data, fixed = NULL, type, learntrials, initialparm = NULL, discount = 1) {
      ndim <- length(rhs.vars(formula)) - 1
      self$ndim <- ndim

      lastTerm <- paste("~", tail(rhs.vars(formula), 1))
      self$criterion <- get_all_vars(lastTerm, d)[, 1]
      if (ncol(get_all_vars(lastTerm, d)) == 2) {
        self$cost <- get_all_vars(lastTerm, d)[, 2]
      }

      self$inferType(data[, lhs.vars(formula)])
      choicerule <- ifelse(self$type == "judgment", NULL, "softmax")

      allowedparm <- matrix(c(rep(0.001, ndim), 0.001, 1,
                              rep(1, ndim),        10, 2),
                              dimnames = list(c(paste0("w", seq_len(ndim)), "lambda", "r"), c("ll", "ul")), ncol = 2)
      allowedparm <- cbind(allowedparm, init = rowMeans(allowedparm))
      allowedparm[grepl("w", rownames(allowedparm)), "init"] <- 1/ndim
      allowedparm[names(initialparm), ] <- initialparm
      if (self$type == "judgment") {
        obs <- data[, lhs.var(formula)]
        allowedparm <- rbind(allowedparm, c(sigma = c(0, max(obs) - min(obs))))
      }

      formulaWithoutLast <- update(formula, as.formula(paste(". ~ ", head(rhs.vars(formula), -1))))

      super$initialize(formula = formulaWithoutLast, data = data, fixedparm = fixed, allowedparm = allowedparm, choicerule = choicerule, discount = discount, model = paste0("Memory-based model [type: ", self$type, "]"))
      self$formula <- formula

      if ( missing(learntrials) ) {
        self$learntrials <- seq_len(nrow(self$input))
      }        
    },
    inferType = function(response) {
      if (!is.null(self$cost)) {
        self$type <- "valuebasedchoice"
      } else if (!is.factor(response) | any(floor(response) != response) | any(floor(self$criterion) != self$criterion) | length(unique(response)) > 2 | length(self$criterion) > 2) {
        self$type <- "choice"
      } else {
        self$type <- "judgment"
      }
    },
    logLik = function() {
      type <- self$type
      if (type == "judgment") {
        pred <- cbind(self$pred, self$parm["sigma"])
        return(gof(pred = pred, obs = self$obs, type = "logliklihood", pdf = "binomial"))
      } else if (type ==  "choice" || type == "valuebasedchoice") {
        pred <- self$predict()

        return(gof(pred = pred, obs = self$obs, type = "loglik"))
      }
      },
      fit = function(method) {
        set.seed(122)
        method <- match.arg(method, c("grid", "solnp"), several.ok = TRUE)
        if (any("grid" %in% method)) {
          grid <- MakeGridList(names = self$freenames,
            ll = self$allowedparm[, "ll"],
            ul = self$allowedparm[, "ul"],
            nsteps = list(w = 5, r = 3, lambda = 5, tau = 5, sigma = 5),
            sumto = list(w = paste0("w", seq_len(self$ndim))))

          out <- sapply(seq_len(nrow(grid$ids)), function(z) {
            self$setparm(GetParmFromGrid(id = z, grid = grid))
            return(self$logLik())
          })

          self$setparm(GetParmFromGrid(which.max(out), grid))
        }
        if (any("solnp" %in% method)) {
        nws <- sum(grepl("^w", self$freenames))
        if (nws > 0) {
          eqfun <- function(x) {
            return(sum(x[grepl("^w", names(x))]))
          }
          eqB <- 1
        }
        LB <- self$allowedparm[self$freenames, "ll"]
        UB <- self$allowedparm[self$freenames, "ul"]

        fn <- function(x) {
          self$setparm(x)
          -1 * self$logLik()
        }

        out <- solnp(self$parm[self$freenames], fun = fn, eqfun = eqfun, eqB = eqB, LB = LB, UB = UB)

        self$optimization <- out
        self$setparm(out$pars)
        }

        invisible(gc())
      },
      predict = function(newdata) {
        if (missing(newdata) & !is.null(self$pred)) {
          return(self$pred)
        } 

        features <- self$input
        criterion <- self$criterion
        firstOutTrial <- 1
        cost <- self$cost

        if (!missing(newdata)) {
          firstOutTrial <- nrow(self$input) + 1
          features <- rbind(features, newdata[, names(self$input)])
          criterion <- rbind(criterion, newdata[, names(self$criterion)])
          if (type == "valuebasedchoice") {
            cost <- rbind(cost, newdata[, names(self$cost)])
          }
        }

        parameter <- self$parm
        args <- list(
          values = as.double(criterion),
          features = as.matrix(features),
          w = as.double(parameter[grepl('w', names(parameter))]),
          r = as.double(parameter['r']),
          q = as.double(parameter['r']),
          lambda = as.double(parameter['lambda']),
          lastLearnTrial = max(self$learntrials),
          firstOutTrial = firstOutTrial)

        ans <- do.call(mem_cpp, args)

        out <- switch(self$type,
          judgment = ans,
          choice = ans,
          valuebasedchoice = ans - cost)

        out <- super$applychoicerule(out)
        out[self$discount] <- NA
        self$pred <- out

        return(as.matrix(as.numeric(out)))
      },
      print = function(digits = 2) {
        super$print(digits = digits)
      }
  )
)

Mem <- function(formula, data, ...) {
   args <- c(list(formula = formula, data = data), list(...))
   obj <- do.call(mem$new, args)
   
   # check(mod) #todo
   if (length(obj$freenames) > 0) {
       #obj$fit("solnp")
   }
   return(obj)
}

setOldClass("mem", "cognitiveModel") # Class "mem" inherits from "cognitiveModel"
setMethod("predict", c("mem"), function(object) object$predict(newdata = newdata))
setMethod("print", c("mem"), function(x) print.default(x, digits = 2))

# predict.mem <- function(obj, newdata) {
#    obj$predict(newdata = newdata)
# }

# print.mem <- function(obj, digits = 2) {
#   obj$print(digits = digits)
# }

