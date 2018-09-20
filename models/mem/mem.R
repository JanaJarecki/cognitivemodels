library(Rcpp)
library(combinat)
library(cogsciutils)
sourceCpp('mem.cpp')
library(R6)
library(Rsolnp)
source("../../utils/classes/cognitiveModel.R", chdir = T)

#invisible(sapply(setdiff(list.files(path="../../utils/", pattern="*.R$", full.names = TRUE), list.files(path="../utils/", pattern="_test.R$", full.names = TRUE)), source, .GlobalEnv))

mem <- R6Class("mem",
  inherit = cognitiveModel,
  public = list(
    type = NULL,
    criterion = NULL,
    cost = NULL,
    learntrials = NULL,
    ndim = NULL,
    initialize = function(formula, data, fixed = NULL, type, learntrials, initialparm = NULL, discount = 1) {
      data <- as.data.frame(data)
      ndim <- length(rhs.vars(formula)) - 1
      self$ndim <- ndim

      lastTerm <- paste("~", tail(rhs.vars(formula), 1))
      self$criterion <- get_all_vars(lastTerm, data)[, 1]
      if (ncol(get_all_vars(lastTerm, data)) == 2) {
        self$cost <- get_all_vars(lastTerm, data)[, 2]
      }

      self$inferType(data[, lhs.vars(formula)])

      allowedparm <- c(list(
        lambda = c(0.001, 1, 0.5),
        r = c(1, 2, 1.5)),
        w = replicate(ndim, list(c(0.001, 1, 1/ndim))))
      allowedparm <- matrix(unlist(allowedparm), ncol = 3, byrow = T, dimnames= list(names(allowedparm), c('ll', 'ul', 'init')))
      
      if (self$type == "judgment") {
        obs <- data[, lhs.vars(formula)]
        rng <- max(obs) - min(obs)
        allowedparm <- rbind(allowedparm, c(0, rng, rng / 2))
        rownames(allowedparm)[nrow(allowedparm)] <- "sigma"
      }
      allowedparm[names(initialparm), ] <- initialparm

      formulaWithoutLast <- update(formula, reformulate(attr(drop.terms(terms(formula), (ndim+1)), "term.labels")))

      super$initialize(formula = formulaWithoutLast, data = data, fixedparm = fixed, allowedparm = allowedparm, choicerule = self$choicerule, discount = discount, model = paste0("Memory-based model [type: ", self$type, "]"))
      self$formula <- formula

      if ( missing(learntrials) ) {
        self$learntrials <- seq_len(nrow(self$input))
      }        
    },
    inferType = function(response) {
      if (!is.null(self$cost)) {
        self$type <- "valuebasedchoice"
        self$choicerule <- "softmax"
      } else if (!is.factor(response) | any(floor(response) != response) | any(floor(self$criterion) != self$criterion) | length(unique(response)) > 2 | length(self$criterion) > 2) {
        self$type <- "judgment"
        self$choicerule <- NULL
      } else {
        self$type <- "choice"
        self$choicerule <- "softmax"
      }
    },
    logLik = function() {
      type <- self$type
      if (type == "judgment") {
        pred <- cbind(self$predict(), self$parm["sigma"])
        return(gof(pred = pred, obs = self$obs, type = "loglik", response = "continuous"))
      } else if (type ==  "choice" || type == "valuebasedchoice") {
        pred <- self$predict()

        return(gof(pred = pred, obs = self$obs, type = "loglik", response = "discrete"))
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
            parm <- GetParmFromGrid(id = z, grid = grid)
            self$setparm(parm)
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
          features <- rbind(features, newdata[, colnames(self$input)])
          lastTerm <- paste("~", tail(rhs.vars(self$formula), 1))
          criterion <- c(criterion, get_all_vars(lastTerm, newdata)[, 1])
          if (self$type == "valuebasedchoice") {
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
        

        if (missing(newdata)) {
          out[self$discount] <- NA
          self$pred <- out
        }

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

