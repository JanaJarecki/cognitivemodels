require(R6)
library(Formula)
library(stringr)
library(Rsolnp)
source("../../utils/classes/cognitiveModel.R", chdir = TRUE)

Cpt <- R6Class("cpt",
  inherit = cognitiveModel,
  public = list(
    nopt = NULL,
    nout = NULL,
    ref = NULL,
    wfun = NULL,
    vfun = NULL,
    initialize = function(formula, nopt, nout, ref, fixed = NULL, data = NULL, choicerule, weighting = c('TK1992'), value = c('TK1992')) {
      # Ranges of parameters following these studiese
      # 1. International comparison study
      # Rieger, M. O., Wang, M., & Hens, T. (2017). Estimating cumulative prospect theory parameters from an international survey. Theory and Decision, 82, 567–596. doi:10.1007/s11238-016-9582-8
      # and
      # 2. parameter stability study
      # Glöckner, A., & Pachur, T. (2012). Cognitive models of risky choice: parameter stability and predictive accuracy of prospect theory. Cognition, 123, 21–32. doi:10.1016/j.cognition.2011.12.002
      allowedparm <- list(
        alpha   = c(0, 2, .8),
        beta    = c(0, 2, .8),
        gammap  = c(0, 2, 1),
        gamman  = c(0, 2, 1),
        lambda  = c(0, 10, 1.5)
        )
      allowedparm <- matrix(unlist(allowedparm), ncol = 3, byrow = T, dimnames= list(names(allowedparm), c('ll', 'ul', 'init')))
     
      super$initialize(formula = formula, data = data, allowedparm = allowedparm, fixedparm = fixed, choicerule =  choicerule, model = paste0("Cumulative prospect theory (", unique(c(weighting, value)), ")"), discount = 0)

      if (length(ref) == 1) {
        ref <- rep(ref, nrow(data))
      }      
      if (length(nout) == 1) {
        nout <- rep(nout, each = nopt)
      }

      self$ref  <- ref
      self$nout <- nout
      self$nopt <- nopt
      self$setWeightingFun(weighting)
      self$setValueFun(value)
    },
    predict = function(type = c("choice", "value"), action = NULL, newdata = NULL) {
      type <- match.arg(type)

      if (!is.null(newdata)) {
        input <- model.frame(formula, newdata)[, -1, drop = F]
      } else {
        input <- self$input
      }
       
      OptionList <- lapply(seq_along(self$nout), function(i) {
        model.matrix(Formula(self$formula), input, rhs = i)[, -1, drop = FALSE]
      })

      v <- lapply(OptionList, function(i) self$cpt(
        x = i[, seq_along(self$nout)],
        p = i[, -seq_along(self$nout)]))
      v <- matrix(unlist(v), ncol = length(v))
      
      nn <- unlist(lapply(OptionList, function(x) unique(str_extract(dimnames(x)[[2]], pattern = "[a-z]+"))[1]))
      if (length(unique(nn)) < self$nopt) {
        nn <- paste0(nn[1], seq_len(self$nopt))
      }
      colnames(v) <- nn

      switch (type,
        choice = super$applychoicerule(v),
        value = v)
    },
    cpt = function(x, p) {
      if (!is.matrix(p)) {
        p <- t(as.matrix(p))
      }
      if (!is.matrix(x)) {
        x <- t(as.matrix(x))
      }

      x <- x - self$ref

      args <- c(as.list(self$parm), list(p = p, x = x))      
      out <- rowSums(do.call(self$wfun, args) * do.call(self$vfun, args))
      
      return(out)
    },
    fit = function() {
      LB <- self$allowedparm[, 'll'] 
      UB <- self$allowedparm[, 'ul']  
      par0 <- self$allowedparm[, 'init']
      fun <- function(parm, self) {
        self$setparm(parm)
        -cogsciutils::gof(obs = self$obs, pred = self$predict()[, 1], type = 'log', response = 'discrete')
      }
      fit <- solnp(pars = par0, fun = fun, LB = LB, UB = UB, self = self)
      self$setparm(fit$pars)
    },
    setWeightingFun = function(type) {
        if (type == "TK1992") {
          # One-parameter specification of cumulative prospect theory
          wfun <- function(p, x, gammap, gamman, ...) {
            if(dim(p)[2] != 2) {
              stop("Argument p in the weighting fun in cpt must have 2 columns,\nbut has ", ncol(p), ", which is: ", head(p))
            }

            # Todo: Check if the cummulative weighting is implemented correctly
            out <- t(sapply(1:nrow(p), function(.rowid, x) {
              x <- x[.rowid, ]
              p <- p[.rowid, ]    
              pn <- p[x < 0]
              pp <- p[x >= 0]

              norder <- order(x[x <  0])
              porder <- order(x[x >= 0], decreasing = TRUE)
              
              # Cummulative sum of the orderes probabilities
              pnord <- cumsum(pn[norder])
              ppord <- cumsum(pp[porder])              

              # Weight the cummulative probabilities
              wpord <- (ppord^gammap / ( rowSums(cbind(ppord, 1-ppord)^gammap) )^(1/gammap))
              wnord <- (pnord^gamman / ( rowSums(cbind(pnord, 1-pnord)^gamman) )^(1/gamman))

              # Subtract the weights
              if (sum(x < 0) > 1) {
                wnord <- wnord - c(0, head(wnord, -1))
              }
              if (sum(x >= 0) > 1) {
                wpord <- wpord - c(0, head(wpord, -1))
              }

              # Reorder weights to the original order
              wp <- wpord[order(porder)]
              wn <- wnord[order(norder)]

              out <- x
              out[x >= 0] <- wp
              out[x <  0] <- wn
              return(out)
            }, x = x))
            # out <- replace(out, x >= 0, (p^gammap / ( rowSums(p^gammap) )^(1/gammap))[x >= 0])
            # out <- replace(out, x < 0, (p^gamman / ( rowSums(p^gamman) )^(1/gamman))[x < 0])
            return(out)
          }
        }
        self$wfun <- wfun
    },
    setValueFun = function(type) {
        if (type == "TK1992") {
          vfun <- function(x, alpha, beta, lambda, ...) {
            out <- x^alpha
            out <- replace(out, x < 0, (-lambda * (-x)^beta )[x<0])
            return(out)
          }
        } 
        self$vfun <- vfun
    }
  )
)


cpt <- function(formula, nopt, nout, ref, fixed = NULL, data, choicerule, weighting = c('TK1992'), value = c('TK1992')) {
  obj <- Cpt$new(formula, nopt, nout, ref, fixed = fixed, data, choicerule, weighting = c('TK1992'), value = c('TK1992'))
  if (length(obj$fixednames) < length(obj$parm)) {
    obj$fit()
  }
  return(obj)
}

predict.Cpt <- function(obj, type = NULL, action = NULL, newdata = NULL) {
  return(obj$predict(type = type, action = action, newdata = newdata))
}


# bReachedFun <- function(env, state) { env$terminal.fitness(state = state + env$evs - 4 * env$vars, budget = env$budget) }