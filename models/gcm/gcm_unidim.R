library(R6)
library(Rsolnp)
library(cogsciutils)
source("../../../cogscimodels/models/gcm/gcm.R")

Gcm_unidim <- R6Class("gcm_unidim",
               inherit = Gcm,
               public = list(
                 initialize = function(formula, data, cat, metric = c("minkowski", "discrete"), fixed, choicerule, discount) {
                   super$initialize(formula = formula, data = data, cat = cat, metric = metric, fixed = fixed, discount = discount, choicerule = choicerule)
                   weightnames <- super$make_weight_names()
                   self$fixednames <- c(weightnames, setdiff(self$fixednames, weightnames))
                   self$freenames <- setdiff(self$freenames, self$fixednames)
                 }
                 # fit = function() {
                 #   fun <- function(parm, self) {
                 #     self$setparm(parm)
                 #     -cogsciutils::gof(obs = self$obs, pred = self$predict(), type = 'log', response = 'discrete')
                 #   }
                 # 
                 #   allowedparm <- self$allowedparm
                 #   LB <- allowedparm[self$freenames, 'll'] 
                 #   UB <- allowedparm[self$freenames, 'ul']  
                 #   par0 <- allowedparm[self$freenames, 'init']
                 # 
                 #   negloglik <- Inf
                 #   for(i in 1:self$ndim) {
                 #     # make all weights to 0 and then make wi to 1
                 #     self$parm[1:self$ndim] <- 0
                 #     self$parm[i] <- 1 
                 #     
                 #     fit <- solnp(pars = par0, fun = fun, LB = LB, UB = UB, self = self)
                 #     if(tail(fit$values, 1) < negloglik) {
                 #       negloglik <- tail(fit$values, 1)
                 #       pars <- c(self$parm[1:self$ndim], fit$pars)
                 #     }
                 #   }
                 #   self$setparm(pars)
                 #   self$loglik <- negloglik # new
                 #   
                 # }
                 # negloglik <- Inf
                 # for(i in 1:self$ndim) {
                 #   # make all weights to 0 and then make wi to 1
                 #   self$parm[1:self$ndim] <- 0
                 #   self$parm[i] <- 1 
                 #   
                 #   fit <- solnp(pars = par0, fun = fun, LB = LB, UB = UB, self = self)
                 #   if(tail(fit$values, 1) < negloglik) {
                 #     pars <- c(self$parm[1:self$ndim], fit$pars)
                 #     negloglik <- tail(fit$values, 1)
                 #   }
                 # }
                 
                 
               )
)

gcm_unidim <- function(formula, data, cat, metric = c("minkowski", "discrete"), fixed, choicerule, discount = 0) {
  obj <- Gcm_unidim$new(formula = formula, data = data, cat = cat, metric = metric, fixed = fixed, choicerule = choicerule, discount = discount)
  if(length(obj$freenames) > 0) {
    obj$fit(unidim = TRUE)
  }
  return(obj)
}
