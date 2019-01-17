library(R6)
library(Rsolnp)
library(cogsciutils)
source("../../../cogscimodels/models/gcm/gcm.R")

Gcm_unidim <- R6Class("gcm_unidim",
               inherit = Gcm,
               public = list(
                 initialize = function(formula, data, cat, metric = c("minkowski", "discrete"), fixed, choicerule, discount) {
                   super$initialize(formula = formula, data = data, cat = cat, metric = metric, fixed = fixed, discount = discount, choicerule = choicerule)
                 },
                 fit = function(...) {
                   if("w1" %in% self$freenames) {
                     print("W1 is free")
                     
                     weightnames <- super$make_weight_names()
                     self$fixednames <- c(weightnames, setdiff(self$fixednames, weightnames))
                     self$freenames <- setdiff(self$freenames, self$fixednames)

                     unidimweights <- matrix(diag(self$ndim), ncol = self$ndim, dimnames = list(NULL, super$make_weight_names()))
                     gofs <- vector("numeric", length = self$ndim)
                     parms <- matrix(nrow = self$ndim, ncol = length(self$parm), dimnames = list(NULL, names(self$parm)))
                     for(i in 1:nrow(unidimweights)) {
                       self$setparm(unidimweights[i, ])
                       super$super_$fit(...)
                       gofs[i] <- self$gofvalue
                       parms[i, ] <- self$parm
                     }
                     print(parms)
                     self$setparm(parms[which.min(gofs), ])
                     self$gofvalue <- min(gofs)
                     
                     self$fixednames <- setdiff(self$fixednames, weightnames)
                     self$freenames <- c(weightnames, self$freenames)
                   } else {
                     super$super_$fit(...)
                   }
                  }
               )
)

gcm_unidim <- function(formula, data, cat, metric = c("minkowski", "discrete"), fixed, choicerule, discount = 0) {
  obj <- Gcm_unidim$new(formula = formula, data = data, cat = cat, metric = metric, fixed = fixed, choicerule = choicerule, discount = discount)
  # if(length(obj$freenames) > 0) {
  #   obj$fit()
  # }
  return(obj)
}
