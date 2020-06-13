Gcm_sim_unidim <- R6Class("gcm_sim_unidim",
                      inherit = Gcm_sim,
                      public = list(
                        initialize = function(formula, data, metric = c("minkowski", "discrete", "threshold"), fixed, choicerule, discount) {
                          super$initialize(formula = formula, data = data, metric = metric, fixed = fixed, discount = discount, choicerule = choicerule)
                        },
                        fit = function(type = c("grid", "solnp"), ...) {
                          if("w1" %in% self$freenames) {
                            weightnames <- super$make_weight_names()
                            self$fixednames <- c(weightnames, setdiff(self$fixednames, weightnames))
                            self$freenames <- setdiff(self$freenames, self$fixednames)
                            
                            unidimweights <- matrix(diag(self$ndim), ncol = self$ndim, dimnames = list(NULL, super$make_weight_names()))
                            unidimweights <- unidimweights[c(1, 3), ] # delete
                            gofs <- vector("numeric", length = self$ndim)
                            gofs <- vector("numeric", length = self$ndim - 1) # delete
                            parms <- matrix(nrow = self$ndim, ncol = length(self$parm), dimnames = list(NULL, names(self$parm)))
                            parms <- matrix(nrow = self$ndim - 1, ncol = length(self$parm), dimnames = list(NULL, names(self$parm))) # delete
                            for(i in 1:nrow(unidimweights)) {
                              self$setparm(unidimweights[i, ])
                              print(unidimweights[i, ])
                              if(length(self$freenames) > 0) {
                                super$super_$fit(type = type, ...)
                                gofs[i] <- self$gofvalue
                                parms[i, ] <- self$parm
                              } else {
                                gofs[i] <- -cogsciutils::gof(obs = self$obs, pred = self$predict(), response = "d", type = "log", discount = self$discount)
                                parms <- unidimweights
                              } 
                            }
                            self$setparm(parms[which.min(gofs), ])
                            self$gofvalue <- min(gofs)
                            
                            self$fixednames <- setdiff(self$fixednames, weightnames)
                            self$freenames <- c(weightnames, self$freenames)
                          } else {
                            super$super_$fit(type = type, ...)
                          }
                        }
                      )
)

gcm_sim_unidim <- function(formula, data, metric = c("minkowski", "discrete", "threshold"), fixed, choicerule, discount = 0) {
  obj <- Gcm_sim_unidim$new(formula = formula, data = data, metric = metric, fixed = fixed, choicerule = choicerule, discount = discount)
  if(length(obj$freenames) > 0) {
    obj$fit(na.rm = TRUE)
  }
  return(obj)
}
