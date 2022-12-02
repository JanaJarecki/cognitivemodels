Gcm_unidim <- R6Class("gcm_unidim",
               inherit = Gcm,
               public = list(
                 initialize = function(formula, data, cat, metric = c("minkowski", "discrete"), fixed, choicerule, discount) {
                   super$initialize(formula = formula, data = data, cat = cat, metric = metric, fixed = fixed, discount = discount, choicerule = choicerule)
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
                       if(length(self$freenames) > 0) {
                         super$super_$fit(type = type, ...)
                         gofs[i] <- self$gofvalue
                         parms[i, ] <- self$parm
                       } else {
                         preds <- self$predict(newdata = cbind(resp = self$obs, self$input))
                         # gofs[i] <- -cognitiveutils::gof(obs = self$obs, pred = preds, response = "d", type = "log", discount = self$discount)
                         
                         gofs[i] <- -log(max(prod(abs(preds - (1 - self$obs))[-c(self$discount)]), 1e-300))
                         parms <- unidimweights
                       } 
                     }
                     if(abs(diff(gofs)) < .01) { # if(which.max(gofs) == which.max(rev(gofs))) {
                       self$setparm(list(w1 = 0, w2 = 1, w3 = 0))
                       self$gofvalue <- NA
                     } else {
                       self$setparm(parms[which.min(gofs), ])
                       self$gofvalue <- min(gofs)
                     }
                     
                     self$fixednames <- setdiff(self$fixednames, weightnames)
                     self$freenames <- c(weightnames, self$freenames)
                   } else {
                     super$super_$fit(type = type, ...)
                   }
                 }
               )
)

gcm_unidim <- function(formula, data, cat, metric = c("minkowski", "discrete"), fixed, choicerule, discount = 0) {
  obj <- Gcm_unidim$new(formula = formula, data = data, cat = cat, metric = metric, fixed = fixed, choicerule = choicerule, discount = discount)
  if(length(obj$freenames) > 0) {
    obj$fit(na.rm = TRUE)
  }
  return(obj)
}
