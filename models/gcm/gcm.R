library(R6)

gcm <- R6Class("gcm",
               public = list(
                 obs = NULL,
                 input = NULL,
                 initialize = function(formula, data) {
                   self$obs <- model.frame(formula, data, rhs = 0)
                   self$input <- model.frame(formula, data, lhs = 0)
                 } 
               )
)