# ==========================================================================
# This contains a template for new models
# That will inherit from cogscimodels
# ==========================================================================
aModel <- function(formula, data, fixed = list(), fit.options = list(), ...) {
  .args <- match.call()[-1]
  obj <- do.call(AModel$new, .args)

  # If the model has free parameters, fit it.
  if ( length(obj$fixednames) < length(obj$parm) ) {
    obj$fit(c('grid', 'solnp'))
  }
  return(obj)
}


AModel <- R6Class("aModel", # Capitalized <-, but lower case in the string
  inherit = Cogscimodel,
  public = list(
    initialize = function(formula, fixed = NULL, data = NULL, choicerule = NULL, fit.options = list()) {
      #
      # Change these lines
      # ------------------------------------------------------------------------
      # If your model has free parameters, define them in define.parameter()
      # otherwise delete the parDef lines, please.
      #
      # Look at the example below
      # we define one parameter named "beta", 0 < beta < 2 (ll=0, ul = 2), and a starting value for fitting of 2, and a value of 1 will make
      # beta have no effect (na)
      # We define another parameter delta, 0 < deltaq < 1, initial value 0.5, and we don't have a value that switches the paramter of na=NA
      # parDef <- define.parameter(beta  = c(ll=0, ul=5, init=2.5,   na=1),
      #                            delta = c(ll=0, ul=1,  init=0.5, na=NA))
      parDef <- define.parameter() # change this if free parameter exist
      response_data <- # "discrete" or "continuous". Is the data that the model will be fit to "discrete" or "continuous" (choices are discrete, value judgments are continuous)

      #
      # DO NOT Change the next lines until "predict"
      # ------------------------------------------------------------------------
      # This makes an object of class cogscimodel
      # That inherits fitting methods and goodness-of-fit methods
      # and printing methods, etc.
      super$initialize(formula = formula,
                       data = data,
                       allowedparm = parDef,
                       fixed = fixed,
                       choicerule =  choicerule,
                       model = class(x)[1], # Model name = class name
                       discount = 0,
                       response = response,
                       fit.options = fit.options)
    },
    predict = function(type = c("response"), newdata = NULL) {
      #
      # DO NOT Change the next few lines until "predict"
      # ------------------------------------------------------------------------
      parameters <- self$getParm() # Do not change, retrieves parameters
      if ( is.null(newdata) ) { # Do not change, retrieves newdata when needed
        input <- self$input()
      } else {
        input <- self$getInput(self$formula, newdata)
      }  


      # Change this, write prediction code below here
      # ------------------------------------------------------------------------
      # 
      # Write a predict function
      # Note:
      # 1. If your model has free parameter, get them using
      #    parameters['parametername'], for example
      #    alpha <- parameters['alpha']
      # 2. Get the input variables to your model using
      #    var1 <- input[, 1]
      #    use the position, not the name
    }
  )
)