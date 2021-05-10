# ==========================================================================
# Package: Cognitivemodels
# File: model-rule.R
# Author: Jana B. Jarecki
# ==========================================================================

# ==========================================================================
# Cognitive Model
# ==========================================================================


#' Rule Model
#' 
#' `rule()` fits a rule model without free parameters.
#' @useDynLib cognitivemodels, .registration = TRUE
#' @return A model of class "rule".
#' 
#' @template cm
#' @author Jana B. Jarecki
#' 
#' 
#' @examples 
#' D <- as.data.frame(expand.grid(f1 = 0:3, f2 = 0:3, f3 = 0:3))
#' bound <- c(0.5, 0.5, 1.5)
#' 
#' M <- rule(~ f1 + f2 + f3, D)
#' M$make_prediction(D, bound)
#' 
#' @export
rule <- function(formula, data, fix = list(), choicerule = NULL, mode, discount = 0L, options = list(), ...) {
  .args <- as.list(rlang::call_standardise(match.call())[-1])
  return(do.call(what = Rule$new, args = .args, envir = parent.frame()))
}

Rule <- R6Class("Rule",
                inherit = Cm,
                public = list(
                  initialize = function(formula, data = data.frame(), fix = list(), choicerule = NULL, mode = "discrete", discount = 0, options = list(), ...) {
                    
                    super$initialize(
                      title = "Rule",
                      formula = formula,
                      data = data,
                      # parspace = self$make_parspace(f=formula, d=data),
                      choicerule = choicerule,
                      mode = mode,
                      fix = fix,
                      discount = discount,
                      # options = c(options, list(solver = c("solnp"))),
                      ...)
                  },
                  make_prediction = function(input = self$input, bound, ...) {
                    input <- matrix(input, nrow = nrow(input), dimnames = list(NULL, paste0("f", 1:ncol(input))))
                    comps <- t(sign(t(input) > bound))
                    return(1 - (comps[, "f2"] * comps[, "f1"] + (1 - comps[, "f2"]) * comps[, "f3"]))
                  },
                  check_input = function() {
                    # This function is called automatically at the end of initialize()
                    # Write code for checking inputs (= data of the RHS of formula) below
                    
                    # -----------------
                    super$check_input() # leave this at the end, it runs background checks
                  }
                )
)
