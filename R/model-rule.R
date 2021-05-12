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
rule_seitz2021 <- function(formula, data, fix = list(), choicerule = NULL, mode, discount = 0L, options = list(), ...) {
  .args <- as.list(rlang::call_standardise(match.call())[-1])
  .args$bound <- c(0.5, 0.5, 1.5)
  return(do.call(what = Rule$new, args = .args, envir = parent.frame()))
}

Rule <- R6Class("Rule",
                inherit = Cm,
                public = list(
                  bound = NULL,
                  initialize = function(formula, data = data.frame(), bound, fix = list(), choicerule = NULL, mode = "discrete", discount = 0, options = list(), ...) {
                    
                    self$bound <- bound
                    super$initialize(
                      title = "Rule",
                      formula = formula,
                      data = data,
                      parspace = make_parspace(),
                      choicerule = choicerule,
                      mode = mode,
                      fix = fix,
                      discount = discount,
                      options = c(options, list(solver = c("solnp"))),
                      ...)
                    if(ncol(self$input) != length(self$bound)) {
                      stop("\nThe input does not have the same amount of features (", ncol(self$input), ") as the bound (", length(self$bound), ").")
                    }
                  },
                  make_prediction = function(type = "response", input, ...) {
                    comps <- t(sign(t(input) > self$bound))
                    cat_choice <- 
                      # if feature 2 > bound, look at feature 1
                      (comps[, 2] * comps[, 1] + 
                         # if feature 2 < bound, look at feature 3
                         (1 - comps[, 2]) * comps[, 3])
                    # in this specific case low values are category 1 (direction of category)
                    cat_choice <- 1 - cat_choice
                    return(cat_choice)
                  },
                  check_input = function() {
                    # This function is called automatically at the end of initialize()
                    # Write code for checking inputs (= data of the RHS of formula) below
                    
                    # -----------------
                    super$check_input() # leave this at the end, it runs background checks
                  }
                )
)
