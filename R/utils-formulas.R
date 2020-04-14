# ==========================================================================
# Package: Cognitivemodels
# File: utils-formula.R
# Author: Jana B. Jarecki
# Changed: 2019-12-13
# ==========================================================================

# ==========================================================================
# Utility functions for formulas
# ==========================================================================

#' Convert a character to formula
#' 
#' Checks if x is a character and if so converts it into RHS formula
#' 
#' @param x A string or RHS formula
#' @export
chr_as_rhs <- function(x) {
  if (is.character(x)) { 
    return(reformulate(x))
  } else { 
    return(x)
  }
}

# ==========================================================================
# Tools to handle formulas
# ==========================================================================

#' LHS of a formula
#' 
#' Returns the left-hand side variables of a formula as strings
#' or NULL if the formula has none
#' 
#' @param formula Objects of class "formula"
#' @param n Integer, the nth LHS-variable is returned, defaults to all LHS
#' @return the right-hand side of the formula or \code{NULL} is no RHS exists
#' @noRd
.lhs_var <- function(formula, n = NULL) {
  if (attr(terms(as.formula(formula)), which = "response")) {
    return(all.vars(formula(Formula::as.Formula(formula), lhs = 1, rhs = 0)))
  } else {
    return(NULL)
  }
}


#' List of RHSs of a formula
#' 
#' Returns a list with all right-hand side variables, by-separators |, of a formula as strings
#' or NULL if the formula has none
#' 
#' @param formula Objects of class "formula"
#' @param n Integer, the nth RHS-variable is returned, defaults to all RHS
#' @return the right-hand side of the formula or \code{NULL} is no RHS exists
#' @noRd
.rhs_varnames <- function(formula, n = NULL) {
  formula <- Formula::as.Formula(formula)
  if (length(attr(terms(formula), which = "term.labels"))) {
    return(lapply(1:length(formula)[2], function(i) attr(terms(formula(formula, lhs = 0, rhs = i, drop=FALSE)), "term.labels")))
  } else {
    return(NULL)
  }
}

#' Vector with length of RHS of formula
#' 
#' @param formula Objects of class "formula"
#' @return A vector with the number of RHS elements in each part separated by a | in the formula
#' @noRd
.rhs_length <- function(formula) {
  formula <- Formula::as.Formula(formula)
  sapply(1:length(formula)[2], function(i) length(attr(terms(formula(formula, lhs=0L, rhs=i)), "term.labels")))
}

#' Matrix with RHS variabled
#' 
#' Returns a list with all right-hand side variables, by-separators |, of a formula as strings
#' or NULL if the formula has none
#' 
#' @param formula Objects of class "formula"
#' @param n Integer, the nth RHS-variable is returned, defaults to all RHS
#' @param data A data frame from which to get the variables
#' @param computed A boolean, \code{"TRUE"} returns variables like \code{log(x)} and \code{FALSE} returns only the input variables.
#' @return the right-hand side of the formula or \code{NULL} is no RHS exists
#' @noRd
.get_rhs_vars <- function(formula, data, n = NULL, computed = TRUE) {
  formula <- Formula::as.Formula(formula)
  if (length(attr(terms(formula), which = "term.labels"))) {
    if (computed == TRUE) {
      return(lapply(1:length(formula)[2], function(i) model.frame(formula(formula, lhs = 0, rhs = i), data = data)))
    } else {
      return(lapply(1:length(formula)[2], function(i) get_all_vars(formula(formula, lhs = 0, rhs = i), data = data)))
    }
  } else {
    return(NULL)
  }
}


#' In gambles from description, add I(1-x) to the last term
#' 
#' @param formula Objects of class "formula"
#' @return An object of class "formula" with the last formula terms added
#' @noRd
.add_missing_prob <- function(formula) {
  formula <- as.Formula(formula)
  # Apply it to each right-hand side
  if (length(formula)[2] > 1) {
    updated <- lapply(1:length(formula)[2], function(i) .add_missing_prob(formula(formula, lhs = i == 1, rhs = i, drop = FALSE)))
    return(as.formula(gsub("\\| \\~", "\\| ", paste(updated, collapse = " | "))))
  }
  
  if (.rhs_length(formula) %% 2 == 0) { return(formula) }
  vars <- .rhs_varnames(formula)[[1]]
  term <- as.formula(paste0("~ . + I(1-", paste0(vars[seq(2, length(vars), 2)], collapse = "-"), ")"))
  return(update(formula, term))
}

#' In gambles from description, check if probabilities sum to 1 and are between 0 and 1
#' 
#' @param self A cognitive model object
#' @return \code{NULL} if the probabilities sum to 1, else stops with an error
#' @noRd
.check_probabilities <- function(self, x = NULL) {
  if (!missing(self)) {
    if (length(self$input) == 0) return(NULL)
    x <- self$input
  }
  if (!is.na(dim(x)[3])) {
    return(sapply(1:dim(x)[3], function(k) {
      j <- seq(2, ncol(x), 2)
      x <- abind::adrop(x[, j, k, drop=FALSE], 3)
      colnames(x) <- .rhs_varnames(self$formula)[[k]][j]
      .check_probabilities(x = x)
      }
    ))
  }
  if (!isTRUE(all.equal(rowSums(x), rep(1L, nrow(x))))) {
    stop("The probability columns in 'data' ", cognitivemodels:::.brackify(colnames(x)), " do not sum to 1'.\n  * Are the probability variables really called ", cognitivemodels:::.brackify(colnames(x)), "?\n  * Do any of these variables in 'data' not sum to 1?", call. = FALSE)
  }
  if (any(x > 1 | x < 0)) {
    stop("The probability columns in 'data' ", cognitivemodels:::.brackify(colnames(x)), " must lie between 0 and 1.\n  * Are the probability variables really called ", cognitivemodels:::.brackify(colnames(x)), "?\n  * Are any of these variables in 'data' above 1 or negative?", call. = FALSE)
  }
}