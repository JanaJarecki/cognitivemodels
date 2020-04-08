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
    return(lapply(1:length(formula)[2], function(i) all.vars(formula(formula, lhs = 0, rhs = i, drop=FALSE))))
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


#' Add I(1-x) to the last term of formula for gambles
#' 
#' @param formula Objects of class "formula"
#' @return An object of class "formula" with the last formula terms added
#' @noRd
.add_missing_prob <- function(formula) {
  formula <- as.Formula(formula)
  # Apply it to each right-hand side
  if (length(formula)[2] > 1) {
    updated <- lapply(1:length(formula)[2], function(i) .add_prob_rhs(formula(formula, lhs = i == 1, rhs = i, drop = FALSE)))
    return(as.formula(gsub("\\| \\~", "\\| ", paste(updated, collapse = " | "))))
  }
  
  if (.rhs_length(formula) %% 2 == 0) { return(formula) }
  vars <- .rhs_varnames(formula)[[1]]
  term <- as.formula(paste0("~ . + I(1-", paste0(vars[seq(2, length(vars), 2)], collapse = "-"), ")"))
  return(update(formula, term))
}