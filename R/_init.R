# ==========================================================================
# Package: Cognitivemodels
# File: _init.R
# Author: Jana B. Jarecki
# ==========================================================================

# ==========================================================================
# Helper file to initialize stuff
# ==========================================================================

#' @details
#' The following cogniitve models are part of this packge:
#' * baseline - constant or 1/N choice models (as sanity check)
#' * bayes - cognitive bayesian updating model
#' * cpt - cummulative prospect theory (Tversky & Kahneman, 1992)
#' * ebm - exemplar-based models
#' * hm1988 - Houston & McNamara's optimal model for risk-sensitive foraging
#' * lwei - linear weighting model
#' * rscpt - risk-sensitive-foraging cpt model
#' * rsfft - risk-sensitive foraging fast and frugal tree
#' * utility - utility models
#' @useDynLib cognitivemodels, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @keywords internal
"_PACKAGE"
#> [1] "_PACKAGE"