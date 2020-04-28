#' Analysis of Deviance for Cogscimodel Fits
#' 
#' `anova` returns an analysis of deviance table for one or more fitted cognitive models
#'
#' @importFrom AICcmodavg AICc
#' @importFrom cognitiveutils akaike_weight
#' @param object,... Objects of class \code{cm}, typically the result of a call to one fo the models. (Or a list of \code{objects} for the \code{"cogscimodellist"} method.)
#' @return An anova-style table
#' @examples
#' None yet.
#' @export
anova.cm <- function(object, ...) {
  ## check for multiple objects, this is like in anova.glm

  dotargs <- list(...)
  named <- if (is.null(names(dotargs))) {
              rep_len(FALSE, length(dotargs)) 
            }  else {
              (names(dotargs) != "")
            }
  if (any(named)) {
    warning("These arguments to 'anova.cm' are invalid and dropped: ",
      paste(deparse(dotargs[named]), collapse=", "))
  }
  dotargs <- dotargs[!named]

  # Allowed models are cogscimodel, lm, merMod
  
  is.allowed.class <- vapply(dotargs, function(x) inherits(x, "cm"), NA) | vapply(dotargs, function(x) inherits(x, "lm"), NA) | vapply(dotargs, function(x) inherits(x, "merMod"), NA)
  dotargs <- dotargs[is.allowed.class]

  ## If multiple objects: anova table for multiple models

  if ( length(dotargs) ) {
    return( anova(structure(c(list(object), dotargs), class="cogscimodellist")) )
  }

  ## Else if single object: construct table and title for a single model
  npar <- object$npar('free')
  ss <- object$SSE()
  ms <- object$MSE()
  table <- data.frame(npar, ss, ms)
  dimnames(table) <- list('',
                          c("N Par", "Sum Sq", "Mean Sq"))
  structure(table, heading = "Sum Sq. Table", class = c("anova", "data.frame"))
}

#' @export
anova.cogscimodellist <- function(models, model.names = NULL) {
  nobs <- vapply(models, nobs, 1L)
  if (var(nobs) > 0) {
     stop("Models were not all fitted to the same size of dataset.")
  }


  ## model names
  calls <- lapply(models, getCall)
  is.cm <- vapply(models, function(x) inherits(x, "cm"), NA)
  if ( is.null(modelnames <- model.names) ) {
    modelnames <- vapply(as.list(calls), function(x) .safeDeparse(x[[1]]), "")
  }
  if (any(substr(modelnames, 1,4) == "new(") ||
      any(duplicated(modelnames[!is.cm])) || ## <- only if S4 objects are *not* properly deparsed
      max(nchar(modelnames)) > 200) {
    warning("Failed to find model names, assigning generic names", call. = FALSE)
    modelnames <- paste0("MODEL", seq_along(modelnames))
  }
  if (length(modelnames) != length(models)) {
    stop('Vector "model.names" and model list have different lengths.', call. = FALSE)
  }
  names(models) <- sub("@env$", '', modelnames) # <- hack

  ## model stats
  llks <- lapply(models, logLik)
  npar <- vapply(models, function(x) ifelse(inherits(x, "cm"), x$npar('free'), x$rank), 0L)
  data <- lapply(calls, `[[`, "data")
  
  ## TODO: make this work with the fit_data argument?

  ## Order models by increasing number of prameters
  ii <- order(npar)
  models <- models[ii]
  llks <- llks[ii]
  npar <- npar[ii]
  calls <- lapply(models, getCall)
  aic <- if (any(nobs/npar < 40)) {
    bottomnote <- "AIC = AICc because a model has npar/nobs < 40"
    vapply(models, AICc, 1)
  } else {
    vapply(models, AIC, 1)
  }
  # data <- lapply(calls, `[[`, "data") #todo: updpate getCall
  # if ( !all(vapply(data, identical, NA, data[[1]])) ) {
  #    stop("all models must be fit to the same data object")
  #  }
  # header <- paste("Data:", abbrDeparse(data[[1]]))
  # subset <- lapply(calls, `[[`, "subset")
  # if(!all(vapply(subset, identical, NA, subset[[1]])))
  #     stop("all models must use the same subset")
  # if (!is.null(subset[[1]]))
  #     header <- c(header, paste("Subset:", abbrDeparse(subset[[1]])))
  # use AICc if n/V < 40
  llk <- unlist(llks)
  tab <- data.frame(npar = npar,
                    AIC = aic,
                    `wAIC` = cognitiveutils::akaike_weight(aic),
                    BIC = vapply(models, BIC, 1), #  "       "
                    logLik = llk,
                    deviance = -2*llk,
                    Chisq = NA,
                    Df = NA,
                    `Pr(>Chisq)` = NA,
                    row.names = 1L:length(models),
                    check.names = FALSE)

  # Check for nested models: do all models have the same name
  is.nested <- ( length( Reduce(intersect, lapply(models, function(x) x$title)) ) == 1 )
  if ( is.nested ) {
    chisq <- 2 * pmax(0, c(NA, diff(llk)))
    dfChisq <- c(NA, diff(npar))
    pChisq <- pchisq(chisq, dfChisq, lower.tail = FALSE)
    tab[, c('Chisq', 'Df', 'Pr(>Chisq)')] <- cbind(chisq, dfChisq, pChisq)
  }

  class(tab) <- c("anova.cm", "anova", class(tab))
  forms <- vapply( lapply(calls, `[[`, "formula"), .safeDeparse ,"")
  fix <- vapply( lapply(calls, `[[`, "fix"), .abbrDeparse, "" )
  fix <- vapply(fix, function(x) gsub("^list|NULL| |\\(|\\)", "", x), "")
  header <- paste("Data:", .abbrDeparse(data[[1]]))
  topnote <- paste("Model ", seq_along(models), ": ", names(models), "(", unlist(forms), "), ", unlist(fix), sep = "", collapse = "\n")
  structure(tab,
    heading = c("Analysis of Fit Table\n",
                header,
                "Models:",
                topnote),
    note = bottomnote)
}

print.anova.cm <- function(x, digits = max(getOption("digits") - 4L, 2L), ...) {
  stats:::print.anova(x, digits = digits, ...)
  args <- as.list(match.call())
  if (length(args[['signif.legend']]) | length(args[['signif.stars']])) {
    cat("---\n")
  }
  cat("Note:", attr(x, "note"), "\n")
}