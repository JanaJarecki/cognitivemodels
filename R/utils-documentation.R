# ==========================================================================
# Package: Cognitivemodels
# File: utils-documentation.R
# Author: Jana B. Jarecki
# Changed: 2019-12-13
# ==========================================================================

# ==========================================================================
# Utilities to make the documentation dynamic
# ==========================================================================



#' Helper function to document the argument 'formula'
#' @importFrom english english
#' @noRd
#' @export
.param_formula <- function(n, risky = FALSE) {
  txt <- "@param formula A [formula][stats::formula], the variables in `data` to be modeled. For example, `y ~ x1 + x2` models response y as function of one stimulus with features x1, x2."
  nm <- cumsum(n)
  n <- c(1, n+1)
  v <- if(risky == FALSE) { "x" } else { c("x", "p") }
  features <- lapply(1:length(nm), function(i) paste0(v, (n[i]:nm[i])))
  rhs <- paste(lapply(features, paste, collapse = " + "), collapse = " | ")
  txt <- sub("x1 \\+ x2", rhs, txt)
  if (risky == TRUE) {
    txt <- sub("\\.$", ", alternating outcomes `x` and probabilities `p`.", txt)
  }
  txt <- sub("x1, x2", paste(sapply(features, paste, collapse = ", "), collapse = " and "), txt)
  if (nm[1] == 1 & length(nm) == 1 || all(nm == 1:length(nm))) {
    txt <- sub("with features", "value", txt)
    if (length(nm) > 1) txt <- sub("value", "with values", txt)
  }
  if (length(nm) > 1) {
    txt <- sub("one stimulus", paste(english::english(length(nm)), "stimuli"), txt)
    txt <- sub("\\.$", " (respectively).", txt)
    txt <- paste(txt, "Lines `|` separate stimuli.")
  }
  return(txt)
}


#' Helper function to document the argument 'fix'
#' @param x The model name as string
#' @param dyn_args A vector of strings with arguments that dynamically generate parameter names, such as forumula
#' @param which A number, which parameter to use in examples
#' @param onlycr A logical, if the model only has the choice rule parameters
#' @noRd
#' @export
.param_fix <- function(x, dyn_args = NULL, which = 1, onlycr = FALSE) {
  txt <- "@param fix (optional) A list or the string `\"start\"`, the fixed model parameters, if missing all parameters are estimated."

  if (onlycr == FALSE) {
    m <- .cm_dummy_model(x)
    space <- m$parspace
    par_orig_order <- rownames(space)
    n <- nrow(space)
  } else {
    space <- make_parspace(tau = c(0, 10, 5, NA))
    par_orig_order <- c("tau", "eps")
    dyn_args = "choicerule"
    n <- 1
    txt <- sub("optional", "optional, only for `choicerule` softmax, epsilon", txt)
  }

  space <- space[c(which, (1:nrow(space))[-which]), , drop = FALSE]
  par <- rownames(space)
  any_na <- !all(is.na(space[, "na"]))

  txt <- paste0(txt, " Model parameter names are _", paste(paste0("`", par_orig_order, "`"), collapse = ", "), "_ (see details - model parameters).")

  if (length(dyn_args)) {
    dyn_args <- paste0("`", dyn_args, "`")
    txt <- sub("names are", paste("names depend on", paste(dyn_args, collapse = ", "), "and can be"), txt)
  }

  txt2 <- '\n* `list(k = 1.5)` sets parameter _`k`_ equal to 1.5.\n'
  if (n > 1) {
    txt2 <- paste(txt2,
      '* `list(k = "j")` sets parameter _`k`_ equal to parameter _`j`_ (estimates _`j`_).\n',
      '* `list(j = "k", k = 1.5)` sets parameter _`j`_ equal to parameter _`k`_ and sets _`k`_ equal to 1.5 (estimates none of the two).\n')
    txt2 <- gsub("j", par[min(which+1, n)], txt2)
  }
  if (any_na == TRUE) {
    txt2 <- paste(txt2,
      "* `list(k = NA)` omits the parameter _`k`_, if possible.\n")
  }

  # Substitute k and j and 1.5 by adequate values for a model
  txt2 <- gsub("k", par[1], txt2)
  set.seed(289)
  txt2 <- gsub("1.5", sprintf("%.2f", space[which, "start"] + runif(1) * (space[which, "ub"] - space[which, "start"])), txt2)

  txt2 <- paste(txt2, '* `"start"` sets all parameters equal to their initial values (estimates none). Useful for building a first test model.')
  
  return(paste(txt, "\n", txt2))
}



#' Returns a cognitive model without data
#' 
#' @param x A string, the model call. For example `"gcm"`, `"cpt"`, `"bayes_beta_c"`.
#' @param formula (optional) A formula.
#' @noRd
.cm_dummy_model <- function(x, formula = ~ x1, ...) {
  args <- c(list(...), list(formula = formula, options = list(fit = FALSE), choicerule = "none"))

  if (grepl("^ebm|^mem|^gcm", x)) {
    args$formula <- ~ x1 + x2
    args$data <- data.frame(x1=1, x2=1, c=0:1)
    if (x == "gcm") { args <- c(args, criterion = ~c) } else { args <- c(args, class = ~c) }
  }
  if (grepl("^bayes", x)) {
    args$formula <- ~x1 + x2
  }
  if (grepl("^cpt|^shortfall|^hm1988", x)) {
    args$formula <- ~ x1 + p1 + x2 + p2
  }
  if (grepl("^shortfall", x)) {
    args$asp <- ~ asp
  }
  if (grepl("softmax|argmax|epsilon|luce", x)) {
    args$choicerule <- NULL
  }
  
  return(
    do.call(x, args)
  )
}


#' Returns a cognitive model without data
#' 
#' @param x A string, the condition specifying when the information regarding a discrete choicerule applies.
#' @noRd
.rd_choicerules <- function() {
  return("If `choicerule = \"softmax\"`: _**`tau`**_  is the temperature or choice softness, higher values cause more equiprobable choices. If `choicerule = \"epsilon\"`: _**`eps`**_ is the error proportion, higher values cause more errors from maximizing.")
}