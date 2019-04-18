#' @importFrom stringr str_extract
#' @inheritParams Cogscimodel cpt
#' @export
cpt <- function(formula, nopt, nout, ref, fixed = NULL, data, choicerule, weighting = c('TK1992'), value = c('TK1992')) {
  obj <- Cpt$new(formula = formula, nopt = nopt, nout = nout, ref = ref, fixed = fixed, data = data, choicerule = choicerule, weighting = c('TK1992'), value = c('TK1992'))
  if ( length(obj$fixednames) < length(obj$parm) ) {
    obj$fit(c('solnp'))
  }
  return(obj)
}


Cpt <- R6Class("cpt",
  inherit = Cogscimodel,
  public = list(
    nopt = NULL,
    nout = NULL,
    ref = NULL,
    wfun = NULL,
    vfun = NULL,
    initialize = function(formula, nopt, nout, ref, fixed = NULl, data = NULL, choicerule = NULL, weighting = c('TK1992'), value = c('TK1992'), fit.options = list(nbest = 1)) {
      # Ranges of parameters following these studiese
      # 1. International comparison study
      # Rieger, M. O., Wang, M., & Hens, T. (2017). Estimating cumulative prospect theory parameters from an international survey. Theory and Decision, 82, 567–596. doi:10.1007/s11238-016-9582-8
      # and
      # 2. parameter stability study
      # Glöckner, A., & Pachur, T. (2012). Cognitive models of risky choice: parameter stability and predictive accuracy of prospect theory. Cognition, 123, 21–32. doi:10.1016/j.cognition.2011.12.002
      if ( any(is.na(fixed)) ) {
        stop('n cpt() ignoring parameters is not (yet) implemented.', call.=FALSE)
      }
      allowedparm <- list( #todo: check if the 'ignore' values are correct
        alpha   = c(0, 2, .8, 1),
        beta    = c(0, 2, .8, 1),
        gammap  = c(0, 2, 1, 1),
        gamman  = c(0, 2, 1, 1),
        lambda  = c(0, 10, 1.5, 1)
        )
      allowedparm <- matrix(unlist(allowedparm), ncol = 4, byrow = TRUE, dimnames= list(names(allowedparm), c('ll', 'ul', 'init', 'na')))
     
      super$initialize(formula = formula, data = data, allowedparm = allowedparm, fixed = fixed, choicerule =  choicerule, model = paste0("Cumulative prospect theory [", unique(c(weighting, value)), "]"), discount = 0, response = 'discrete', fit.options = fit.options)

      if (length(ref) == 1) {
        ref <- rep(ref, nrow(data))
      }      
      if (length(nout) == 1) {
        nout <- rep(nout, each = nopt)
      }

      self$ref  <- ref
      self$nout <- nout
      self$nopt <- nopt
      self$setWeightingFun(weighting)
      self$setValueFun(value)
    },
    predict = function(type = c("choice", "value"), action = NULL, newdata = NULL, newref = NULL) {
      type <- match.arg(type)

      if ( !is.null(newdata) ) {
        input <- model.frame(self$formula, newdata)[, -1, drop = F]
        ref <- if(length(newref) == 1) { rep(newref, nrow(input)) } else { newref }
      } else {
        input <- self$input
        ref <- self$ref
      }
       
      OptionList <- lapply(seq_along(self$nout), function(i) {
        model.matrix(Formula(self$formula), input, rhs = i)[, -1, drop = FALSE]
      })

      v <- lapply(OptionList, function(i) self$cpt(
        x = i[, seq_along(self$nout)],
        p = i[, -seq_along(self$nout)], ref = ref))
      v <- matrix(unlist(v), ncol = length(v))
      
      nn <- unlist(lapply(OptionList, function(x) unique(str_extract(dimnames(x)[[2]], pattern = "[a-z]+"))[1]))
      if (length(unique(nn)) < self$nopt) {
        nn <- paste0(nn[1], seq_len(self$nopt))
      }
      colnames(v) <- nn

      switch (type,
        choice = super$applychoicerule(v),
        value = v)
    },
    cpt = function(x, p, ref) {
      if (!is.matrix(p)) {
        p <- t(as.matrix(p))
      }
      if (!is.matrix(x)) {
        x <- t(as.matrix(x))
      }

      x <- x - ref

      args <- c(as.list(self$parm), list(p = p, x = x))      
      out <- rowSums(do.call(self$wfun, args) * do.call(self$vfun, args))
      
      return(out)
    },
    setWeightingFun = function(type) {
      if (type == "TK1992") {
        # One-parameter specification of cumulative prospect theory (?)
        wfun <- function(p, x, gammap, gamman, ...) {
          if(dim(p)[2] != 2) {
            stop("Argument p in the weighting fun in cpt must have 2 columns,\nbut has ", ncol(p), ", which is: ", head(p))
        }
        id <- as.numeric(factor(apply(cbind(p,x), 1, paste, collapse=''), ordered = TRUE))
        px <- cbind(p,x,id)
        p_unique <- p[!duplicated(id), , drop = F]
        x_unique <- x[!duplicated(id), , drop = F]
        out <- t(sapply(1:nrow(p_unique), function(.rowid, x, p) {
          x <- x[.rowid, ]
          p <- p[.rowid, ]
          pn <- p[x < 0]
          pp <- p[x >= 0]
          # Store the order
          norder <- order(x[x <  0])
          porder <- order(x[x >= 0], decreasing = TRUE)
          # Cummulative sum of the orderes probabilities
          pnord <- cumsum(pn[norder])
          ppord <- cumsum(pp[porder])
          # Weight the cummulative probabilities
          wpord <- (ppord^gammap / ( rowSums(cbind(ppord, 1-ppord)^gammap) )^(1/gammap))
          wnord <- (pnord^gamman / ( rowSums(cbind(pnord, 1-pnord)^gamman) )^(1/gamman))
          # Subtract the weights
          if (sum(x < 0) > 1) {
            wnord <- wnord - c(0, head(wnord, -1))
          }
          if (sum(x >= 0) > 1) {
            wpord <- wpord - c(0, head(wpord, -1))
          }
          # Reorder weights to the original order
          wp <- wpord[order(porder)]
          wn <- wnord[order(norder)]
          out <- x
          out[x >= 0] <- wp
          out[x <  0] <- wn
          return(out)
          }, x = x_unique, p = p_unique))
        return(out[id,])
        }
      }
      self$wfun <- wfun
    },
    setValueFun = function(type) {
        if (type == "TK1992") {
          vfun <- function(x, alpha, beta, lambda, ...) {
            out <- x^alpha
            out <- replace(out, x < 0, (-lambda * (-x)^beta )[x<0])
            return(out)
          }
        } 
        self$vfun <- vfun
    }
  )
)