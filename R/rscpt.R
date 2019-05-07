#' Risk-sensitive foraging cognitive prospect theory
#' #' @inheritParams Cogscimodel rscpt
rscpt <- function(env, formula = NULL, fixed = NULL, data = NULL, choicerule, ref) {
    obj <- Rscpt$new(env = env, data = data, formula = formula, fixed = fixed, choicerule = choicerule, ref = ref)
    return(obj)
}

Rscpt <- R6Class("rscpt",
  inherit = cognitiveModel,
  public = list(
    env = "rsftenvironment",
    V = NULL,
    EV = NULL,
    ref = NULL,
    initialize = function(env, ref, formula = NULL, fixed = NULL, data = NULL, choicerule) {
      if (is.null(formula)) {
        formula <- ~ timehorizon + state
      } 
      if (is.null(data)) {
        time_state_names <- attr(terms(formula), 'term.labels')
        data <- env$makeData(time_state_names)
      }
      allowedparm = matrix(NA, nc = 3, dimnames = list(NULL, c('ll', 'ul', 'init')))

      super$initialize(formula = formula, data = data, allowedparm = allowedparm, fixed = fixed, choicerule =  choicerule, model = "RS CPT", discount = 0)

      self$env <- env
      self$ref <- ref
      self$makeValueMat()
      self$makeEVMat()

      },
    predict = function(type = c("choices", "values", "ev", "pstates"), action = NULL, newdata = NULL) {
      type = match.arg(type)

        if (is.null(action)) {
          action <- ifelse(self$env$n.actions == 2, 1, self$env$actions)
        }
        if (!is.null(newdata)) {
          data <- get_all_vars(self$formula, newdata)
        } else {
          data <- self$input
        }

        trials <- data[, 1]
        states <- data[, 2]
        rows <- match(states, self$env$states)
        cols <- match(trials, rev(self$env$trials))
        acts <- rep(seq_len(self$env$n.actions), each = length(trials))

        v <- self$V[cbind(rows, cols, acts)]
        v <- matrix(v, ncol = self$env$n.actions, dimnames = list(NULL, self$env$actions))

        if (type == "choices") {
          cc <- super$applychoicerule(v)
        } else if (type == "ev") {
          ev <- self$EV[cbind(rows, cols, 1)]
        } else if (type == "pstates") {
          ps <- self$V
          ps[] <- super$applychoicerule(matrix(ps, nc = self$env$n.actions))
          ps[is.na(ps)] <- 0L
          ps <- self$env$policyPS(ps)[cbind(rows, cols)]
        }

        switch(type,
          choices = cc[, action],
          values = v[, action],
          pstates = ps,
          ev = ev)
    },
    makeData = function(type = c("budget", "trial budget", "total outcomes")) {
      E <- self$env$environment
      timehorizon <- self$input[, 1]
      n <- length(timehorizon)
      states <- self$input[, 2]
      budget <- rep(self$env$budget, n)
      type = match.arg(type)

      X <- as.data.frame(matrix(replicate(n, E[,c(2,1),]), nrow = n, byrow = T))

      xcolid <- c(sapply(1:dim(E)[1], function(x) seq(x, ncol(X), length(E[,,1]))))

      X$budget <- budget

      if (type == "budget") {
        X[, xcolid] <- X[, xcolid] + states
        return(X)
      } else if (type == "trial budget") {
        X$budget <- (X$budget - states) / timehorizon
        return(X)
      } else if (type == "total outcomes") {
        X[, xcolid] <- states + X[, xcolid] * timehorizon
        return(X)
      }
    },
    makeValueMat = function() {
      E <- self$env$environment

      fml <- sapply(1:dim(E)[3], function(x) {
        i <- dim(E)[2] * (x-1) + 1
        i <- length(E[,,1]) * (x-1) + 1
        j <- length(E[,,1]) * x
        paste0("V", i:j, collapse = "+")
      })
      fml <- paste("~", paste(fml, collapse = "|"))

      X <- self$makeData(self$ref)

      args <- list(
        formula = as.formula(fml),
        ref = X[, "budget"],
        nopt = self$env$n.actions,
        nout = dim(E)[2],
        choicerule = self$choicerule,
        fixed = c(alpha = 0.88, beta = 0.88, lambda = 2.25, gammap = 0.61, gamman = 0.69),
        data = X
        )
      mod <- do.call(cpt, args)
      out <- mod$predict('value', 1:2)
      
      tt <- as.character(self$input[, 1])
      ss <- as.character(self$input[, 2])
      rownames(out) <- paste(ss, tt)
      colnames(out) <- self$env$actions

      V <- self$env$makeStateTrialActionMat()
      for (i in 1:length(tt)) {
        s <- ss[i]
        t <- tt[i]
        V[s, t, ] <- c(out[paste(s,t), ])
      }
      self$V <- V
    },
    makeEVMat = function() {
      EV <- array(NA, dim = dim(self$V)[1:2], dimnames = dimnames(self$V)[1:2])
      P <- t(apply(self$V, 1, self$applychoicerule))
      P <- array(P, dim = dim(self$V), dimnames = dimnames(self$V))
      P[is.na(P)] <- 0

      for (s in rownames(EV)) {
        for (tr in self$env$trials) {
          EV[s,tr] <- self$env$policyEV(P, s0 = as.numeric(s), t0 = as.numeric(tr))
        }
      }
      dim(EV) <- c(dim(EV), 1)
      self$EV <- EV
    }
  )
)