#' Class for risk-sensitive foraging environments
#'
#' @param budget Numeric, minimum requirement.
#' @param n.trials  Numeric, number of trials.
#' @param initial.state Numeric, starting state.
#' @param ... Numeric matrix (n x 2) with probabilities in column one and outcomes in columnn 2.
#' @param terminal.fitness Function that specifies the terminal reward (what you get at the end of n.trials), defaults to "get zero if below budget and 1 otherwise". See details.
#' @details The argument \code{terminal.fitness} must be a function with exactly two arguments \coge{budget} and \code{state}, that returns for each budget and state the terminal reward.
#' @export 
rsenvironment <- function(budget, ..., n.trials, initial.state, terminal.fitness = function(state, budget) { as.numeric(state >= budget) } ) {
  Rsenvironment$new(n.trials = n.trials, terminal.fitness, budget = budget, initial.state = initial.state, ...)
}

Rsenvironment <- R6Class("rsenvironment",
    public = list(
        environment = NULL,
        n.trials = NULL,
        n.actions = NULL,
        n.states = NULL,
        actions = NULL,
        trials = NULL,
        states = NULL,
        vars = NULL,
        evs = NULL,
        terminal.fitness = NULL,
        budget = NULL,
        initial.state = 0,
        stateTrialMat = NULL,
        absorbing.states = NULL,
        T = NULL,
        initialize = function(n.trials, terminal.fitness, budget, initial.state, absorbing.states = function(x) { return(cbind(x, -1)) }, ...) {
          tmp = list(...)
          self$environment      <- self$checkenvironment(tmp)
          self$budget           <- budget
          self$initial.state    <- initial.state
          self$terminal.fitness <- terminal.fitness
          self$absorbing.states <- absorbing.states
          self$n.actions        <- dim(self$environment)[3]
          actions               <- dimnames(self$environment)[[3]]
          if (is.null(actions)) {
            actions <- seq_len(self$n.actions)
          }
          self$actions          <- actions
          self$n.actions        <- length(actions)
          self$trials           <- seq_len(n.trials)
          self$n.trials         <- n.trials
          
          self$vars <- apply(self$environment, 3, function(z) varG(z[,1],z[,2]))
          self$evs <- apply(self$environment, 3, function(z) z[,1] %*% z[,2])
         
          self$makeStateTrialMat() # call before makeTransitionMatrix()!
          self$states <- c(na.omit(unique(c(self$stateTrialMat))))
          self$n.states <- length(self$states)
          self$makeTransitionMatrix()
        },
        checkenvironment = function(x) {
          if(is.list(x)) {
            nr <- max(unlist(lapply(x, nrow)))
            nc <- max(unlist(lapply(x, ncol)))
            x <- array(unlist(x), dim = c(nr, nc, length(x)), dimnames = list(NULL, NULL, names(x)))
           }
           if (dim(x)[3] != 2) {
             stop("Environment must contain two options, but has ", dim(x)[3], ".")
           }
           if (isTRUE( all.equal(apply(x[, 1, , drop = F], 3, colSums), rep(1, dim(x)[2]) )) ) {
            stop("Probabilities in the environment must sum to 1, but sums are ", paste(tmp2, sep = ",", collapse = ", "), ".")
           }
           x[, 2, ] <- x[, 2, ] * as.numeric(x[, 1, ] > 0)
           if (any(apply(x, 3, function(z) any(duplicated(z[z[,1]>0, 2]))))) {
            stop("One of the options has the same outcome twice, outcomes cannot be duplicated,\nthe options are: ", paste(x, collapse = ", "))
           }
           return(x)
        },
        makeStateTrialMat = function() {
          outcomes <- c(unique(self$environment[, 2, ][self$environment[, 1, ] > 0]))
          ans <- list(self$initial.state)
          for (i in self$trials + 1) {
              ans[[i]] <- unique(self$absorbing.states(c(outer(ans[[i-1]], outcomes, `+`)))[,1])
          }
          ans <- lapply(ans, sort)        
          ans <- sapply(ans, "[", i = seq_len(max(sapply(ans, length))))
          colnames(ans) <- rev(seq_len(ncol(ans))-1)
          self$stateTrialMat <- ans
        },
        makeTransitionMatrix = function() {
          states <- self$states
          T <- array(0, dim = c(length(states), length(states), self$n.trials, self$n.actions), dimnames = list(states, states, self$trials, self$actions))
          for(i in self$actions) {
            for(t in self$trials) {
              for(s1 in c(na.omit(self$stateTrialMat[, t]))) {
                for(s2 in list(outer(s1, self$environment[, 2, i][self$environment[, 1, i] > 0], `+`))) {
                  s1 <- match(s1, dimnames(T)[[1]])
                  s2 <- match(self$absorbing.states(c(s2))[,1], dimnames(T)[[2]])
                  T[s1, s2, t, i] <- self$environment[, 1, i][self$environment[, 1, i] > 0] + T[s1, s2, t, i]
                }
              }
            }
          }
          for (t in self$trials) {
            absorbing <- self$absorbing.states(self$states)
            s1 <- which(absorbing[, 2] > 0)
            s2 <- match(absorbing[s1, 1], self$states)
            T[s1, ,t , ] <- 0
            T[s1, s2, t, ] <- 1
          }
          self$T <- T
        },
        makeStateTrialActionMat = function(x = NA, states, trials) {
          if (missing(trials)) {
            trials <- self$trials
            timehorizon <- pmatch(trials, colnames(self$stateTrialMat))
          }
          if (missing(states)) {
            states <- as.vector(na.omit(unique(c(self$stateTrialMat))))
          }
          return(array(x, dim = c(length(states), length(timehorizon), self$n.actions), list(states, timehorizon, self$actions)))
        },
        policyEV = function(policy, s0 = self$initial.state, t0 = 1) {
          ev <- matrix(1) # initialize
          tr <- t0 # initialize
          while (tr <= self$n.trials) {
            T <- self$T[, , tr, ]
            if (tr == t0) { 
              # set transition pr to 0 if they do not start at s0
              T[-match(s0, dimnames(T)[[1]]), , ] <- 0
            }
            P <- policy[, tr, , drop = FALSE]
            P <- array(rep(P, each = dim(T)[[1]]), dim = dim(T), dimnames = dimnames(T))
            TxP <- T * aperm(P, c(2,1,3))
            ev <- colSums(TxP * rowSums(ev))
            tr <- tr + 1
          }
          ev <- rowSums(ev)
          fitness <- self$terminal.fitness(budget = self$budget, state = as.numeric(dimnames(T)[[1]]))
          ev <- ev %*% fitness
          return(ev)
        },
        policyPS = function(policy, s0 = self$initial.state, t0 = 1) {
          ps <- matrix(1)
          tr <- t0 # initialize
          while (tr <= self$n.trials) {
            T <- self$T[, , tr, ]
            if (tr == t0) { 
              T[-match(s0, dimnames(T)[[1]]), , ] <- 0
            }
            P <- T
            P[] <- rep(policy[, tr, , drop = FALSE], each = dim(T)[[1]])
            TxP <- T * aperm(P, c(2,1,3))
            ps <- cbind(ps[, 1:tr], rowSums(colSums(TxP * ps[, tr])))
            tr <- tr + 1
          }          
          ps[-match(s0, dimnames(T)[[1]]), 1] <- 0
          return(ps)
        },
        makePolicy = function(type) {
          type = match.arg(type, c("random", self$actions, "ev"))
          pr <- rep(0, self$n.actions)
          if (type == "random") {
            pr <- (pr + 1) / self$n.actions
          } else if (type == "ev") {
            pr <- cogsciutils::choicerule(t(self$evs), "argmax")
          } else {
            pr[match(type, self$actions)] <- 1
          }
          nrows <- self$n.trials * dim(self$T)[1]
          policy <- array(c(rep(pr, each = nrows)), dim = c(dim(self$T)[1], self$n.trials, self$n.actions), dimnames = list(dimnames(self$T)[[1]], self$trials, self$actions))
          return(policy)
        },
        makeData = function(colnames) {
          return(melt(self$stateTrialMat[, self$trials, drop = F], na.rm = TRUE, varnames = c(NA, colnames[1]), value.name = colnames[2])[, -1])
        },
        print = function() {
          cat("rsenvironment",
          "\nReach", self$budget, "in", self$n.trials, "trials with options:\n",
          unlist(sapply(seq_len(dim(self$environment)[3]), function(x) paste("  ", dimnames(self$environment)[[3]][x], paste0("(", apply(self$environment[, , x], 1, function(y) paste0(rev(y), collapse = ", ")), ")", collapse = " or "), "\n"))),
          "\nInitial state of", self$initial.state,
          "\n"
        )
      })
    )




print.rsenvironment <- function(obj) {
  obj$print()
}

plot.rsenvironment <- function(obj) {
  budgets <- obj$budget
  n.trials <- obj$n.trials
  outcomes <- as.data.table(dcast(melt(obj$environment), Var3 + Var1 ~ ...))
  setnames(outcomes, c("Option", "Nr", "Pr", "Value"))
  outcomes <- outcomes[Pr>0]

  outcomes[, Var := obj$var()[Option]]
  outcomes[, Option.label := paste0(Option, ": +", min(Value), ", ", percent(Pr[which.min(Value)]), ", +", max(Value), "   |  EV = ", Pr %*% Value ,", SD = ", round(sqrt(Var),2)), by = Option]
  outcomes[, Option.label := factor(Option.label, levels = Option.label[c(which.max(Var), which.min(Var))])]  

  minmax <- data.table(Trial=1:(n.trials+1), ymin = outcomes[, min(Value)] * 0:(n.trials), ymax = outcomes[, max(Value)] * 0:(n.trials))

  

  Points <- sapply(seq_len(n.trials+1) - 1, function(x) outcomes[, max(Value[Var==max(Var)])] * x + outcomes[, min(Value[Var==min(Var)])] * (n.trials-x))
  Points <- sapply(budgets, function(b) c(max(Points[which.max(Points >= b)]), which.max(Points >= b)))
  Points <- data.table(t(Points))[V2>1 & !(V2 == max(V2) & V1 == max(budgets))]
  setnames(Points, c("end.value", "trial"))
  Points[, start.value := (trial-1) * outcomes[, max(Value[Var==max(Var)])]]

  lt2 <- 6
  sz <- 1
  adj <- max(minmax$ymax)/40
  y.max <- max(c(budgets, minmax$ymax))

  p <- ggplot(data.frame(Trial=1:n.trials), aes(x=Trial)) +
    geom_hline(yintercept = budgets, linetype = 1, size = 1) +
    geom_ribbon(data=minmax, aes(ymin=ymin,ymax=ymax,fill="Possible points"), alpha= .77)  +geom_hline(yintercept = budgets, linetype = 1) +
    geom_abline(data = outcomes, aes(slope = Value, color = Option.label, intercept = -Value), size = sz) +
   scale_x_continuous(limits=c(1,n.trials+1), breaks = seq_len(n.trials+1), labels = c(1:n.trials, "End"), expand=c(0,0)) +
   scale_y_continuous("Point total", limits = c(0, y.max + 1), breaks = 0:y.max, expand = c(0,0)) +
  theme(panel.grid.major = element_line(color="grey90"), legend.position = c(.04,.9), legend.direction = "vertical", legend.justification = "left", legend.box.background = element_rect(color = 'black'), legend.box.margin = margin(.5, .5, .5, .5, 'lines')) +
  scale_color_manual("Options", values=c("#d8b365", "#5ab4ac")) +
  scale_fill_manual("", values=c("#f5f5f5")) +
  guides(fill = guide_legend(override.aes = list(color = "#d8b365", alpha = 1, size = sz, shape = 16, geom = "point"), order = 2, geom = "point"), color = guide_legend(order=1, override.aes = list(linetype=1)), linetype = NA) +
  geom_text(data = data.frame(b = budgets, t = 1), aes(x=t+.05, y = b, label=paste("Budget", b)), nudge_y = adj, hjust = 0, family = 'Roboto', size = 2.5, fontface = 'italic') +
  ggtitle("Earnings Budgets for the Options in the Environment")
  # if (nrow(Points) > 0) {
  #   p <- p +
  #   geom_segment(data=Points, aes(x = trial, xend = n.trials+1, y = start.value, yend=end.value), color = "#5ab4ac", linetype=lt2, size = sz) +
  #   geom_point(data=Points, aes(x=trial, y=start.value), size = 3, shape = 21, fill = "#5ab4ac", color = "white") +
  #   geom_text(data=Points, aes(x=trial, y=start.value), label = LETTERS[1:nrow(Points)], nudge_y = .8, nudge_x = -.1, size = 5)
  # }

  return(p)

}