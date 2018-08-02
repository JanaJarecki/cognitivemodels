library(R6)
library(ggplot2)
source("functions.R")
Rsenvironment <- R6Class("rsenvironment",
    public = list(
        environment = "array",
        n.trials = "numeric",
        n.actions = "numeric",
        actions = NULL,
        trials = NULL,
        vars = NULL,
        evs = NULL,
        terminal.fitness = NULL,
        budget = NULL,
        initial.state = 0,
        stateTrialMat = NULL,
        transitionPrMat = NULL,
        initialize = function(n.trials, terminal.fitness, budget, initial.state, ...) {
          tmp = list(...)
          self$environment = self$checkenvironment(tmp)
          self$budget = budget
          self$initial.state = initial.state
          self$terminal.fitness = terminal.fitness

          self$n.actions <- dim(self$environment)[3]
          actions <- dimnames(self$environment)[[3]]
          if (is.null(actions)) {
            actions <- seq_len(self$n.actions)
          }
          self$actions = actions
          self$n.actions = length(actions)

          self$trials = seq_len(n.trials)
          self$n.trials = n.trials
          
          self$vars = apply(self$environment, 3, function(z) varG(z[,1], z[,2]))
          self$evs = apply(self$environment, 3, function(z) z[,1] %*% z[,2])
         
          self$makeStateTrialMat() # needs to be called before makeTransitionPrMat          
          self$makeTransitionPrMat()
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
           if (FALSE %in% (apply(x[, 1, ], 2, sum) == 1)) {
             stop("Probabilities in the environment must sum to 1, but sum is ", apply(x[, 1, ], 2, sum), ".")
           }
           return(x)
        },
        makeStateTrialMat = function() {
          outcomes <- c(unique(self$environment[, 2, ]))
          ans <- list(self$initial.state)
          for (i in self$trials + 1) {
              ans[[i]] <- unique(c(outer(ans[[i-1]], outcomes, `+`)))
          }
          ans <- lapply(ans, sort)        
          ans <- sapply(ans, "[", i = seq_len(max(sapply(ans, length))))
          colnames(ans) <- rev(seq_len(ncol(ans))-1)
          self$stateTrialMat <- ans
        },
        makeTransitionPrMat = function() {
          mat <- self$stateTrialMat
          x.states <- unique(c(mat[, -ncol(mat)]))
          y.states <- unique(c(mat[, -1        ]))
          x.states <- x.states[!is.na(x.states)]
          y.states <- y.states[!is.na(y.states)]
          out <- array(NA, dim = c(length(x.states), length(y.states), self$n.actions), dimnames = list(x.states, y.states, self$actions))

          for(i in seq_len(self$n.actions)) {
             for(x in x.states) {
                out[as.character(x), as.character(x + self$environment[, 2, i]), i] <- self$environment[, 1, i]
             }
          }
          self$transitionPrMat <- out
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
        print = function() {
          cat("<rsftEnvironment>",
          "\nReach", self$budget, "in", self$n.trials, "trials with options:\n",
          unlist(sapply(seq_len(dim(self$environment)[3]), function(x) paste("  ", dimnames(self$environment)[[3]][x], paste0("(", apply(self$environment[, , x], 1, function(y) paste0(rev(y), collapse = ", ")), ")", collapse = " or "), "\n"))),
          "\nInitial state of", self$initial.state,
          "\n"
        )
      })
    )

rsenvironment <- function(budget, ..., terminal.fitness = function(state, budget) { as.numeric(state >= budget) }, n.trials, initial.state) {
  Rsenvironment$new(n.trials = n.trials, terminal.fitness, budget = budget, initial.state = initial.state, ...)
}

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


