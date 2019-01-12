source("../models/rsfft/rsfft.R", chdir = TRUE)
library(data.table)

probs <- cbind(rbind(c(.6,.4), c(.6,.4), c(.5,.5)), rbind(c(.1,.9), c(.7,.3), c(.2,.8)))
outcomes <- cbind(rbind(c(1,2), c(1,2), c(1,2)), rbind(c(5,6), c(5,7), c(5,6)))
budget <- c(10,10,25)
state <- c(0,0,0)
timehorizon <- c(5,5,5)

dt <- as.data.table(cbind(probs, outcomes, budget, state, timehorizon))
setnames(dt, 1:8, c(paste0('p',1:4), paste0('x',1:4)))
dt$choice <- c(1,1,1)

mod <- rsfft(choice ~ (x1 + x2 + p1 + p2) | (x3 + x4 + p3 + p4), sbt = ~ state + budget + timehorizon, nopt = 2, nout = 2, data = dt, choicerule = "eps", terminal.fitness.fun = function(state, budget) { ((state - budget) >= 0) * state}, fixed = c(maxlv= .5, minhv = 2.5, pmaxlv = 0.5, eps = .3))
mod

mod

mod$features
mod$input
mod$predict('node')


cbind(dt,
  node = mod$predict("node", action = 1:2),
  round(mod$predict("c", action = 1:2),2),
  round(mod$predict("v", action = 1:2),2))


dt
mod$input
mod$state
mod$budget
self <- mod

sourceCpp("../models/rsfft/rsfft.cpp")

terminal.fitness <- function(budget, state) { ((state - budget) >= 0) * state }


env <- rsenvironment(
   initial.state = 0,
   n.trials = 1,
   S = matrix(c(.5, .5, 4, 8), ncol=2),
   R = matrix(c(.5, .5, 0, 12), ncol=2),
   budget = 0,
   terminal.fitness = function(budget, state) (state >= budget) * state)
env
model <- rsfft(env = env, choicerule = 'soft')
predict(model, 'c', 1:2)




model_a <- rsfft(env = env, choicerule = 'arg', )
model_e <- rsfft(env = env, choicerule = 'arg', fixed = c(eps = .2))

cbind(model_a$data, model_a$predict("v", 1:2), EV_A = model_a$predict("ev"), EV_E = model_e$predict("ev"), model_a$predict("choice", 1:2), model_e$predict("choice", 1:2))

env$policyEV(env$makePolicy("random"))
env$policyEV(env$makePolicy("S"))
env$policyEV(env$makePolicy("R"))
model <- hm1988(environment = env, formula = y ~ timehorizon + state)
predict(model, "ev", 1:2)

policy <- array(t(apply(model$utilityStateTrialActionMat, 1, choicerule, type = "arg")), dim = dim(model$utilityStateTrialActionMat), dimnames = dimnames(model$utilityStateTrialActionMat))
env$policyEV(policy)
predict(model, "ev", 1:2)

env <- rsenvironment(
   initial.state = 0,
   n.trials = 1,
   S = matrix(c(1, 0, 1, 0), ncol=2),
   R = matrix(c(.5, .5, 0, 2), ncol=2),
   budget = 2,
   terminal.fitness = function(budget, state) (state >= budget))

env$policyEV(env$makePolicy("random"))
env$policyEV(env$makePolicy("S"))
env$policyEV(env$makePolicy("R"))
mean(c(0, 1 ,2, 1, 2, 3)) 





model <- rsfft(environment = env)
model$predict(type = "choice")
model$predict(type = "value")
cbind(model$input, FFT = model$predict(type = "value", 1:2))

utilityStateTrialActionMat <- model$utilityStateTrialActionMat
utilityStateTrialActionMat[is.na(env$stateTrialMat[, -5])] <- NA
choicePrMat <- env$transitionPrMat

# trial 1
  # Pr (state in t=2) = Pr (choice action | state, trial) x Pr (transition from s to s')
# trial 2
  # Pr (choice action | state, t=3) x Pr (state in t = 2)
# trial 3
  # Pr (choice action | state, t=3) x Pr (state in t = 3)
# ...
# trial T+1
  # Pr (state in t = T+1) * terminal fitness (state)

choicepr <- array(0, dim = c(4,3,2), dimnames = list(c(10,11,12,13),3:1,NULL)) # state x trial x action
choicepr[1,1,] <- c(.7,.3)
choicepr[1:2,2,] <- c(.9,.5,.1,.5)
transitions <- array(0, dim = c(4,4,2), dimnames = list(10:13,10:13,NULL))
transitions["10","11",1] <- 1
transitions["10","11",2] <- .5
transitions["10","12",2] <- .5
transitions["11","12",1] <- 1
transitions["12", "13",2] <- 1
fitness <- array(c(0,1,1,0), dim = c(4,1), dimnames = list(10:13, NULL))

init <- "10"
t <- 1
pr <- transitions[,1,1]
pr[init] <- 1

while(t <= 3) {
  pr <- rowSums(sapply(1:2, function(i) (pr * transitions[, , i])[which(pr>0), ] * choicepr[which(pr>0), t, i]
    )
  t <- t + 1
}



# t1 - t2

library("tensorA")






choicerule()
apply(aperm(model$utilityStateTrialActionMat, c(3,1,2)), 3, choicerule, type = "argmax")

apply(, 3, cogsciutils::choicerule, type = "argmax")




summary(abs(x[, 3] - x[, 4]))



model$parm

env


self <- model

round(cbind(model$data, predict(model, 'response')),4)


source('FastAndFrugalTreeEV.R', chdir = TRUE)
model2 <- FastAndFrugalTreeEV(formula = ~ state + timehorizon, environment = env, par = 1)

round(cbind(model$data, predict(model, 'response'), predict(model2, 'response')), 4 )