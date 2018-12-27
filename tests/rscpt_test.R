source("../models/rscpt/rscpt.R", chdir = TRUE)

env <- rsenvironment(
   initial.state = 5,
   n.trials = 4,
   S = matrix(c(.1, .9, 10, 4), ncol=2),
   R = matrix(c(.6, .4, 5, 4), ncol=2),
   budget = 40,
   terminal.fitness = function(budget, state) (state >= budget) * state)

mod_b <- rscpt(env = env, choicerule = 'soft', ref = 'b')
mod_tr <- rscpt(env = env, choicerule = 'soft', ref = 'tr')
mod_to <- rscpt(env = env, choicerule = 'soft', ref = 'total outcomes')
cbind(mod$input, b=mod_b$predict("v", 1:2), tr=mod_tr$predict("v", 1:2), to=mod_to$predict("v", 1:2))

om <- hm1988(env = env, choicerule = 'arg')
cbind(mod$input, mod$predict("ev", 1:2), apply(om$predict('ev', 1:2),1,max))


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