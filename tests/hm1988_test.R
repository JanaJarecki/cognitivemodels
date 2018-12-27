rm(list=ls(all=T)) # Empty workspace
source("../models/hm1988/hm1988.R", chdir = TRUE)

# Houston, A. I., & McNamara, J. M. (1988). A framework for the functional analysis of behaviour. Behavioural and Brain Science, 11, 117–163.
env <- rsenvironment(budget = 12, n.trials = 4, initial.state = 10, a1 = matrix(c(0.1, 0.8, 0.1, 0, 1, 2), nc = 2), a2 = matrix(c(0.4, 0.2, 0.4, 0, 1, 2), nc = 2))
model <- hm1988(env = env, choicerule = 'arg')
round(cbind(model$data, model$predict('ev', 1:2)), 3)

# Test with different choice rules
model_l <- hm1988(env = env, choicerule = 'luc')
model_a <- hm1988(env = env, choicerule = 'arg')
model_e <- hm1988(env = env, choicerule = 'eps', fixed = c(eps=.5))

round(cbind(model$input, model_l$predict("v", 1:2), EV_A = model_a$predict("ev"), EV_L = model_l$predict("ev"), EV_E = model_e$predict("ev"), model_a$predict("choice", 1:2), model_l$predict("choice", 1:2), model_e$predict("choice", 1:2)),2)


# Searcy, G. D., & Pietras, C. J. (2011). Optimal risky choice in humans: effects of amount of variability. Behavioural Processes, 87, 88–99. doi:10.1016/j.beproc.2011.01.008
# Experiment 1
budget <- 28 # was 28 and 36
env <- rsenvironment(budget = budget, n.trials = 5, initial.state = 0, LV = matrix(c(0.5, 0.5, 4, 8), nc = 2), HV = matrix(c(.5, .5, 0, 12), nc = 2), terminal.fitness = function(state, budget) (state >= budget) * state)
model <- hm1988(environment = env, formula = y ~ timehorizon + state)
round(cbind(model$data, predict(model, "v", 1:2)), 3)

# Experiment 2: fixed instead of low-variance option
budget <- 48 # was 28 and 48
env <- rsenvironment(budget = budget, n.trials = 5, initial.state = 0, LV = matrix(c(1, 0, 6, 0), nc = 2), HV = matrix(c(.5, .5, 0, 12), nc = 2), terminal.fitness = function(state, budget) (state >= budget) * state)
model <- hm1988(environment = env, formula = y ~ timehorizon + state)
round(cbind(model$data, predict(model, "v", 1:2)), 1)
# There seems to be an error in Table A.2. Experiment 2 on p. 99 of the paper
# Since in trial 3 one can onle have accumulated 0, 6, 12, 18, 24 earnings the max is 2 x 12 = 24.
# but the paper says trial 3 accumulated earnings 30

# Pietras, C. J., Searcy, G. D., Huitema, B. E., & Brandt, A. E. (2008). Effects of monetary reserves and rate of gain on human risky choice under budget constraints, 78, 358–373. doi:10.1016/j.beproc.2008.01.016
initial <- c(pos = 9, neg.rate = 9, neg.reserves = 3, neg.rate.reserves = 3, pos.rate.reserves = 10)
HVoptions <- list(pos = c(.5, .5, 0, 14), neg.rate = c(.5, .5, 0, 12), neg.reserves = c(.5, .5, 0, 14), neg.rate.reserves = c(.5, .5, 0, 12), pos.rate.reserves = c(.5, .5, 0, 20))
LVoptions <- sapply(HVoptions, function(z) mean(z[3:4]))
cond <- "neg.rate.reserves"

env <- rsenvironment(budget = 40, n.trials = 5, initial.state = initial[cond], Fixed = matrix(c(1, 0, LVoptions[[cond]], 0), nc = 2), Variable = matrix(HVoptions[[cond]], nc = 2), terminal.fitness = function(state, budget) (state >= budget) * state)
model <- hm1988(environment = env, formula = y ~ timehorizon + state)
res <- round(cbind(model$data, predict(model, "v", 1:2)), 1)
res <- res[res[, 3] != res[, 4], ]
cbind(res[, 1:2], x = names(res)[3:4][apply(res[, 3:4], 1, which.max)], res[, 3:4])


# 
env <- rsenvironment(budget = 28, n.trials = 5, initial.state = 0, LV = matrix(c(.5, .5, 8, 4), nc = 2), HV = matrix(c(.5, .5, 0, 12), nc = 2), terminal.fitness = function(state, budget) (state >= budget) * state)
model <- hm1988(environment = env, formula = y ~ timehorizon + state)
round(cbind(model$data, predict(model, "v", 1:2)), 2)


