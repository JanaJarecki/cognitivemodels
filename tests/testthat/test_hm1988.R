context("hm1988")
library(cogscimodels)

# Houston, A. I., & McNamara, J. M. (1988). A framework for the functional analysis of behaviour. Behavioural and Brain Science, 11, 117–163. doi:10.1017/S0140525X00053061
# p. 118 - 119
test_that('Values of hm1988', {
  env <- rsenvironment(
    budget = 12,
    n.trials = 4,
    initial.state = 11,
    a1 = matrix(c(0.1, 0.8, 0.1, 0, 1, 2), nc = 2),
    a2 = matrix(c(0.4, 0.2, 0.4, 0, 1, 2), nc = 2))
  val11 <- cbind(a1=c(0.9, 0.99, 0.999, 0.9999),
                 a2=c(0.6, 0.96, 0.996, 0.9996)) # Table 1
  nd <-  data.frame(timehorizon = 1:4, state = 11)
  mod <- hm1988(env = env, choicerule = 'arg')
  expect_equal(mod$predict('v', newdata = nd, 1:2), val11)
  expect_equal(mod$predict('r', newdata = nd), rep(1,4))

  env <- rsenvironment(
    budget = 12,
    n.trials = 4,
    initial.state = 10,
    a1 = matrix(c(0.1, 0.8, 0.1, 0, 1, 2), nc = 2),
    a2 = matrix(c(0.4, 0.2, 0.4, 0, 1, 2), nc = 2))
  mod <- hm1988(env = env, choicerule = 'arg')
  val10 <- cbind(a1=c(0.1, 0.86, 0.978, 0.997),
                 a2=c(0.4, 0.74, 0.942, 0.991)) # Table 1
  nd$state <- 10
  expect_equal(mod$predict('v', newdata = nd, 1:2), val10)
  expect_equal(mod$predict('r', newdata = nd), c(0, rep(1,3)))

  env <- rsenvironment(
    budget = 12,
    n.trials = 4,
    initial.state = 9,
    a1 = matrix(c(0.1, 0.8, 0.1, 0, 1, 2), nc = 2),
    a2 = matrix(c(0.4, 0.2, 0.4, 0, 1, 2), nc = 2))
  mod <- hm1988(env = env, choicerule = 'arg')
  val9 <- cbind(a1=c(0, 0.41, 0.831, 0.9654),
                a2=c(0, 0.44, 0.744, 0.9276)) # Table 1
  nd$state <- 9
  expect_equal(mod$predict('v', newdata =nd, 1:2), val9)
  expect_equal(mod$predict('r', newdata = nd), c(0.5, 0, rep(1,2)))
})

# # Searcy, G. D., & Pietras, C. J. (2011). Optimal risky choice in humans: effects of amount of variability. Behavioural Processes, 87, 88–99. doi:10.1016/j.beproc.2011.01.008
# # Experiment 1
# budget <- 28 # was 28 and 36
# env <- rsenvironment(budget = budget, n.trials = 5, initial.state = 0, LV = matrix(c(0.5, 0.5, 4, 8), nc = 2), HV = matrix(c(.5, .5, 0, 12), nc = 2), terminal.fitness = function(state, budget) (state >= budget) * state)
# model <- hm1988(environment = env, formula = y ~ timehorizon + state)
# round(cbind(model$data, predict(model, "v", 1:2)), 3)

# # Experiment 2: fixed instead of low-variance option
# budget <- 48 # was 28 and 48
# env <- rsenvironment(budget = budget, n.trials = 5, initial.state = 0, LV = matrix(c(1, 0, 6, 0), nc = 2), HV = matrix(c(.5, .5, 0, 12), nc = 2), terminal.fitness = function(state, budget) (state >= budget) * state)
# model <- hm1988(environment = env, formula = y ~ timehorizon + state)
# round(cbind(model$data, predict(model, "v", 1:2)), 1)
# # There seems to be an error in Table A.2. Experiment 2 on p. 99 of the paper
# # Since in trial 3 one can onle have accumulated 0, 6, 12, 18, 24 earnings the max is 2 x 12 = 24.
# # but the paper says trial 3 accumulated earnings 30

# # Pietras, C. J., Searcy, G. D., Huitema, B. E., & Brandt, A. E. (2008). Effects of monetary reserves and rate of gain on human risky choice under budget constraints, 78, 358–373. doi:10.1016/j.beproc.2008.01.016
# initial <- c(pos = 9, neg.rate = 9, neg.reserves = 3, neg.rate.reserves = 3, pos.rate.reserves = 10)
# HVoptions <- list(pos = c(.5, .5, 0, 14), neg.rate = c(.5, .5, 0, 12), neg.reserves = c(.5, .5, 0, 14), neg.rate.reserves = c(.5, .5, 0, 12), pos.rate.reserves = c(.5, .5, 0, 20))
# LVoptions <- sapply(HVoptions, function(z) mean(z[3:4]))
# cond <- "neg.rate.reserves"

# env <- rsenvironment(budget = 40, n.trials = 5, initial.state = initial[cond], Fixed = matrix(c(1, 0, LVoptions[[cond]], 0), nc = 2), Variable = matrix(HVoptions[[cond]], nc = 2), terminal.fitness = function(state, budget) (state >= budget) * state)
# model <- hm1988(environment = env, formula = y ~ timehorizon + state)
# res <- round(cbind(model$data, predict(model, "v", 1:2)), 1)
# res <- res[res[, 3] != res[, 4], ]
# cbind(res[, 1:2], x = names(res)[3:4][apply(res[, 3:4], 1, which.max)], res[, 3:4])


# # 
# env <- rsenvironment(budget = 28, n.trials = 5, initial.state = 0, LV = matrix(c(.5, .5, 8, 4), nc = 2), HV = matrix(c(.5, .5, 0, 12), nc = 2), terminal.fitness = function(state, budget) (state >= budget) * state)
# model <- hm1988(environment = env, formula = y ~ timehorizon + state)
# round(cbind(model$data, predict(model, "v", 1:2)), 2)


