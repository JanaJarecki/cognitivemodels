# Houston, A. I., & McNamara, J. M. (1988). A framework for the functional analysis of behaviour. Behavioural and Brain Science, 11, 117–163. doi:10.1017/S0140525X00053061
# p. 118 - 119
test_that("Values of hm1988", {
  D11 <- D10 <- D9 <- data.frame(
    a1   = 0,    a2 = 1,    a3 = 2,
    pa11 = 0.1, pa12 = 0.8, pa13 = 0.1,
    pa21 = 0.4, pa22 = 0.2, pa23 = 0.4,
    b = 12,
    nt = 4,
    init = 11,
    s = 11,
    t = 4:1)
  D10$s <- D10$init <- 10
  D9$s <- D9$init <- 9
  # initial status = 11
  val11 <- cbind(a1pa1=c(0.9, 0.99, 0.999, 0.9999),
                 a1pa2=c(0.6, 0.96, 0.996, 0.9996)) # Table 1
  .args <- list(formula = ~ a1 + pa11 + a2 + pa12 + a3 + pa13 | a1 + pa21 + a2 + pa22 + a3 + pa23, budget = ~b, ntrials = ~nt, states = ~s, initstate = ~init, trials=~t, data = D11, choicerule = "argmax")
  M <- do.call(hm1988, .args)
  expect_equal(M$predict("value", action = 1:2), val11)
  expect_equal(M$predict("response", action = 1:2), cbind(a1pa1=rep(1,4), a1pa2=0))
  .args$data <- .args$data[1,]
  M1 <- do.call(hm1988, .args)
  expect_equal(M1$predict("value", action = 1:2), val11[1,,drop=FALSE])
  expect_equal(M1$predict("response", action = 1:2), cbind(a1pa1=1, a1pa2=0))

  # initial status = 10
  val10 <- cbind(a1pa1=c(0.1, 0.86, 0.978, 0.997),
                 a1pa2=c(0.4, 0.74, 0.942, 0.991)) # Table 1
  .args$data <- D10
  M <- do.call(hm1988, .args)
  expect_equal(M$predict("value", action = 1:2), val10)
  expect_equal(M$predict("response", action = 1:2), cbind(a1pa1=c(0,1,1,1), a1pa2=c(1,0,0,0)))
  .args$data <- .args$data[1,]
  M1 <- do.call(hm1988, .args)
  expect_equal(M1$predict("value", action = 1:2), val10[1,,drop=FALSE])
  expect_equal(M1$predict("response", action = 1:2), cbind(a1pa1=0, a1pa2=1))

  # initial status = 9
  val9 <- cbind(a1pa1=c(0, 0.41, 0.831, 0.9654),
                a1pa2=c(0, 0.44, 0.744, 0.9276)) # Table 1
  .args$data <- D9
  M <- do.call(hm1988, .args)
  expect_equal(M$predict("value", action = 1:2), val9)
  expect_equal(M$predict("response", action = 1:2), cbind(a1pa1=c(0.5,0,1,1), a1pa2=c(0.5,1,0,0)))
  .args$data <- .args$data[1,]
  M1 <- do.call(hm1988, .args)
  expect_equal(M1$predict("value", action = 1:2), val9[1,,drop=FALSE])
  expect_equal(M1$predict("response", action = 1:2), cbind(a1pa1=0.5, a1pa2=0.5))

  # multiple values / multiple environments
  D <- rbind(D11,D10,D9)
  .args$data <- D
  M <- do.call(hm1988, .args)
  expect_equal(M$predict("values", action = 1:2), rbind(val11,val10,val9))

  new_order <- sample(1:nrow(D)) # test shuffled order
  .args$data <- D[new_order,]
  M <- do.call(hm1988, .args)
  expect_equal(M$predict("values", action = 1:2), rbind(val11,val10,val9)[new_order,])
  # .args$choicerule = "softmax"
  # .args$data$y = cr_argmax(rbind(val11,val10,val9)[new_order,1])
  # .args$formula <- y ~ a1 + pa11 + a2 + pa12 + a3 + pa13 | a1 + pa21 + a2 + pa22 + a3 + pa23
  # .args$fix <- list()
  # M <-do.call(hm1988, .args)
  # M$predict()
})

test_that("Input formats of hm1988", {
  D <- data.frame(
    a1   = 0,    a2 = 1,    a3 = 2,
    pa11 = 0.1, pa12 = 0.8, pa13 = 0.1,
    pa21 = 0.4, pa22 = 0.2, pa23 = 0.4,
    s = 11,
    t = 4:1,
    init = 10,
    b = 12,
    nt = 4)
  #  full input format
  M <- hm1988(formula = ~ a1 + pa11 + a2 + pa12 + a3 + pa13 | a1 + pa21 + a2 + pa22 + a3 + pa23, budget = ~b, ntrials = ~nt, states = ~s, initstate = ~init, trials=~t, data = D, choicerule = "argmax")
  # Argument 'budget' as number
  M2 <- hm1988(formula = ~ a1 + pa11 + a2 + pa12 + a3 + pa13 | a1 + pa21 + a2 + pa22 + a3 + pa23, budget = 12, ntrials = ~nt, states = ~s, initstate = ~init, trials=~t, data = D, choicerule = "argmax")
  expect_equal(M2$predict("value", 1:2), M$predict("value", 1:2))
  # Additional argument 'ntrials' as number
  M2 <- hm1988(formula = ~ a1 + pa11 + a2 + pa12 + a3 + pa13 | a1 + pa21 + a2 + pa22 + a3 + pa23, budget = 12, ntrials = 4, states = ~s, initstate = ~init, trials=~t, data = D, choicerule = "argmax")
  expect_equal(M2$predict("value", 1:2), M$predict("value", 1:2))
  # Additional argument 'initstate' as number
  M2 <- hm1988(formula = ~ a1 + pa11 + a2 + pa12 + a3 + pa13 | a1 + pa21 + a2 + pa22 + a3 + pa23, budget = 12, ntrials = 4, states = ~s, initstate = 10, trials=~t, data = D, choicerule = "argmax")
  expect_equal(M2$predict("value", 1:2), M$predict("value", 1:2))
  # Additional argument 'states' as number
  M2 <- hm1988(formula = ~ a1 + pa11 + a2 + pa12 + a3 + pa13 | a1 + pa21 + a2 + pa22 + a3 + pa23, budget = 12, ntrials = 4, states = 11, initstate = 10, trials=~t, data = D, choicerule = "argmax")
  expect_equal(M2$predict("value", 1:2), M$predict("value", 1:2))
  # Additional argument 'states' as number
  M2 <- hm1988(formula = ~ a1 + pa11 + a2 + pa12 + a3 + pa13 | a1 + pa21 + a2 + pa22 + a3 + pa23, budget = 12, ntrials = 4, states = 11, initstate = 10, trials = 4:1, data = D, choicerule = "argmax")
  expect_equal(M2$predict("value", 1:2), M$predict("value", 1:2))
  # statess NOT supplied
  # M2 <- hm1988(formula = ~ a1 + pa11 + a2 + pa12 + a3 + pa13 | a1 + pa21 + a2 + pa22 + a3 + pa23, budget = 12, ntrials = 4, initstate = 10, data = D, choicerule = "argmax")
  # pp <- cbind(t=M2$get_timehorizons(), s=M2$get_states(), M2$predict("v", 1:2))
  # dcast(as.data.table(pp), s ~ t, value.var="a1pa2")
})


# test_that("Newdata prediction hm1988", {
#   D <- data.frame(
#     a1   = 0,    a2 = 1,    a3 = 2,
#     pa11 = 0.1, pa12 = 0.8, pa13 = 0.1,
#     pa21 = 0.4, pa22 = 0.2, pa23 = 0.4,
#     s = 11,
#     t = 4:1)
#   M <- hm1988(formula = ~ a1 + pa11 + a2 + pa12 + a3 + pa13 | a1 + pa21 + a2 + pa22 + a3 + pa23, budget = 12, ntrials = 4, states = ~s, initstate = 11, trials=~t, data = D, choicerule = "argmax")
#   expect_equal(predict(M, type = "values", 1:2), predict(M, newdata = D, type = "values", 1:2)) 
#   predict(M, newdata = rbind(D,D), type = "values", 1:2)
# })


# test_that("Choicerule in hm1988", {
#   D <- data.frame(
#     a1   = 0,    a2 = 1,    a3 = 2,
#     pa11 = 0.1, pa12 = 0.8, pa13 = 0.1,
#     pa21 = 0.4, pa22 = 0.2, pa23 = 0.4,
#     s = 11,
#     t = 4:1,
#     nt = 4,
#     choice = 1)
#   #  full input format
#   M <- hm1988(formula = choice ~ a1 + pa11 + a2 + pa12 + a3 + pa13 | a1 + pa21 + a2 + pa22 + a3 + pa23, budget = 12, ntrials = 4, states = ~s, initstate = 10, trials=~t, data = D, choicerule = "softmax", options = list(fit = T))
# })




# # Searcy, G. D., & Pietras, C. J. (2011). Optimal risky choice in humans: effects of amount of variability. Behavioural Processes, 87, 88–99. doi:10.1016/j.beproc.2011.01.008
# # Experiment 1
# budget <- 28 # was 28 and 36
# env <- rsenvironment(budget = budget, n.trialss = 5, initial.states = 0, LV = matrix(c(0.5, 0.5, 4, 8), nc = 2), HV = matrix(c(.5, .5, 0, 12), nc = 2), terminal.fitness = function(states, budget) (states >= budget) * states)
# model <- hm1988(environment = env, formula = y ~ timehorizon + states)
# round(cbind(model$data, predict(model, "v", 1:2)), 3)

# # Experiment 2: fix instead of low-variance option
# budget <- 48 # was 28 and 48
# env <- rsenvironment(budget = budget, n.trialss = 5, initial.states = 0, LV = matrix(c(1, 0, 6, 0), nc = 2), HV = matrix(c(.5, .5, 0, 12), nc = 2), terminal.fitness = function(states, budget) (states >= budget) * states)
# model <- hm1988(environment = env, formula = y ~ timehorizon + states)
# round(cbind(model$data, predict(model, "v", 1:2)), 1)
# # There seems to be an error in Table A.2. Experiment 2 on p. 99 of the paper
# # Since in trials 3 one can onle have accumulated 0, 6, 12, 18, 24 earnings the max is 2 x 12 = 24.
# # but the paper says trials 3 accumulated earnings 30

# # Pietras, C. J., Searcy, G. D., Huitema, B. E., & Brandt, A. E. (2008). Effects of monetary reserves and rate of gain on human risky choice under budget constraints, 78, 358–373. doi:10.1016/j.beproc.2008.01.016
# initial <- c(pos = 9, neg.rate = 9, neg.reserves = 3, neg.rate.reserves = 3, pos.rate.reserves = 10)
# HVoptions <- list(pos = c(.5, .5, 0, 14), neg.rate = c(.5, .5, 0, 12), neg.reserves = c(.5, .5, 0, 14), neg.rate.reserves = c(.5, .5, 0, 12), pos.rate.reserves = c(.5, .5, 0, 20))
# LVoptions <- sapply(HVoptions, function(z) mean(z[3:4]))
# cond <- "neg.rate.reserves"

# env <- rsenvironment(budget = 40, n.trialss = 5, initial.states = initial[cond], fix = matrix(c(1, 0, LVoptions[[cond]], 0), nc = 2), Variable = matrix(HVoptions[[cond]], nc = 2), terminal.fitness = function(states, budget) (states >= budget) * states)
# model <- hm1988(environment = env, formula = y ~ timehorizon + states)
# res <- round(cbind(model$data, predict(model, "v", 1:2)), 1)
# res <- res[res[, 3] != res[, 4], ]
# cbind(res[, 1:2], x = names(res)[3:4][apply(res[, 3:4], 1, which.max)], res[, 3:4])


# # 
# env <- rsenvironment(budget = 28, n.trialss = 5, initial.states = 0, LV = matrix(c(.5, .5, 8, 4), nc = 2), HV = matrix(c(.5, .5, 0, 12), nc = 2), terminal.fitness = function(states, budget) (states >= budget) * states)
# model <- hm1988(environment = env, formula = y ~ timehorizon + states)
# round(cbind(model$data, predict(model, "v", 1:2)), 2)


