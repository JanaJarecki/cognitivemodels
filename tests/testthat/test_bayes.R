# Test of bayesian cognitive models

test_that('Predicted Values for 1 row, 2 alternatives', {
  D <- data.frame(x = c(0,0,1,1), y = c(1,1,0,0), z = c(0,0,0,0))
  DC <- as.data.frame(apply(D, 2, cumsum))
  fp <- list(delta = 1, x=1, y=1, z=1, `I(1 - x)`=1, `I(1 - y)`=1, sigma=1e-06)
  m1 <- bayes(~ x + y, data = D[1,],  fix = fp[c(1:3,7)], mode = "c")
  m2 <- bayes(~ x + y, D[1,], fix=list(delta=1, priorpar=c(1,1), sigma=1e-06), mode = "c")
  m3 <- bayes_beta_c(~ x + y, D[1,], fix = fp[c(1:3,7)])
  mc2 <- bayes_beta_c(~ x + y, data = DC[1,], fix = fp[c(1:3,7)], format = "cumulative")
  m1$predict("max")
  m2$predict("max")

  expect_equivalent(m1$predict("mean"), cbind(pr_x=c(.5)))
  expect_equal(m1$predict("mean"), m2$predict("mean"))
  expect_equal(m1$predict("mean"), m3$predict("mean"))
  expect_equal(m1$predict("mean"), mc2$predict("mean"))
  expect_equivalent(m1$predict("max"), cbind(pr_x=c(NaN)))
  expect_equal(m1$predict("max"), m2$predict("max"))
  expect_equal(m1$predict("max"), m3$predict("max"))
  expect_equal(m1$predict("max"), mc2$predict("max"))
})


test_that('Predicted Values for 2 alternatives', {
  D <- data.frame(x = c(0,0,1,1), y = c(1,1,0,0), z = c(0,0,0,0))
  DC <- as.data.frame(apply(D, 2, cumsum))
  fp <- list(delta = 1, x=1, y=1, z=1, `I(1 - x)`=1, `I(1 - y)`=1, sigma=1e-06)
  m1 <- bayes(~ x + y, data = D,  fix = fp[1:3], m="d", c="n")
  m2 <- bayes(~ x + y, D, fix=list(delta=1, priorpar=c(1,1)), m="d", c="n")
  m3 <- bayes_beta_c(~ x + y, D, fix = fp[c(1:3,7)])
  mc <- bayes(~ x + y, data = DC, fix = fp[c(1:3,7)], format = "cumulative", mode = "c")
  mc2 <- bayes_beta_c(~ x + y, data = DC, fix = fp[c(1:3,7)], format = "cumulative")
  mcc <- bayes(~ x + y, data = DC, fix = fp[c(1:3,7)], format = "count", mode = "c")
  expect_equal(m1$predict("mean"), c(.5,1/3,1/4,2/5))
  expect_equal(m1$predict("mean"), m2$predict("mean"))
  expect_equal(m1$predict("mean"), m3$predict("mean"))
  expect_equal(m1$predict("mean"), mc$predict("mean"))
  expect_equal(m1$predict("mean"), mc2$predict("mean"))
  expect_equal(m1$predict("mean")[-1], mcc$predict("mean")[-4])
  expect_equal(m1$predict("max"), c(NA,0,0,1/3))
  expect_equal(m1$predict("max"), m2$predict("max"))
  expect_equal(m1$predict("max"), m3$predict("max"))
  expect_equal(m1$predict("max"), mc$predict("max"))
  expect_equal(m1$predict("max"), mc2$predict("max"))
  expect_equal(m1$predict("max")[-1], mcc$predict("max")[-4])
})


test_that("Predicted values for 1 alternative are correct", {
  D <- data.frame(x = c(0,0,1,1), y = c(1,1,0,0), z = c(0,0,0,0))
  DC <- as.data.frame(apply(D, 2, cumsum))
  fp <- list(delta = 1, x=1, y=1, z=1, `I(1 - x)`=1, `I(1 - y)`=1, sigma=1e-06)
  m1 <- bayes(~ x, data = D,  fix = fp[c(1,2,5)], m="d", c="n")
  m2 <- bayes(~ x, D, fix=list(delta=1, priorpar=c(1,1)), m="d", c ="n")
  m3 <- bayes_beta_c(~ x, D, fix = fp[c(1,2,5,7)])
  expect_equivalent(m1$predict("mean"), cbind(pr_x = c(.5,1/3,1/4,2/5)))
  expect_equal(m1$predict("mean"), m2$predict("mean"))
  expect_equal(m1$predict("mean"), m3$predict("mean"))

  expect_equivalent(m1$predict("max"), cbind(pr_x = c(NA,0,0,1/3)))
  expect_equal(m1$predict("max"), m2$predict("max"))
  expect_equal(m1$predict("max"), m3$predict("max"))
})


test_that("Predicted values for 2 stimuli, 1 alternative are correct", {
  D <- data.frame(x = c(0,0,1,1), y = c(1,1,0,0), z = c(0,0,0,0))
  DC <- as.data.frame(apply(D, 2, cumsum))
  fp <- list(delta = 1, x=1, y=1, z=1, `I(1 - x)`=1, `I(1 - y)`=1, sigma = 1e-06)
  m1 <- bayes(~ x | y, data = D,  fix = fp[-4])
  m2 <- bayes(~ x | y, D, fix=list(delta=1, priorpar=c(1,1,1,1), sigma=1e-06))
  expect_equivalent(m1$predict("mean"),
    cbind(pr_x=c(.5,1/3,1/4,2/5), pr_y=c(.5,2/3,3/4,3/5)))
  expect_equal(m1$predict("mean"), m2$predict("mean"))
  expect_equivalent(m1$predict("max"),
    cbind(pr_x = c(NA,0,0,1/3), pr_y = c(NA,1,1,2/3)))
  expect_equal(m1$predict("max"), m2$predict("max"))
})

test_that("Predicted values for 2 stimuli, 1 alternative are correct", {
  D <- data.frame(x = c(0,0,1,1), y = c(1,1,0,0), z = c(0,0,0,0))
  DC <- as.data.frame(apply(D, 2, cumsum))
  fp <- list(delta = 1, x=1, y=1, z=1, `I(1 - x)`=1, `I(1 - y)`=1)
  m1 <- bayes(~ x + z | y, data = D,  fix = fp[-5], m = "d", c = "n")
  m2 <- bayes(~ x + z | y, D, fix=list(delta=1, priorpar=c(1,1,1,1)), m = "d", c = "n")
  expect_equivalent(m1$predict("mean"),
    cbind(pr_x=c(.5,.5,.5,2/3), pr_y=c(.5,2/3,3/4,.6)))
  expect_equal(m1$predict("mean"), m2$predict("mean"))
  expect_equal(m1$predict("max"),
    cbind(pr_x = c(NA,NA,NA,1), pr_y=c(NA,1,1,2/3)))
  expect_equal(m1$predict("max"), m2$predict("max"))
})


test_that('Inferred prior distribution matches the target distribution', {
  D <- data.frame(x = c(0,0,1,1), y = c(1,1,0,0), z = c(0,0,0,0))
  DC <- as.data.frame(apply(D, 2, cumsum))
  fp <- list(delta = 1, x=1, y=1, z=1, `I(1 - x)`=1, `I(1 - y)`=1)
  m <- bayes(~ x + y, data = D, fix = fp[1:3], mode = "d", c = "n")
  expect_equal(m$priordist, "beta-binomial")
  m <- bayes(~ x, data = D, fix = c(fp[c(1,2)], `I(1 - x)`=1), mode = "d", c = "n")
  expect_equal(m$priordist, "beta-binomial")
  m <- bayes(~ x + y + z, data = D, fix = fp[1:4], mode = "d", c = "n")
  expect_equal(m$priordist, "dirichlet-multinomial")
})


test_that("Error handling when input is mis-specified", {
  D <- data.frame(x = c(0,0,1,1), y = c(1,1,0,0), z = c(0,0,0,0))
  DC <- as.data.frame(apply(D, 2, cumsum))
  fp <- list(delta = 1, x=1, y=1, z=1, `I(1 - x)`=1, `I(1 - y)`=1)
  expect_error(bayes(~ x + y, data = D, fix = list(prior.x=1, prior.y=1)))
  expect_error(bayes(~ x + y, data = D, fix = list(prior=1)))
  expect_error(bayes(~ x + y, data = DC, format = "raw"))
  expect_error(bayes(~ x, data = DC + 2, format = "count"))
})


test_that("Learning rate 'delta' works as expected", {
  D <- data.frame(x = c(0,0,1,1), y = c(1,1,0,0), z = c(0,0,0,0))
  DC <- as.data.frame(apply(D, 2, cumsum))
  fp <- list(delta = 1, x=1, y=1, z=1, `I(1 - x)`=1, `I(1 - y)`=1)
  M <- bayes(~ x + y, data = D, fix = list(delta=0, x=1, y=1, sigma=1e-06))
  expect_equivalent(M$predict(), rep(0.5, 4))
  M <- bayes(~ x + y, data = D, fix = list(delta=0, x=1.7, y=0.3, sigma=1e-06))
  expect_equivalent(M$predict(), rep(0.85, 4))
  M <- bayes(~ x + y, data = D, fix = list(delta=10, x=1, y=1, sigma=1e-06))
  expect_equivalent(M$predict(), cbind(c(0.5,0.0833, 0.045,0.343)), tol=0.01)
})

test_that("Estimated parameter values match target values", {
  expect_coef_equivalent <- function(fix, target, y) {
    D <- data.frame(x=rep(0,10), z=rep(1,10),
      y = c(0.5,0.3,0.25,0.19,0.16,0.14,0.12,0.11,0.1,0.08))
    if (y==1) D$y <- 1-D$y
    if (y==2) D$y <- rep(.999, 10)
    if (y==3) D$y <- rep(.01, 10)
    M <- bayes_beta_d(y ~ x + z, data = D, fix=as.list(fix), choicerule="none")
    expect_equivalent(coef(M), target, tol=0.09)
  }

  expect_coef_equivalent(fix=c(x=1,z=1),  target=1,        y=0)
  expect_coef_equivalent(fix=c(delta=1),  target=c(1,1),   y=0)
  expect_coef_equivalent(fix=NULL,        target=c(1,1,1), y=0)

  expect_coef_equivalent(fix=c(x=1,z=1), target=0,            y=1)
  expect_coef_equivalent(fix=c(delta=1), target=c(1.99,0.01), y=2)
  expect_coef_equivalent(fix=c(delta=1), target=c(0.03,1.97), y=3)

  expect_coef_equivalent(fix=NULL, target=c(delta=0, prior.x=2, prior.z=0),y=2)
  expect_coef_equivalent(fix=NULL, target=c(delta=0, prior.x=0, prior.z=2),y=3)
})



test_that("parameter constraints from 'sum_prior' are correct.", {
  expect <- function(x, target) {    
    M <- bayes_beta_d(y ~ x + z, data = data.frame(), fix=c(delta=1), choicerule="none", prior_sum = x)
    expect_equivalent(M$constraints$rhs[2], target)
  }
  D <- data.frame(x=rep(0,10), z=1, y=1)
  expect(1L, 1L)
  expect(2L, 2L)
  expect(4L, 4L)
  expect(x = NULL, 2L)
})