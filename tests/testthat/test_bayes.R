context("bayes")
library(cogscimodels)

D <- data.frame(x = c(0,0,1,1), y = c(1,1,0,0), z = c(0,0,0,0))
DC <- as.data.frame(apply(D, 2, cumsum))
fp <- list(delta = 1, x=1, y=1, z=1, `I(1 - x)`=1, `I(1 - y)`=1)

test_that('Predicted Values for 1 row, 2 alternatives', {
  m1 <- bayes(~ x + y, data = D[1,],  fix = fp[1:3])
  m2 <- bayes(~ x + y, D[1,], fix=list(delta=1, priorpar=c(1,1)))
  m3 <- bayes_beta(~ x + y, D[1,], fix = fp[1:3])
  mc <- bayes(~ x + y, data = DC[1,], fix = fp[1:3], format = "cumulative")
  mc2 <- bayes_beta(~ x + y, data = DC[1,], fix = fp[1:3], format = "cumulative")
  expect_equal(m1$predict("mean"), cbind(pr_x=c(.5), pr_y=c(.5)))
  expect_equal(m1$predict("mean"), m2$predict("mean"))
  expect_equal(m1$predict("mean"), m3$predict("mean"))
  expect_equal(m1$predict("mean"), mc$predict("mean"))
  expect_equal(m1$predict("mean"), mc2$predict("mean"))
  expect_equal(m1$predict("max"), cbind(pr_x=c(NaN), pr_y=c(NaN)))
  expect_equal(m1$predict("max"), m2$predict("max"))
  expect_equal(m1$predict("max"), m3$predict("max"))
  expect_equal(m1$predict("max"), mc$predict("max"))
  expect_equal(m1$predict("max"), mc2$predict("max"))
})


test_that('Predicted Values for 2 Alternatives', {
  m1 <- bayes(~ x + y, data = D,  fix = fp[1:3])
  m2 <- bayes(~ x + y, D, fix=list(delta=1, priorpar=c(1,1)))
  m3 <- bayes_beta(~ x + y, D, fix = fp[1:3])
  mc <- bayes(~ x + y, data = DC, fix = fp[1:3], format = "cumulative")
  mc2 <- bayes_beta(~ x + y, data = DC, fix = fp[1:3], format = "cumulative")
  mcc <- bayes(~ x + y, data = DC, fix = fp[1:3], format = "count")
  expect_equal(m1$predict("mean"), cbind(pr_x=c(.5,1/3,1/4,2/5), pr_y=c(.5,2/3,3/4,3/5)))
  expect_equal(m1$predict("mean"), m2$predict("mean"))
  expect_equal(m1$predict("mean"), m3$predict("mean"))
  expect_equal(m1$predict("mean"), mc$predict("mean"))
  expect_equal(m1$predict("mean"), mc2$predict("mean"))
  expect_equal(m1$predict("mean")[-1,], mcc$predict("mean")[-4,])
  expect_equal(m1$predict("max"), cbind(pr_x=c(NA,0,0,1/3),pr_y=c(NA,1,1,2/3)))
  expect_equal(m1$predict("max"), m2$predict("max"))
  expect_equal(m1$predict("max"), m3$predict("max"))
  expect_equal(m1$predict("max"), mc$predict("max"))
  expect_equal(m1$predict("max"), mc2$predict("max"))
  expect_equal(m1$predict("max")[-1,], mcc$predict("max")[-4,])
})

test_that("Predicted Values for 1 Alternative", {
  m1 <- bayes(~ x, data = D,  fix = fp[c(1,2,5)])
  m2 <- bayes(~ x, D, fix=list(delta=1, priorpar=c(1,1)))
  m3 <- bayes_beta(~ x, D, fix = fp[c(1,2,5)])
  expect_equivalent(m1$predict("mean"), cbind(pr_x = c(.5,1/3,1/4,2/5)))
  expect_equal(m1$predict("mean"), m2$predict("mean"))
  expect_equal(m1$predict("mean"), m3$predict("mean"))

  expect_equivalent(m1$predict("max"), cbind(pr_x = c(NA,0,0,1/3)))
  expect_equal(m1$predict("max"), m2$predict("max"))
  expect_equal(m1$predict("max"), m3$predict("max"))

})

test_that("Predicted Values for 2 Stimuli a 1 Alternative", {
  m1 <- bayes(~ x | y, data = D,  fix = fp[-4])
  m2 <- bayes(~ x | y, D, fix=list(delta=1, priorpar=c(1,1,1,1)))
  expect_equivalent(m1$predict("mean"),
    cbind(pr_x=c(.5,1/3,1/4,2/5), pr_y=c(.5,2/3,3/4,3/5)))
  expect_equal(m1$predict("mean"), m2$predict("mean"))
  
  expect_equivalent(m1$predict("max"),
    cbind(pr_x = c(NA,0,0,1/3), pr_y = c(NA,1,1,2/3)))
  expect_equal(m1$predict("max"), m2$predict("max"))
})

test_that("Predicted Values for 2 Stimuli a 1 Alternative", {
  m1 <- bayes(~ x + z | y, data = D,  fix = fp[-5])
  m2 <- bayes(~ x + z | y, D, fix=list(delta=1, priorpar=c(1,1,1,1)))
  expect_equivalent(m1$predict("mean"),
    cbind(pr_x=c(.5,.5,.5,2/3), pred_z=c(.5,.5,.5,1/3), pr_y=c(.5,2/3,3/4,.6)))
  expect_equal(m1$predict("mean"), m2$predict("mean"))
  
  expect_equal(m1$predict("max"),
    cbind(pr_x = c(NA,NA,NA,1), pr_z = c(NA,NA,NA,0), pr_y=c(NA,1,1,2/3)))
  expect_equal(m1$predict("max"), m2$predict("max"))
})


test_that('Inferred Prior distribution types', {
  m <- bayes(~ x + y, data = D, fix = fp[1:3])
  expect_equal(m$priordist, "beta-binomial")
  m <- bayes(~ x, data = D, fix = c(fp[c(1,2)], `I(1 - x)`=1))
  expect_equal(m$priordist, "beta-binomial")
  m <- bayes(~ x + y + z, data = D, fix = fp[1:4])
  expect_equal(m$priordist, "dirichlet-multinomial")
})

test_that("Error handling", {
  expect_error(bayes(~ x + y, data = D, fix = list(prior.x=1, prior.y=1)))
  expect_error(bayes(~ x + y, data = D, fix = list(prior=1)))
  expect_error(bayes(~ x + y, data = DC, format = "raw"))
  expect_error(bayes(~ x, data = DC + 2, format = "count"))
})


test_that("Learning rate 'delta'", {
  M <- bayes(~ x + y, data = D, fix = list(delta=0, x=1, y=1))
  expect_equivalent(M$predict(), matrix(0.5, 4, 2))
  M <- bayes(~ x + y, data = D, fix = list(delta=0, x=0.7, y=0.3))
  expect_equivalent(M$predict(), matrix(c(.7,.3), 4, 2, byrow = TRUE))
  M <- bayes(~ x + y, data = D, fix = list(delta=10, x=1, y=1))
  expect_equivalent(M$predict(), cbind(c(0.5,0.0833, 0.045,0.343), c(0.5,0.917,0.954,0.656)), tol=0.01)
})

test_that("Estimated parameter values", {
  D <- data.frame(x=rep(0,10), z=rep(1,10), y = c(0.5,0.3,0.25,0.19,0.16,0.14,0.12,0.11,0.1,0.08))
  D$y2 <- 1-D$y
  D$yx <- rep(0.99, 10)
  D$yz <- rep(0.01, 10)
  # Free delta
  M <- bayes(y ~ x + z, data = D, fix = list(x=1, z=1))
  expect_equivalent(M$coef(), 1, tol=0.09)
  # Free priors 
  M <- bayes(y ~ x + z, data = D, fix = list(delta=1))
  expect_equivalent(M$coef(), c(1,1), tol=0.05)
  # Both free
  M <- bayes(y ~ x + z, data = D)
  expect_equivalent(M$coef(), c(1,1,1), tol=0.07)

  # Abcolut no learning at all
  M <- bayes(y2 ~ x + z, data = D, fix = list(x=1, z=1))
  expect_equivalent(M$coef(), 0, tol=0.01)
  # all p(x) = 0.99 -> high prior on x
  M <- bayes(yx ~ x + z, data = D, fix =list(delta=1))
  expect_equivalent(M$coef(), c(1.99,0.01), tol=0.01)

  M <- bayes(yz ~ x + z, data = D, fix =list(delta=1))
  expect_equivalent(M$coef(), c(0.03,1.97), tol=0.01)

  M <- bayes(yx ~ x + z, data = D)  
  expect_equivalent(M$coef(), c(delta=0, prior.x=2, prior.z=0), tol=0.1)

  M <- bayes(yz ~ x + z, data = D)
  expect_equivalent(M$coef(), c(delta=0, prior.x=0, prior.z=2), tol=0.2)
})



# M <- bayesianm(~ x + y, data = D, fix = list(delta=5, prior.x=1, prior.y=1), mode="continuous")
# M$predict()
# M$predict("psd")
# lapply(M$predict("draws"), function(x) apply(x, 2, sd))