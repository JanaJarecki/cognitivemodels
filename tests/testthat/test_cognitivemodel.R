test_that("Bayesian - Predicted Values for 1 row, 2 alternatives", {
  D <- data.frame(x = c(0,0,1,1), y = c(1,1,0,0), z = c(0,0,0,0))
  DC <- as.data.frame(apply(D, 2, cumsum)) # cumulative data
  fp <- list(delta = 1, x=1, y=1, z=1, `I(1 - x)`=1, `I(1 - y)`=1)
  m1 <- cognitivemodel(data = D[1,]) +
    bayes(~ x + y, fix = fp[1:3])
  expect_equivalent(m1$predict("mean"), 0.50)
  expect_equivalent(m1$predict("max"), NaN)
})


test_that("Bayesian - Predicted Values for 2 alternatives", {
  D <- data.frame(x = c(0,0,1,1), y = c(1,1,0,0), z = c(0,0,0,0))
  DC <- as.data.frame(apply(D, 2, cumsum)) # cumulative data
  fp <- list(delta = 1, x=1, y=1, z=1, `I(1 - x)`=1, `I(1 - y)`=1)
  m1 <- cognitivemodel(data = D) +
    bayes(~ x + y, fix = fp[1:3])
  expect_equal(m1$predict("mean"), c(.5,1/3,1/4,2/5))
  expect_equal(m1$predict("max"), c(NA,0,0,1/3))
})


test_that("Bayesian - Estimated parameter values", {
  expect_par_equal <- function(fix, target) {
    M <- cognitivemodel(data = D) +
      bayes_beta_d(y ~ x + z, fix = fix)
    fit(M)
    expect_equal(M$coef(), target, tol=0.09)
  }

  D <- data.frame(x=rep(0,10), z=rep(1,10), y = c(0.5,0.3,0.25,0.19,0.16,0.14,0.12,0.11,0.1,0.08))
  expect_par_equal(fix = list(x=1,z=1), target = c(delta=1))
  expect_par_equal(list(delta=1), c(x=1,z=1))
  expect_par_equal(NULL, c(delta=1,x=1,z=1))
  # Absolut no learning at all
  D$y <- 1-D$y
  expect_par_equal(list(x=1,z=1), c(delta=0))
  # all p(x) = 0.99 -> prior on x
  D$y <- rep(0.99, 10)
  expect_par_equal(list(delta=1), c(x=1.99,z=0.01))
  expect_par_equal(NULL, c(delta=0, x=2, z=0))
  # all p(x) = 0.01 -> prior on z
  D$y  <- rep(0.01, 10)
  expect_par_equal(list(delta=1), c(x=0.01,z=1.99))
  expect_par_equal(NULL, c(delta=0, x=0, z=2))
})


test_that("Bayesian + Utility - Predicted Values for 2 alternatives", {
  D <- data.frame(x = c(0,0,1,1), y = c(1,1,0,0), z = c(0,0,0,0))
  DC <- as.data.frame(apply(D, 2, cumsum)) # cumulative data
  fp <- list(delta = 1, x=1, y=1, z=1, `I(1 - x)`=1, `I(1 - y)`=1)
  m1 <- cognitivemodel(data = D) +
    bayes_beta_c(~ x + y,fix = list(delta=1,x=1,y=1)) +
    utility_pow_c(y ~ x, fix = list(rp=NA,rn=NA)) + 
    function (pred, data, par) {
      y <- data$pr_x * pred
      rp <- par["rp"]
      y <- (sign(rp) * y)^((1/rp)*(rp!=0)) * exp(y)^(rp==0)
      y / ifelse(data$x==0,1,data$x)
    }
    
  skip("skip")
  expect_equal(m1$predict("mean"), c(.5,1/3,1/4,2/5))
  expect_equal(m1$predict("max"), c(NA,0,0,1/3))
})