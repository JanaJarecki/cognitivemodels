# ==========================================================================
# Test for Cognitive Model
# Stacking of models
# ==========================================================================


# 1. Model predictions -----------------------------------------------------
test_that("Bayesian model - predictions", {
  expect_equiv <- function(data, x, target) {
    fp <- list(delta = 1, x=1, y=1, z=1, `I(1 - x)`=1, `I(1 - y)`=1)
    M <- cognitivemodel(data = data) +
      bayes_beta_d(~ x + y, fix = fp[1:3], choicerule = "none")
    expect_equivalent(M$predict(x), target)
  }
  # Two alternatives
  D <- data.frame(x = c(0,0,1,1), y = c(1,1,0,0), z = c(0,0,0,0))
  expect_equiv(data = D[1,], x = "mean", 0.50)
  expect_equiv(data = D[1,], x = "max", NaN)
  expect_equiv(data = D, x = "mean", c(.5,1/3,1/4,2/5))
  expect_equiv(data = D, x = "max", c(NA,0,0,1/3))
})


# 1. Parameter estimation -----------------------------------------------------
test_that("Parameter estimation", {
  expect_par_equal <- function(fix, target) {
    M <- cognitivemodel(data = D) +
      bayes_beta_d(y ~ x + z, fix = fix, choicerule = "none", priorsconstrained = TRUE)
    fit(M)
    expect_equal(M$coef(), target, tol=0.09)
  }

  D <- data.frame(x=rep(0,10), z=rep(1,10), y = c(0.5,0.3,0.25,0.19,0.16,0.14,0.12,0.11,0.1,0.08))
  expect_par_equal(fix = list(x=1,z=1), target = c(delta=1))
  expect_par_equal(fix = list(delta=1), target = c(x=1,z=1))
  expect_par_equal(fix = NULL,          target = c(delta=1,x=1,z=1))

  # Absolut no learning at all
  D$y <- 1-D$y
  expect_par_equal(list(x=1,z=1), c(delta=0))

  # all p(x) = 0.99 -> prior on x
  D$y <- rep(0.99, 10)
  expect_par_equal(list(delta=1), c(x=1.99,z=0.01))
  expect_par_equal(NULL, c(delta=0, x=2, z=0))
  
  # all p(x) = 0.01 -> prior on z
  # @todo If y = 0.001 the bayes_beta_c model fails to converge
  # @body The model does not converge, but it converges fine if y = 0.01. Why?
  D$y  <- rep(0.01, 10)
  expect_par_equal(fix=list(delta=1), c(x=0.01,z=1.99))
  expect_par_equal(fix=NULL, c(delta=0, x=0, z=2))
})



test_that("Bayesian + Utility - Predicted Values for 2 alternatives", {
  D <- data.frame(x = c(0,0,1,1), y = c(1,1,0,0), z = c(0,0,0,0))
  DC <- as.data.frame(apply(D, 2, cumsum)) # cumulative data
  fp <- list(delta = 1, x=1, y=1, z=1, `I(1 - x)`=1, `I(1 - y)`=1)
  m1 <- cognitivemodel(data = D) +
    bayes_beta_c(~ x + y,fix = list(delta=1,x=1,y=1,sigma=0), priorsconstrained=TRUE) +
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



test_that("Bayesian + Utility - Parameter estimation", {
  expect_par_equal <- function(fix, target) {
    M <- cognitivemodel(data = D) +
      bayes_beta_d( ~ x + z, fix = fix, choicerule="none", priorsconstrained = TRUE) +
      utility_pow_c(y ~ pr_x, fix = list(rn=NA,sigma=0.0001))
    fit(M, options = list(solver = "solnp"))
    expect_equal(M$coef(), target, tol=0.09)
  }
  D <- data.frame(x = c(1,1,0,0), z = c(0,1,1,1), y = c(.7,.7,0,0))
  expect_par_equal(fix = c(delta = 1), target = c(x = 2, z = 0, rp = 20))
})



# test_that("csm - prediced value identities", {
#   D <- data.frame(x1 = 1, x2 = 2, obsx = c(0,0,0), y1=1, y2=0, obsy = c(1,0,1), y=1)
#   cpar <- c(alpha=0.88, beta=0.88, lambda=2.25, gammap=0.61, gamman=0.69)
#   bpar <- list(delta=1, priorpar = c(1,1,1,1))

#   M <- cogscimodel(data = D) +
#     bayes_beta(~ obsx | obsy, fix = bpar)
#   expect_equal(M$predict(), cbind(pr_obsx = c(0.5,0.33,0.25), pr_obsy = c(0.5, 0.66,.50)), tol = 0.01)

#   M <- cogscimodel(data = D) +
#     bayes_beta(~ obsx | obsy, fix = bpar) +
#     softmax(~ pr_obsx | pr_obsy, fix = c(tau = 1))
#   expect_equal(M$predict(), cbind(pred_pr_obsx = c(0.5,0.42,0.43), pred_pr_obsy = c(0.5, 0.58,.56)), tol = 0.01)

#   M <- cogscimodel(data = D) +
#     bayes_beta(~ obsx | obsy, fix = bpar) +
#     cpt(~ x1 + pr_obsx + x2 | y1 + pr_obsy + y2, ref = 0L, fix = cpar)
#   expect_equal(M$predict(), cbind(pr_x = c(1.35,1.43,1.46), pr_y = c(0.42,0.51,0.42)), tol = 0.01)

#   M <- cogscimodel(data = D) +
#     bayes_beta(~ obsx | obsy, fix = bpar) +
#     cpt(~ x1 + pr_obsx + x2 | y1 + pr_obsy + y2, ref = 0L, fix = cpar) +
#     luce(~ pr_x | pr_y)
#   expect_equal(M$predict(), cbind(pred_pr_x = c(0.76,0.73,0.77), pred_pr_y = c(0.23,0.26,0.22)), tol = 0.01)
# })

# test_that("csm - baseline", {
#   D <- data.frame(x1 = 1, x2 = 2, obsx = c(0,0,0), y1=1, y2=0, obsy = c(1,0,1), y=1)
#   M <- cogscimodel(data = D) +
#     baseline_const(obsy ~ ., const = 0.5, mode = "discrete")
#   expect_equivalent(M$predict(), rep(0.5,3))
# })

# test_that("csm - ebm", {
#   data(nosofsky1989)
#   D <- nosofsky1989[nosofsky1989$condition == "angle", ]
#   anglepar <- c(lambda=3.20, angle=.98, size=.02, b0=.43, b1=.57,r=2,q=2)
#   target <- 1L - c(94,56,19,01,96,62,23,03,92,55,14,01,98,56,13,01) / 100
#   M <- cogscimodel(data = D) +
#     gcm(pobs ~ angle + size, criterion = ~true_cat, fix = anglepar)
#   expect_equivalent(M$predict(newdata = D), target, tol = 0.01)

#   M <- cogscimodel(data = D) +
#     gcm(pobs ~ angle + size, criterion = ~true_cat, fix = c(r=2,q=2), options = list(fit_data = D, fit_n = D$N)) 
#   npar(M, "free")

#   fit(M)
#   M$fit()
#   M

#   M$parspace
#   M$options
#   MM <- gcm(pobs ~ angle + size, ~true_cat, data = D, fix = c(r=2,q=2), options = list(fit_data = D, fit_n = D$N))
#   MM
#   MM$predict(newdata = D)

#  M$predict(newdata = D)
#   expect_equivalent(M$predict(), rep(0.5,3))
# })