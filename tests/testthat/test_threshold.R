# 1. Predictins --------------------------------------------------------
test_that("Prediction identities", {
  D <- data.frame(y = rep(0:1, each=5), x = 1:10)
  M <- threshold(~ x, D, list(nu=5), mode = "d", choicerule = "n")
  expect_equivalent(M$predict(), c(pr_x = D$x-5))

  M$set_par(c(nu = 1))
  expect_equivalent(M$predict(), D$x - 1)

  M <- threshold(~ x, D, list(nu=5,tau=1), mode = "d", choicerule = "softmax")
  expect_equivalent(M$predict(), cr_softmax(D$x-5, 1))
})

# 2. Parameter fitting ---------------------------------------------------
test_that("Parameter fitting", {
  D <- data.frame(y = rep(0:1, each=5), x = 1:10)
  M <- threshold(y ~ x, D, list(tau = 1), m = "d", choicerule = "softmax")
  expect_equivalent(coef(M), 5)

  D$y <- c(0, rep(1,9))
  M <- threshold(y ~ x, D, list(tau = 1), m = "d", choicerule = "softmax")
  expect_equivalent(coef(M), 1)

  D$y <- c(rep(0,9), 1)
  M <- threshold(y ~ x, D, list(tau = 1), m = "d", choicerule = "softmax")
  expect_equivalent(coef(M), 9, tol = 0.01)
})

