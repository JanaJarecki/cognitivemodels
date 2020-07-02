# 1. Predictive testing -----------------------------------------------------
#    Test if model(s) generate(s) the expected predictions
test_that("Softmax - Prediction identities", {
  expect_pred_equal <- function(D, tau, f) {
    res <- cbind(exp(D$x/tau), exp(D$x2/tau))/(exp(D$x/tau) + exp(D$x2/tau))
    colnames(res) <- c("pr_x", "pr_x2")
    M <- softmax(f, D, c(tau = tau))
    nn <- ifelse(M$nstim <= 2, 1, M$nstim)
    expect_equivalent(M$predict(), res[, 1:nn])
  }
  D <- data.frame(y = c(1,1,0), x = c(.1,.2,.3), x2 = 1-c(.1,.2,.3))
  expect_pred_equal(D[1,], tau = 1,  f = y ~ x | x2)
  expect_pred_equal(D,     tau = 1,  f = y ~ x | x2)
  expect_pred_equal(D[1,], tau = 1,  f = y ~ x)
  expect_pred_equal(D,     tau = 1,  f = y ~ x)
  expect_pred_equal(D[1,], tau = .5, f = y ~ x | x2)
  expect_pred_equal(D,     tau = .5, f = y ~ x | x2)
  expect_pred_equal(D[1,], tau = .5, f = y ~ x)
  expect_pred_equal(D,     tau = .5, f = y ~ x)
})


test_that("Epsilon greedy - Prediction identities", {
  test_pred_equal <- function(D, eps, f) {
    D <- data.frame(y = c(1,1,0), x = c(.1,.2,.3), x2 = 1-c(.1,.2,.3))
    res <- cbind(c(0,0,0), c(1,1,1)) * (1-eps) + eps / 2
    colnames(res) <- c("pr_x", "pr_x2")
    M <- epsilon_greedy(f, D, c(eps = eps))
    nn <- ifelse(nstim(M) < 3, 1, 2)
    expect_equivalent(M$predict(), res[, 1:nn])
  }
  
  test_pred_equal(D =  D[1, ], eps = .33, f = y ~ x | x2)
  test_pred_equal(D =  D,      eps = .33, f = y ~ x | x2)
  test_pred_equal(D =  D[1,],  eps = .33, f = y ~ x)
  test_pred_equal(D =  D,      eps = .33, f = y ~ x)

  test_pred_equal(D =  D[1, ], eps = .1, f = y ~ x | x2)
  test_pred_equal(D =  D,      eps = .1, f = y ~ x | x2)
  test_pred_equal(D =  D[1,],  eps = .1, f = y ~ x)
  test_pred_equal(D =  D,      eps = .1, f = y ~ x)
})

test_that("Epsilon - Prediction identities", {
  test_pred_equal <- function(D, eps, f) {
    D <- data.frame(y = c(1,1,0), x = c(.1,.2,.3), x2 = 1-c(.1,.2,.3))
    res <- cbind(D$x, D$x2) * (1-eps) + eps / 2
    colnames(res) <- c("pr_x", "pr_x2")
    M <- epsilon(f, D, c(eps = eps))
    nn <- ifelse(nstim(M) < 3, 1, 2)
    expect_equivalent(M$predict(), res[, 1:nn])
  }
  
  test_pred_equal(D =  D[1, ], eps = .33, f = y ~ x | x2)
  test_pred_equal(D =  D,      eps = .33, f = y ~ x | x2)
  test_pred_equal(D =  D[1,],  eps = .33, f = y ~ x)
  test_pred_equal(D =  D,      eps = .33, f = y ~ x)

  test_pred_equal(D =  D[1, ], eps = .1, f = y ~ x | x2)
  test_pred_equal(D =  D,      eps = .1, f = y ~ x | x2)
  test_pred_equal(D =  D[1,],  eps = .1, f = y ~ x)
  test_pred_equal(D =  D,      eps = .1, f = y ~ x)
})


test_that("Luce  - Prediction identities", {
  test_pred_equal <- function(D, f, res = NULL) {
    M <- luce(f, D)
    nn <- ifelse(nstim(M) < 3, 1, 2)
    if (is.null(res)) res <- (cbind(D$x, D$x2)/(D$x + D$x2))[, 1:nn]
    expect_equivalent(M$predict(), res)
  }
  D <- data.frame(y = c(1,1,0), x = c(.1,.2,.3), x2 = 1-c(.1,.2,.3))
  test_pred_equal(D =  D[1, ], f = y ~ x | x2)
  test_pred_equal(D =  D,      f = y ~ x | x2)
  test_pred_equal(D =  D[1,], f = y ~ x, c(1))
  test_pred_equal(D =  D,     f = y ~ x, c(1,1,1))
})


test_that("Argmax greedy - Prediction identities", {
  test_pred_equal <- function(D, f) {
    M <- argmax(f, D)
    res <- cbind(c(0,0.5,1), c(1,0.5,0))[1:nrow(D), , drop=F]
    nn <- ifelse(nstim(M) < 3, 1, 2)
    expect_equivalent(M$predict(), res[, 1:nn])
  }
  D <- data.frame(y = c(1,1,0), x = c(.1,.5,.9), x2 = 1-c(.1,.5,.8))
  test_pred_equal(D =  D[1, ], f = y ~ x | x2)
  test_pred_equal(D =  D,      f = y ~ x | x2)
  test_pred_equal(D =  D[1,], f = y ~ x)
  test_pred_equal(D =  D,     f = y ~ x)
})



# 2. Parameter estimation ----------------------------------------------------
test_that("Epsilon greedy - Parameter estimation", {
  expect_par_equivalent <- function(y, target) {
    D <- data.frame(y = y, x = c(.8,.8,.2,.2))
    M <- epsilon_greedy(y ~ x, D, options = list(fit = T))

    expect_equivalent(M$coef(), target, tol=0.05)
  }
  expect_par_equivalent(y = c(1,1,0,0), 0)

  D <- data.frame(y = c(1,1,0,0), x = c(.8,.8,.2,.2))
  M <- epsilon_greedy(y ~ x, D)
  expect_equivalent(M$coef(), 0, tol=0.05)

  D$y2 <- c(0,0,1,1)
  M <- epsilon_greedy(y2 ~ x, D)
  expect_equivalent(M$coef(), 1, tol=0.05)

  D$y3 <- c(1,0,0,0)
  M <- epsilon_greedy(y3 ~ x, D)
  expect_equivalent(M$coef(), 0.5, tol=0.05)

  D$y4 <- c(1,1,1,0)
  M <- epsilon_greedy(y4 ~ x, D)
  expect_equivalent(M$coef(), 0.5, tol=0.05)
})



test_that("Epsilon estimated parameter values", {
  D <- data.frame(y = c(1,1,0,0), x = c(.8,.8,.2,.2))
  M <- epsilon(y ~ x, D)
  M2 <- epsilon(y ~ x | I(1-x), D)
  expect_equivalent(M$coef(), 0, tol=0.05)
  expect_equal(M$coef(), M2$coef())

  D$y2 <- c(0,0,1,1)
  M <- epsilon(y2 ~ x, D)
  M2 <- epsilon(y2 ~ x | I(1-x), D)
  expect_equivalent(M$coef(), 1, tol=0.05)
  expect_equal(M$coef(), M2$coef())

  D$y3 <- c(1,0,0,0)
  M <- epsilon(y3 ~ x, D)
  M2 <- epsilon(y3 ~ x | I(1-x), D)
  expect_equivalent(M$coef(), 0.16, tol=0.05)
  expect_equal(M$coef(), M2$coef())

  D$y4 <- c(1,1,1,0)
  M <- epsilon(y4 ~ x, D)
  M2 <- epsilon(y4 ~ x | I(1-x), D)
  expect_equivalent(M$coef(), 0.16, tol=0.05)
  expect_equal(M$coef(), M2$coef())
})


test_that("Epsilon greedy estimated parameter values", {
  D <- data.frame(y = c(1,1,0,0), x = c(.8,.8,.2,.2))
  M <- epsilon_greedy(y ~ x, D)
  M2 <- epsilon_greedy(y ~ x | I(1-x), D)
  expect_equivalent(M$coef(), 0, tol=0.05)
  expect_equal(M$coef(), M2$coef())

  D$y2 <- c(0,0,1,1)
  M <- epsilon_greedy(y2 ~ x, D)
  M2 <- epsilon_greedy(y2 ~ x | I(1-x), D)
  expect_equivalent(M$coef(), 1, tol=0.05)
  expect_equal(M$coef(), M2$coef())

  D$y3 <- c(1,0,0,0)
  M <- epsilon_greedy(y3 ~ x, D)
  M2 <- epsilon_greedy(y3 ~ x | I(1-x), D)
  expect_equivalent(M$coef(), 0.5, tol=0.05)
  expect_equal(M$coef(), M2$coef())

  D$y4 <- c(1,1,1,0)
  M <- epsilon_greedy(y4 ~ x, D)
  M2 <- epsilon_greedy(y4 ~ x | I(1-x), D)
  expect_equivalent(M$coef(), 0.5, tol=0.05)
  expect_equal(M$coef(), M2$coef())
})


test_that("Softmax estimated parameter values", {
  D <- data.frame(y = c(1,1,0,0), x = c(.51,.51,.49,.49))
  M <- softmax(y ~ x, D)
  M2 <- softmax(y ~ x | I(1-x), D)
  expect_equivalent(M$coef(), 0.0001)
  expect_equal(M$coef(), M2$coef())

  D$y2 <- c(.5,.5,.5,.5)
  D$x2 <- c(1, 0, 1, 0)
  M <- softmax(y2 ~ x2, D)
  M2 <- softmax(y2 ~ x2 | I(1-x2), D)
  expect_equivalent(M$coef(), 10)
  expect_equal(M$coef(), M2$coef())
})
