context("choicerules")



test_that("Softmax predicted values", {
  D <- data.frame(y = c(1,1,0), x = c(.1,.2,.3))
  D$x2 <- 1 - D$x
  res <- cbind(exp(D$x), exp(D$x2))/(exp(D$x) + exp(D$x2))
  colnames(res) <- c("pr_x", "pr_x2")

  M <- softmax(y ~ x | x2, D[1,], c(tau = 1))
  expect_equal(M$predict(), res[1,])

  M <- softmax(y ~ x | x2, D, c(tau = 1))
  expect_equivalent(M$predict(), res)

  M <- softmax(y ~ x, D[1,], c(tau = 1))
  expect_equal(M$predict(), res[1,1])

  M <- softmax(y ~ x, D, c(tau = 1))
  expect_equal(M$predict(), res[,1])
})


test_that("Epsilon greedy predicted values", {
  D <- data.frame(y = c(1,1,0), x = c(.1,.2,.3))
  eps <- 0.33
  D$x2 <- 1 - D$x
  res <- cbind(c(0,0,0), c(1,1,1)) * (1-eps) + eps / 2
  colnames(res) <- c("pr_x", "pr_x2")

  M <- epsilon_greedy(y ~ x | x2, D[1, ], c(eps = eps))
  expect_equal(M$predict(), res[1,])

  M <- epsilon_greedy(y ~ x | x2, D, c(eps=eps))
  expect_equivalent(M$predict(), res)

  M <- epsilon_greedy(y ~ x, D[1, ], c(eps=eps))
  expect_equivalent(M$predict(), res[1,1])

  M <- epsilon_greedy(y ~ x, D, c(eps=eps))
  expect_equivalent(M$predict(), res[,1])
})

test_that("Epsilon predicted values", {
  D <- data.frame(y = c(1,1,0), x = c(.1,.2,.3))
  eps <- 0.33
  D$x2 <- 1 - D$x
  res <- cbind(D$x, D$x2) * (1-eps) + eps / 2
  colnames(res) <- c("pr_x", "pr_x2")

  M <- epsilon(y ~ x | x2, D[1, ], c(eps = eps))
  expect_equal(M$predict(), res[1,])

  M <- epsilon(y ~ x | x2, D, c(eps=eps))
  expect_equivalent(M$predict(), res)

  M <- epsilon(y ~ x, D[1, ], c(eps=eps))
  expect_equivalent(M$predict(), res[1,1])

  M <- epsilon(y ~ x, D, c(eps=eps))
  expect_equivalent(M$predict(), res[,1])
})

test_that("Luce predicted values", {
  D <- data.frame(y = c(1,1,0), x = c(.1,.2,.3))
  D$x2 <- 1 - D$x
  res <- cbind(D$x, D$x2)/(D$x + D$x2)
  colnames(res) <- c("pr_x", "pr_x2")

  M <- luce(y ~ x | x2, D[1, ])
  expect_equal(M$predict(), res[1,])

  M <- luce(y ~ x | x2, D)
  expect_equivalent(M$predict(), res)

  M <- luce(y ~ x, D[1L, ])
  expect_equivalent(M$predict(), cbind(pred_x=1))

  M <- luce(y ~ x, D)
  expect_equivalent(M$predict(), cbind(pred_x = c(1,1,1)))
})



test_that("Argmax greedy predicted values", {
  D <- data.frame(y = c(1,1,0), x = c(.1,.5,.9))
  D$x2 <- 1 - D$x
  res <- cbind(c(0,0.5,1), c(1,0.5,0))
  colnames(res) <- c("pr_x", "pr_x2")

  M <- argmax(y ~ x | x2, D[1, ])
  expect_equal(M$predict(), res[1,])

  M <- argmax(y ~ x | x2, D)
  expect_equivalent(M$predict(), res)

  M <- argmax(y ~ x, D[1, ])
  expect_equivalent(M$predict(), res[1,1])

  M <- argmax(y ~ x, D)
  expect_equivalent(M$predict(), res[, 1])
})



test_that("Epsilon greedy parameter estimation", {
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
