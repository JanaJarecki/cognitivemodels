context("utility")
library(cogscimodels)

#
# Examples from Wakker (2008), p. 1331
# --------------------------------------------------------------------------
D <- data.frame(x = c(50,50,50), p = c(0.75, 0.50, 0.25), x2 = 10, y = c(29, 19, 13.5))
D$p2 <- 1 - D$p
test_that("Power utility: predicted value identities", {
  M <- utility_pow(~ x | x2, D, list(rp=2, rn="rp"))
  ce <- function(pred, pow, p = D[, c("p", "p2")]) {
    y <- rowSums((abs(pred) * p))
    sign(y) * y^(1/pow)
  }
  expect_equal(ce(M$predict(), 2), c(43.59, 36.06, 26.46), tol = 0.001)

  M$set_par(c(rp = 1))
  expect_equal(ce(M$predict(), 1), c(40, 30, 20), tol = 0.001)

  M$set_par(c(rp = 0.1))
  expect_equal(ce(M$predict(), 0.1), c(34.24, 23.10, 15.33), tol = 0.001)

  M$set_par(c(rp = 0))
  expect_equal(exp(ce(M$predict(), 1)), c(33.44, 22.36, 14.95), tol = 0.001)

  M$set_par(c(rp = -0.1))
  expect_equal(ce(M$predict(), -0.1), c(32.61, 21.65, 14.60), tol = 0.001)

  M$set_par(c(rp = -0.52))
  expect_equal(ce(M$predict(), -0.52), c(29.01, 18.98, 13.42), tol = 0.001)

  M$set_par(c(rp = -1))
  expect_equal(ce(M$predict(), -1), c(25, 16.67, 12.50), tol = 0.001)
})


# test_that("Exponential utility: predicted value identities", {
#   M <- utility_exp(~ x | x2, D, list(alpha = 0.064))
#   ce <- function(pred, pow, p = D[, c("p", "p2")]) {
#     log(rowSums((pred * p)), pow)
#   }
#   expect_equal(ce(M$predict(), 0.064), c(28.40, 19.67, 14.10), tol = 0.001)
# })