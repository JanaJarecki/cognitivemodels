# Examples from Wakker (2008), p. 1331
# Wakker, P. P. (2008). Explaining the characteristics of the power (CRRA) utility family. Health Economics, 17(12), 1329–1344. https://doi.org/10.1002/hec.1331
D <- data.frame(x = c(50,50,50), p = c(0.75, 0.50, 0.25), x2 = 10, y = c(29, 19, 13.5))
D$p2 <- 1 - D$p

test_that("Discrete Power utility: predicted value identities", {
  # calculates the certainty equivalent
  ce <- function(pred, pow, p = D[, c("p", "p2")]) {
    y <- rowSums((abs(pred) * p))
    res <- sign(y) * if(pow != 0) { y^(1/pow) } else { exp(y) }
    matrix(res)
  }
  expect_pred_equal <- function(fix, target) {
    M <- cognitivemodel(D) + utility_pow_d(~ x | x2, fix = fix, choicerule = "none")
    expect_equal(ce(M$predict(), fix), target, tol = 0.001)
  }
  expect_pred_equal(fix = c(rp=  2), c(43.59, 36.06, 26.46))
  expect_pred_equal(fix = c(rp=  1), c(40, 30, 20))
  expect_pred_equal(fix = c(rp=0.1), c(34.24, 23.10, 15.33))
  expect_pred_equal(fix  =c(rp=  0), c(33.44, 22.36, 14.95))
  expect_pred_equal(fix = c(rp=-0.1), c(32.61, 21.65, 14.60))
  expect_pred_equal(fix = c(rp=-0.52), c(29.01, 18.98, 13.42))
  expect_pred_equal(fix = c(rp=-1), c(25, 16.67, 12.50))
})

test_that("Discrete Power utility: parameter fitting", {
  M <- cognitivemodel(D) +
    utility_pow_d(y ~ x | x2, choicerule = "none") +
    function(pred, data, par) ce(pred = pred, pow = par[["rp"]])

  fit(M, options = list(fit_measure = "mse", solver = "optimx"))
  predict(M)
})
