# ==========================================================================
# Test for Baseline Cognitive Models
# ==========================================================================

# 1. Model predictions -----------------------------------------------------
test_that("Baseline_const_d - Prediction Identities", {
  expect_pred_equal <- function(target, data) {
    M <- baseline_const_d(y ~ ., target[1], data)
    expect_equal(M$predict(), target)
  }
  D <- data.frame(y = c(1,1,0), x = c(1,2,3), z = c(0,4,2))

  # Prediction identities
  expect_pred_equal(target = rep(0.50, nrow(D)), data = D)
  expect_pred_equal(target = rep(0.70, nrow(D)), data = D)
  expect_pred_equal(target = rep(0.123, nrow(D)), data = D)

  # One-row data frame
  expect_pred_equal(target = c(pr_y = 0.50),  data = D[1,])
  expect_pred_equal(target = c(pr_y = 0.70),  data = D[1,])
  expect_pred_equal(target = c(pr_y = 0.123), data = D[1,])
})




test_that("Baseline_mean_d - Prediction Identities", {
  expect_pred_equal <- function(target, data) {
    M <- baseline_mean_d(y ~ ., data)
    expect_equal(M$predict(), target)
  }
  D <- data.frame(y = c(1,1,0), x = c(1,2,3), z = c(0,4,2))

  # Prediction identities
  expect_pred_equal(target = rep(2/3, nrow(D)), data = D)

  # One-row data frame
  expect_pred_equal(target = c(pr_y = 1), data = D[1,])

  expect_equal(npar(baseline_mean_d(y ~ ., D)), 1L)
})