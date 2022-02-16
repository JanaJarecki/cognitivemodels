# ==========================================================================
# Test for Implemented Distance Metrics
#
# Minkowski Metric
# Mahalanobis Distance 
# ==========================================================================
library(Rcpp)

# ==========================================================================
# Minkowski Metric 
# ==========================================================================
test_that("Minkowski Metric correct", {
  true_dist <- function(x, y, w, r = 1, q = 1) {
    sum(w * abs(x-y)^r)^(q/r)
  }
  x <- c(1, 1); y <- c(4, 4); w <- c(0.5, 0.5)
  expect_equal(minkowski(x, y, w, r = 1, q = 1), true_dist(x, y, w, r = 1, q = 1), tol = .01)
  expect_equal(minkowski(x, y, w, r = 1, q = 2), true_dist(x, y, w, r = 1, q = 2), tol = .01)
  expect_equal(minkowski(x, y, w, r = 2, q = 1), true_dist(x, y, w, r = 2, q = 1), tol = .01)
  expect_equal(minkowski(x, y, w, r = 2, q = 2), true_dist(x, y, w, r = 2, q = 2), tol = .01)
})

# ==========================================================================
# Mahalanobis Distance 
# ==========================================================================
test_that("unweighted one-feature MOD correct", {
  # Weighted MOD calculated by hand
  true_dist <- function(x, y, s, w, q) {
    sum(w * (x - y) %*% s * (w * (x - y)))^(q/2)
  }
  
  n <- 1 # one feature
  w <- rep(1, n) # unweighted
  
  # integer features
  x <- 1; y <- 4
  
  ## few exemplars with low variance
  s <- matrix(c(3, 4, 5), ncol = n)
  s <- solve(var(s))
  expect_equal(mahalanobis(x, y, s, w, q = 1), true_dist(x, y, s, w, q = 1), tol = .01)
  expect_equal(mahalanobis(x, y, s, w, q = 2), true_dist(x, y, s, w, q = 2), tol = .01)
  
  ## few exemplars with high variance
  s <- matrix(c(1, 4, 7), ncol = n)
  s <- solve(var(s))
  expect_equal(mahalanobis(x, y, s, w, q = 1), true_dist(x, y, s, w, q = 1), tol = .01)
  
  ## many exemplars with low variance
  s <- matrix(rep(c(3, 4, 5), 10), ncol = n)
  s <- solve(var(s))
  expect_equal(mahalanobis(x, y, s, w, q = 1), true_dist(x, y, s, w, q = 1), tol = .01)
  
  ## many exemplars with high variance
  s <- matrix(rep(c(1, 4, 7), 10), ncol = n)
  s <- solve(var(s))
  expect_equal(mahalanobis(x, y, s, w, q = 1), true_dist(x, y, s, w, q = 1), tol = .01)
  
  # double features in y and s
  x <- 1; y <- 4.5
  
  ## few exemplars with low variance
  s <- matrix(c(3.5, 4.5, 5.5), ncol = n)
  s <- solve(var(s))
  expect_equal(mahalanobis(x, y, s, w, q = 1), true_dist(x, y, s, w, q = 1), tol = .01)
  
  ## few exemplars with high variance
  s <- matrix(c(1.5, 4.5, 7.5), ncol = n)
  s <- solve(var(s))
  expect_equal(mahalanobis(x, y, s, w, q = 1), true_dist(x, y, s, w, q = 1), tol = .01)
  
  ## many exemplars with low variance
  s <- matrix(rep(c(3.5, 4.5, 5.5), 10), ncol = n)
  s <- solve(var(s))
  expect_equal(mahalanobis(x, y, s, w, q = 1), true_dist(x, y, s, w, q = 1), tol = .01)
  
  ## many exemplars with high variance
  s <- matrix(rep(c(1.5, 4.5, 7.5), 10), ncol = n)
  s <- solve(var(s))
  expect_equal(mahalanobis(x, y, s, w, q = 1), true_dist(x, y, s, w, q = 1), tol = .01)
})

test_that("unweighted two-feature MOD correct", {
  # Weighted MOD calculated by hand
  true_dist <- function(x, y, s, w, q) {
    sum(w * (x - y) %*% s * (w * (x - y)))^(q/2)
  }
  
  n <- 2 # two features
  w <- rep(1, n) # unweighted
  
  # integer features
  x <- c(1, 1); y <- c(4, 4)
  
  ## few exemplars with low variance and low covariance
  s <- matrix(c(3, 3, 4, 4, 5, 5,
                3, 5, 3, 5, 3, 5), ncol = n)
  s <- solve(var(s))
  expect_equal(mahalanobis(x, y, s, w, q = 1), true_dist(x, y, s, w, q = 1), tol = .01)
  expect_equal(mahalanobis(x, y, s, w, q = 2), true_dist(x, y, s, w, q = 2), tol = .01)
  
  ## few exemplars with low variance and high covariance
  s <- matrix(c(3, 3, 4, 4, 5, 5,
                3, 4, 4, 4, 4, 5), ncol = n)
  s <- solve(var(s))
  expect_equal(mahalanobis(x, y, s, w, q = 1), true_dist(x, y, s, w, q = 1), tol = .01)
  
  ## few exemplars with high variance and low covariance
  s <- matrix(c(1, 1, 4, 4, 7, 7,
                1, 7, 1, 7, 1, 7), ncol = n)
  s <- solve(var(s))
  expect_equal(mahalanobis(x, y, s, w, q = 1), true_dist(x, y, s, w, q = 1), tol = .01)
  
  ## few exemplars with high variance and high covariance
  s <- matrix(c(1, 1, 4, 4, 7, 7,
                1, 4, 4, 4, 4, 7), ncol = n)
  s <- solve(var(s))
  expect_equal(mahalanobis(x, y, s, w, q = 1), true_dist(x, y, s, w, q = 1), tol = .01)
  
  ## many exemplars with low variance and low covariance
  s <- matrix(c(rep(c(3, 3, 4, 4, 5, 5), 10),
                rep(c(3, 5, 3, 5, 3, 5), 10)), ncol = n)
  s <- solve(var(s))
  expect_equal(mahalanobis(x, y, s, w, q = 1), true_dist(x, y, s, w, q = 1), tol = .01)
  
  ## many exemplars with low variance and high covariance
  s <- matrix(c(rep(c(3, 3, 4, 4, 5, 5), 10),
                rep(c(3, 4, 4, 4, 4, 5), 10)), ncol = n)
  s <- solve(var(s))
  expect_equal(mahalanobis(x, y, s, w, q = 1), true_dist(x, y, s, w, q = 1), tol = .01)
  
  ## many exemplars with high variance and low covariance
  s <- matrix(c(rep(c(1, 1, 4, 4, 7, 7), 10),
                rep(c(1, 7, 1, 7, 1, 7), 10)), ncol = n)
  s <- solve(var(s))
  expect_equal(mahalanobis(x, y, s, w, q = 1), true_dist(x, y, s, w, q = 1), tol = .01)
  
  ## many exemplars with high variance and high covariance
  s <- matrix(c(rep(c(1, 1, 4, 4, 7, 7), 10),
                rep(c(1, 4, 4, 4, 4, 7), 10)), ncol = n)
  s <- solve(var(s))
  expect_equal(mahalanobis(x, y, s, w, q = 1), true_dist(x, y, s, w, q = 1), tol = .01)
  
  # double features in y and s
  x <- c(1, 1); y <- c(4.5, 4.5)
  
  ## few exemplars with low variance and low covariance
  s <- matrix(c(3.5, 3.5, 4.5, 4.5, 5.5, 5.5,
                3.5, 5.5, 3.5, 5.5, 3.5, 5.5), ncol = n)
  s <- solve(var(s))
  expect_equal(mahalanobis(x, y, s, w, q = 1), true_dist(x, y, s, w, q = 1), tol = .01)
  
  ## few exemplars with low variance and high covariance
  s <- matrix(c(3.5, 3.5, 4.5, 4.5, 5.5, 5.5,
                3.5, 4.5, 4.5, 4.5, 4.5, 5.5), ncol = n)
  s <- solve(var(s))
  expect_equal(mahalanobis(x, y, s, w, q = 1), true_dist(x, y, s, w, q = 1), tol = .01)
  
  ## few exemplars with high variance and low covariance
  s <- matrix(c(1.5, 1.5, 4.5, 4.5, 7.5, 7.5,
                1.5, 7.5, 1.5, 7.5, 1.5, 7.5), ncol = n)
  s <- solve(var(s))
  expect_equal(mahalanobis(x, y, s, w, q = 1), true_dist(x, y, s, w, q = 1), tol = .01)
  
  ## few exemplars with high variance and high covariance
  s <- matrix(c(1.5, 1.5, 4.5, 4.5, 7.5, 7.5,
                1.5, 4.5, 4.5, 4.5, 4.5, 7.5), ncol = n)
  s <- solve(var(s))
  expect_equal(mahalanobis(x, y, s, w, q = 1), true_dist(x, y, s, w, q = 1), tol = .01)
  
  ## many exemplars with low variance and low covariance
  s <- matrix(c(rep(c(3.5, 3.5, 4.5, 4.5, 5.5, 5.5), 10),
                rep(c(3.5, 5.5, 3.5, 5.5, 3.5, 5.5), 10)), ncol = n)
  s <- solve(var(s))
  expect_equal(mahalanobis(x, y, s, w, q = 1), true_dist(x, y, s, w, q = 1), tol = .01)
  
  ## many exemplars with low variance and high covariance
  s <- matrix(c(rep(c(3.5, 3.5, 4.5, 4.5, 5.5, 5.5), 10),
                rep(c(3.5, 4.5, 4.5, 4.5, 4.5, 5.5), 10)), ncol = n)
  s <- solve(var(s))
  expect_equal(mahalanobis(x, y, s, w, q = 1), true_dist(x, y, s, w, q = 1), tol = .01)
  
  ## many exemplars with high variance and low covariance
  s <- matrix(c(rep(c(1.5, 1.5, 4.5, 4.5, 7.5, 7.5), 10),
                rep(c(1.5, 7.5, 1.5, 7.5, 1.5, 7.5), 10)), ncol = n)
  s <- solve(var(s))
  expect_equal(mahalanobis(x, y, s, w, q = 1), true_dist(x, y, s, w, q = 1), tol = .01)
  
  ## many exemplars with high variance and high covariance
  s <- matrix(c(rep(c(1.5, 1.5, 4.5, 4.5, 7.5, 7.5), 10),
                rep(c(1.5, 4.5, 4.5, 4.5, 4.5, 7.5), 10)), ncol = n)
  s <- solve(var(s))
  expect_equal(mahalanobis(x, y, s, w, q = 1), true_dist(x, y, s, w, q = 1), tol = .01)
})

test_that("weighted two-feature MOD correct", {
  # Weighted MOD calculated by hand
  true_dist <- function(x, y, s, w, q) {
    sum(w * (x - y) %*% s * (w * (x - y)))^(q/2)
  }
  
  n <- 2 # two features
  
  # equal weighting
  w <- rep(0.5, n)
  
  ## integer features
  x <- c(1, 1); y <- c(4, 4)
  s <- matrix(c(3, 3, 4, 4, 5, 5,
                3, 5, 3, 5, 3, 5), ncol = n)
  s <- solve(var(s))
  expect_equal(mahalanobis(x, y, s, w, q = 1), true_dist(x, y, s, w, q = 1), tol = .01)
  expect_equal(mahalanobis(x, y, s, w, q = 2), true_dist(x, y, s, w, q = 2), tol = .01)
  
  ## double features in y and s
  x <- c(1, 1); y <- c(4.5, 4.5)
  s <- matrix(c(3.5, 3.5, 4.5, 4.5, 5.5, 5.5,
                3.5, 5.5, 3.5, 5.5, 3.5, 5.5), ncol = n)
  s <- solve(var(s))
  expect_equal(mahalanobis(x, y, s, w, q = 1), true_dist(x, y, s, w, q = 1), tol = .01)
  expect_equal(mahalanobis(x, y, s, w, q = 2), true_dist(x, y, s, w, q = 2), tol = .01)
  
  # high f1 weighting
  w <- c(0.8, 0.2)
  
  ## integer features
  x <- c(1, 1); y <- c(4, 4)
  s <- matrix(c(3, 3, 4, 4, 5, 5,
                3, 5, 3, 5, 3, 5), ncol = n)
  s <- solve(var(s))
  expect_equal(mahalanobis(x, y, s, w, q = 1), true_dist(x, y, s, w, q = 1), tol = .01)
  expect_equal(mahalanobis(x, y, s, w, q = 2), true_dist(x, y, s, w, q = 2), tol = .01)
  
  ## double features in y and s
  x <- c(1, 1); y <- c(4.5, 4.5)
  s <- matrix(c(3.5, 3.5, 4.5, 4.5, 5.5, 5.5,
                3.5, 5.5, 3.5, 5.5, 3.5, 5.5), ncol = n)
  s <- solve(var(s))
  expect_equal(mahalanobis(x, y, s, w, q = 1), true_dist(x, y, s, w, q = 1), tol = .01)
  expect_equal(mahalanobis(x, y, s, w, q = 2), true_dist(x, y, s, w, q = 2), tol = .01)
  
  # low f1 weighting
  w <- c(0.2, 0.8)
  
  ## integer features
  x <- c(1, 1); y <- c(4, 4)
  s <- matrix(c(3, 3, 4, 4, 5, 5,
                3, 5, 3, 5, 3, 5), ncol = n)
  s <- solve(var(s))
  expect_equal(mahalanobis(x, y, s, w, q = 1), true_dist(x, y, s, w, q = 1), tol = .01)
  expect_equal(mahalanobis(x, y, s, w, q = 2), true_dist(x, y, s, w, q = 2), tol = .01)
  
  ## double features in y and s
  x <- c(1, 1); y <- c(4.5, 4.5)
  s <- matrix(c(3.5, 3.5, 4.5, 4.5, 5.5, 5.5,
                3.5, 5.5, 3.5, 5.5, 3.5, 5.5), ncol = n)
  s <- solve(var(s))
  expect_equal(mahalanobis(x, y, s, w, q = 1), true_dist(x, y, s, w, q = 1), tol = .01)
  expect_equal(mahalanobis(x, y, s, w, q = 2), true_dist(x, y, s, w, q = 2), tol = .01)
  
  # exclusive f1 weighting
  w <- c(1, 0)
  
  ## integer features
  x <- c(1, 1); y <- c(4, 4)
  s <- matrix(c(3, 3, 4, 4, 5, 5,
                3, 5, 3, 5, 3, 5), ncol = n)
  s <- solve(var(s))
  expect_equal(mahalanobis(x, y, s, w, q = 1), true_dist(x, y, s, w, q = 1), tol = .01)
  expect_equal(mahalanobis(x, y, s, w, q = 2), true_dist(x, y, s, w, q = 2), tol = .01)
  
  ## double features in y and s
  x <- c(1, 1); y <- c(4.5, 4.5)
  s <- matrix(c(3.5, 3.5, 4.5, 4.5, 5.5, 5.5,
                3.5, 5.5, 3.5, 5.5, 3.5, 5.5), ncol = n)
  s <- solve(var(s))
  expect_equal(mahalanobis(x, y, s, w, q = 1), true_dist(x, y, s, w, q = 1), tol = .01)
  expect_equal(mahalanobis(x, y, s, w, q = 2), true_dist(x, y, s, w, q = 2), tol = .01)
  
  # exclusive f2 weighting
  w <- c(0, 1)
  
  ## integer features
  x <- c(1, 1); y <- c(4, 4)
  s <- matrix(c(3, 3, 4, 4, 5, 5,
                3, 5, 3, 5, 3, 5), ncol = n)
  s <- solve(var(s))
  expect_equal(mahalanobis(x, y, s, w, q = 1), true_dist(x, y, s, w, q = 1), tol = .01)
  expect_equal(mahalanobis(x, y, s, w, q = 2), true_dist(x, y, s, w, q = 2), tol = .01)
  
  ## double features in y and s
  x <- c(1, 1); y <- c(4.5, 4.5)
  s <- matrix(c(3.5, 3.5, 4.5, 4.5, 5.5, 5.5,
                3.5, 5.5, 3.5, 5.5, 3.5, 5.5), ncol = n)
  s <- solve(var(s))
  expect_equal(mahalanobis(x, y, s, w, q = 1), true_dist(x, y, s, w, q = 1), tol = .01)
  expect_equal(mahalanobis(x, y, s, w, q = 2), true_dist(x, y, s, w, q = 2), tol = .01)
})

test_that("more than two-feature MOD correct", {
  # Weighted MOD calculated by hand
  true_dist <- function(x, y, s, w, q) {
    sum(w * (x - y) %*% s * (w * (x - y)))^(q/2)
  }
  
  n <- 3 # three features
  
  # equal weighting
  w <- c(0.5, 0.3, 0.2)
  
  ## integer features
  x <- c(1, 1, 1); y <- c(4, 4, 4)
  s <- matrix(c(3, 3, 4, 4, 5, 5,
                3, 5, 3, 5, 3, 5,
                5, 3, 5, 3, 3, 5), ncol = n)
  s <- solve(var(s))
  expect_equal(mahalanobis(x, y, s, w, q = 1), true_dist(x, y, s, w, q = 1), tol = .01)
  expect_equal(mahalanobis(x, y, s, w, q = 2), true_dist(x, y, s, w, q = 2), tol = .01)
  
  ## double features in y and s
  x <- c(1, 1, 1); y <- c(4.5, 4.5, 4.5)
  s <- matrix(c(3.5, 3.5, 4.5, 4.5, 5.5, 5.5,
                3.5, 5.5, 3.5, 5.5, 3.5, 5.5,
                5.5, 3.5, 5.5, 3.5, 3.5, 5.5), ncol = n)
  s <- solve(var(s))
  expect_equal(mahalanobis(x, y, s, w, q = 1), true_dist(x, y, s, w, q = 1), tol = .01)
  expect_equal(mahalanobis(x, y, s, w, q = 2), true_dist(x, y, s, w, q = 2), tol = .01)
})
