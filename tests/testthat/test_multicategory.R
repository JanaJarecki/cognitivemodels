# ==========================================================================
# Test for GCM with multiple categories
#
# ==========================================================================
# library(Rcpp)

# 1. Predicted values --------------------------------------------------
# 1.a Identity between analogous one category tasks
test_that("One category correct", {
  d <- matrix(c(1,1,1,0,
                1,1,1,0,
                0,0,0,1,
                0,0,0,1,
                0,1,0,1,
                1,0,1,0), 6, 4, T)
  predicts_one_class <- function(sim, w1 = .5, r = 2, q = 2, lambda = 1) {
    gcm(formula = ~ V1 + V2, class = ~ V3, data = d, choicerule = "none", 
        fix = list(V1 = w1, r = r, q = q, lambda = lambda, b1 = .5), similarity = sim)$predict()
  }
  predicts_same_class <- function(sim, w1 = .5, r = 2, q = 2, lambda = 1) {
    gcm(formula = ~ V1 + V2, class = ~ V3 + I(1-V3), data = d, choicerule = "none", 
        fix = list(V1 = w1, r = r, q = q, lambda = lambda), similarity = sim)$predict()
  }
  predicts_two_classes <- function(sim, w1 = .5, r = 2, q = 2, lambda = 1) {
    gcm(formula = ~ V1 + V2, class = ~ V3 + V4, data = d, choicerule = "none",
        fix = list(V1 = w1, r = r, q = q, lambda = lambda), similarity = sim)$predict()
  }
  expect_equal(predicts_one_class("minkowski"), predicts_same_class("minkowski"), tol = .01)
  expect_equal(predicts_one_class("mahalanobis"), predicts_same_class("mahalanobis"), tol = .01)
  expect_equal(predicts_one_class("minkowski"), predicts_two_classes("minkowski"), tol = .01)
  expect_equal(predicts_one_class("mahalanobis"), predicts_two_classes("mahalanobis"), tol = .01)
  expect_equal(predicts_same_class("minkowski"), predicts_two_classes("minkowski"), tol = .01)
  expect_equal(predicts_same_class("mahalanobis"), predicts_two_classes("mahalanobis"), tol = .01)
})

# 1.b Identity between conjoint category and constituent categories (Minkowski only)
test_that("One conjoint category correct", {
  d <- matrix(c(1,1,0,1,0,0,
                1,0,1,0,1,0,
                0,1,1,0,0,1,
                1,0,1,0,1,0,
                0,1,1,0,0,1,
                1,1,0,1,0,0), 6, 6, T)
  
  # Conjoint categories are V4 + V5 + V6
  predicts <- function(class = "~ V4 + V5 + V6") {
    as.matrix(gcm(formula = ~ V1 + V2 + V3, class = as.formula(class), data = d, choicerule = "none",
                  options = list(fit = FALSE), similarity = "minkowski")$predict())[-1, ]
  }
  
  expect_equal(rowSums(predicts()),     predicts("~ I(V4 + V5) + V6"), tol = .01)
  expect_equal(1 - predicts()[, 2],     predicts("~ I(V4 + V6) + V5"), tol = .01)
  expect_equal(1 - predicts()[, 1],     predicts("~ I(V5 + V6) + V4"), tol = .01)
  expect_equal(predicts()[, 1],         predicts("~ V4 + I(V5 + V6)"), tol = .01)
  expect_equal(predicts()[, 2],         predicts("~ V5 + I(V4 + V6)"), tol = .01)
  expect_equal(1 - rowSums(predicts()), predicts("~ V6 + I(V4 + V5)"), tol = .01)
})

test_that("Two conjoint categories correct", {
  d <- matrix(c(1,0,1,0,1,0,0,0,
                1,0,0,1,0,1,0,0,
                0,1,1,0,0,0,1,0,
                0,1,0,1,0,0,0,1,
                1,0,1,0,1,0,0,0,
                1,0,0,1,0,1,0,0,
                0,1,1,0,0,0,1,0,
                0,1,0,1,0,0,0,1), 8, 8, T)
  
  # Conjoint categories are V5 + V6 + V7 + V8
  predicts <- function(class = "~ V5 + V6 + V7 + V8") {
    gcm(formula = ~ V1 + V2 + V3 + V4, class = as.formula(class), data = d, choicerule = "none",
        options = list(fit = FALSE), similarity = "minkowski")$predict()
  }
  
  expect_equal(    rowSums(predicts()[, c(1,2)]), predicts("~ I(V5 + V6) + I(V7 + V8)"), tol = .01)
  expect_equal(    rowSums(predicts()[, c(1,3)]), predicts("~ I(V5 + V7) + I(V6 + V8)"), tol = .01)
  expect_equal(1 - rowSums(predicts()[, c(2,3)]), predicts("~ I(V5 + V8) + I(V6 + V7)"), tol = .01)
  expect_equal(    rowSums(predicts()[, c(2,3)]), predicts("~ I(V6 + V7) + I(V5 + V8)"), tol = .01)
  expect_equal(1 - rowSums(predicts()[, c(1,3)]), predicts("~ I(V6 + V8) + I(V5 + V7)"), tol = .01)
  expect_equal(1 - rowSums(predicts()[, c(1,2)]), predicts("~ I(V7 + V8) + I(V5 + V6)"), tol = .01)
  expect_equal(    rowSums(predicts())[-1],       predicts("~ I(V5 + V6 + V7) + V8")[-1], tol = .01)
  expect_equal(1 - predicts()[-1, 3],             predicts("~ I(V5 + V6 + V8) + V7")[-1], tol = .01)
  expect_equal(1 - predicts()[-1, 2],             predicts("~ I(V5 + V7 + V8) + V6")[-1], tol = .01)
  expect_equal(1 - predicts()[-1, 1],             predicts("~ I(V6 + V7 + V8) + V5")[-1], tol = .01)
})

# 2. Error handling --------------------------------------------------
test_that("Input error handlers", {
  d <- matrix(c(1,0,1,0,1,1,
                1,0,0,1,0,1,
                0,1,1,0,0,0,
                0,1,0,1,0,2), 4, 6, T)
  expect_error( # wrong combination of categories, should be V3 + V4 
    gcm(~ V1 + V2, ~ V3 * V4, data = d, options = list(fit = FALSE))
  )
  expect_error( # wrong combination of categories, should be V3 + V4 
    gcm(~ V1 + V2, ~ V3 : V4, data = d, options = list(fit = FALSE))
  )
  expect_error( # wrong combination of categories, should be V3 + V4 
    gcm(~ V1 + V2, ~ V3 / V4, data = d, options = list(fit = FALSE))
  )
  expect_error( # wrong combination of categories, should be V3 + V4 
    gcm(~ V1 + V2, ~ V3 | V4, data = d, options = list(fit = FALSE))
  )
  expect_error( # wrong combination of categories, should be V3 + V4 + V5 
    gcm(~ V1 + V2, ~ V3 + V4 * V5, data = d, options = list(fit = FALSE))
  )
  expect_error( # wrong combination of categories, should be V3 + V4 + V5 
    gcm(~ V1 + V2, ~ V3 | V4 * V5, data = d, options = list(fit = FALSE))
  )
  expect_error( # wrong combination of categories, should be V3 + V4 
    gcm(~ V1 + V2, ~ I(V3 * V4), data = d, options = list(fit = FALSE))
  )
  expect_error( # category with more than three values 
    gcm(~ V1 + V2, ~ V6, data = d, options = list(fit = FALSE))
  )
  expect_error( # object belongs to more than one category 
    gcm(~ V1 + V2, ~ V5 + V6, data = d[1:2, ], options = list(fit = FALSE))
  )
  expect_error( # no choices specified and parameters need to be estimated
    gcm(~ V1 + V2, ~ V3 + V4, data = d)
  )
  expect_error( # ommission of class
    gcm(~ V1 + V2, data = d, options = list(fit = FALSE))
  )
})
