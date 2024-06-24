# ==========================================================================
# Test for Rule Model (rule_seitz2021)
# ==========================================================================

# 1. Model predictions -----------------------------------------------------
test_that("rule_seitz2021 - Prediction Identities", {
  expect_pred_equal <- function(target, data, cr = NULL, fix = NULL) {
    M <- rule_seitz2021(c ~ x + y + z, data, choicerule = cr, fix = fix)
    expect_equal(M$predict(), target)
  }
  
  D <- as.data.frame(expand.grid(x = 0:1, y = 0:1, z = 1:2, c = 1))
  cat <- c(1, 1, 1, 0, 0, 0, 1, 0)

  # Prediction identities
  expect_pred_equal(target = cat, data = D)
  expect_pred_equal(target = rev(cat), data = D[8:1, ])
  expect_pred_equal(target = cat[c(1, 1, 2, 3, 5, 8)], data = D[c(1, 1, 2, 3, 5, 8), ])
  
  # Prediction identities with epsilon greedy
  expect_pred_equal(target = c(.95, .95, .95, .05, .05, .05, .95, .05), data = D, cr = "epsilon", fix = list(eps = .1))
  expect_pred_equal(target = c(.9, .9, .9, .1, .1, .1, .9, .1), data = D, cr = "epsilon", fix = list(eps = .2))
  expect_pred_equal(target = rep(.5, 8), data = D, cr = "epsilon", fix = list(eps = 1))
  
  # One-row data frame
  expect_pred_equal(target = c(pr_c = 1), data = D[1, ])
  expect_pred_equal(target = c(pr_c = 0), data = D[4, ])
  expect_pred_equal(target = c(pr_c = .9), data = D[7, ], cr = "epsilon", fix = list(eps = .2))
  expect_pred_equal(target = c(pr_c = .1), data = D[8, ], cr = "epsilon", fix = list(eps = .2))
})

# 2. Parameter fitting ---------------------------------------------------
test_that("rule_seitz2021 - Parameter fitting", {
  expect_par_equal <- function(target, data, c) {
    data$c <- c
    M <- rule_seitz2021(c ~ x + y + z, data, choicerule = "epsilon")
    expect_equal(M$par, target, tolerance = .0011)
  }

  expect_pred_equal <- function(target, data, c) {
    data$c <- c
    M <- rule_seitz2021(c ~ x + y + z, data, choicerule = "epsilon")
    expect_equal(M$predict(), target, tolerance = .001)
  }
  
  D <- as.data.frame(expand.grid(x = 0:1, y = 0:1, z = 1:2))
  cat <- c(1, 1, 1, 0, 0, 0, 1, 0)
  
  expect_par_equal(target = list(eps = 0), data = D, c = cat)
  expect_pred_equal(target = cat, data = D, c = cat)
  
  expect_par_equal(target = list(eps = 1), data = D, c = 1 - cat)
  expect_pred_equal(target = rep(0.5, 8), data = D, c = 1 - cat)
  
  expect_par_equal(target = list(eps = 1), data = D, c = 0)
  expect_pred_equal(target = rep(0.5, 8), data = D, c = 0)

  expect_par_equal(target = list(eps = 1), data = D, c = 1)
  expect_pred_equal(target = rep(0.5, 8), data = D, c = 1)
  
})

# 3. Formal tests -----------------------------------------------------------
# 3.a. Formula entry ----------------------------------------------------------
test_that("rule_seitz2021 - Formula formats", {
  expect_pred_equal <- function(target, formula, data, cr = NULL, fix = NULL) {
    M <- rule_seitz2021(formula, data, choicerule = cr, fix = fix)
    expect_equal(M$predict(), target)
  }
  
  cat <- c(1, 1, 1, 0, 0, 0, 1, 0)
  
  # Prediction identities
  D <- as.data.frame(expand.grid(x = 0:1, y = 0:1, z = 1:2, c = 1))
  expect_pred_equal(target = cat, formula = ~ x + y + z, data = D)
  expect_pred_equal(target = cat, formula = c ~ x + y + z, data = D)

  D <- as.data.frame(expand.grid(f1 = 0:1, f2 = 0:1, f3 = 1:2, y = 1))
  expect_pred_equal(target = cat, formula = ~ f1 + f2 + f3, data = D)
  expect_pred_equal(target = cat, formula = y ~ f1 + f2 + f3, data = D)
})

# 3.b Errors ---------------------------------------------------------------
test_that("rule_seitz2021 - Errors", {
  D <- as.data.frame(expand.grid(x = 0:1, y = 0:1, z = 1:2, c = 1))
  
  cat <- c(1, 1, 1, 0, 0, 0, 1, 0)
  
  # Errors: Not all features included in formula
  expect_error( 
    rule_seitz2021(~ x + y, data = D)
  )
  expect_error( 
    rule_seitz2021(~ x + z, data = D)
  )
  expect_error( 
    rule_seitz2021(~ y + z, data = D)
  )
  expect_error( 
    rule_seitz2021(~ x, data = D)
  )
  expect_error( 
    rule_seitz2021(~ y, data = D)
  )
  expect_error( 
    rule_seitz2021(~ z, data = D)
  )
  expect_error( # matrix
    rule_seitz2021(~ x + y + z, data = as.matrix(D))
  )
})


