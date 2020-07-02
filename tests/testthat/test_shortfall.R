# Tests for the shortfall model

# 1. Predicted values --------------------------------------------------
# 1.a Predictions identical to paper
test_that("Prediction identitites", {
  expect_pred_equal <- function(fml, dt, target) {
    pars <- c(beta = 5, delta = 0.5) 
    M <- shortfall_d(fml, data = dt, asp = ~a, fix = pars, choicerule = "none")
    M2 <- shortfall_c(fml, data = dt, asp = ~a, fix = c(pars, sigma = 0))
    expect_equal(M$predict("value"), target)
    expect_equal(predict(M, "value"), target)
    expect_equal(predict(M2, "value"), target)
  }

  D <- data.frame(
    x1 = rep(1,3), x2 = rep(2,3), px = rep(.5,3),
    y1 = 0:2,      y2 = rep(3,3), py = rep(.5,3),
    z1 = 0:2,      z2 = rep(3,3), pz = rep(.5,3),
    a = rep(1,3),
    c = c(1,1,0))
  target <- cbind(pr_x = 1.5, pr_y = c(.25, 2, 2.5), pr_z = c(.25, 2, 2.5))

  # One stimulus - two values
  expect_pred_equal(fml = c ~ x1 + px + x2 + I(1-px),
    dt = D, target = target[, 1])
  # Two stimuli - two values
  expect_pred_equal(fml = c ~ x1 + px + x2 + I(1-px) | y1 + py + y2 + I(1-py),
    dt = D, target = target[, 1:2])
  # Three stimuli - two values
  expect_pred_equal(fml = c~x1+px+x2+I(1-px)|y1+py+y2+I(1-py)|z1+pz+z2+I(1-pz),
    dt = D, target = target[, 1:3])

  # Input with only one row only
  expect_pred_equal(fml = c ~ x1 + px + x2 + I(1-px),
    dt = D[1, ], target = c(target[1, 1]))
  expect_pred_equal(fml = c ~ x1 + px + x2 + I(1-px) | y1 + py + y2 + I(1-py),
    dt = D[1, ], target = target[1, 1:2])
  expect_pred_equal(fml = c~x1+px+x2+I(1-px)|y1+py+y2+I(1-py)|z1+pz+z2+I(1-pz),
    dt = D[1, ], target = target[1, 1:3])
})


# 1.b Predictions correct with fixed parameters (lower, higher, boundaries)
test_that("Prediction identities - parameter change", {
  dt <- data.frame(x1 = rep(1,3), x2 = rep(2,3), px = rep(.5,3),
                y1 = 0:2,      y2 = rep(3,3), py = rep(.5,3),
                aspiration = rep(1,3),
                choice = c(1,1,0))
  expect_parchange_equal <- function(par, value, result, tol = .01) {
    pars <- c(beta = 5, delta = 0.5)
    fix <- replace(pars, names(pars) == par, value)
    M <- shortfall_d(choice ~ x1 + px + x2 + I(1-px) | y1 + py + y2 + I(1-py), data = dt, asp = ~aspiration, fix = fix, choicerule = "none")
    expect_equal(c(M$predict('value')), result, tol = tol)
  }
  
  expect_parchange_equal("beta", 0, c(1.5, 1.5, 1.5, 1.5, 2, 2.5))
  expect_parchange_equal("beta", 10, c(1.5, 1.5, 1.5, -1, 2, 2.5))
  expect_parchange_equal("beta", 2.5, c(1.5, 1.5, 1.5, 0.875, 2, 2.5))
  expect_parchange_equal("beta", 7.5, c(1.5, 1.5, 1.5, -0.375, 2, 2.5))
  
  expect_parchange_equal("delta", 0, c(1.5, 1.5, 1.5, 1.5, 2, 2.5))
  expect_parchange_equal("delta", 1, c(1.5, 1.5, 1.5, -1, 2, 2.5))
  expect_parchange_equal("delta", 0.25, c(1.5, 1.5, 1.5, 0.875, 2, 2.5))
  expect_parchange_equal("delta", 0.75, c(1.5, 1.5, 1.5, -0.375, 2, 2.5))
})


# 1.c Predictions identical with equality-constrained parameters
test_that("Prediction identities - equal parameters", {
  dt <- data.frame(x1 = rep(1,3), x2 = rep(2,3), px = rep(.5,3),
                y1 = 0:2,      y2 = rep(3,3), py = rep(.5,3),
                aspiration = rep(1,3),
                choice = c(1,1,0))
  pars <- c(beta = 5, delta = 0.5)
  pars["beta"] <- pars["delta"]
  M <- shortfall_d(choice ~ x1 + px + x2 + I(1-px) | y1 + py + y2 + I(1-py), data = dt, asp = ~aspiration, fix = pars, choicerule = "none")
  tol <- .01
  expect_equal(M$predict('value')[1,'pr_x'], c('pr_x'=1.5), tol = tol)
  expect_equal(M$predict('value')[2,'pr_x'], c('pr_x'=1.5), tol = tol)
  expect_equal(M$predict('value')[3,'pr_x'], c('pr_x'=1.5), tol = tol)
  expect_equal(M$predict('value')[1,'pr_y'], c('pr_y'=1.375), tol = tol)
  expect_equal(M$predict('value')[2,'pr_y'], c('pr_y'=2), tol = tol)
  expect_equal(M$predict('value')[3,'pr_y'], c('pr_y'=2.5), tol = tol)
})

# 2. Parameter recovery -------------------------------------------------------
test_that("Parameter recovery", {
  expect_par_equal <- function(dt, target, ll, tol = .01) {
    fml <- choice ~ a1 + pa1 + a2 + pa2 + a3 + pa3 + a4 + pa4 + a5 + pa5 | b1 + pb1 + b2 + pb2 + b3 + pb3 + b4 + pb4 + b5 + pb5
    M <- shortfall_d(fml, data = dt, asp = ~evA | evB, choicerule = "softmax")
    expect_equal(coef(M), target, tol = tol)
    expect_equivalent(c(-2 * logLik(M)), ll, tol = .01)
  }

  # Load shortfall_d test data
  data(shortfalltest)
  d <- split(shortfalltest, shortfalltest$id)

  expect_par_equal(d$`2`,  c(beta=0.75, delta=0.76, tau=1/0.57), ll = 162.6)
  expect_par_equal(d$`5`,  c(beta=0.71, delta=1, tau=1/0.48), ll = 183.5)
  expect_par_equal(d$`14`, c(beta=4.71, delta=0.24, tau=1/0.19), ll = 209.1, tol = .06)

  # Todo: Participant #13 gives a corner solution, check this, maybe implement something
  # model <- shortfall_d(fml, data = shortfall_dtest[shortfall_dtest$id==13,], asp = ~evA | evB, choicerule = "softmax")
  # expect_equal(model$coef(), c(beta=0.98, delta=0.93, tau=1/0.04), tol = .01)
  # expect_equal(-2*model$logLik(), 183.5, tol = .001)
})



# 3. Formal tests -------------------------------------------------------------
# 3.1. Identities of different input formats
test_that("Input format - data.table", {
  skip("Skipped to avoid data.table dependency")
  expect_pred_equivalent <- function(data, result) {
    pars <- c(beta = 5, delta = 0.5)
    M <- shortfall_d(choice ~ y1 + py + y2 + I(1-py), data = as.data.table(data), asp = ~aspiration, fix = pars, choicerule = "none")
    expect_equivalent(M$predict(), result, tol=tol)
  }
  D <- dt[, -(1:3)]
  expect_pred_equivalent(D[1, ], 0.25)
  expect_pred_equivalent(D[2, ], 2)
  expect_pred_equivalent(D[3, ], 2.5)
  expect_pred_equivalent(D, c(0.25, 2, 2.5))
  expect_pred_equivalent(D[3:1, ], c(2.5, 2, 0.25))
  expect_pred_equivalent(D[c(2,1,3), ], c(2, 0.25, 2.5))
})


test_that("Input format - tibble", {
  skip("Skipped to avoide tibble dependency")
  expect_pred_equivalent <- function(data, result, tol = .01) {
    M <- shortfall_d(choice ~ y1 + py + y2 + I(1-py), data = as_tibble(data), asp = ~aspiration, fix = pars, choicerule = "none")
    expect_equivalent(M$predict(), result, tol=tol)
  }
  
  expect_pred_equivalent(D[1, ], 0.25)
  expect_pred_equivalent(D[2, ], 2)
  expect_pred_equivalent(D[3, ], 2.5)
  expect_pred_equivalent(D, c(0.25, 2, 2.5))
  expect_pred_equivalent(D[3:1, ], c(2.5, 2, 0.25))
  expect_pred_equivalent(D[c(2,1,3), ], c(2, 0.25, 2.5))
})

# 3.b. Different variations of the formula entry
test_that("Input format - probability entry", {
  dt <- data.frame(x1 = rep(1,3), x2 = rep(2,3), px = rep(.5,3),
                y1 = 0:2,      y2 = rep(3,3), py = rep(.5,3),
                aspiration = rep(1,3),
                choice = c(1,1,0))
  pars <- c(beta = 5, delta = 0.5) 
  M1 <- shortfall_d(choice ~ x1 + px + x2           | y1 + py + y2          , data = dt, asp = ~aspiration, fix = pars, choicerule = "none")$predict()
  M2 <- shortfall_d(choice ~ x1 + px + x2           | y1 + py + y2 + I(1-py), data = dt, asp = ~aspiration, fix = pars, choicerule = "none")$predict()
  M3 <- shortfall_d(choice ~ x1 + px + x2 + I(1-px) | y1 + py + y2          , data = dt, asp = ~aspiration, fix = pars, choicerule = "none")$predict()
  M4 <- shortfall_d(choice ~ x1 + px + x2 + I(1-px) | y1 + py + y2 + I(1-py), data = dt, asp = ~aspiration, fix = pars, choicerule = "none")$predict()
  expect_equal(M1, M2)
  expect_equal(M1, M3)
  expect_equal(M2, M3)
  expect_equal(M1, M4)
  expect_equal(M2, M4)
  expect_equal(M3, M4)
  
  M1 <- shortfall_d(choice ~ x1 + px + x2           | y1 + py + y2          , data = dt[1, ], asp = ~aspiration, fix = pars, choicerule = "none")$predict()
  M2 <- shortfall_d(choice ~ x1 + px + x2           | y1 + py + y2 + I(1-py), data = dt[1, ], asp = ~aspiration, fix = pars, choicerule = "none")$predict()
  M3 <- shortfall_d(choice ~ x1 + px + x2 + I(1-px) | y1 + py + y2          , data = dt[1, ], asp = ~aspiration, fix = pars, choicerule = "none")$predict()
  M4 <- shortfall_d(choice ~ x1 + px + x2 + I(1-px) | y1 + py + y2 + I(1-py), data = dt[2, ], asp = ~aspiration, fix = pars, choicerule = "none")$predict()
  expect_equal(M1, M2); expect_equal(M1, M3); expect_equal(M2, M3)
  expect_false(isTRUE(all.equal(M1, M4))); expect_false(isTRUE(all.equal(M2, M4))); expect_false(isTRUE(all.equal(M3, M4))) 
  
  expect_error( # probabilities don't sum to 1
    shortfall_d(choice ~ x1 + px + x2 + I(2*(1-px)) | y1 + py + y2, data = dt, asp = ~aspiration, fix = pars, choicerule = "none")
  )
  expect_error(
    shortfall_d(choice ~ x1 + px + x2 | y1 + py + y2 + I(2*(1-py)), data = dt, asp = ~aspiration, fix = pars, choicerule = "none")
  )
  expect_error(
    shortfall_d(choice ~ x1 + px + x2 + I(2*(1-px)) | y1 + py + y2 + I(2*(1-py)), data = dt, asp = ~aspiration, fix = pars, choicerule = "none")
  )
})


# 3.c Error handling
test_that("Input error handlers", {
  dt <- data.frame(x1 = rep(1,3), x2 = rep(2,3), px = rep(.5,3),
                y1 = 0:2,      y2 = rep(3,3), py = rep(.5,3),
                aspiration = rep(1,3),
                choice = c(1,1,0))
  expect_error(shortfall_d(choice ~ x1 + px + x2 + I(1-px) | y1 + py + y2 + I(1-py), data = dt, asp = ~aspiration, fix = list(angle = "size"), choicerule = "none"))
  expect_error(shortfall_d(choice ~ x1 + px + x2 + I(1-px) | y1 + py + y2 + I(1-py), data = dt, asp = ~aspiration, fix = list(c(pars[names(pars) != "beta"], beta = NA)), choicerule = "none"))

  expect_error( # wrong order, should be x1 + px + x2 + I(1-px)
    shortfall_d(choice ~ x1 + x2 + px + I(1-px), data = dt, asp = ~aspiration, fix = pars, choicerule = "none")
  )
  expect_error( # wrong order in the x stimuli
    shortfall_d(choice ~ x1 + x2 + px + I(1-px) | y1 + py + y2 + I(1-py), data = dt, asp = ~aspiration, fix = pars, choicerule = "none")
  )
  expect_error( # wrong order in the y stimuli
    shortfall_d(choice ~ x1 + px + x2 + I(1-px) | y1 + y2 + py + I(1-py), data = dt, asp = ~aspiration, fix = pars, choicerule = "none")
  )
  expect_error( # wrong order in the x and the y stimuli
    shortfall_d(choice ~ x1 + x2 + px + I(1-px) | y1 + y2 + py + I(1-py), data = dt, asp = ~aspiration, fix = pars, choicerule = "none")
  )
  expect_error( # wrong order without last probability
    shortfall_d(choice ~ x1 + x2 + px, data = dt, asp = ~aspiration, fix = pars, choicerule = "none")
  )
  expect_error( # wrong order without last option
    shortfall_d(choice ~ x1 + px, data = dt, asp = ~aspiration, fix = pars, choicerule = "none")
  )
  expect_error( # matrix
    shortfall_d(choice ~ x1 + px + x2 + I(1-px), data = as.matrix(dt), asp = ~aspiration, fix = pars, choicerule = "none")
  )
  expect_error(
    shortfall_d(choice ~ x1 + px + x2 + I(1-px), data = as.matrix(dt[1, ]), asp = ~aspiration, fix = pars, choicerule = "none")
  )
  expect_error( # no choices specified and parameters need to be estimated
    shortfall_d(~ x1 + px + x2 + I(1-px), data = dt, asp = ~aspiration, choicerule = "none")
  )
  expect_error( # non-binary choices and parameters need to be estimated
    shortfall_d(py ~ x1 + px + x2 + I(1-px), data = dt, asp = ~aspiration, choicerule = "none")
  )
  expect_error( # ommission of probabilities
    shortfall_d(choice ~ x1 + x2 + y1 + y2, data = dt, asp = ~aspiration, fix = pars, choicerule = "none")
  )
})