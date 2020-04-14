context("shortfall")
# library(cogscimodels)
library(data.table)

# 0. Data set, standard parameters, and tolerance
dt <- data.frame(x1 = rep(1,3), x2 = rep(2,3), px = rep(.5,3),
                y1 = 0:2,      y2 = rep(3,3), py = rep(.5,3),
                aspiration = rep(1,3),
                choice = c(1,1,0))
pars <- c(beta = 5, delta = 0.5) 
M <- shortfall(choice ~ x1 + px + x2 + I(1-px) | y1 + py + y2 + I(1-py), data = dt, asp = ~aspiration, fix = pars)
tol <- .01 

# 1. Predictive testing
test_that("Prediction identitites", {
  expect_equal(M$predict('value')[1,'pr_x'], c('pr_x'=1.5), tol = tol)
  expect_equal(M$predict('value')[2,'pr_x'], c('pr_x'=1.5), tol = tol)
  expect_equal(M$predict('value')[3,'pr_x'], c('pr_x'=1.5), tol = tol)
  expect_equal(M$predict('value')[1,'pr_y'], c('pr_y'=0.25), tol = tol)
  expect_equal(M$predict('value')[2,'pr_y'], c('pr_y'=2), tol = tol)
  expect_equal(M$predict('value')[3,'pr_y'], c('pr_y'=2.5), tol = tol)
})

# 1.a. Parameter value changes (lower, higher, boundaries)
test_that("Prediction identities after parameter change", {
  expect_parchange_equal <- function(par, value, result) {
    fix <- replace(pars, names(pars) == par, value)
    M <- shortfall(choice ~ x1 + px + x2 + I(1-px) | y1 + py + y2 + I(1-py), data = dt, asp = ~aspiration, fix = fix)
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


# 1.b. Parameter restrictions
expect_error(shortfall(choice ~ x1 + px + x2 + I(1-px) | y1 + py + y2 + I(1-py), data = dt, asp = ~aspiration, fix = list(angle = "size")))
expect_error(shortfall(choice ~ x1 + px + x2 + I(1-px) | y1 + py + y2 + I(1-py), data = dt, asp = ~aspiration, fix = list(c(pars[names(pars) != "beta"], beta = NA))))

# 1.c. Equal parameters
pars["beta"] <- pars["delta"]
M <- shortfall(choice ~ x1 + px + x2 + I(1-px) | y1 + py + y2 + I(1-py), data = dt, asp = ~aspiration, fix = pars)
test_that("Prediction identities", {
  expect_equal(M$predict('value')[1,'pr_x'], c('pr_x'=1.5), tol = tol)
  expect_equal(M$predict('value')[2,'pr_x'], c('pr_x'=1.5), tol = tol)
  expect_equal(M$predict('value')[3,'pr_x'], c('pr_x'=1.5), tol = tol)
  expect_equal(M$predict('value')[1,'pr_y'], c('pr_y'=1.375), tol = tol)
  expect_equal(M$predict('value')[2,'pr_y'], c('pr_y'=2), tol = tol)
  expect_equal(M$predict('value')[3,'pr_y'], c('pr_y'=2.5), tol = tol)
})

# 2. Parameter recovery
data(shortfalltest) # Load shortfall test data
d <- split(shortfalltest, shortfalltest$id)
fml <- choice ~ a1 + pa1 + a2 + pa2 + a3 + pa3 + a4 + pa4 + a5 + pa5 | b1 + pb1 + b2 + pb2 + b3 + pb3 + b4 + pb4 + b5 + pb5

test_that("Parameter estimates == estimates in paper", {
  model <- shortfall(fml, data = d$`2`, asp = ~evA | evB, choicerule = "softmax")
  expect_equal(model$coef(), c(beta=0.75, delta=0.76, tau=1/0.57), tol = .01)
  expect_equal(-2 * model$logLik(), 162.6, tol = .01)

  model <- shortfall(fml, data = d$`5`, asp = ~evA | evB, choicerule = "softmax")
  expect_equal(model$coef(), c(beta=0.71, delta=1, tau=1/0.48), tol = .01)
  expect_equal(-2 * model$logLik(), 183.5, tol = .01)

  model <- shortfall(fml, data = d$`14`, asp = ~evA | evB, choicerule = "softmax")
  expect_equal(model$coef(), c(beta=4.71, delta=0.24, tau=1/0.19), tol = .05)
  expect_equal(-2 * model$logLik(), 209.1, tol = .01)

  # Todo: Participant #13 gives a corner solution, check this, maybe implement something
  # model <- shortfall(fml, data = shortfalltest[shortfalltest$id==13,], asp = ~evA | evB, choicerule = "softmax")
  # expect_equal(model$coef(), c(beta=0.98, delta=0.93, tau=1/0.04), tol = .01)
  # expect_equal(-2*model$logLik(), 183.5, tol = .001)
})

# 3. Formal tests
# 3.a. One-row test set and test sets with different orders
pars <- c(beta = 5, delta = 0.5) 
D <- dt[, -(1:3)]
test_that("Prediction identities data frame", {
  expect_pred_equivalent <- function(data, result) {
    M <- shortfall(choice ~ y1 + py + y2 + I(1-py), data = as.data.frame(data), asp = ~aspiration, fix = pars)
    expect_equivalent(M$predict(), result, tol=tol)
  }
  
  expect_pred_equivalent(D[1, ], 0.25)
  expect_pred_equivalent(D[2, ], 2)
  expect_pred_equivalent(D[3, ], 2.5)
  expect_pred_equivalent(D, c(0.25, 2, 2.5))
  expect_pred_equivalent(D[3:1, ], c(2.5, 2, 0.25))
  expect_pred_equivalent(D[c(2,1,3), ], c(2, 0.25, 2.5))
})

test_that("Prediction identities data table", {
  expect_pred_equivalent <- function(data, result) {
    M <- shortfall(choice ~ y1 + py + y2 + I(1-py), data = as.data.table(data), asp = ~aspiration, fix = pars)
    expect_equivalent(M$predict(), result, tol=tol)
  }
  
  expect_pred_equivalent(D[1, ], 0.25)
  expect_pred_equivalent(D[2, ], 2)
  expect_pred_equivalent(D[3, ], 2.5)
  expect_pred_equivalent(D, c(0.25, 2, 2.5))
  expect_pred_equivalent(D[3:1, ], c(2.5, 2, 0.25))
  expect_pred_equivalent(D[c(2,1,3), ], c(2, 0.25, 2.5))
})

test_that("Prediction identities tibble", {
  expect_pred_equivalent <- function(data, result) {
    M <- shortfall(choice ~ y1 + py + y2 + I(1-py), data = as_tibble(data), asp = ~aspiration, fix = pars)
    expect_equivalent(M$predict(), result, tol=tol)
  }
  
  expect_pred_equivalent(D[1, ], 0.25)
  expect_pred_equivalent(D[2, ], 2)
  expect_pred_equivalent(D[3, ], 2.5)
  expect_pred_equivalent(D, c(0.25, 2, 2.5))
  expect_pred_equivalent(D[3:1, ], c(2.5, 2, 0.25))
  expect_pred_equivalent(D[c(2,1,3), ], c(2, 0.25, 2.5))
})

# 3.b. Formula entry
test_that("Probability entry", {
  M1 <- shortfall(choice ~ x1 + px + x2           | y1 + py + y2          , data = dt, asp = ~aspiration, fix = pars)$predict()
  M2 <- shortfall(choice ~ x1 + px + x2           | y1 + py + y2 + I(1-py), data = dt, asp = ~aspiration, fix = pars)$predict()
  M3 <- shortfall(choice ~ x1 + px + x2 + I(1-px) | y1 + py + y2          , data = dt, asp = ~aspiration, fix = pars)$predict()
  M4 <- shortfall(choice ~ x1 + px + x2 + I(1-px) | y1 + py + y2 + I(1-py), data = dt, asp = ~aspiration, fix = pars)$predict()
  expect_equal(M1, M2); expect_equal(M1, M3); expect_equal(M2, M3); expect_equal(M1, M4); expect_equal(M2, M4); expect_equal(M3, M4)
  
  M1 <- shortfall(choice ~ x1 + px + x2           | y1 + py + y2          , data = dt[1, ], asp = ~aspiration, fix = pars)$predict()
  M2 <- shortfall(choice ~ x1 + px + x2           | y1 + py + y2 + I(1-py), data = dt[1, ], asp = ~aspiration, fix = pars)$predict()
  M3 <- shortfall(choice ~ x1 + px + x2 + I(1-px) | y1 + py + y2          , data = dt[1, ], asp = ~aspiration, fix = pars)$predict()
  M4 <- shortfall(choice ~ x1 + px + x2 + I(1-px) | y1 + py + y2 + I(1-py), data = dt[2, ], asp = ~aspiration, fix = pars)$predict()
  expect_equal(M1, M2); expect_equal(M1, M3); expect_equal(M2, M3)
  expect_false(isTRUE(all.equal(M1, M4))); expect_false(isTRUE(all.equal(M2, M4))); expect_false(isTRUE(all.equal(M3, M4))) 
  
  expect_error( # probabilities don't sum to 1
    shortfall(choice ~ x1 + px + x2 + I(2*(1-px)) | y1 + py + y2, data = dt, asp = ~aspiration, fix = pars)
  )
  expect_error(
    shortfall(choice ~ x1 + px + x2 | y1 + py + y2 + I(2*(1-py)), data = dt, asp = ~aspiration, fix = pars)
  )
  expect_error(
    shortfall(choice ~ x1 + px + x2 + I(2*(1-px)) | y1 + py + y2 + I(2*(1-py)), data = dt, asp = ~aspiration, fix = pars)
  )
})

test_that("shortfall input errors", {
  expect_error( # wrong order, should be x1 + px + x2 + I(1-px)
    shortfall(choice ~ x1 + x2 + px + I(1-px), data = dt, asp = ~aspiration, fix = pars)
  )
  expect_error( # wrong order in the x stimuli
    shortfall(choice ~ x1 + x2 + px + I(1-px) | y1 + py + y2 + I(1-py), data = dt, asp = ~aspiration, fix = pars)
  )
  expect_error( # wrong order in the y stimuli
    shortfall(choice ~ x1 + px + x2 + I(1-px) | y1 + y2 + py + I(1-py), data = dt, asp = ~aspiration, fix = pars)
  )
  expect_error( # wrong order in the x and the y stimuli
    shortfall(choice ~ x1 + x2 + px + I(1-px) | y1 + y2 + py + I(1-py), data = dt, asp = ~aspiration, fix = pars)
  )
  expect_error( # wrong order without last probability
    shortfall(choice ~ x1 + x2 + px, data = dt, asp = ~aspiration, fix = pars)
  )
  expect_error( # wrong order without last option
    shortfall(choice ~ x1 + px, data = dt, asp = ~aspiration, fix = pars)
  )
  expect_error( # matrix
    shortfall(choice ~ x1 + px + x2 + I(1-px), data = as.matrix(dt), asp = ~aspiration, fix = pars)
  )
  expect_error(
    shortfall(choice ~ x1 + px + x2 + I(1-px), data = as.matrix(dt[1, ]), asp = ~aspiration, fix = pars)
  )
  expect_error( # no choices specified and parameters need to be estimated
    shortfall(~ x1 + px + x2 + I(1-px), data = dt, asp = ~aspiration)
  )
  expect_error( # non-binary choices and parameters need to be estimated
    shortfall(py ~ x1 + px + x2 + I(1-px), data = dt, asp = ~aspiration)
  )
  expect_error( # ommission of probabilities
    shortfall(choice ~ x1 + x2 + y1 + y2, data = dt, asp = ~aspiration, fix = pars)
  )
})