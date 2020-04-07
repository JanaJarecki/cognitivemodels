context("shortfall")
# library(cogscimodels)
library(data.table)

# 0. Data set, standard parameters, and tolerance
dt <- data.frame(x1 = rep(1,3), x2 = rep(2,3), px = rep(.5,3),
                y1 = 0:2,      y2 = rep(3,3), py = rep(.5,3),
                aspiration = rep(1,3),
                choice = c(1,1,0))
par <- c(beta = 5, delta = 0.5) 
M <- shortfall(choice ~ x1 + px + x2 + I(1-px) | y1 + py + y2 + I(1-py), data = dt, asp = ~aspiration, fix = par)
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
pars <- M$parspace[, 1:2]
pars <- pars[sort(rownames(pars)), ]
pars <- data.frame(pars, (pars + par)/2)

solutions <- array(data = c(rep(1.5, 8),
                            1.5, -1, 0.875, -0.375, 1.5, -1, 0.875, -0.375,
                            rep(1.5, 8),
                            rep(2, 8), 
                            rep(1.5, 8),
                            rep(2.5, 8)), 
                   dim = c(4, 2, 6), dimnames = list(c("lb", "ub", "lb.1", "ub.1"), names(par), c("x1", "y1", "x2", "y2", "x3", "y3")))

apply(as.matrix(expand.grid(1:length(par), 1:ncol(pars))), 1, function(i) {
  fix <- par
  fix[i[1]] <- pars[i[1], i[2]]
  M <- shortfall(choice ~ x1 + px + x2 + I(1-px) | y1 + py + y2 + I(1-py), data = dt, asp = ~aspiration, fix = fix)
  test_that("Prediction identitites to Tversky & Kahneman (1992)", {
    expect_equal(M$predict('value')[1,'pr_x'], c('pr_x' = solutions[i[2], i[1], 1]), tol = tol)
    expect_equal(M$predict('value')[1,'pr_y'], c('pr_y' = solutions[i[2], i[1], 2]), tol = tol)
    expect_equal(M$predict('value')[2,'pr_x'], c('pr_x' = solutions[i[2], i[1], 3]), tol = tol)
    expect_equal(M$predict('value')[2,'pr_y'], c('pr_y' = solutions[i[2], i[1], 4]), tol = tol)
  })
})

# 1.b. Parameter restrictions
expect_error(shortfall(choice ~ x1 + px + x2 + I(1-px) | y1 + py + y2 + I(1-py), data = dt, asp = ~aspiration, fix = list(angle = "size")))
expect_error(shortfall(choice ~ x1 + px + x2 + I(1-px) | y1 + py + y2 + I(1-py), data = dt, asp = ~aspiration, fix = list(c(par[names(par) != "beta"], beta = NA))))


data(shortfalltest) # Load shortfall test data
d2  <- shortfalltest[shortfalltest$id==2,]
d5  <- shortfalltest[shortfalltest$id==5,]
d14 <- shortfalltest[shortfalltest$id==14,]
fml <- choice ~ a1 + pa1 + a2 + pa2 + a3 + pa3 + a4 + pa4 + a5 + pa5 | b1 + pb1 + b2 + pb2 + b3 + pb3 + b4 + pb4 + b5 + pb5

# Do tests
test_that("Parameter estimates == estimates in paper", {
  model <- shortfall(fml, data = d2, asp = ~evA | evB, choicerule = "softmax")
  expect_equal(model$coef(), c(beta=0.75, delta=0.76, tau=1/0.57), tol = .01)
  expect_equal(-2 * model$logLik(), 162.6, tol = .01)

  model <- shortfall(fml, data = d5, asp = ~evA | evB, choicerule = "softmax")
  expect_equal(model$coef(), c(beta=0.71, delta=1, tau=1/0.48), tol = .01)
  expect_equal(-2 * model$logLik(), 183.5, tol = .01)

  model <- shortfall(fml, data = d14, asp = ~evA | evB, choicerule = "softmax")
  expect_equal(model$coef(), c(beta=4.71, delta=0.24, tau=1/0.19), tol = .05)
  expect_equal(-2 * model$logLik(), 209.1, tol = .01)

  # Todo: Participant #13 gives a corner solution, check this, maybe implement something
  # model <- shortfall(fml, data = shortfalltest[shortfalltest$id==13,], asp = ~evA | evB, choicerule = "softmax")
  # expect_equal(model$coef(), c(beta=0.98, delta=0.93, tau=1/0.04), tol = .01)
  # expect_equal(-2*model$logLik(), 183.5, tol = .001)
})

test_that("Input errors", {
  expect_error(shortfall(choice ~ a1 + a2 + a3 + a4, asp = ~ evA, data = d2))
})