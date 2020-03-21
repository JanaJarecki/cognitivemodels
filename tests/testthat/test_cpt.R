# Tversky, A., & Kahneman, D. (1992). Advances in prospect theory: cumulative representation of uncertainty. Journal of Risk and Uncertainty, 5, 297–323. doi:10.1007/BF00122574
context("cpt")
library(cogsciMs)

# 0. Data set, standard parameters, and tolerance
dt <- data.frame(
  x1 = c(100, -100),
  px = 1,
  x2 = 0,
  y1 = c(200, -200),
  py = c(.71, .64),
  y2 = 0,
  rp = 1) # Tversky & Kahneman, 1992, pp. 313
tk_par <- c(alpha = 0.88, beta = 0.88, lambda = 2.25, gammap = 0.61, gamman = 0.69) 
M <- cpt(rp ~ x1 + px + x2 | y1 + py + y2, ref = 0, data = dt, fix = tk_par)
tol <- .01 

# 1. Predictive testing
test_that("Prediction identitites to Tversky & Kahneman (1992)", {
  expect_equal(M$predict('value')[1,'pr_x'], c('pr_x'=57), tol = tol)
  expect_equal(M$predict('value')[1,'pr_y'], c('pr_y'=57), tol = tol)
  expect_equal(M$predict('value')[2,'pr_x'], c('pr_x'=-129), tol = tol)
  expect_equal(M$predict('value')[2,'pr_y'], c('pr_y'=-129), tol = tol)
})

# 1.a. Parameter value changes (lower, higher, boundaries)
tk_par <- tk_par[sort(names(tk_par))]
pars <- M$parspace[, 1:2]
pars <- pars[sort(rownames(pars)), ]
pars <- data.frame(pars, (pars + tk_par)/2)

solutions <- array(data = c(4.65, 10000, 7.58, 758.58, rep(57.4, 16),
                            2.86, 21616, 5.56, 1112.26, rep(57.23, 8), 105.9, 69.61, 21.01, 77.53, rep(57.23, 4),
                            rep(-129.47, 4), -10.36, -22500, -17.07, -1706.8, rep(-129.47, 8), 0, -575.44, -64.74, -352.46,
                            rep(-129.87, 4), -6.5, -49053.55, -12.62, -2524.03, 0, -132.92, -56.27, -154.08, rep(-129.87, 4), 0, -577.21, -64.94, -353.54), 
                   dim = c(4, 5, 4), dimnames = list(c("lb", "ub", "lb.1", "ub.1"), names(tk_par), c("x1", "y1", "x2", "y2")))

apply(as.matrix(expand.grid(1:length(tk_par), 1:ncol(pars))), 1, function(i) {
  fix <- tk_par
  fix[i[1]] <- pars[i[1], i[2]]
  M <- cpt(rp ~ x1 + px + x2 | y1 + py + y2, ref = 0, data = dt, fix = fix)
  test_that("Prediction identitites to Tversky & Kahneman (1992)", {
    expect_equal(M$predict('value')[1,'pr_x'], c('pr_x' = solutions[i[2], i[1], 1]), tol = tol)
    expect_equal(M$predict('value')[1,'pr_y'], c('pr_y' = solutions[i[2], i[1], 2]), tol = tol)
    expect_equal(M$predict('value')[2,'pr_x'], c('pr_x' = solutions[i[2], i[1], 3]), tol = tol)
    expect_equal(M$predict('value')[2,'pr_y'], c('pr_y' = solutions[i[2], i[1], 4]), tol = tol)
  })
})

# 1.b. Parameter restrictions
expect_error(cpt(rp ~ x1 + px + x2 | y1 + py + y2, ref = 0, data = dt, fix = list(angle = "size")))
cpt(rp ~ x1 + px + x2 | y1 + py + y2, ref = 0, data = dt, fix = list(lambda = NA))

test_that("Prediction identities", {
  D <- data.frame(x1=c(1,2,2),x2=c(2,1,1),p=c(0.66,0.66,0.50))
  M <- cpt(~x1+p+x2, ref=0L, data=D[1,], fix=tk_par)
  expect_equivalent(M$predict(), 1.285, tol=tol)
  M <- cpt(~x1+p+x2, ref=0L, data=D[2,], fix=tk_par)
  expect_equivalent(M$predict(), 1.427, tol=tol)
  M <- cpt(~x1+p+x2, ref=0L, data=D[3,], fix=tk_par)
  expect_equivalent(M$predict(), 1.353, tol = tol)
  M <- cpt(~x1+p+x2, ref=0L, data=D, fix=tk_par)
  expect_equivalent(M$predict(), c(1.285, 1.427, 1.353), tol = tol)
  M <- cpt(~x1+p+x2, ref=0L, data=D[3:1,], fix=tk_par)
  expect_equivalent(M$predict(), c(1.353, 1.427, 1.285), tol = tol)
  M <- cpt(~x1+p+x2, ref=0L, data=D[c(2,1,3),], fix=tk_par)
  expect_equivalent(M$predict(), c(1.427, 1.285, 1.353), tol = tol)
})

test_that("CPT errors", {
  expect_error( # wrong order, should be x1 + px + x2
    cpt(rp ~ x2 + x1 + px, ref=0, data = dt)
    )
  expect_error( # Wrong order in the first of two RHS stimuli
    cpt(rp ~ x2 + x1 + px | y1 + py + y2, ref=0, data = dt)
    )
  expect_error( # Wrong order with last probability submitted
    cpt(rp ~ x2 + x1 + px + I(1-px), ref=0, data = dt)
    )
  expect_error(
    cpt(rp ~ x2 + x1 + px + I(1-px) | y1 + py + y2, ref=0, data = dt)
    )
  expect_error( # Error in only the second of two RHS
    cpt(rp ~ x1 + px + x2 | y2 + y1 + py, ref=0, choicerule=NULL, data = dt)
    )
  expect_error(
    cpt(rp ~ x1 + px + x2 + I(1-px) | y2 + y1 + py + I(1-py), ref=0, choicerule=NULL, data = dt)
    )
  expect_error( # Error in both RHSs
    cpt(rp ~ x2 + x1 + px | y2 + y1 + py, ref=0, choicerule=NULL, data = dt)
    )
  expect_error(
    cpt(rp ~ x2 + x1 + px + I(1-px) | y2 + y1 + py + I(1-py), ref=0, choicerule=NULL, data = dt)
    )
})
