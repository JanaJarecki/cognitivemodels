context("cpt")
library(cogsciMs)

# Tversky, A., & Kahneman, D. (1992). Advances in prospect theory: cumulative representation of uncertainty. Journal of Risk and Uncertainty, 5, 297–323. doi:10.1007/BF00122574
  # p. 313
  dt <- data.frame(
    x1 = c(100, -100),
    px = 1,
    x2 = 0,
    y1 = c(200, -200),
    py = c(.71,.64),
    y2 = 0,
    rp = 1)
tk_par <- c(alpha = 0.88, beta = 0.88, lambda = 2.25, gammap = 0.61, gamman = 0.69)

test_that("Prediction identitites to Tversky & Kahneman (1992)", {
  M <- cpt(rp ~ x1 + px + x2 | y1 + py + y2, ref = 0, data = dt, fix = tk_par)
  expect_equal(M$predict('value')[1,'pr_x'], c('pr_x'=57), tol = .01)
  expect_equal(M$predict('value')[1,'pr_y'], c('pr_y'=57), tol = .1)
  expect_equal(M$predict('value')[2,'pr_x'], c('pr_x'=-129), tol = .01)
  expect_equal(M$predict('value')[2,'pr_y'], c('pr_y'=-129), tol = .01)
})

test_that("Prediction identities", {
  D <- data.frame(x1=c(1,2,2),x2=c(2,1,1),p=c(0.66,0.66,0.50))
  M <- cpt(~x1+p+x2, ref=0L, data=D[1,], fix=tk_par)
  expect_equivalent(M$predict(), 1.285, tol=0.01)
  M <- cpt(~x1+p+x2, ref=0L, data=D[2,], fix=tk_par)
  expect_equivalent(M$predict(), 1.427, tol=0.001)
  M <- cpt(~x1+p+x2, ref=0L, data=D[3,], fix=tk_par)
  expect_equivalent(M$predict(), 1.353, tol = 0.001)
  M <- cpt(~x1+p+x2, ref=0L, data=D, fix=tk_par)
  expect_equivalent(M$predict(), c(1.285, 1.427, 1.353), tol = 0.001)
  M <- cpt(~x1+p+x2, ref=0L, data=D[3:1,], fix=tk_par)
  expect_equivalent(M$predict(), c(1.353, 1.427, 1.285), tol = 0.001)
  M <- cpt(~x1+p+x2, ref=0L, data=D[c(2,1,3),], fix=tk_par)
  expect_equivalent(M$predict(), c(1.427, 1.285, 1.353), tol = 0.001)
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