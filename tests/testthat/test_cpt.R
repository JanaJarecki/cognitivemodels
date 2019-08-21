context("cpt")
library(cogscimodels)

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

test_that("CPT predictions vs. Tversky & Kahneman (1992)", {
  model <- cpt(rp ~ x1 + px + x2 | y1 + py + y2, ref = 0, choicerule = NULL, data = dt, fixed = tk_par)
  expect_equal(model$predict('value')[1,'x'], c('x'=57), tol = .01)
  expect_equal(model$predict('value')[1,'y'], c('y'=57), tol = .1)
  expect_equal(model$predict('value')[2,'x'], c('x'=-129), tol = .01)
  expect_equal(model$predict('value')[2,'y'], c('y'=-129), tol = .01)
})

test_that("CPT errors", {
  expect_error( # wrong order, should be x1 + px + x2
    cpt(rp ~ x2 + x1 + px, ref=0, choicerule=NULL, data = dt)
    )
  expect_error(
    cpt(rp ~ x2 + x1 + px | y1 + py + y2, ref=0, choicerule=NULL, data = dt)
    )
  expect_error( # Error in only the first of two RHS
    cpt(rp ~ x2 + x1 + px + I(1-px), ref=0, choicerule=NULL, data = dt)
    )
  expect_error(
    cpt(rp ~ x2 + x1 + px + I(1-px) | y1 + py + y2, ref=0, choicerule=NULL, data = dt)
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