context("CPT values")
library(cogscimodels)

test_that("CPT predicted value identities", {
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
  model <- cpt(rp ~ x1 + x2 + px + I(1-px) | y1 + y2 + py + I(1-py), nopt = 2, nout = 2, ref = 0, choicerule = NULL, data = dt, fixed = c(alpha = 0.88, beta = 0.88, lambda = 2.25, gammap = 0.61, gamman = 0.69))
  expect_equal(model$predict('value')[1,'x'], c('x'=57), tol = .01)
  expect_equal(model$predict('value')[1,'y'], c('y'=57), tol = .01)
  expect_equal(model$predict('value')[2,'x'], c('x'=-129), tol = .01)
  expect_equal(model$predict('value')[2,'y'], c('y'=-129), tol = .01)

  # More examples
})

test_that("CPT errors", {
  expect_error(cpt(rp ~ x1 + x2 + px + I(1-px) | y1 + y2 + py + I(1-py), nopt = 2, nout = 2, ref = 0, choicerule = NULL, data = dt, fixed = c(alpha = NA, beta = 0.88, lambda = 2.25, gammap = 0.61, gamman = 0.69)))
})