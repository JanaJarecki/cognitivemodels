test_that("Grid search in Bayesian model", {
  expect_coef_equivalent <- function(fix, target, y) {
    D <- data.frame(x=rep(0,10), z=rep(1,10),
      y = c(0.5,0.3,0.25,0.19,0.16,0.14,0.12,0.11,0.1,0.08))
    if (y==1) D$y <- 1-D$y
    if (y==2) D$y <- rep(.999, 10)
    if (y==3) D$y <- rep(.01, 10)
    M <- bayes_beta_d(y ~ x + z, data = D, fix=as.list(fix), choicerule="none", options = list(solver = "grid"))
    expect_equivalent(coef(M), target, tol=0.09)
  }
  expect_coef_equivalent(fix=c(x=1,z=1),  target=1.1,      y=0)
  expect_coef_equivalent(fix=c(delta=1),  target=c(1,1),   y=0)
  expect_coef_equivalent(fix=NULL,        target=c(1,1,1), y=0)

  expect_coef_equivalent(fix=c(x=1,z=1), target=0,            y=1)
  expect_coef_equivalent(fix=c(delta=1), target=c(1.99,0.01), y=2)
  expect_coef_equivalent(fix=c(delta=1), target=c(0.03,1.97), y=3)

  expect_coef_equivalent(fix=NULL, target=c(delta=0, prior.x=2, prior.z=0),y=2)
  expect_coef_equivalent(fix=NULL, target=c(delta=0, prior.x=0, prior.z=2),y=3)
})
