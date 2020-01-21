context("chain models")

test_that("csm - prediced value identities", {
  D <- data.frame(x1 = 1, x2 = 2, obsx = c(0,0,0), y1=1, y2=0, obsy = c(1,0,1), y=1)
  cpar <- c(alpha=0.88, beta=0.88, lambda=2.25, gammap=0.61, gamman=0.69)
  bpar <- list(delta=1, priorpar = c(1,1,1,1))

  M <- cogscimodel(data = D) +
    bayes_beta(~ obsx | obsy, fix = bpar)
  expect_equal(M$predict(), cbind(pr_obsx = c(0.5,0.33,0.25), pr_obsy = c(0.5, 0.66,.50)), tol = 0.01)

  M <- cogscimodel(data = D) +
    bayes_beta(~ obsx | obsy, fix = bpar) +
    softmax(~ pr_obsx | pr_obsy, fix = c(tau = 1))
  expect_equal(M$predict(), cbind(pred_pr_obsx = c(0.5,0.42,0.43), pred_pr_obsy = c(0.5, 0.58,.56)), tol = 0.01)

  M <- cogscimodel(data = D) +
    bayes_beta(~ obsx | obsy, fix = bpar) +
    cpt(~ x1 + pr_obsx + x2 | y1 + pr_obsy + y2, ref = 0L, fix = cpar)
  expect_equal(M$predict(), cbind(pr_x = c(1.35,1.43,1.46), pr_y = c(0.42,0.51,0.42)), tol = 0.01)

  M <- cogscimodel(data = D) +
    bayes_beta(~ obsx | obsy, fix = bpar) +
    cpt(~ x1 + pr_obsx + x2 | y1 + pr_obsy + y2, ref = 0L, fix = cpar) +
    luce(~ pr_x | pr_y)
  expect_equal(M$predict(), cbind(pred_pr_x = c(0.76,0.73,0.77), pred_pr_y = c(0.23,0.26,0.22)), tol = 0.01)
})

test_that("csm - baseline", {
  D <- data.frame(x1 = 1, x2 = 2, obsx = c(0,0,0), y1=1, y2=0, obsy = c(1,0,1), y=1)
  M <- cogscimodel(data = D) +
    baseline_const(obsy ~ ., const = 0.5, mode = "discrete")
  expect_equivalent(M$predict(), rep(0.5,3))
})

test_that("csm - ebm", {
  data(nosofsky1989)
  D <- nosofsky1989[nosofsky1989$condition == "angle", ]
  anglepar <- c(lambda=3.20, angle=.98, size=.02, b0=.43, b1=.57,r=2,q=2)
  target <- 1L - c(94,56,19,01,96,62,23,03,92,55,14,01,98,56,13,01) / 100
  M <- cogscimodel(data = D) +
    gcm(pobs ~ angle + size, criterion = ~true_cat, fix = anglepar)
  expect_equivalent(M$predict(newdata = D), target, tol = 0.01)

  M <- cogscimodel(data = D) +
    gcm(pobs ~ angle + size, criterion = ~true_cat, fix = c(r=2,q=2), options = list(fit_data = D, fit_n = D$N)) 
  npar(M, "free")

  fit(M)
  M$fit()
  M

  M$parspace
  M$options
  MM <- gcm(pobs ~ angle + size, ~true_cat, data = D, fix = c(r=2,q=2), options = list(fit_data = D, fit_n = D$N))
  MM
  MM$predict(newdata = D)

 M$predict(newdata = D)
  expect_equivalent(M$predict(), rep(0.5,3))
})


