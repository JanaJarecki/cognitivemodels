test_that("Estimated parameter values match target values", {
  expect_coef_equivalent <- function(fix, target, discount) {
    D <- data.frame(x=rep(0,10), z=rep(1,10),
      y = c(0.5,0.3,0.25,0.19,0.16,0.14,0.12,0.11,0.1,0.08))
    D <- rbind(data.frame(x=1,z=0,y=0.50)[rep(1,discount),], D)
    M <- bayes_beta_d(y ~ x + z, data = D, fix=as.list(fix), choicerule="none", discount = discount)
    expect_equivalent(coef(M), target, tol=0.09)
  }

  expect_coef_equivalent(fix=c(x=1,z=1),  target=1, discount = 0)
  expect_coef_equivalent(fix=c(x=1,z=1),  target=1, discount = 1)
  expect_coef_equivalent(fix=c(x=1,z=1),  target=1, discount = 2)
})

# test_that("Parameter estimates compared to Nosofsky (1989)", {
#   d <- data.frame(f1 = rep(0:1,5), f2 = rep(0:1,each=5), true = rep(0:1, each=5), obs = c(1,0,1,0,0,1,1,1,1,1))

#   expect_est_equal <- function(discount, target) {
#     set.seed(42)
#     M <- gcm(formula = obs ~ f1 + f2, criterion = ~ true, data = d, fix = c(r=1,q=1,b1=0.50,lambda=0.1), discount = 0, choicerule = "none")
#     coef(M)
#     cbind(d, pred=predict(M))
#     expect_equal(coef(M), target, tol = 0.01)
#   }
#   expect_est_equal(discount = 0, c(f1 = 0.01, f2 = 0.99))
#   expect_est_equal(discount = 2)
#   expect_est_equal(fix = NULL, discount = 100)

# }
