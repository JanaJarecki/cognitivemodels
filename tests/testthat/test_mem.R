pred_by_hand <- function(d, lambda, tau) {
  w <- c(0.5, 0.5)
  res <- NULL
  for(i in 1:nrow(d)) {
    if (i > 1) {
      dd <- d[1:i, ]
      Dist <- apply(abs(dd[rep(i, i-1), 1:2, drop = F] - dd[-i, 1:2, drop = F]), 1, `%*%`, w)
      Sim <- exp(-lambda * Dist)
      V <- sum(Sim * dd[-i, "crit"]) / sum(Sim)      
    } else {
      V <- 0.5
    }
    Pr <- exp(V/tau) / (exp(V/tau) + exp(0/tau))
    res <- rbind(res, cbind(V = V, Pr = Pr))
  }
  res    
}


# 1. Predictions -------------------------------------------------------
# 1.a Predicted values
test_that("Prediction of MEM model", {  
  skip("work in progress")
  d <- data.frame(
    f1 = c(1,  1, 0),
    f2 = c(1,  0, 0),
    crit = c(1, 0.5, NA),
    val = 0,
    obs = NA
  )
pred <- pred_by_hand(d, lambda = 0.5, tau = 2) 

  M <- cognitivemodel(data = d) +
     mem(formula = ~ f1 + f2,
        crit = ~ crit,
        discount = 0L) +
     softmax(~ pr_c | val)

  expect_equivalent(predict(M)[, 1], pred[, 2])
})


test_that("Fitting of MEM model", {
  skip("wip")
    d <- data.frame(
    f1 = c(1,  1, 0),
    f2 = c(1,  0, 0),
    crit = c(1, 0.5, NA),
    val = 0,
    obs = NA
  )
  pred <- pred_by_hand(d, lambda = 0.5, tau = 0.1)
  d$obs <- pred[, 2]
  M <- cognitivemodel(data = d) +
     mem(formula = ~ f1 + f2,
        crit = ~ crit,
        discount = 0L) +
     softmax(obs ~ pr_c | val)

  M$fit(options = list(solver = c("grid", "solnp")))
  expect_equal(coef(M), c(lambda = 0.5, tau = 0.1), tol = 0.001)

  M$set_par(c(lambda = 0.5, tau = 0.1))
  expect_equal(predict(M), pred[,2], tol = 0.00001)

  # Fit both lambda and tau
  pred <- pred_by_hand(d, lambda = 5, tau = 0.6)
  d$obs <- pred[, 2]
  M <- cognitivemodel(data = d) +
     mem(formula = ~ f1 + f2,
        crit = ~ crit,
        discount = 0L) +
     softmax(obs ~ pr_c | val)
  M$fit(options = list(solver = c("grid", "solnp")))

  expect_equal(coef(M), c(lambda = 5, tau = 0.6), tol = 0.001)
})