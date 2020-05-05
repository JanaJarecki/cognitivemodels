# Juslin, P., Olsson, H., & Olsson, A.-C. (2003). Exemplar effects in categorization and multiple-cue judgment. Journal of Experimental Psychology: General, 132, 133-156. doi: 10.1037/0096-3445.132.1.133
context("ebm")

calc_pred <- function(dt, par, newdata) {
  r <- par["r"]; q <- par["q"]; lambda <- par["lambda"]; w <- par[c("f1", "f2")] # defines parameters
  sapply(1:nrow(dt), function(i) {
    p <- unlist(dt[i, 1:2]) # defines probe
    if(!newdata & i == 1) {
      return(mean(range(dt[, "c"])))
    } else {
      if(!newdata) dt <- dt[1:(i-1), ] # use only previously seen exemplars
      x <- dt[, 1:2] # defines exemplars
      
      dist <- as.matrix(abs(sweep(x, 2, p))) # calculates distance
      dist <- ((dist^r) %*% w)^(1/r)
      sim <- exp(-lambda * dist^q) # calculates similarity
      return(as.vector((dt[, "c"] %*% sim) / sum(sim))) # makes prediction
    }
  })
}

# 0. Data set, standard parameters, and tolerance
dt <- data.frame(
  f1 = c(1, 1, 2, 2),
  f2 = c(1, 2, 1, 2),
  rp = c(12, 14, 14, 16),
  c = c(10, 20, 5, 15)) 
pars <- c(f1 = .5, f2  = .5, r = 1.5, q = 1.5, lambda = 1)
M <- ebm_j(rp ~ f1 + f2, ~c, data = dt, fix = pars)
tol <- .01 

# 1. Predictive testing
test_that("Prediction identities", {
  expect_equal(M$predict(newdata = dt)[1], calc_pred(dt, pars, TRUE)[1], tol = tol)
  expect_equal(M$predict(newdata = dt)[2], calc_pred(dt, pars, TRUE)[2], tol = tol)
  expect_equal(M$predict(newdata = dt)[3], calc_pred(dt, pars, TRUE)[3], tol = tol)
  expect_equal(M$predict(newdata = dt)[4], calc_pred(dt, pars, TRUE)[4], tol = tol)
  expect_equal(M$predict(newdata = dt), calc_pred(dt, pars, TRUE), tol = tol)
  expect_equal(M$predict()[1], calc_pred(dt, pars, FALSE)[1], tol = tol)
  expect_equal(M$predict()[2], calc_pred(dt, pars, FALSE)[2], tol = tol)
  expect_equal(M$predict()[3], calc_pred(dt, pars, FALSE)[3], tol = tol)
  expect_equal(M$predict()[4], calc_pred(dt, pars, FALSE)[4], tol = tol)
})

# 1.a. Parameter value changes (lower, higher, boundaries)
test_that("Prediction identities after parameter change", {
  expect_parchange_equal <- function(par, value, newdata = NULL) {
    fix <- replace(pars, names(pars) == par, value)
    M <- ebm_j(rp ~ f1 + f2, ~c, data = dt, fix = fix)
    expect_equal(c(M$predict(newdata = newdata)), calc_pred(dt, fix, !is.null(newdata)), tol = tol)
  }
  
  expect_parchange_equal("f1", .001, dt)
  expect_parchange_equal("f1", 1, dt)
  expect_parchange_equal("f1", 0.25, dt)
  expect_parchange_equal("f1", 0.75, dt)
  expect_parchange_equal("f1", .001)
  expect_parchange_equal("f1", 1)
  expect_parchange_equal("f1", 0.25)
  expect_parchange_equal("f1", 0.75)
  
  expect_parchange_equal("f2", .001, dt)
  expect_parchange_equal("f2", 1, dt)
  expect_parchange_equal("f2", 0.25, dt)
  expect_parchange_equal("f2", 0.75, dt)
  expect_parchange_equal("f2", .001)
  expect_parchange_equal("f2", 1)
  expect_parchange_equal("f2", 0.25)
  expect_parchange_equal("f2", 0.75)
  
  expect_parchange_equal("lambda", .001, dt)
  expect_parchange_equal("lambda", 10, dt)
  expect_parchange_equal("lambda", 0.5, dt)
  expect_parchange_equal("lambda", 5, dt)
  expect_parchange_equal("lambda", .001)
  expect_parchange_equal("lambda", 10)
  expect_parchange_equal("lambda", 0.5)
  expect_parchange_equal("lambda", 5)

  expect_parchange_equal("r", 1, dt)
  expect_parchange_equal("r", 2, dt)
  expect_parchange_equal("r", 1.25, dt)
  expect_parchange_equal("r", 1.75, dt)
  expect_parchange_equal("r", 1)
  expect_parchange_equal("r", 2)
  expect_parchange_equal("r", 1.25)
  expect_parchange_equal("r", 1.75)

  expect_parchange_equal("q", 1, dt)
  expect_parchange_equal("q", 2, dt)
  expect_parchange_equal("q", 1.25, dt)
  expect_parchange_equal("q", 1.75, dt)
  expect_parchange_equal("q", 1)
  expect_parchange_equal("q", 2)
  expect_parchange_equal("q", 1.25)
  expect_parchange_equal("q", 1.75)
})

# 1.b. Parameter restrictions
expect_error(ebm_j(rp ~ f1 + f2, ~c, data = dt, fix = list(angle = "size")))
expect_error(ebm_j(rp ~ f1 + f2, ~c, data = dt, fix = list(c(pars[names(pars) != "lambda"], lambda = NA))))

# 1.c. Equal parameters
pars["lambda"] <- pars["r"]
M <- ebm_j(rp ~ f1 + f2, ~c, data = dt, fix = pars)
test_that("Prediction identitites to equal parameters", {
  expect_equal(M$predict(newdata = dt)[1], calc_pred(dt, pars, TRUE)[1], tol = tol)
  expect_equal(M$predict(newdata = dt)[2], calc_pred(dt, pars, TRUE)[2], tol = tol)
  expect_equal(M$predict(newdata = dt)[3], calc_pred(dt, pars, TRUE)[3], tol = tol)
  expect_equal(M$predict(newdata = dt)[4], calc_pred(dt, pars, TRUE)[4], tol = tol)
  expect_equal(M$predict()[1], calc_pred(dt, pars, FALSE)[1], tol = tol)
  expect_equal(M$predict()[2], calc_pred(dt, pars, FALSE)[2], tol = tol)
  expect_equal(M$predict()[3], calc_pred(dt, pars, FALSE)[3], tol = tol)
  expect_equal(M$predict()[4], calc_pred(dt, pars, FALSE)[4], tol = tol)
})

M <- ebm_j(rp ~ f1 + f2, ~c, data = dt, fix = list(f1 = "f2", lambda = "q", r = "q" , q = 1.5))
test_that("Prediction identitites to equal parameters", {
  expect_equal(M$predict(newdata = dt)[1], calc_pred(dt, pars, TRUE)[1], tol = tol)
  expect_equal(M$predict(newdata = dt)[2], calc_pred(dt, pars, TRUE)[2], tol = tol)
  expect_equal(M$predict(newdata = dt)[3], calc_pred(dt, pars, TRUE)[3], tol = tol)
  expect_equal(M$predict(newdata = dt)[4], calc_pred(dt, pars, TRUE)[4], tol = tol)
  expect_equal(M$predict()[1], calc_pred(dt, pars, FALSE)[1], tol = tol)
  expect_equal(M$predict()[2], calc_pred(dt, pars, FALSE)[2], tol = tol)
  expect_equal(M$predict()[3], calc_pred(dt, pars, FALSE)[3], tol = tol)
  expect_equal(M$predict()[4], calc_pred(dt, pars, FALSE)[4], tol = tol)
})

# ToDo: partial feedback, eine Zeile mit criterion auf NA, predictions machen
# ToDo: 3. orthographic mistakes