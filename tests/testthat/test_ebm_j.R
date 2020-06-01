# Juslin, P., Olsson, H., & Olsson, A.-C. (2003). Exemplar effects in categorization and multiple-cue judgment. Journal of Experimental Psychology: General, 132, 133-156. doi: 10.1037/0096-3445.132.1.133
context("ebm")

calc_pred <- function(dt, par, newdata) {
  r <- par["r"]; q <- par["q"]; lambda <- par["lambda"]; w <- par[c("f1", "f2")] # defines parameters
  c_true <- which(!is.na(dt[, "c"]))
  sapply(1:nrow(dt), function(i) {
    p <- unlist(dt[i, 1:2]) # defines probe
    if(!newdata & (i == 1 | length(intersect(c_true, 1:(i-1))) == 0)) {
      return(mean(range(dt[c_true, "c"])))
    } else {
      if(!newdata) dt <- dt[intersect(c_true, 1:(i-1)), ] # use only previously seen exemplars with feedback
      if(newdata) dt <- dt[c_true, ] # use only exemplars with feedback
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
    if(par %in% c("f1", "f2")) fix[grepl("^f", names(fix)) & !grepl(par, names(fix))] <- 1 - value # this is a hack
    M <- ebm_j(rp ~ f1 + f2, ~c, data = dt, fix = fix)
    expect_equal(c(M$predict(newdata = newdata)), calc_pred(dt, fix, !is.null(newdata)), tol = tol)
  }
  
  expect_parchange_equal("f1", .001, dt)
  expect_parchange_equal("f1", .999, dt)
  expect_parchange_equal("f1", 0.25, dt)
  expect_parchange_equal("f1", 0.75, dt)
  expect_parchange_equal("f1", .001)
  expect_parchange_equal("f1", .999)
  expect_parchange_equal("f1", 0.25)
  expect_parchange_equal("f1", 0.75)
  
  expect_parchange_equal("f2", .001, dt)
  expect_parchange_equal("f2", .999, dt)
  expect_parchange_equal("f2", 0.25, dt)
  expect_parchange_equal("f2", 0.75, dt)
  expect_parchange_equal("f2", .001)
  expect_parchange_equal("f2", .999)
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

# 2. Parameter recovery (Albrecht 2019, Exp 1)
data(albrecht2019exp1)
fml <- crit_response ~ f1 + f2 + f3

fit_ebm_j <- function(dt) {
  ebm_j(fml, ~crit_correct, data = dt, options = list(solver = "solnp",
                                                      lb = c(f1 = -10,
                                                             f2 = -10,
                                                             f3 = -10),
                                                      ub = c(f1 = 10,
                                                             f2 = 10,
                                                             f3 = 10)), 
        fix = list(q = 1, r = 1))
}

dt <- albrecht2019exp1[subj <= 41, list(fit_ebm_j = list(fit_ebm_j(dt = .SD))), by = list(subj)]
coefs <- dt[, as.list(coef(fit_ebm_j[[1]])), by = list(subj)]
coefs <- coefs[, -1][, lapply(.SD, mean)]

# 3. Formal tests
# 3.a. One-row test set and test sets with different orders
test_that("Prediction identities data frame", {
  expect_pred_equivalent <- function(data, result, newdata = NULL) {
    M <- ebm_j(rp ~ f1 + f2, ~c, data = as.data.frame(data), fix = pars)
    expect_equivalent(M$predict(newdata = newdata), result, tol=tol)
  }
  
  expect_pred_equivalent(dt[1, ], calc_pred(dt[1, ], pars, FALSE))
  expect_pred_equivalent(dt[2, ], calc_pred(dt[2, ], pars, FALSE))
  expect_pred_equivalent(dt[3, ], calc_pred(dt[3, ], pars, FALSE))
  expect_pred_equivalent(dt[4, ], calc_pred(dt[4, ], pars, FALSE))
  expect_pred_equivalent(dt[1, ], calc_pred(dt[1, ], pars, TRUE), newdata = dt[1, ])
  expect_pred_equivalent(dt[2, ], calc_pred(dt[2, ], pars, TRUE), newdata = dt[2, ])
  expect_pred_equivalent(dt[3, ], calc_pred(dt[3, ], pars, TRUE), newdata = dt[3, ])
  expect_pred_equivalent(dt[4, ], calc_pred(dt[4, ], pars, TRUE), newdata = dt[4, ])
  expect_pred_equivalent(dt, calc_pred(dt, pars, FALSE))
  expect_pred_equivalent(dt, calc_pred(dt, pars, TRUE), newdata = dt)
  expect_pred_equivalent(dt[4:1, ], calc_pred(dt[4:1, ], pars, FALSE))
  expect_pred_equivalent(dt[4:1, ], calc_pred(dt[4:1, ], pars, TRUE), newdata = dt[4:1, ])
  expect_pred_equivalent(dt[c(2,1,4,3), ], calc_pred(dt[c(2,1,4,3), ], pars, FALSE))
  expect_pred_equivalent(dt[c(2,1,4,3), ], calc_pred(dt[c(2,1,4,3), ], pars, TRUE), newdata = dt[c(2,1,4,3), ])
})

test_that("Prediction identities data table", {
  expect_pred_equivalent <- function(data, result, newdata = NULL) {
    M <- ebm_j(rp ~ f1 + f2, ~c, data = as.data.table(data), fix = pars)
    expect_equivalent(M$predict(newdata = newdata), result, tol=tol)
  }
  
  expect_pred_equivalent(dt[1, ], calc_pred(dt[1, ], pars, FALSE))
  expect_pred_equivalent(dt[2, ], calc_pred(dt[2, ], pars, FALSE))
  expect_pred_equivalent(dt[3, ], calc_pred(dt[3, ], pars, FALSE))
  expect_pred_equivalent(dt[4, ], calc_pred(dt[4, ], pars, FALSE))
  expect_pred_equivalent(dt[1, ], calc_pred(dt[1, ], pars, TRUE), newdata = dt[1, ])
  expect_pred_equivalent(dt[2, ], calc_pred(dt[2, ], pars, TRUE), newdata = dt[2, ])
  expect_pred_equivalent(dt[3, ], calc_pred(dt[3, ], pars, TRUE), newdata = dt[3, ])
  expect_pred_equivalent(dt[4, ], calc_pred(dt[4, ], pars, TRUE), newdata = dt[4, ])
  expect_pred_equivalent(dt, calc_pred(dt, pars, FALSE))
  expect_pred_equivalent(dt, calc_pred(dt, pars, TRUE), newdata = dt)
  expect_pred_equivalent(dt[4:1, ], calc_pred(dt[4:1, ], pars, FALSE))
  expect_pred_equivalent(dt[4:1, ], calc_pred(dt[4:1, ], pars, TRUE), newdata = dt[4:1, ])
  expect_pred_equivalent(dt[c(2,1,4,3), ], calc_pred(dt[c(2,1,4,3), ], pars, FALSE))
  expect_pred_equivalent(dt[c(2,1,4,3), ], calc_pred(dt[c(2,1,4,3), ], pars, TRUE), newdata = dt[c(2,1,4,3), ])
})

test_that("Prediction identities tibble", {
  expect_pred_equivalent <- function(data, result, newdata = NULL) {
    M <- ebm_j(rp ~ f1 + f2, ~c, data = as_tibble(data), fix = pars)
    expect_equivalent(M$predict(newdata = newdata), result, tol=tol)
  }
  
  expect_pred_equivalent(dt[1, ], calc_pred(dt[1, ], pars, FALSE))
  expect_pred_equivalent(dt[2, ], calc_pred(dt[2, ], pars, FALSE))
  expect_pred_equivalent(dt[3, ], calc_pred(dt[3, ], pars, FALSE))
  expect_pred_equivalent(dt[4, ], calc_pred(dt[4, ], pars, FALSE))
  expect_pred_equivalent(dt[1, ], calc_pred(dt[1, ], pars, TRUE), newdata = dt[1, ])
  expect_pred_equivalent(dt[2, ], calc_pred(dt[2, ], pars, TRUE), newdata = dt[2, ])
  expect_pred_equivalent(dt[3, ], calc_pred(dt[3, ], pars, TRUE), newdata = dt[3, ])
  expect_pred_equivalent(dt[4, ], calc_pred(dt[4, ], pars, TRUE), newdata = dt[4, ])
  expect_pred_equivalent(dt, calc_pred(dt, pars, FALSE))
  expect_pred_equivalent(dt, calc_pred(dt, pars, TRUE), newdata = dt)
  expect_pred_equivalent(dt[4:1, ], calc_pred(dt[4:1, ], pars, FALSE))
  expect_pred_equivalent(dt[4:1, ], calc_pred(dt[4:1, ], pars, TRUE), newdata = dt[4:1, ])
  expect_pred_equivalent(dt[c(2,1,4,3), ], calc_pred(dt[c(2,1,4,3), ], pars, FALSE))
  expect_pred_equivalent(dt[c(2,1,4,3), ], calc_pred(dt[c(2,1,4,3), ], pars, TRUE), newdata = dt[c(2,1,4,3), ])
})

test_that("Prediction identities matrix", {
  expect_pred_equivalent <- function(data, result, newdata = NULL) {
    M <- ebm_j(rp ~ f1 + f2, ~c, data = as.matrix(data), fix = pars)
    expect_equivalent(M$predict(newdata = newdata), result, tol=tol)
  }
  
  expect_pred_equivalent(dt[1, ], calc_pred(dt[1, ], pars, FALSE))
  expect_pred_equivalent(dt[2, ], calc_pred(dt[2, ], pars, FALSE))
  expect_pred_equivalent(dt[3, ], calc_pred(dt[3, ], pars, FALSE))
  expect_pred_equivalent(dt[4, ], calc_pred(dt[4, ], pars, FALSE))
  expect_pred_equivalent(dt[1, ], calc_pred(dt[1, ], pars, TRUE), newdata = dt[1, ])
  expect_pred_equivalent(dt[2, ], calc_pred(dt[2, ], pars, TRUE), newdata = dt[2, ])
  expect_pred_equivalent(dt[3, ], calc_pred(dt[3, ], pars, TRUE), newdata = dt[3, ])
  expect_pred_equivalent(dt[4, ], calc_pred(dt[4, ], pars, TRUE), newdata = dt[4, ])
  expect_pred_equivalent(dt, calc_pred(dt, pars, FALSE))
  expect_pred_equivalent(dt, calc_pred(dt, pars, TRUE), newdata = dt)
  expect_pred_equivalent(dt[4:1, ], calc_pred(dt[4:1, ], pars, FALSE))
  expect_pred_equivalent(dt[4:1, ], calc_pred(dt[4:1, ], pars, TRUE), newdata = dt[4:1, ])
  expect_pred_equivalent(dt[c(2,1,4,3), ], calc_pred(dt[c(2,1,4,3), ], pars, FALSE))
  expect_pred_equivalent(dt[c(2,1,4,3), ], calc_pred(dt[c(2,1,4,3), ], pars, TRUE), newdata = dt[c(2,1,4,3), ])
})

# 3.b. Formula entry
test_that("ebm_j input errors", {
  expect_error( # wrong order in formula, should be rp ~ f1 + f2
    ebm_j(f1 + f2 ~ rp, ~c, data = dt, fix = pars)
  )
  expect_error( # no choices specified and parameters need to be estimated
    ebm_j(~ f1 + f2, ~c, data = dt)
  )
})

# 3.c. Partial feedback, eine Zeile mit criterion auf NA, predictions machen
dt[c(1, 3), "c"] <- NA
M <- ebm_j(rp ~ f1 + f2, ~c, data = dt, fix = pars)
test_that("Partial feedback", {
  expect_equal(M$predict(newdata = dt)[1], calc_pred(dt, pars, TRUE)[1], tol = tol)
  expect_equal(M$predict(newdata = dt)[2], calc_pred(dt, pars, TRUE)[2], tol = tol)
  expect_equal(M$predict(newdata = dt)[3], calc_pred(dt, pars, TRUE)[3], tol = tol)
  expect_equal(M$predict(newdata = dt)[4], calc_pred(dt, pars, TRUE)[4], tol = tol)
  expect_equal(M$predict(newdata = dt), calc_pred(dt, pars, TRUE), tol = tol)
  expect_equal(M$predict()[1], calc_pred(dt, pars, FALSE)[1], tol = tol)
  expect_equal(M$predict()[2], calc_pred(dt, pars, FALSE)[2], tol = tol)
  expect_equal(M$predict()[3], calc_pred(dt, pars, FALSE)[3], tol = tol)
  expect_equal(M$predict()[4], calc_pred(dt, pars, FALSE)[4], tol = tol)
  expect_equal(M$predict(), calc_pred(dt, pars, FALSE), tol = tol)
})
