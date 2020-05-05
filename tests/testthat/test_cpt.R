# Tversky, A., & Kahneman, D. (1992). Advances in prospect theory: cumulative representation of uncertainty. Journal of Risk and Uncertainty, 5, 297–323. doi:10.1007/BF00122574

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
M <- cpt(rp ~ x1 + px + x2 | y1 + py + y2, ref = 0, data = dt, fix = tk_par, choicerule = "none")
tol <- .01 
# todo: gemischtes Gamble

# 1. Predictive testing
test_that("Prediction identities to Tversky & Kahneman (1992)", {
  expect_equal(M$predict('value')[1,'pr_x'], c('pr_x'=57), tol = tol)
  expect_equal(M$predict('value')[1,'pr_y'], c('pr_y'=57), tol = tol)
  expect_equal(M$predict('value')[2,'pr_x'], c('pr_x'=-129), tol = tol)
  expect_equal(M$predict('value')[2,'pr_y'], c('pr_y'=-129), tol = tol)
})

# 1.a. Parameter value changes (lower, higher, boundaries)
test_that("Prediction identities after parameter change", {
  expect_parchange_equal <- function(par, value, result) {
    fix <- replace(tk_par, names(tk_par) == par, value)
    M <- cpt(rp ~ x1 + px + x2 | y1 + py + y2, ref = 0, data = dt, fix = fix, choicerule = "none")
    expect_equal(c(M$predict('value')), result, tol = tol)
  }

  expect_parchange_equal("alpha", .001, c(1, -129.47, 0.54, -129.87))
  expect_parchange_equal("alpha", 2, c(10000, -129.47, 21626, -129.87))
  expect_parchange_equal("alpha", 0.44, c(7.58, -129.47, 5.56, -129.87))
  expect_parchange_equal("alpha", 1.44, c(758.58, -129.47, 1112.26, -129.87))

  expect_parchange_equal("beta", .001, c(57.4, -2.26, 57.23, -1.23))
  expect_parchange_equal("beta", 2, c(57.4, -22500, 57.23, -49053.55))
  expect_parchange_equal("beta", 0.44, c(57.4, -17.07, 57.23, -12.62))
  expect_parchange_equal("beta", 1.44, c(57.4, -1706.80, 57.23, -2524.03))
  
  expect_parchange_equal("gamman", .001, c(57.4, -129.47, 57.23, 0))
  expect_parchange_equal("gamman", 2, c(57.4, -129.47, 57.23, -132.92))
  expect_parchange_equal("gamman", 0.345, c(57.4, -129.47, 57.23, -56.27))
  expect_parchange_equal("gamman", 1.345, c(57.4, -129.47, 57.23, -154.08))
  
  expect_parchange_equal("gammap", .001, c(57.4, -129.47, 0, -129.87))
  expect_parchange_equal("gammap", 2, c(57.4, -129.47, 69.61, -129.87))
  expect_parchange_equal("gammap", 0.305, c(57.4, -129.47, 21.01, -129.87))
  expect_parchange_equal("gammap", 1.305, c(57.4, -129.47, 77.53, -129.87))
  
  expect_parchange_equal("lambda", .001, c(57.4, -0.06, 57.23, -0.06))
  expect_parchange_equal("lambda", 10, c(57.4, -575.44, 57.23, -577.21))
  expect_parchange_equal("lambda", 1.125, c(57.4, -64.77, 57.23, -64.97))
  expect_parchange_equal("lambda", 6.125, c(57.4, -352.46, 57.23, -353.54))
})

# 1.b. Parameter restrictions
expect_error(cpt(rp ~ x1 + px + x2 | y1 + py + y2, ref = 0, data = dt, fix = list(angle = "size"), choicerule = "none"))
expect_error(cpt(rp ~ x1 + px + x2 | y1 + py + y2, ref = 0, data = dt, fix = list(c(tk_par[names(tk_par) != "lambda"], lambda = NA)), choicerule = "none"))

# 1.c. Equal parameters
tk_par["gammap"] = tk_par["gamman"]
M <- cpt(rp ~ x1 + px + x2 | y1 + py + y2, ref = 0, data = dt, fix = tk_par, choicerule = "none")
test_that("Prediction identitites to Tversky & Kahneman (1992)", {
  expect_equal(M$predict('value')[1,'pr_x'], c('pr_x'=57.5), tol = tol)
  expect_equal(M$predict('value')[1,'pr_y'], c('pr_y'=63), tol = tol)
  expect_equal(M$predict('value')[2,'pr_x'], c('pr_x'=-129.5), tol = tol)
  expect_equal(M$predict('value')[2,'pr_y'], c('pr_y'=-129.9), tol = tol)
})

M <- cpt(rp ~ x1 + px + x2 | y1 + py + y2, ref = 0, data = dt, choicerule = "none", fix = list(alpha = "beta", beta = 0.88, lambda = 2.25, gammap = "gamman", gamman = 0.69))
test_that("Prediction identitites to Tversky & Kahneman (1992)", {
  expect_equal(M$predict('value')[1,'pr_x'], c('pr_x'=57.5), tol = tol)
  expect_equal(M$predict('value')[1,'pr_y'], c('pr_y'=63), tol = tol)
  expect_equal(M$predict('value')[2,'pr_x'], c('pr_x'=-129.5), tol = tol)
  expect_equal(M$predict('value')[2,'pr_y'], c('pr_y'=-129.9), tol = tol)
})


# 2. Parameter recovery
data(cpttest)
cpttest[, decision := decision - 1]
fml <- decision ~ o1 + p1 + o2 + p2 | o3 + p3 + o4 + p4

fit_cpt_soft <- function(dt) {
  model <- cpt(fml, ref = 0, data = dt, choicerule = "softmax", options = list(ub = c(tau = 25), solver = "solnp"), fix = list(alpha = "beta"))
}
fit_cpt_arg <- function(dt) {
  model <- cpt(fml, ref = 0, data = dt, choicerule = "argmax", options = list(fit_measure = "accuracy", solver = "solnp"), fix = list(alpha = "beta"))
}
res <- cpttest[subject == 1, list(fit_soft = list(fit_cpt_soft(dt = .SD),
                                                  fit_cpt_arg( dt = .SD))), by = list(repetition, subject)]

test_that("Parameter estimates == estimates in paper", {
  expect_equal(model$coef(), c(alpha = 0.74, beta=0.74, gammap = 0.61, gamman = 0.89, lambda = 1.27, tau = 1/0.06), tol = tol)
  expect_equal(-2 * model$logLik(), 136.8, tol = tol)
})

# 3. Formal tests
# 3.a. One-row test set and test sets with different orders
tk_par <- c(alpha = 0.88, beta = 0.88, lambda = 2.25, gammap = 0.61, gamman = 0.69) 
D <- data.frame(x1=c(1,2,2),x2=c(2,1,1),p=c(0.66,0.66,0.50))

test_that("Prediction identities data frame", {
  expect_pred_equivalent <- function(data, result) {
    M <- cpt(~x1+p+x2, ref = 0L, data = as.data.frame(data), fix = tk_par, choicerule = "none")
    expect_equivalent(M$predict(), result, tol=tol)
  }

  expect_pred_equivalent(D[1, ], 1.285)
  expect_pred_equivalent(D[2, ], 1.427)
  expect_pred_equivalent(D[3, ], 1.353)
  expect_pred_equivalent(D, c(1.285, 1.427, 1.353))
  expect_pred_equivalent(D[3:1, ], c(1.353, 1.427, 1.285))
  expect_pred_equivalent(D[c(2,1,3), ], c(1.427, 1.285, 1.353))
})

# test_that("Prediction identities data table", {
#   expect_pred_equivalent <- function(data, result) {
#     M <- cpt(~x1+p+x2, ref = 0L, data = as.data.table(data), fix = tk_par, choicerule = "none")
#     expect_equivalent(M$predict(), result, tol=tol)
#   }
  
#   expect_pred_equivalent(D[1, ], 1.285)
#   expect_pred_equivalent(D[2, ], 1.427)
#   expect_pred_equivalent(D[3, ], 1.353)
#   expect_pred_equivalent(D, c(1.285, 1.427, 1.353))
#   expect_pred_equivalent(D[3:1, ], c(1.353, 1.427, 1.285))
#   expect_pred_equivalent(D[c(2,1,3), ], c(1.427, 1.285, 1.353))
# })

# test_that("Prediction identities tibble", {
#   expect_pred_equivalent <- function(data, result) {
#     M <- cpt(~x1+p+x2, ref = 0L, data = as_tibble(data), fix = tk_par, choicerule = "none")
#     expect_equivalent(M$predict(), result, tol=tol)
#   }
  
#   expect_pred_equivalent(D[1, ], 1.285)
#   expect_pred_equivalent(D[2, ], 1.427)
#   expect_pred_equivalent(D[3, ], 1.353)
#   expect_pred_equivalent(D, c(1.285, 1.427, 1.353))
#   expect_pred_equivalent(D[3:1, ], c(1.353, 1.427, 1.285))
#   expect_pred_equivalent(D[c(2,1,3), ], c(1.427, 1.285, 1.353))
# })

# 3.b. Formula entry
test_that("Probability entry", {
  M1 <- cpt(rp ~ x1 + px + x2           | y1 + py + y2          , ref = 0, data = dt, fix = tk_par, choicerule = "none")$predict()
  M2 <- cpt(rp ~ x1 + px + x2           | y1 + py + y2 + I(1-py), ref = 0, data = dt, fix = tk_par, choicerule = "none")$predict()
  M3 <- cpt(rp ~ x1 + px + x2 + I(1-px) | y1 + py + y2          , ref = 0, data = dt, fix = tk_par, choicerule = "none")$predict()
  M4 <- cpt(rp ~ x1 + px + x2 + I(1-px) | y1 + py + y2 + I(1-py), ref = 0, data = dt, fix = tk_par, choicerule = "none")$predict()
  expect_equal(M1, M2); expect_equal(M1, M3); expect_equal(M2, M3); expect_equal(M1, M4); expect_equal(M2, M4); expect_equal(M3, M4)

  M1 <- cpt(rp ~ x1 + px + x2           | y1 + py + y2          , ref = 0, data = dt[1, ], fix = tk_par, choicerule = "none")$predict()
  M2 <- cpt(rp ~ x1 + px + x2           | y1 + py + y2 + I(1-py), ref = 0, data = dt[1, ], fix = tk_par, choicerule = "none")$predict()
  M3 <- cpt(rp ~ x1 + px + x2 + I(1-px) | y1 + py + y2          , ref = 0, data = dt[1, ], fix = tk_par, choicerule = "none")$predict()
  M4 <- cpt(rp ~ x1 + px + x2 + I(1-px) | y1 + py + y2 + I(1-py), ref = 0, data = dt[2, ], fix = tk_par, choicerule = "none")$predict()
  expect_equal(M1, M2); expect_equal(M1, M3); expect_equal(M2, M3)
  expect_false(isTRUE(all.equal(M1, M4))); expect_false(isTRUE(all.equal(M2, M4))); expect_false(isTRUE(all.equal(M3, M4))) 
  
  expect_error( # probabilities don't sum to 1
    cpt(rp ~ x1 + px + x2 + I(1-py) | y1 + py + y2, ref = 0, data = dt, fix = tk_par, choicerule = "none")
  )
  expect_error(
    cpt(rp ~ x1 + px + x2 | y1 + py + y2 + I(1-px), ref = 0, data = dt, fix = tk_par, choicerule = "none")
  )
  expect_error(
    cpt(rp ~ x1 + px + x2 + I(1-py) | y1 + py + y2 + I(1-px), ref = 0, data = dt, fix = tk_par, choicerule = "none")
  )
})

test_that("CPT errors", {
  expect_error( # wrong order, should be x1 + px + x2
    cpt(rp ~ x2 + x1 + px, ref=0, data = dt, choicerule = "none")
    )
  expect_error( # Wrong order in the first of two RHS stimuli
    cpt(rp ~ x2 + x1 + px | y1 + py + y2, ref=0, data = dt, choicerule = "none")
    )
  expect_error( # Wrong order with last probability submitted
    cpt(rp ~ x2 + x1 + px + I(1-px), ref=0, data = dt, choicerule = "none")
    )
  expect_error( 
    cpt(rp ~ x2 + x1 + px + I(1-px) | y1 + py + y2, ref=0, data = dt, choicerule = "none")
    )
  expect_error( # Error in only the second of two RHS
    cpt(rp ~ x1 + px + x2 | y2 + y1 + py, ref=0, data = dt, choicerule = "none")
    )
  expect_error( 
    cpt(rp ~ x1 + px + x2 + I(1-px) | y2 + y1 + py + I(1-py), ref=0, data = dt, choicerule = "none")
    )
  expect_error( # Error in both RHSs
    cpt(rp ~ x2 + x1 + px | y2 + y1 + py, ref=0, data = dt, choicerule = "none")
    )
  expect_error(
    cpt(rp ~ x2 + x1 + px + I(1-px) | y2 + y1 + py + I(1-py), ref=0, data = dt, choicerule = "none")
    )
  expect_error( # matrix
    cpt(rp ~ x1 + px + x2, ref=0L, data=as.matrix(dt[1,]), fix=tk_par, choicerule = "none")
    )
  expect_error(
    cpt(rp ~ x1 + px + x2, ref=0L, data=as.matrix(dt), fix=tk_par, choicerule = "none")
    )
})

# 3.c. Tests with > 2 options per gamble
dt <- data.frame(
  x1 = c(100, -100),
  px = 1,
  x2 = 0,
  y1 = c(200, -200),
  py = c(.71, .64),
  y2 = 0,
  z1 = c(300, -300), 
  pz = c(.42, .38),
  z2 = 0,
  rp = 1) 
M <- cpt(rp ~ x1 + px + x2 | y1 + py + y2 | z1 + pz + z2, ref = 0, data = dt, fix = tk_par, choicerule = "none")
test_that("Prediction identitites", {
  expect_equal(M$predict('value')[1,'pr_x'], c('pr_x'=57), tol = tol)
  expect_equal(M$predict('value')[1,'pr_y'], c('pr_y'=57), tol = tol)
  expect_equal(M$predict('value')[1,'pr_z'], c('pr_z'=57), tol = tol)
  expect_equal(M$predict('value')[2,'pr_x'], c('pr_x'=-129), tol = tol)
  expect_equal(M$predict('value')[2,'pr_y'], c('pr_y'=-129), tol = tol)
  expect_equal(M$predict('value')[2,'pr_z'], c('pr_z'=-129), tol = tol)
})

# 3.c. Tests with > 2 outcomes per option
dt <- data.frame(
  x1 = c(100, -100),
  px1 = .4,
  x2 = 0,
  px2 = .4,
  x3 = c(50, -50),
  y1 = c(200, -200),
  py1 = .4,
  y2 = 0,
  py2 = .4,
  y3 = c(100, -100),
  rp = 1)
expect_error(cpt(rp ~ x1 + px1 + x2 + px2 + x3 | y1 + py1 + y2 + py2 + y3, ref = 0, data = dt, fix = tk_par, choicerule = "none")$predict())
