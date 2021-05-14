# ==========================================================================
# Test for Cumulative Prospece Theory Cognitive Model
#
# Tversky, A., & Kahneman, D. (1992). Advances in prospect theory: cumulative
#     representation of uncertainty. Journal of Risk and Uncertainty, 5,
#     297–323. doi:10.1007/BF00122574
# ==========================================================================


# 1. Model predictions -----------------------------------------------------
test_that("Prediction identities to Tversky & Kahneman (1992)", {
  # Tversky & Kahneman, 1992, pp. 313
  dt <- data.frame(rp = 1,
    x1 = c(100, -100), px = 1, x2 = 0,
    y1 = c(200, -200), py = c(.71, .64), y2 = 0,
    z1 = c(300, -300), pz = c(.42, .38), z2 = 0)
  tk_par <- c(alpha=0.88, beta=0.88, lambda=2.25, gammap=0.61, gamman=0.69)
  tol <- .001

  # Two two-outcome stimuli
  M <- cpt_d(rp ~ x1 + px + x2 | y1 + py + y2, , data = dt, fix = tk_par, choicerule = "none")
  expect_equal(M$predict('value')[1,'pr_x'], c('pr_x'=57), tol = .01)
  expect_equal(M$predict('value')[1,'pr_y'], c('pr_y'=57), tol = .01)
  expect_equal(M$predict('value')[2,'pr_x'], c('pr_x'=-129), tol = .01)
  expect_equal(M$predict('value')[2,'pr_y'], c('pr_y'=-129), tol = .01)

  # Three two-ourcome stimuli
  M <- cpt_d(rp ~ x1 + px + x2 | y1 + py + y2 | z1 + pz + z2, data = dt, fix = tk_par, choicerule = "none")
  expect_equal(M$predict('value')[1,'pr_x'], c('pr_x'=57), tol = .01)
  expect_equal(M$predict('value')[1,'pr_y'], c('pr_y'=57), tol = .01)
  expect_equal(M$predict('value')[1,'pr_z'], c('pr_z'=57), tol = .01)
  expect_equal(M$predict('value')[2,'pr_x'], c('pr_x'=-129), tol = .01)
  expect_equal(M$predict('value')[2,'pr_y'], c('pr_y'=-129), tol = .01)
  expect_equal(M$predict('value')[2,'pr_z'], c('pr_z'=-129), tol = .01)

  # Two Three-outcome stimuli
  dt <- data.frame(rp = 1,
    x1 = c(100, -100), px1 =   0.4,       x2 = 50, px2 = 0.4, x3 = 0,
    y1 = c(200, -200), py1 = c(.71, .64), y2 = 0,  py2 = 0.1, y3 = 30,
    z1 = c(300, -300), pz1 = c(.42, .38), z2 = 0,  pz2 = .22, z3 = 100)
  M <- cpt_d(rp ~ x1 + px1 + x2 + px2 + x3 | y1 + py1 + y2 + py2 + y3, data = dt[1,], fix = tk_par, choicerule = "none")
  expect_equal(M$predict("value"), c(pr_x = 28.716, pr_y = 60.6477), tol = tol)

  # Three three-outcome stimuli
  M <- cpt_d(rp ~ x1 + px1 + x2 + px2 + x3 | y1 + py1 + y2 + py2 + y3 | z1 + pz1 + z2 + pz2 + z3, data = dt[1,], fix = tk_par, choicerule = "none")
  expect_equal(M$predict("value"), c(pr_x = 28.716, pr_y = 60.6477, pr_z = 69.6543), tol = tol)
})


# 1.a. Parameter value changes (lower, higher, boundaries)
test_that("Prediction identities after parameter change", {
  expect_parchange_equal <- function(par, value, result) {
    dt <- data.frame(rp = 1,
      x1 = c(100, -100), px = 1,           x2 = 0,
      y1 = c(200, -200), py = c(.71, .64), y2 = 0)
    tk_par <- c(alpha=0.88, beta=0.88, lambda=2.25, gammap=0.61, gamman=0.69)
    fix <- replace(tk_par, names(tk_par) == par, value)
    M <- cpt_d(rp ~ x1 + px + x2 | y1 + py + y2, , data = dt, fix = fix, choicerule = "none")
    tol <- .01
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
# 1.c. Equal parameters
test_that("Prediction identitites with equal parameters", {
  dt <- data.frame(rp = 1,
      x1 = c(100, -100), px = 1,           x2 = 0,
      y1 = c(200, -200), py = c(.71, .64), y2 = 0)
  expect_equ_equal <- function(x, target, tol = 0.001, d = dt) {
    tk_par <- list(alpha=0.88, beta=0.88, lambda=2.25,gammap=0.61,gamman=0.69)
    fix <- replace(tk_par, names(tk_par) %in% names(x), x)
    M <- cpt_d(rp ~ x1 + px + x2 | y1 + py + y2, data = d, fix = fix, choicerule = "none")
    expect_equal(M$predict("value"), target, tol = tol)
  }

  # Equality restrictions
  expect_equ_equal(x = list(gammap = "gamman"),
    cbind(pr_x = c(57.5, -129.5), pr_y = c(63, -129.9)))
  expect_equ_equal(x = list(alpha = "beta", gammap = "gamman"),
    cbind(pr_x = c(57.5, -129.5), pr_y = c(63, -129.9)))

  # Errors on wrong restriction specification
  expect_error(cpt_d(rp ~ x1 + px + x2 | y1 + py + y2, data = dt, fix = list(angle = "size"), choicerule = "none"))
  expect_error(cpt_d(rp ~ x1 + px + x2 | y1 + py + y2, , data = dt, fix = list(c(tk_par[names(tk_par) != "lambda"], lambda = NA)), choicerule = "none"))
})


# 2. Parameter recovery ----------------------------------------------------
test_that("Parameter recovery equal to Pachur et al.", {
  # Skip the tests below skip() (takes too long)
  # Uncomment for running the test
  skip("Skipping parameter estimation tests")
  data(cpt_dtest)
  cpt_dtest[, decision := 1 - (decision - 1)]
  fml <- decision ~ o1 + p1 + o2 + p2 | o3 + p3 + o4 + p4

  fit_cpt_d_5_g2 <- function(dt) {
    cpt_d(fml, , data = dt, choicerule = "softmax", options = list(ub = c(tau = 25), solver = "solnp"), fix = list(alpha = "beta"))
  }
  fit_cpt_d_4_g2 <- function(dt) {
    cpt_d(fml, , data = dt, choicerule = "softmax", options = list(ub = c(tau = 25), solver = "solnp"), fix = list(alpha = "beta", gammap = "gamman"))
  }
  fit_cpt_d_4_pc <- function(dt) {
    cpt_d(fml, , data = dt, choicerule = "argmax", options = list(fit_measure = "accuracy", solver = "solnp"), fix = list(alpha = "beta"))
  }
  fit_cpt_d_3_pc <- function(dt) {
    cpt_d(fml, , data = dt, choicerule = "argmax", options = list(fit_measure = "accuracy", solver = "solnp"), fix = list(alpha = "beta", gammap = "gamman"))
  }
  gp2012 <- cpt_dtest[, list(fit_5_g2 = list(fit_cpt_d_5_g2(dt = .SD)),
                         fit_4_g2 = list(fit_cpt_d_4_g2(dt = .SD)),
                         fit_4_pc = list(fit_cpt_d_4_pc(dt = .SD)),
                         fit_3_pc = list(fit_cpt_d_3_pc(dt = .SD))), by = list(repetition, subject)]

  fit_5_g2 <- gp2012[, as.list(coef(fit_5_g2[[1]])), by = list(repetition, subject)][, -2][, lapply(.SD, median), by = repetition]
  fit_4_g2 <- gp2012[, as.list(coef(fit_4_g2[[1]])), by = list(repetition, subject)][, -2][, lapply(.SD, median), by = repetition]
  fit_4_pc <- gp2012[, as.list(coef(fit_4_pc[[1]])), by = list(repetition, subject)][, -2][, lapply(.SD, median), by = repetition]
  fit_3_pc <- gp2012[, as.list(coef(fit_3_pc[[1]])), by = list(repetition, subject)][, -2][, lapply(.SD, median), by = repetition]
  fit_5_g2[, tau := 1/tau]
  fit_4_g2[, tau := 1/tau]
  ll_5_g2 <- gp2012[, .(ll = logLik(fit_5_g2[[1]])), by = list(repetition, subject)][, -2][, lapply(.SD, median), by = repetition]
  ll_4_g2 <- gp2012[, .(ll = logLik(fit_4_g2[[1]])), by = list(repetition, subject)][, -2][, lapply(.SD, median), by = repetition]
  expect_equal(unlist(round(fit_5_g2[repetition == 1, -1], 2)), c(alpha = 0.74, beta = 0.74, gammap = 0.61, gamman = 0.89, lambda = 1.27, tau = 0.06), tol = tol)
  expect_equal(unlist(round(fit_5_g2[repetition == 2, -1], 2)), c(alpha = 0.76, beta = 0.76, gammap = 0.58, gamman = 0.89, lambda = 1.19, tau = 0.06), tol = tol)
  expect_equal(unlist(round(fit_4_g2[repetition == 1, -1], 2)), c(alpha = 0.71, beta = 0.71, gammap = 0.72, gamman = 0.72, lambda = 1.35, tau = 0.08), tol = tol)
  expect_equal(unlist(round(fit_4_g2[repetition == 2, -1], 2)), c(alpha = 0.70, beta = 0.70, gammap = 0.70, gamman = 0.70, lambda = 1.31, tau = 0.11), tol = tol)
  expect_equal(unlist(round(fit_4_pc[repetition == 1, -1], 2)), c(alpha = 0.68, beta = 0.68, gammap = 0.74, gamman = 0.83, lambda = 1.55), tol = tol)
  expect_equal(unlist(round(fit_4_pc[repetition == 2, -1], 2)), c(alpha = 0.74, beta = 0.74, gammap = 0.65, gamman = 0.85, lambda = 1.55), tol = tol)
  expect_equal(unlist(round(fit_3_pc[repetition == 1, -1], 2)), c(alpha = 0.64, beta = 0.64, gammap = 0.77, gamman = 0.77, lambda = 1.66), tol = tol)
  expect_equal(unlist(round(fit_3_pc[repetition == 2, -1], 2)), c(alpha = 0.67, beta = 0.67, gammap = 0.69, gamman = 0.69, lambda = 1.80), tol = tol)
  expect_equal(-2 * c(ll_5_g2$ll), c(136.8, 132.7), tol = tol)
  expect_equal(-2 * c(ll_4_g2$ll), c(155.8, 150.4), tol = tol)
})


# 3. Formal tests -----------------------------------------------------------
# 3.a. One-row data and shuffled-row data
test_that("Formal test - single-row data and shuffled data", {
  expect_pred_equivalent <- function(data, result, tol = .001) {
    tk_par <- c(alpha=0.88, beta=0.88, lambda=2.25, gammap=0.61, gamman=0.69) 
    M <- cpt_d(~x1+p+x2, data, fix = tk_par, choicerule = "none")
    expect_equivalent(M$predict(), result, tol=tol)
  }
  D <- data.frame(x1=c(1,2,2),x2=c(2,1,1),p=c(0.66,0.66,0.50))
  expect_pred_equivalent(D[1, ], 1.285)
  expect_pred_equivalent(D[2, ], 1.427)
  expect_pred_equivalent(D, c(1.285, 1.427, 1.353))
  expect_pred_equivalent(D[3:1, ], c(1.353, 1.427, 1.285))
  expect_pred_equivalent(D[c(2,1,3), ], c(1.427, 1.285, 1.353))
})

test_that("Formal test - data.table", {
  skip("Skipping data.table tests to avoid dependency")
  expect_pred_equivalent <- function(data, result) {
    M <- cpt_d(~x1+p+x2, L, data = as.data.table(data), fix = tk_par, choicerule = "none")
    expect_equivalent(M$predict(), result, tol=tol)
  }
  expect_pred_equivalent(D[1, ], 1.285)
  expect_pred_equivalent(D[2, ], 1.427)
  expect_pred_equivalent(D, c(1.285, 1.427, 1.353))
  expect_pred_equivalent(D[3:1, ], c(1.353, 1.427, 1.285))
  expect_pred_equivalent(D[c(2,1,3), ], c(1.427, 1.285, 1.353))
})


test_that("Formal test - tibble", {
  skip("Skipping tibble tests to avoid dependency")
  expect_pred_equivalent <- function(data, result) {
    tk_par <- c(alpha=0.88, beta=0.88, lambda=2.25, gammap=0.61, gamman=0.69) 
    D <- data.frame(x1=c(1,2,2),x2=c(2,1,1),p=c(0.66,0.66,0.50))
    M <- cpt_d(~x1+p+x2, L, data = as_tibble(data), fix = tk_par, choicerule = "none")
    expect_equivalent(M$predict(), result, tol=tol)
  }  
  expect_pred_equivalent(D[1, ], 1.285)
  expect_pred_equivalent(D[2, ], 1.427)
  expect_pred_equivalent(D[3, ], 1.353)
  expect_pred_equivalent(D, c(1.285, 1.427, 1.353))
  expect_pred_equivalent(D[3:1, ], c(1.353, 1.427, 1.285))
  expect_pred_equivalent(D[c(2,1,3), ], c(1.427, 1.285, 1.353))
})


# 3.b. Formula entry ----------------------------------------------------------
test_that("Formula formats", {
  expect_fml_equal <- function(f, data) {
    tk_par <- c(alpha=0.88, beta=0.88, lambda=2.25, gammap=0.61, gamman=0.69) 
    target <- cpt_d(rp ~ x1 + px + x2 + I(1-px) | y1 + py + y2 + I(1-py), data = data, fix = tk_par, choicerule = "none")$predict()
    M <- cpt_d(f, data = data, fix = tk_par, choicerule = "none")
    expect_equal(M$predict(), target)
  }
  dt <- data.frame(rp = 1,
      x1 = c(100, -100), px = 1,           x2 = 0,
      y1 = c(200, -200), py = c(.71, .64), y2 = 0)

  expect_fml_equal(f = rp ~ x1 + px + x2           | y1 + py + y2, dt)
  expect_fml_equal(rp ~ x1 + px + x2 + I(1-px) | y1 + py + y2, dt)
  expect_fml_equal(rp ~ x1 + px + x2           | y1 + py + y2 + I(1-py), dt)

  expect_fml_equal(rp ~ x1 + px + x2           | y1 + py + y2, dt[1,])
  expect_fml_equal(rp ~ x1 + px + x2 + I(1-px) | y1 + py + y2, dt[1,])
  expect_fml_equal(rp ~ x1 + px + x2          | y1 + py + y2 + I(1-py), dt[1,])
})


# 3.c Errors ---------------------------------------------------------------
test_that("cpt_d errors", {
  dt <- data.frame(rp = 1,
      x1 = c(100, -100), px = 1,           x2 = 0,
      y1 = c(200, -200), py = c(.71, .64), y2 = 0)

  # Errors: Probabilities don't sum to 1
  expect_error( 
    cpt_d(rp ~ x1 + px + x2 + I(1-py) | y1 + py + y2, data = dt, choicerule = "none"))
  expect_error(
    cpt_d(rp ~ x1 + px + x2 | y1 + py + y2 + I(1-px), data = dt, choicerule = "none"))
  expect_error(
    cpt_d(rp ~ x1 + px + x2 + I(1-py) | y1 + py + y2 + I(1-px), data = dt, choicerule = "none"))
  expect_error( # wrong order, should be x1 + px + x2
    cpt_d(rp ~ x2 + x1 + px, ref=0, data = dt, choicerule = "none")
    )
  expect_error( # Wrong order in the first of two RHS stimuli
    cpt_d(rp ~ x2 + x1 + px | y1 + py + y2, ref=0, data = dt, choicerule = "none")
    )
  expect_error( # Wrong order with last probability submitted
    cpt_d(rp ~ x2 + x1 + px + I(1-px), ref=0, data = dt, choicerule = "none")
    )
  expect_error( 
    cpt_d(rp ~ x2 + x1 + px + I(1-px) | y1 + py + y2, ref=0, data = dt, choicerule = "none")
    )
  expect_error( # Error in only the second of two RHS
    cpt_d(rp ~ x1 + px + x2 | y2 + y1 + py, ref=0, data = dt, choicerule = "none")
    )
  expect_error( 
    cpt_d(rp ~ x1 + px + x2 + I(1-px) | y2 + y1 + py + I(1-py), ref=0, data = dt, choicerule = "none")
    )
  expect_error( # Error in both RHSs
    cpt_d(rp ~ x2 + x1 + px | y2 + y1 + py, ref=0, data = dt, choicerule = "none")
    )
  expect_error(
    cpt_d(rp ~ x2 + x1 + px + I(1-px) | y2 + y1 + py + I(1-py), ref=0, data = dt, choicerule = "none")
    )
  expect_error( # matrix
    cpt_d(rp ~ x1 + px + x2, ref=0L, data=as.matrix(dt[1,]), fix=tk_par, choicerule = "none")
    )
  expect_error(
    cpt_d(rp ~ x1 + px + x2, ref=0L, data=as.matrix(dt), fix=tk_par, choicerule = "none")
    )
})