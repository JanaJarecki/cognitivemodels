# ==========================================================================
# Test for Cumulative Prospece Theory Cognitive Model
#
# Tversky, A., & Kahneman, D. (1992). Advances in prospect theory: cumulative
#     representation of uncertainty. Journal of Risk and Uncertainty, 5,
#     297–323. doi:10.1007/BF00122574
# ==========================================================================


# 1. Model predictions -----------------------------------------------------
test_that("predictions are correct", {
  par <- c(alpha=0.88, beta=0.88, lambda=2.7, gammap=0.61, gamman=0.69)
  rowSums(cpt_d( ~ x1 + p + y1 | x2 + p2 + y2, data = data.frame(x1 = 21, p = 1, y1 = 0, x2 = 18, p2= 0.5, y2 = c(0,0)), fix = par, weighting = NA, choicerule = "none")$predict(type = "value"))

  cpt_mem_d( ~ x1 + p + x2 | x1 + p + x2, mem = ~mem, data = data.frame(x1 = 9, p = 0.5, x2 = -9, mem = c(30,30)), fix = par, weighting = NA, choicerule = "none")$predict()

  m$input

  dt <- data.frame(
    memory = 0,
    xh = -2000,
    pxh = 1,
    yh = 0)
  par <- c(alpha=0.88, beta=0.88, lambda=2.7, gammap=0.61, gamman=0.69)
  cpt_d( ~ xh + pxh + yh, data = dt, fix = par, choicerule = "none")$predict()


  # Thaler & Johnson (1990), Table 4, problems 1 - 4
  thaler14 <- data.frame(
    id = 1:4,
    memory = c(15, 0, -2.25, -7.5),
    xh = c( 4.5,  2.25,  2.25,  2.25),
    pxh = .5, 
    yh = c(-4.5, -2.25, -2.25, -2.25),
    xl = 0, pxl = 1, yl = 0)
  # One-stage variant of the problems
  thaler14one <- thaler14
  thaler14one[, c("xh","yh","xl")] <- thaler14[,c("xh","yh","xl")] + thaler14[,"memory"]
  thaler14one$memory <- 0
  thaler14 <- rbind(thaler14, thaler14one)

  cpt_d( ~ xh + pxh + yh | xl + pxl + yl, data = thaler14, fix = par, choicerule = "none")$predict()
  M <- cpt_mem_d( ~ xh + pxh + yh | xl + pxl + yl, mem = ~memory, data = thaler14, fix = par, choicerule = "none")
  predict(M)


  dt <- data.frame(memory = 30,
    xh = 9, pxh = .5, yh = -9,
    xl = 0, pxl = 1, yl = 0)
  par <- c(alpha=0.88, beta=0.88, lambda=2, gammap=0.61, gamman=0.69)
  M <- cpt_mem_d( ~ xh + pxh + yh | xl + pxl + yl, mem = ~memory, data = dt, fix = par, choicerule = "none")
  predict(M)
  v <- function(x) {

  }

  dt <- data.frame(
    mem = c(7, -4.333, 7, -4.333),
    xh = c(8, -8, 8, -8), pxh = c(.4,.6,.4,.6), yh = c(2, -2, 2, -2),
    xl = c(5, -7, 5, -7), pxl = c(.7,.3,.7,.3), yl = c(3,-5,3,-5))
  par <- c(alpha=0.88, beta=0.88, lambda=2, gammap=0.61, gamman=0.69)
  tol <- .001

  # Two two-outcome stimuli
  M <- cpt_mem_d( ~ xh + pxh + yh | xl + pxl + yl, data = dt, mem = ~ mem, fix = par, choicerule = "none", weighting = NA)
  predict(M)


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

# 1.b. Parameter restrictions

# 1.c. Equal parameters

# 2. Parameter recovery ----------------------------------------------------

# 3. Formal tests -----------------------------------------------------------
# 3.a. One-row data and shuffled-row data

# 3.b. Formula entry ----------------------------------------------------------

# 3.c Errors ---------------------------------------------------------------
