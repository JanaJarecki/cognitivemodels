# Tversky, A., & Kahneman, D. (1992). Advances in prospect theory: cumulative representation of uncertainty. Journal of Risk and Uncertainty, 5, 297–323. doi:10.1007/BF00122574
context("cpt")
# library(cogsciMs)
library(dplyr)

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
M <- cpt(rp ~ x1 + px + x2 | y1 + py + y2, ref = 0, data = dt, fix = tk_par)
tol <- .01 

# 1. Predictive testing
test_that("Prediction identitites to Tversky & Kahneman (1992)", {
  expect_equal(M$predict('value')[1,'pr_x'], c('pr_x'=57), tol = tol)
  expect_equal(M$predict('value')[1,'pr_y'], c('pr_y'=57), tol = tol)
  expect_equal(M$predict('value')[2,'pr_x'], c('pr_x'=-129), tol = tol)
  expect_equal(M$predict('value')[2,'pr_y'], c('pr_y'=-129), tol = tol)
})

# 1.a. Parameter value changes (lower, higher, boundaries)
tk_par <- tk_par[sort(names(tk_par))]
pars <- M$parspace[, 1:2]
pars <- pars[sort(rownames(pars)), ]
pars <- data.frame(pars, (pars + tk_par)/2)

solutions <- array(data = c(1, 10000, 7.58, 758.58, rep(57.4, 16),
                            0.54, 21616, 5.56, 1112.26, rep(57.23, 8), 0, 69.61, 21.01, 77.53, rep(57.23, 4),
                            rep(-129.47, 4), -2.26, -22500, -17.07, -1706.8, rep(-129.47, 8), -0.06, -575.44, -64.77, -352.46,
                            rep(-129.87, 4), -1.23, -49053.55, -12.62, -2524.03, 0, -132.92, -56.27, -154.08, rep(-129.87, 4), -0.06, -577.21, -64.97, -353.54), 
                   dim = c(4, 5, 4), dimnames = list(c("lb", "ub", "lb.1", "ub.1"), names(tk_par), c("x1", "y1", "x2", "y2")))

apply(as.matrix(expand.grid(1:length(tk_par), 1:ncol(pars))), 1, function(i) {
  fix <- tk_par
  fix[i[1]] <- pars[i[1], i[2]]
  M <- cpt(rp ~ x1 + px + x2 | y1 + py + y2, ref = 0, data = dt, fix = fix)
  test_that("Prediction identitites to Tversky & Kahneman (1992)", {
    expect_equal(M$predict('value')[1,'pr_x'], c('pr_x' = solutions[i[2], i[1], 1]), tol = tol)
    expect_equal(M$predict('value')[1,'pr_y'], c('pr_y' = solutions[i[2], i[1], 2]), tol = tol)
    expect_equal(M$predict('value')[2,'pr_x'], c('pr_x' = solutions[i[2], i[1], 3]), tol = tol)
    expect_equal(M$predict('value')[2,'pr_y'], c('pr_y' = solutions[i[2], i[1], 4]), tol = tol)
  })
})

# 1.b. Parameter restrictions
expect_error(cpt(rp ~ x1 + px + x2 | y1 + py + y2, ref = 0, data = dt, fix = list(angle = "size")))
expect_error(cpt(rp ~ x1 + px + x2 | y1 + py + y2, ref = 0, data = dt, fix = list(c(tk_par[names(tk_par) != "lambda"], lambda = NA))))

# 1.c. Equal parameters
tk_par["gammap"] = tk_par["gamman"]
M <- cpt(rp ~ x1 + px + x2 | y1 + py + y2, ref = 0, data = dt, fix = tk_par)
test_that("Prediction identitites to Tversky & Kahneman (1992)", {
  expect_equal(M$predict('value')[1,'pr_x'], c('pr_x'=57.5), tol = tol)
  expect_equal(M$predict('value')[1,'pr_y'], c('pr_y'=63), tol = tol)
  expect_equal(M$predict('value')[2,'pr_x'], c('pr_x'=-129.5), tol = tol)
  expect_equal(M$predict('value')[2,'pr_y'], c('pr_y'=-129.9), tol = tol)
})

# 2. Parameter recovery
dt_tk1992 <- data.table(
  x1 = c(rep(c(50, -50), each = 3), 
         rep(c(100, -100), each = 5),
         rep(c(200, -200), each = 5),
         rep(c(400, -400), each = 2),
         rep(c(100, -100), each = 3),
         rep(c(150, -150), each = 5),
         rep(c(200, -200), each = 5)),
  px = c(rep(c(.10, .50, .90), 2), 
         rep(c(.05, .25, .50, .75, .95), 2),
         rep(c(.01, .10, .50, .90, .99), 2),
         rep(c(.01, .99), 2),
         rep(c(.10, .50, .90), 2), 
         rep(c(.05, .25, .50, .75, .95), 2),
         rep(c(.05, .25, .50, .75, .95), 2)),
  x2 = c(rep(0, 30), rep(c(50, -50), each = 3), rep(c(50, -50), each = 5), rep(c(100, -100), each = 5)))
dt_tk1992[, y1 := x1 * px + x2 * (1-px)]
dt_tk1992[, py := 1]
dt_tk1992[, y2 := 0]

tk_par <- c(alpha = 0.88, beta = 0.88, lambda = 2.25, gammap = 0.61, gamman = 0.69) 
M <- cpt(~ x1 + px + x2 | y1 + py + y2, ref = 0, data = dt_tk1992, fix = tk_par)
dt_tk1992$rp <- apply(M$predict(), 1, function(i) {
  ifelse(i[1] > i[2], 1, 0)
})
# M <- cpt(rp ~ x1 + px + x2 | y1 + py + y2, ref = 0, data = dt_tk1992, choicerule = "argmax")

# 3. Formal tests
# 3.a. One-row test set and test sets with different orders
tk_par <- c(alpha = 0.88, beta = 0.88, lambda = 2.25, gammap = 0.61, gamman = 0.69) 
D <- data.frame(x1=c(1,2,2),x2=c(2,1,1),p=c(0.66,0.66,0.50))
form <- c("data frame", "tibble", "data table", "matrix") # tibble
sapply(1:length(form), function(i) {
  print(paste0("D is a ", form[i]))
  if(form[i] == "data frame") D <- as.data.frame(D)
  if(form[i] == "tibble") D <- as_tibble(D)
  if(form[i] == "data table") D <- as.data.table(D)
  if(form[i] == "matrix") {
    D <- as.matrix(D)
    test_that("Prediction identities", {
      expect_error(cpt(~x1+p+x2, ref=0L, data=D[1,], fix=tk_par))
      expect_error(cpt(~x1+p+x2, ref=0L, data=D, fix=tk_par))
    })
  } else {
    test_that("Prediction identities", {
      M <- cpt(~x1+p+x2, ref=0L, data=D[1,], fix=tk_par)
      expect_equivalent(M$predict(), 1.285, tol=tol)
      M <- cpt(~x1+p+x2, ref=0L, data=D[2,], fix=tk_par)
      expect_equivalent(M$predict(), 1.427, tol=tol)
      M <- cpt(~x1+p+x2, ref=0L, data=D[3,], fix=tk_par)
      expect_equivalent(M$predict(), 1.353, tol = tol)
      M <- cpt(~x1+p+x2, ref=0L, data=D, fix=tk_par)
      expect_equivalent(M$predict(), c(1.285, 1.427, 1.353), tol = tol)
      M <- cpt(~x1+p+x2, ref=0L, data=D[3:1,], fix=tk_par)
      expect_equivalent(M$predict(), c(1.353, 1.427, 1.285), tol = tol)
      M <- cpt(~x1+p+x2, ref=0L, data=D[c(2,1,3),], fix=tk_par)
      expect_equivalent(M$predict(), c(1.427, 1.285, 1.353), tol = tol)
    })
  }
})

# 3.b. Formula entry
test_that("Probability entry", {
  M1 <- cpt(rp ~ x1 + px + x2           | y1 + py + y2          , ref = 0, data = dt, fix = tk_par)$predict()
  M2 <- cpt(rp ~ x1 + px + x2           | y1 + py + y2 + I(1-py), ref = 0, data = dt, fix = tk_par)$predict()
  M3 <- cpt(rp ~ x1 + px + x2 + I(1-px) | y1 + py + y2          , ref = 0, data = dt, fix = tk_par)$predict()
  M4 <- cpt(rp ~ x1 + px + x2 + I(1-px) | y1 + py + y2 + I(1-py), ref = 0, data = dt, fix = tk_par)$predict()
  expect_equal(M1, M2); expect_equal(M1, M3); expect_equal(M2, M3); expect_equal(M1, M4); expect_equal(M2, M4); expect_equal(M3, M4)

  M1 <- cpt(rp ~ x1 + px + x2           | y1 + py + y2          , ref = 0, data = dt[1, ], fix = tk_par)$predict()
  M2 <- cpt(rp ~ x1 + px + x2           | y1 + py + y2 + I(1-py), ref = 0, data = dt[1, ], fix = tk_par)$predict()
  M3 <- cpt(rp ~ x1 + px + x2 + I(1-px) | y1 + py + y2          , ref = 0, data = dt[1, ], fix = tk_par)$predict()
  M4 <- cpt(rp ~ x1 + px + x2 + I(1-px) | y1 + py + y2 + I(1-py), ref = 0, data = dt[2, ], fix = tk_par)$predict()
  expect_equal(M1, M2); expect_equal(M1, M3); expect_equal(M2, M3)
  expect_false(isTRUE(all.equal(M1, M4))); expect_false(isTRUE(all.equal(M2, M4))); expect_false(isTRUE(all.equal(M3, M4))) 
  
  expect_error( # probabilities don't sum to 1
    cpt(rp ~ x1 + px + x2 + I(1-py) | y1 + py + y2, ref = 0, data = dt, fix = tk_par)
  )
  expect_error(
    cpt(rp ~ x1 + px + x2 | y1 + py + y2 + I(1-px), ref = 0, data = dt, fix = tk_par)
  )
  expect_error(
    cpt(rp ~ x1 + px + x2 + I(1-py) | y1 + py + y2 + I(1-px), ref = 0, data = dt, fix = tk_par)
  )
})

test_that("CPT errors", {
  expect_error( # wrong order, should be x1 + px + x2
    cpt(rp ~ x2 + x1 + px, ref=0, data = dt)
    )
  expect_error( # Wrong order in the first of two RHS stimuli
    cpt(rp ~ x2 + x1 + px | y1 + py + y2, ref=0, data = dt)
    )
  expect_error( # Wrong order with last probability submitted
    cpt(rp ~ x2 + x1 + px + I(1-px), ref=0, data = dt)
    )
  expect_error( 
    cpt(rp ~ x2 + x1 + px + I(1-px) | y1 + py + y2, ref=0, data = dt)
    )
  expect_error( # Error in only the second of two RHS
    cpt(rp ~ x1 + px + x2 | y2 + y1 + py, ref=0, choicerule=NULL, data = dt)
    )
  expect_error( 
    cpt(rp ~ x1 + px + x2 + I(1-px) | y2 + y1 + py + I(1-py), ref=0, choicerule=NULL, data = dt)
    )
  expect_error( # Error in both RHSs
    cpt(rp ~ x2 + x1 + px | y2 + y1 + py, ref=0, choicerule=NULL, data = dt)
    )
  expect_error(
    cpt(rp ~ x2 + x1 + px + I(1-px) | y2 + y1 + py + I(1-py), ref=0, choicerule=NULL, data = dt)
    )
  # TO DO: put matrix here
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
expect_error(cpt(rp ~ x1 + px + x2 | y1 + py + y2 | z1 + pz + z2, ref = 0, data = dt, fix = tk_par))

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
expect_error(cpt(rp ~ x1 + px1 + x2 + px2 + x3 | y1 + py1 + y2 + py2 + y3, ref = 0, data = dt, fix = tk_par)$predict())
