# ==========================================================================
# Test for Exemplar-based Cognitive Model
#
# Nosofsky, R. M. (1989). Further tests of an exemplar-similarity approach to
#   relating identification and categorization. Perception & Psychophysics,
#   45, 279–290. doi:10.3758/BF03204942
# ==========================================================================


# 1. Model predictions -----------------------------------------------------
test_that("Prediction identities to Nosofsky (1989)", {
  data(nosofsky1989)
  d <- nosofsky1989
  # Data from Table 3 and Figure 2
  # Parameter from Table 5 "size", pred from Fig. 5 "size"
  # criss = c(lambda=1.62,angle=.80,size=.20,b0=.45, b1=.55,r=2,q=2)
  # diag =  c(lambda=2.42,angle=.81,size=.19,b0=.49, b1=.51,r=2,q=2)
  expect_ebm_nosofsky_equivalent <- function(cond, fix, target) {
    M <- ebm(obs_cat ~ angle + size, data = d[d$condition == cond & !is.na(d$true_cat), ], criterion = ~true_cat, fix = fix, choicerule = "none", mode = "discrete", discount = 0)
    test_d <- d[1:16,]
    test_d$true_cat <- NA
    expect_equivalent(M$predict(newdata = test_d), target, tol = .01)
  }

  expect_ebm_nosofsky_equivalent(cond = "size",
    fix = c(angle=.10,size=.90,lambda=1.60,b0=.5,b1=.5,q=2,r=2),
    target = c(0.01,0.01,0.01,0.01,0.14,0.16,0.19,0.17,0.69,0.67,0.72,0.72,0.97,0.98,0.99,0.98))
  expect_ebm_nosofsky_equivalent(cond = "angle",
    fix = c(lambda=3.20,angle=.98,size=.02,b0=.43, b1=.57,r=2,q=2),
    target = c(0.06,0.44,0.81,0.99,0.04,0.38,0.77,0.97,0.08,0.45,0.86,0.99,0.02,0.44,0.87,0.99))
})



# 2. Parameter recovery ----------------------------------------------------
test_that("Parameter estimates compared to Nosofsky (1989)", {
  data(nosofsky1989)
  d <- nosofsky1989
  expect_est_equal <- function(fix, target, tol = 0.01) {
    fix <- c(q=2, r=2, fix)
    fitd <- d[d$condition == condition,]
    M <- gcm(formula = pobs ~ angle + size, criterion = ~ true_cat, data = d[d$condition == condition & !is.na(d$true_cat), ], fix = fix, options = list(fit_data = fitd, fit_args = list(n = fitd$N), fit=T), discount = 0, choicerule = "none")
    expect_equal(coef(M), target, tol = tol)
  }

  condition <- "size"
  expect_est_equal(fix = NULL,
    target = c(angle=.10,size=.90,lambda=1.60,b0=.50,b1=.50))
  expect_est_equal(fix = c(angle = .5, size = .5),
    target = c(lambda=2.40, b0=.49, b1=.51))
  expect_est_equal(fix = list(angle = .5, size = "angle"),
    target = c(lambda=2.40, b0=.49, b1=.51))
  expect_est_equal(fix = list(angle = "size", size = 0.5),
    target = c(lambda=2.40, b0=.49, b1=.51))
  expect_est_equal(fix = c(b0 = .5, b1 = .5),
    target = c(angle=.10, size=.90, lambda=1.60))

  condition <- "diag"
  expect_est_equal(fix = NULL,
    target = c(angle=.81,size=.19,lambda=2.42,b0=.49,b1=.51))
  expect_est_equal(fix = c(angle = .5, size = .5),
    target = c(lambda=1.81, b0=.48, b1=.52), tol = 0.04)
  expect_est_equal(fix = c(b0 = .5, b1 = .5),
    target = c(angle=.81, size=0.19, lambda=2.42))
  expect_est_equal(fix = list(b0 = .5, b1 = "b0"),
    target = c(angle=.81, size=0.19, lambda=2.42))
  expect_est_equal(fix = list(b0 = "b1", b1 = 0.5),
    target = c(angle=.81, size=0.19, lambda=2.42)) 
})

# 3. Formal tests -----------------------------------------------------------
# 3.a. Errors
test_that('EBM error handlers', {
  d <- as.data.frame(matrix(c(1,1,1,1,0,
                              1,1,1,0,0,
                              0,0,0,1,0,
                              0,0,0,0,1,
                              0,0,1,1,1,
                              1,1,0,0,1), 6, 5, T))
  expect_error(ebm(formula = ~ V1 + V2 + V3 + V4 | V5, data = d, discount = 0, fix = c(V = rep(.30, 4), r = 1, lambda = 1)))
})


# test_that("Parameter estimates with softmax", {
#   d <- data.frame(
#     f1    = c(0,1,0,1,0,1,0,1),
#     f2    = c(1,0,1,0,1,0,1,0),
#     f3    = c(1,1,1,1,1,1,1,1),
#     ca    = c(1,0,1,0,1,0,1,0),
#     yrand = c(1,1,0,0,1,1,0,0),
#     yperf = c(1,0,1,0,1,0,1,0))
#   # M <- start(d = d) %+%
#   #   ebmcj(~ f1 + f2 + f3, criterion = ~ ca, fix = c(lambda=1,r=1,q=1,f1=1/3,f2=1/3,f3=1/3)) %+%
#   #   softmax(yperf ~ pred_f) %>%
#   #   end(options = list(solver = "solnp"))  
# })

#   

#   expect_equal(cognitiveutils::SSE(d$obs_cat_size/d$N_size, M$predict(d), n = d$N_size), 0.015, tol = .001)
#   expect_equal(cognitiveutils::Loglikelihood(d$obs_cat_size/d$N_size, M$predict(d), n = nn, pdf = 'binomial', binomial.coef = TRUE, mode = 'd'), -40.08, tol = 0.02)
#   M$setPar(c('lambda'=2.38, 'size'=.50, 'angle'=.50, 'b0'=.49, 'b1'=.51))
#   expect_equal( 1-M$predict(d[9,]), 0.48, .01)
#   expect_equal(cognitiveutils::SSE(d$obs_cat_size/d$N_size, M$predict(d), n = nn), 0.077, tol = .01)
#   expect_equal(cognitiveutils::Loglikelihood(d$obs_cat_size/d$N_size, M$predict(d), n = nn, binomial.coef= TRUE, pdf = 'binom'), -71, tol = 0.1)

#   #
#   # Angle condition
#   # --------------------------------------------------------------------------
#   # M parameter from Table 5 "angle", pred from Fig. 5 "angle"
#   M <- ebm(~ angle + size | true_cat_angle, data = d[!is.na(d$true_cat_angle),], fix = as.list(par['angle',]), mode = 'c', discount = 0)
#    # 
#   expect_equal(1-M$predict(d), pangle, tol = .01)
#   M$setPar(c('lambda' = 3.57, 'size' = .50, 'angle' = .50, 'b0' = .45, 'b1' = .55))
#   expect_equal(1-M$predict(d[14,]), .18, tol = .1)

#   #
#   # Criss-cross conditin
#   # --------------------------------------------------------------------------
#   # M parameter from Table 5 "criss-cross", pred from Fig. 5 "criss-cross"
#   M <- ebm(pobs_criss ~ angle + size | true_cat_criss, data = d[!is.na(d$true_cat_criss),], fix = as.list(par['criss',]), mode = 'c', discount = 0)
#   pcriss <- 1-c(22,37,55,76,40,49,56,61,64,57,48,36,81,56,33,19)/100 # 
#   expect_equal(M$predict(d), pcriss, tol = .02)

#   #
#   # Diagonal condition
#   # ---------------------------------------------------------------------------
#   # M parameter from Table 5 "diagonal", pred from Fig. 5 "diagonal"
#   M <- ebm(~ angle + size | true_cat_diag, data = d[!is.na(d$true_cat_diag),], fix = as.list(par['diag',]), mode = 'c')
#   pdiag <- 1 - c(43,78,89,96,22,51,69,85,13,29,47,78,04,10,21,56)/100 # 
#   expect_equal(M$predict(d), pdiag, tol = .02)
# })






# dt <- data.frame(x1 = c(1,0,1,0), x2 = c(0,1,0,1), ve = c(10,11,10,11), price = 10)
# m <- ebm(~ x1 + x2 | ve | price, data = dt[1:2,], fix = list(x1=.5, x2=.5, lambda = 1, r = 1, q = 1), mode = 'j')
# m$predict()
# m$predict(newdata = dt[3:4,], firstout = 1)
# Softmax(cbind(10.26894,10),1)
# Softmax(cbind(10.73106,10),1)

# exp(-0)/(exp(-0)+exp(-1))*10+exp(-1)/(exp(-0)+exp(-1))*11

# # Test subjective estimaion of valus and chioce predictions of 
# dt <- data.table(f1      = c(1,1,0,0,1,0,1,0,0),
#                  f2      = c(1,1,0,0,1,0,1,0,0),
#                  f3      = c(1,1,0,0,0,1,1,1,0),
#                  choices = c(0,0,1,1,0,1,0,1,0),
#                  prices  = c(1,1,3,3,2,2,1,2,3))
# dt[, vest := ve(prices, choices)]
# mod <- ebm(choices ~ f1 + f2 + f3 | vest | prices, data = dt[1:3], fix = list(f1=1/3, f2=1/3, f3=1/3, r=1, q=1, lambda = 1, tau = 1))
# modv <- ebm(choices ~ f1 + f2 + f3 | vest | prices, data = dt[1:3], fix = list(f1=1/3, f2=1/3, f3=1/3, r=1, q=1, lambda = 1), mode = 'j')
# dt[1:3, vpred := modv$predict()]
# dt[1:3, cpred := mod$predict()]
# dt[4:9, vpred2 := modv$predict(.SD)]
# dt[4:9, cpred2 := mod$predict(.SD)]
# dt[, vpred3 := modv$predict(.SD[4:9], firstout=1)]
# dt[, cpred3 := mod$predict(.SD[4:9], firstout=1)]

# # Calculate the predicted value for trial 4 per hand
# syp <- 2 * exp(-1) + exp(0) # denominator, sum of similarities
# vp <- (2 * exp(-1) * dt[2, vest] + exp(0) * dt[3, vest]) / syp # predicted value
# 1/(1+exp(-(vp-3))) # predicted choices


# dt <- data.table(matrix(c(
#   1,1,0,0,57,
#   1,0,1,0,56,
#   0,0,0,1,51,
#   1,1,1,0,59), nr = 4, byrow = TRUE))

# mm <- ebm(~ V1 + V2 + V3 + V4 | V5, data = dt, fix = list(r = 1, q = 1, V1 = .4, V2 = .3, V3 = .2, V4 = .1, lambda = .50))
# mm$predict(newdata = data.table(matrix(c(1,1,1,1), nr = 1)))




  # # Crisscross condition
  # fml <- pobs_criss ~ angle + size | true_cat_criss
  # nn <- d$N_criss
  # keep <- !is.na(d$true_cat_criss)
  # M <- ebm(fml, data = d[keep, ], fix = list(q = 2, r = 2), mode = 'c', fit.options = list(n = nn, newdata = d), discount = 0) # no constraints
  # expect_equal(unlist(M$par), par['criss', names(M$par)], tol = .02)
  # M <- ebm(fml, data = d[keep, ], fix = list(q = 2, r = 2, angle = .5, size = .5), mode = 'c', fit.options = list(n = nn, newdata = d), discount = 0) # weights constrained
  # expect_equal(unlist(M$par), c(angle=.50, size=.50, lambda=1.23, r=2, q=2, b0=.45, b1=.55), tol = .01)
  # M <- ebm(fml, data = d[keep, ], fix = list(q = 2, r = 2, b0 = .5, b1 = .5), mode = 'c', fit.options = list(n = nn, newdata = d), discount = 0) # bias constrained
  # expect_equal(unlist(M$par), c(angle=.93, size=0.07, lambda=3, r=2, q=2, b0=.50, b1=.50), tol = .01)

