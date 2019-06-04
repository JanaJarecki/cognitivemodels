context("EBM values")
library(cogscimodels)

# Nosofsky, R. M. (1989). Further tests of an exemplar-similarity approach to relating identification and categorization. Perception & Psychophysics, 45, 279–290. doi:10.3758/BF03204942
  # From Table 3 and Figure 2
  d <- data.frame(
    angle = c(0.312,0.918,1.405,2.062,0.228,0.844,1.324,1.885,0.374,0.916,1.473,2.128,0.135,0.889,1.451,2.061),
    size = c(-0.241,-0.264,-0.187,-0.227,0.64,0.662,0.687,0.623,1.555,1.501,1.544,1.52,2.352,2.412,2.493,2.382),
    true_cat_size = ifelse(1:16 %in% c(2,5,7,8), 0, ifelse(1:16 %in% c(11,12,14), 1, NA)),
    true_cat_angle = ifelse(1:16 %in% c(2,6,9), 0, ifelse(1:16 %in% c(3,7,12,15), 1, NA)),
    true_cat_criss = ifelse(1:16 %in% c(4,7,10,13), 0, ifelse(1:16 %in% c(1,2,15,16), 1, NA)),
    true_cat_diag = ifelse(1:16 %in% c(2,3,7,12), 0, ifelse(1:16 %in% c(5,10,14,15), 1, NA)),
    obs_cat_size = c(2,4,2,1,35,8,39,39,51,56,170,179,72,229,71,71),
    obs_cat_angle = c(3,116,258,80,1,97,202,80,25,35,71,259,6,35,227,80),
    obs_cat_criss = c(168,138,29,49,40,40,102,27,21,103,41,50,52,30,160,195),
    obs_cat_diag = c(46,48,25,3,228,37,77,16,78,211,44,67,83,242,171,31)
    )
# Model parameter from Table 5 "size", pred from Fig. 5 "size"
parm <- rbind(size = c(lambda=1.60,angle=.10,size=.90,b0=.50,b1=.50,r=2,q=2),
                angle = c(3.20,.98,.02,.43,.57,2,2),
                criss = c(1.62,.80,.20,.45,.55,2,2),
                diag = c(2.42, .81,.19,.49,.51,2,2))


test_that('EBM [choice] parameter estimation', {
  d$N_size <- d$obs_cat_size + c(72,255,72,73,234,66,208,226,23,18,55,58,2,8,3,3)
  d$N_angle <- d$obs_cat_angle + c(79,155,48,2,81,190,60,2,262,47,11,4,76,47,24,2)
  d$N_criss <- d$obs_cat_criss + c(48,94,45,162,34,34,138,47,53,120,33,24,180,44,63,41)
  d$N_diag <- d$obs_cat_diag + c(40,200,242,83,65,49,180,70,8,57,42,199,3,27,58,55)
  d$pobs_size <- d$obs_cat_size/d$N_size
  d$pobs_angle <- d$obs_cat_angle/d$N_angle
  d$pobs_criss <- d$obs_cat_criss/d$N_criss
  d$pobs_diag <- d$obs_cat_diag/d$N_diag
  
  # Size condition
  fml <- pobs_size ~ angle + size | true_cat_size
  nn <- d$N_size
  keep <- !is.na(d$true_cat_size)
  model <- ebm(fml, data = d[keep, ], fixed = list(q = 2, r = 2), type = 'choice', fit.options = list(n = nn, newdata = d), discount = 0) # no constraints
  expect_equal(unlist(model$parm), parm['size', names(model$parm)], tol = .01)
  model <- ebm(fml, data = d[keep, ], fixed = list(q = 2, r = 2, angle = .5, size = .5), type = 'c', fit.options = list(n = nn, newdata = d), discount = 0) # weights constrained
  expect_equal(unlist(model$parm), c(angle=.50, size=.50, lambda=2.40, r=2, q=2, b0=.49, b1=.51), tol = .01)
  model <- ebm(fml, data = d[keep, ], fixed = list(q = 2, r = 2, b0 = .5, b1 = .5), type = 'c', fit.options = list(n = nn, newdata = d), discount = 0) # bias constrained
  expect_equal(unlist(model$parm), c(angle=.10, size=.90, lambda=1.60, r=2, q=2, b0=.50, b1=.50), tol = .01)
  
  # Angle condition
  fml <- pobs_angle ~ angle + size | true_cat_angle
  nn <- d$N_angle
  keep <- !is.na(d$true_cat_angle)
  model <- ebm(fml, data = d[keep, ], fixed = list(q = 2, r = 2), discount = 0, type = 'c', fit.options = list(n = nn, newdata = d)) # no constraints
  expect_equal(unlist(model$parm), parm['angle', names(model$parm)], tol = .01)
  model <- ebm(fml, data = d[keep, ], fixed = list(q = 2, r = 2, angle = .5, size = .5), discount = 0, type = 'c', fit.options = list(n = nn, newdata = d)) # weights constrained
  expect_equal(unlist(model$parm), c(angle=.50, size=.50, lambda=3.57, r=2, q=2, b0=.45, b1=.55), tol = .01)
  model <- ebm(fml, data = d[keep, ], fixed = list(q = 2, r = 2, b0 = .5, b1 = .5), type = 'c', discount = 0, fit.options = list(n = nn, newdata = d)) # bias constrained
  expect_equal(unlist(model$parm), c(angle=1.00, size=0, lambda=3.09, r=2, q=2, b0=.50, b1=.50), tol = .01)

  # Crisscross condition
  fml <- pobs_criss ~ angle + size | true_cat_criss
  nn <- d$N_criss
  keep <- !is.na(d$true_cat_criss)
  model <- ebm(fml, data = d[keep, ], fixed = list(q = 2, r = 2), type = 'c', fit.options = list(n = nn, newdata = d), discount = 0) # no constraints
  expect_equal(unlist(model$parm), parm['criss', names(model$parm)], tol = .02)
  model <- ebm(fml, data = d[keep, ], fixed = list(q = 2, r = 2, angle = .5, size = .5), type = 'c', fit.options = list(n = nn, newdata = d), discount = 0) # weights constrained
  expect_equal(unlist(model$parm), c(angle=.50, size=.50, lambda=1.23, r=2, q=2, b0=.45, b1=.55), tol = .01)
  model <- ebm(fml, data = d[keep, ], fixed = list(q = 2, r = 2, b0 = .5, b1 = .5), type = 'c', fit.options = list(n = nn, newdata = d), discount = 0) # bias constrained
  expect_equal(unlist(model$parm), c(angle=.93, size=0.07, lambda=3, r=2, q=2, b0=.50, b1=.50), tol = .01)

  # Diagonal condition
  fml <- pobs_diag ~ angle + size | true_cat_diag
  nn <- d$N_diag
  keep <- !is.na(d$true_cat_diag)
  model <- ebm(fml, data = d[keep, ], fixed = list(q = 2, r = 2), type = 'c', fit.options = list(n = nn, newdata = d), discount = 0) # no constraints
  expect_equal(unlist(model$parm), parm['diag', names(model$parm)], tol = .01)
  model <- ebm(fml, data = d[keep, ], fixed = list(q = 2, r = 2, angle = .5, size = .5), type = 'c', fit.options = list(n = nn, newdata = d), discount = 0) # weights constrained
  expect_equal(unlist(model$parm), c(angle=.50, size=.50, lambda=1.81, r=2, q=2, b0=.48, b1=.52), tol = .05)
  model <- ebm(fml, data = d[keep, ], fixed = list(q = 2, r = 2, b0 = .5, b1 = .5), type = 'c', fit.options = list(n = nn, newdata = d), discount = 0) # bias constrained
  expect_equal(unlist(model$parm), c(angle=.81, size=0.19, lambda=2.42, r=2, q=2, b0=.50, b1=.50), tol = .01)
  })


test_that('EBM [choice] predictions', {
  model <- ebm(obs_cat_size ~ angle + size | true_cat_size, data = d[!is.na(d$true_cat_size), ], fixed = as.list(parm['size',]), type = 'c', discount = 0)
  psize <- 1-c(.99,.99,.99,.99,.86,.84,.81,.83,.31,.33,.28,.28,.03,.02,.01,.02)
  nn <- d$N_size
  expect_equal(model$predict(d), psize, tol = .01)
  #expect_equal(cogsciutils::SSE(d$obs_cat_size/d$N_size, model$predict(d), n = d$N_size), 0.015, tol = .001)
  expect_equal(Loglikelihood(d$obs_cat_size/d$N_size, model$predict(d), n = nn, pdf = 'binomial', binomial.coef = TRUE, response = 'd'), -40.08, tol = 0.02)
  model$setparm(c('lambda'=2.38, 'size'=.50, 'angle'=.50, 'b0'=.49, 'b1'=.51))
  expect_equal( 1-model$predict(d[9,]), 0.48, .01)
  expect_equal(cogsciutils::SSE(d$obs_cat_size/d$N_size, model$predict(d), n = nn), 0.077, tol = .01)
  expect_equal(Loglikelihood(d$obs_cat_size/d$N_size, model$predict(d), n = nn, binomial.coef= TRUE, pdf = 'binom'), -71, tol = 0.1)

  # Angle condition
  # Model parameter from Table 5 "angle", pred from Fig. 5 "angle"
  model <- ebm(~ angle + size | true_cat_angle, data = d[!is.na(d$true_cat_angle),], fixed = as.list(parm['angle',]), type = 'c', discount = 0)
  pangle <- c(94,56,19,01,96,62,23,03,92,55,14,01,98,56,13,01)/100 # 
  expect_equal(1-model$predict(d), pangle, tol = .01)
  model$setparm(c('lambda' = 3.57, 'size' = .50, 'angle' = .50, 'b0' = .45, 'b1' = .55))
  expect_equal(1-model$predict(d[14,]), .18, tol = .1)

  # Criss-cross condition
  # Model parameter from Table 5 "criss-cross", pred from Fig. 5 "criss-cross"
  model <- ebm(pobs_criss ~ angle + size | true_cat_criss, data = d[!is.na(d$true_cat_criss),], fixed = as.list(parm['criss',]), type = 'c', discount = 0)
  pcriss <- c(22,37,55,76,40,49,56,61,64,57,48,36,81,56,33,19)/100 # 
  expect_equal(1-model$predict(d), pcriss, tol = .02)

  # Diafonal condition
  # Model parameter from Table 5 "diagonal", pred from Fig. 5 "diagonal"
  model <- ebm(~ angle + size | true_cat_diag, data = d[!is.na(d$true_cat_diag),], fixed = as.list(parm['diag',]), type = 'c')
  pdiag <- c(43,78,89,96,22,51,69,85,13,29,47,78,04,10,21,56)/100 # 
  expect_equal(1-model$predict(d), pdiag, tol = .02)
})


test_that('EBM error handlers', {
  d <- as.data.frame(matrix(c(1,1,1,1,0,
                              1,1,1,0,0,
                              0,0,0,1,0,
                              0,0,0,0,1,
                              0,0,1,1,1,
                              1,1,0,0,1), 6, 5, T))
  expect_error(ebm(formula = ~ V1 + V2 + V3 + V4 | V5, data = d, discount = 0, fixed = c(V = rep(.30, 4), r = 1, lambda = 1)))
})




# dt <- data.frame(x1 = c(1,0,1,0), x2 = c(0,1,0,1), ve = c(10,11,10,11), price = 10)
# m <- ebm(~ x1 + x2 | ve | price, data = dt[1:2,], fixed = list(x1=.5, x2=.5, lambda = 1, r = 1, q = 1), type = 'j')
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
# mod <- ebm(choices ~ f1 + f2 + f3 | vest | prices, data = dt[1:3], fixed = list(f1=1/3, f2=1/3, f3=1/3, r=1, q=1, lambda = 1, tau = 1))
# modv <- ebm(choices ~ f1 + f2 + f3 | vest | prices, data = dt[1:3], fixed = list(f1=1/3, f2=1/3, f3=1/3, r=1, q=1, lambda = 1), type = 'j')
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

# mm <- ebm(~ V1 + V2 + V3 + V4 | V5, data = dt, fixed = list(r = 1, q = 1, V1 = .4, V2 = .3, V3 = .2, V4 = .1, lambda = .50))
# mm$predict(newdata = data.table(matrix(c(1,1,1,1), nr = 1)))