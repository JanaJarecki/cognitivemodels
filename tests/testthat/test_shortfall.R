context("shortfall")
library(cogscimodels)
library(data.table)


# dt <- data.frame(x1 = rep(1,3), x2 = rep(2,3), px = rep(.5,3),
#                 y1 = 0:2,      y2 = rep(3,3), py = rep(.5,3),
#                 aspiration = rep(1,3),
#                 choice = c(1,1,0))

# model <- shortfall(choice ~ x1 + x2 + px + I(1-px) | y1 + y2 + py + I(1-py), data = dt, asp = ~aspiration, choicerule = 'softmax')


# prep <- function (pp) {
#     # Column1: mode (1=a, 0=b)--> this is taking the reversing into account
#     # Column2: mode time (ms)
#     # Column3: reversed mode (1=A, 0=B)
#     # Note: The trials are sorted accoring to the trial ID, which means that trail 1 corresponds to the same gamble in all participants
#     d1 <- 'C:/Users/Jana Jarecki/Documents/shortfall_data_for_jana/shortfall_data_for_jana/txtFiles/stim_part1.txt'
#     d2 <- 'C:/Users/Jana Jarecki/Documents/shortfall_data_for_jana/shortfall_data_for_jana/txtFiles/stim_part2.txt'

#     stim1 <- data.table::fread(d1, col.names = c('a1','a2','a3','b1','b2','b3'))
#     stim1[, c('pa1','pa2','pa3','pb1','pb2','pb3') := 1/3]
#     stim2 <- data.table::fread(d2, col.names = c(paste0('a',1:5), paste0('b',1:5)))
#     stim2[, c(paste0('pa', 1:5), paste0('pb',1:5)) := 1/5]

#     stim <- as.data.table(rbind(stim1,stim2,fill=TRUE))
#     stim[is.na(stim)] <- 0L

#     d <- rbind(
#       fread(paste0('C:/Users/Jana Jarecki/Documents/shortfall_data_for_jana/shortfall_data_for_jana/processed_part1/',pp,'.txt'), select = 1:2,
#         col.names = c('choice', 'rt')), 
#       fread(paste0('C:/Users/Jana Jarecki/Documents/shortfall_data_for_jana/shortfall_data_for_jana/processed_part2/',pp,'.txt'), select = 1:2,
#         col.names = c('choice', 'rt')))
#     d <- cbind(d, stim)
#     d[1:100, evA := rowMeans(cbind(a1,a2,a3))]
#     d[1:100, evB := rowMeans(cbind(b1,b2,b3))]
#     d[101:200, evA := rowMeans(cbind(a1,a2,a3,a4,a5))]
#     d[101:200, evB := rowMeans(cbind(b1,b2,b3,b4,b5))]
#     d[, id := pp]
#     setcolorder(d, c(ncol(d), 1:(ncol(d)-1)))
#     return(d[])
#   }

# shortfalltest <- rbindlist(lapply(c(2,5,14), prep))
# save(shortfalltest, file="../../data/shortfalltest.RData")


data(shortfalltest) # Load shortfall test data
d2  <- shortfalltest[shortfalltest$id==2,]
d5  <- shortfalltest[shortfalltest$id==5,]
d14 <- shortfalltest[shortfalltest$id==14,]
fml <- choice ~ a1 + pa1 + a2 + pa2 + a3 + pa3 + a4 + pa4 + a5 + pa5 | b1 + pb1 + b2 + pb2 + b3 + pb3 + b4 + pb4 + b5 + pb5

# Do tests
test_that("Parameter estimates == estimates in paper", {
  model <- shortfall(fml, data = d2, asp = ~evA | evB, choicerule = "softmax")
  expect_equal(model$coef(), c(beta=0.75, delta=0.76, tau=1/0.57), tol = .01)
  expect_equal(-2 * model$logLik(), 162.6, tol = .001)

  model <- shortfall(fml, data = d5, asp = ~evA | evB, choicerule = "softmax")
  expect_equal(model$coef(), c(beta=0.71, delta=1, tau=1/0.48), tol = .01)
  expect_equal(-2 * model$logLik(), 183.5, tol = .001)

  model <- shortfall(fml, data = d14, asp = ~evA | evB, choicerule = "softmax")
  expect_equal(model$coef(), c(beta=4.71, delta=0.24, tau=1/0.19), tol = .011)
  expect_equal(-2 * model$logLik(), 209.1, tol = .001)

  # Todo: Participant #13 gives a corner solution, check this, maybe implement something
  # model <- shortfall(fml, data = shortfalltest[shortfalltest$id==13,], asp = ~evA | evB, choicerule = "softmax")
  # expect_equal(model$coef(), c(beta=0.98, delta=0.93, tau=1/0.04), tol = .01)
  # expect_equal(-2*model$logLik(), 183.5, tol = .001)
})

test_that("Input errors", {
  expect_error(shortfall(choice ~ a1 + a2 + a3 + a4, asp = ~ evA, data = d2))
})