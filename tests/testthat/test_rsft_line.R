context("rsft_line")
library(cogscimodels)


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
D <- data.frame(
    x1 = 0, x2 = 4, px = 0.5,
    y1 = 1, y2 =3, py = 0.5,
    s = 0,
    t = 1,
    nt = 1,
    b = 2)
# Do tests
test_that("Parameter estimates == estimates in paper", {
  model <- rsft_line(~ x1 + px + x2 + I(1-px) | y1 + py + y2 + I(1-py), states = ~s, trials = ~t, ntrials = ~nt, budget = ~b, data = D, choicerule = "softmax", fix = list(tau = .5))
  model$predict() 
})