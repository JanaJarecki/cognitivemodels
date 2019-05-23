library(cogscimodels)



# dt <- data.frame(x1 = rep(1,3), x2 = rep(2,3), px = rep(.5,3),
#                 y1 = 0:2,      y2 = rep(3,3), py = rep(.5,3),
#                 aspiration = rep(1,3),
#                 choice = c(1,1,0))

# model <- shortfall(choice ~ x1 + x2 + px + I(1-px) | y1 + y2 + py + I(1-py), data = dt, asp = ~aspiration, choicerule = 'softmax')




# Column1: response (1=a, 0=b)
# Column2: response time (ms)
# Column3: reversed response (1=A, 0=B)
# Note: The trials are sorted accoring to the trial ID, which means that trail 1 corresponds to the same gamble in all participants.
setwd('C:/Users/Jana Jarecki/AppData/Local/Temp/shortfall_data_for_jana/shortfall_data_for_jana')
stim1 <- fread('txtFiles/stim_part1.txt', col.names = c('a1','a2','a3','b1','b2','b3'))
stim1[, c('pa1','pa2','pb1','pb2') := 1/3]
stim2 <- fread('txtFiles/stim_part2.txt', col.names = c(paste0('a',1:5), paste0('b',1:5)))
stim2[, c(paste0('pa', 1:4), paste0('pb',1:4)) := 1/5]
stim <- rbind(stim1,stim2,fill=TRUE)
stim[is.na(stim)] <- 0L
d <- rbind(fread('processed_part1/1.txt', col.names = c('choice', 'rt', 'reversed')), fread('processed_part2/1.txt', col.names = c('choice', 'rt', 'reversed')))
d <- cbind(d, stim)
d[reversed==1, choice := 1L-choice]
d[1:100, evA := rowMeans(cbind(a1,a2,a3))]
d[1:100, evB := rowMeans(cbind(b1,b2,b3))]
d[101:200, evB := rowMeans(cbind(a1,a2,a3,a4,a5))]
d[101:200, evB := rowMeans(cbind(b1,b2,b3,b4,b5))]
model <- shortfall(choice ~ b1 + b2 + b3 + b4 + b4 + I(1-pb1+pb2+pb3+pb4) | a1 + a2 + a3 + a4 + a5 + pa1 + pa2 + pa3 + pa4 + I(1-pa1+pa2+pa3+pa4), data = d, asp = ~evB | evA, choicerule = 'softmax')
model
model$coef()
