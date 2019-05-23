library(cogscimodels)



# dt <- data.frame(x1 = rep(1,3), x2 = rep(2,3), px = rep(.5,3),
#                 y1 = 0:2,      y2 = rep(3,3), py = rep(.5,3),
#                 aspiration = rep(1,3),
#                 choice = c(1,1,0))

d <- fread('processed_part1/1.txt', col.names = c('choice', 'rt', 'reversed'))
stim <- fread('txtFiles/stim_part1.txt')
stim[, p := 1/3]
d <- cbind(d, stim)
d[reversed==1, choice := 1L-choice]
setwd("C:\\Users\\Jana Jarecki\\Dropbox\\96-Resources\\R-Statistics\\cogscimodels\\R")
load_all()


# model <- shortfall(choice ~ x1 + x2 + px + I(1-px) | y1 + y2 + py + I(1-py), data = dt, asp = ~aspiration, choicerule = 'softmax')
