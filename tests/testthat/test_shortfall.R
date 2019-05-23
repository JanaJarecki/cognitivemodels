library(cogscimodels)



dt <- data.frame(x1 = rep(1,3), x2 = rep(2,3), px = rep(.5,3),
                y1 = 0:2,      y2 = rep(3,3), py = rep(.5,3),
                aspiration = rep(1,3),
                choice = c(1,1,0))


model <- shortfall(choice ~ x1 + x2 + px + I(1-px) | y1 + y2 + py + I(1-py), data = dt, asp = ~aspiration, choicerule = 'softmax')
