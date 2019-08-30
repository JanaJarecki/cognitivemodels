context("parameterrecovery")
library(cogscimodels)

dt <- data.frame(x1 = rep(1,3), x2 = rep(2,3), px = rep(.5,3),
                y1 = 0:2,      y2 = rep(3,3), py = rep(.5,3),
                aspiration = rep(1,3),
                choice = c(1,1,0))

model <- shortfall(choice ~ x1 + px + x2 + I(1-px) | y1 + py + y2 + I(1-px), asp = ~aspiration, data = dt, choicerule = 'softmax', fix = list(delta = NA))
# # check fo r
test_that('Parameter Recovery', {}
# x <- parameterrecovery(model, nruns = 2)
# x
  )