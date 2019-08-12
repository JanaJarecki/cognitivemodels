context("utility")
library(cogscimodels)

D <- data.frame(y = 0, x = c(0,1,2,3))
utility(y ~ x, D)