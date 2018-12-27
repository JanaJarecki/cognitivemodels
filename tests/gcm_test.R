library(data.table)

setwd("~/cogscimodels/tests")
source("../models/gcm/gcm.R", chdir = TRUE)

dt <- data.table(c = c(0, 1, 1),
                 f1 = c(2, 1, 2),
                 f2 = c(1, 3, 2),
                 cat = c(1, 0, 1))

formula <- c ~ f1 + f2

m <- gcm$new(formula, data = dt, cat = ~ cat, metric = "d")
m$input
m$cat
m$metric
m$stimulus_frequencies
m$parm
m$predict()
m$parm
