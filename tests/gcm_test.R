library(data.table)

setwd("~/cogscimodels/tests")
source("../models/gcm/gcm.R", chdir = TRUE)

files <- list.files(path = "~/Masterarbeit/Experiment_Python/Categorization Experiment/data/pretest/csv/", pattern = "6", full.names = TRUE) # *.csv$
dt <- rbindlist(lapply(files, fread, fill = TRUE))
dt <- dt[block == "training", ]
stim <- matrix(as.numeric(t(dt[, strsplit(as.character(stim), split = "")])), ncol = 3)
dt <- data.table(stim, dt[, list(true_cat, response)])

formula <- response ~ V1 + V2 + V3

dt <- data.table(c = c(0, 1, 1),
                 f1 = c(2, 1, 2),
                 f2 = c(1, 3, 2),
                 true_cat = c(1, 0, 1))

formula <- c ~ f1 + f2

m <- gcm$new(formula, data = dt, cat = ~ true_cat, metric = "d", fixed = c(r = 1, p = 1))
m$constr
m$input
m$cat
m$metric
m$stimulus_frequencies
m$parm
m$predict()
m$parm
m$fit()
