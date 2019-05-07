rm(list=ls(all=T)) # Empty workspace
library(data.table)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("../models/gcm/gcm.R", chdir = TRUE)

# Learning and test set
dt <- data.table(response = c(1, 1, 0, 0),
                 f1 = c(4, 1, 4, 1),
                 f2 = c(1, 2, 3, 4),
                 true_cat = c(1, 0, 1, 0))

formula <- response ~ f1 + f2

args <- list(formula = formula, cat = ~ true_cat, metric = "mink", fixed = c(r = 1, p = 1, c = 1, tau = 0.5), choicerule = "soft")
m <- do.call(gcm, c(args, data = list(dt), discount = 0))
m$ndim
