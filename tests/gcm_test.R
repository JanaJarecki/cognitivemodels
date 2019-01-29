rm(list=ls(all=T)) # Empty workspace
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("../models/gcm/gcm.R", chdir = TRUE)

# Learning and test set
dt <- data.table(response = c(0, 1, 0),
                 f1 = c(2, 1, 2),
                 f2 = c(1, 3, 2),
                 true_cat = c(1, 0, 1))

formula <- response ~ f1 + f2

args <- list(formula = formula, cat = ~ true_cat, metric = "mink", fixed = c(r = 1, p = 1, c = 1, tau = 0.5), choicerule = "soft")
m <- do.call(gcm, c(args, data = list(dt), discount = 0))


