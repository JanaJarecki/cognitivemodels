rm(list=ls(all=T)) # Empty workspace
source("../models/gcm/gcm_unidim.R", chdir = TRUE)

# Learning and test set
dt <- data.table(response = c(1, 0, 1, 1, 0, 1, 1, 0, 1, 0, 1, 1),
                 f1 = c(2, 1, 2, 2, 1, 2, 2, 1, 2, 2, 2, 1),
                 f2 = c(1, 2, 2, 1, 2, 2, 1, 2, 2, 1, 2, 2), 
                 true_cat = c(0, 0, 1, 0, 0, 1, 0, 0, 1, NA, NA, NA))

formula <- response ~ f1 + f2

args <- list(formula = formula, cat = ~ true_cat, metric = "mink", fixed = c(r = 1, p = 1, c = 1, tau = 0.5), choicerule = "soft")
m <- do.call(gcm_unidim, c(args, data = list(dt), discount = 0))
