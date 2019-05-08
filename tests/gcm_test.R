rm(list=ls(all=T)) # Empty workspace
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(data.table)
library(cogscimodels)
library(devtools)

# Learning and test set
dt <- data.table(response = c(0, 0, 1, 0, 1),
                 f1 = c(4, 4, 1, 4, 1),
                 f2 = c(1, 3, 1, 4, 4),
                 true_cat = c(0, 0, 1, 0, 1))

formula <- response ~ f1 + f2

args <- list(formula = formula, cat = ~ true_cat, metric = "mink", fixed = c(c = 1, r = 1, p = 1, tau = 1), choicerule = "soft")
m <- do.call(gcm, c(args, data = list(dt), discount = 0))

# devtools::load_all
# To Do:
# Make environment where discount leads to different ws than no discount