rm(list=ls(all=T)) # Empty workspace
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(R6)
library(data.table)
library(cogscimodels)
library(devtools)
library(Formula)
library(Rsolnp)

source("~/cogscimodels/R/cogscimodel-class.R", chdir = TRUE)
source("~/cogscimodels/R/gcm_unidim.R", chdir = TRUE)
source("~/cogscimodels/R/gcm.R", chdir = TRUE)

# Stimuli
# dt <- data.table(response = c(0, 0, 1, 0, 1),
#                  f1 = c(4, 4, 1, 4, 1),
#                  f2 = c(1, 3, 1, 4, 4),
#                  true_cat = c(0, 0, 1, 0, 1))

# dt <- data.table(response = c(0, 1, 0, 1, 1),
#                  f1 = c(4, 1, 3, 3, 2),
#                  f2 = c(1, 4, 2, 2, 3),
#                  true_cat = c(0, 1, 0, 0, 1))

n <- 20
dt <- data.table(response = sample(c(0, 1), size = n, replace = TRUE),
                 f1 = rnorm(n, mean = 2.5, sd = 0.5), 
                 f2 = rnorm(n, mean = 2.5, sd = 0.5))
dt[, true_cat := ifelse(abs(f1 - f2) < 0.5, 1, 0)]
formula <- response ~ f1 + f2

args <- list(formula = formula, cat = ~ true_cat, metric = "d", fixed = c(c = 1, r = 1, p = 1, tau = 1), choicerule = "soft")
m <- do.call(gcm, c(args, data = list(dt), discount = 0))

# devtools::load_all()
# To Do:
# Make environment where discount leads to different ws than no discount