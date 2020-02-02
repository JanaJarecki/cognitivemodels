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

# Categorization example
# dt <- data.table(response = c(0, 1, 0, 1, 1),
#                  f1 = c(4, 1, 3, 3, 2),
#                  f2 = c(1, 4, 2, 2, 3),
#                  true_cat = c(0, 1, 0, 0, 1))

n <- 20
dt <- data.table(response = sample(c(0, 1), size = n, replace = TRUE),
                 f1 = rnorm(n, mean = 2.5, sd = 0.5),
                 f2 = rnorm(n, mean = 2.5, sd = 0.5))
dt[, true_cat := ifelse(abs(f1 - f2) < 0.5, 1, 0)]

# dt <- data.table(response = c(0, 0, 1, 0, 1),
#                  f1 = c(4, 4, 1, 4, 1),
#                  f2 = c(1, 3, 1, 1, 4),
#                  true_cat = c(0, 0, 1, 0, 1))
formula <- response ~ f1 + f2

args <- list(formula = formula, cat = ~ true_cat, metric = "mah", output = "cat", fixed = c(c = 1, r = 1, p = 1, tau = 1), choicerule = "soft")
m <- do.call(gcm, c(args, data = list(dt), discount = 0))

c1 <- dt[true_cat == 1, 2:3]
c0 <- dt[true_cat == 0, 2:3]
cov1 <- var(dt[true_cat == 1, 2:3])
cov0 <- var(dt[true_cat == 0, 2:3])
w <- c(m$parm$w1, m$parm$w2)
p <- dt[1, 2:3]
sim1 <- sum(apply(c1, 1, function(x) {
  d <- p - x
  exp(-sqrt(as.matrix(w*d) %*% solve(cov1) %*% t(as.matrix(w*d))))
}))
sim0 <- sum(apply(c0, 1, function(x) {
  d <- p - x
  exp(-sqrt(as.matrix(w*d) %*% solve(cov0) %*% t(as.matrix(w*d))))
}))
sim1/(sim1 + sim0)


# Judgment example
dt <- data.table(response = c(32, 21, 34, 26, 33),
                 f1 = c(4, 4, 1, 4, 1),
                 f2 = c(1, 3, 1, 1, 4),
                 true_cat = c(21, 26, 32, 21, 35))
formula <- response ~ f1 + f2

args <- list(formula = formula, cat = ~ true_cat, metric = "mink", output = "judg", fixed = c(c = 1, r = 1, p = 1, tau = 1), choicerule = "soft")
m <- do.call(gcm, c(args, data = list(dt), discount = 0))

# devtools::load_all()
