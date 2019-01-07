library(data.table)

setwd("~/cogscimodels/tests")
source("../models/gcm/gcm.R", chdir = TRUE)

files <- list.files(path = "~/Masterarbeit/Experiment_Python/Categorization Experiment/data/pretest/csv/", pattern = "6", full.names = TRUE) # *.csv$
dt <- rbindlist(lapply(files, fread, fill = TRUE))
dt <- dt[block == "training", ]
stim <- matrix(as.numeric(t(dt[, strsplit(as.character(stim), split = "")])), ncol = 3)

# Fitting with solnp
learn <- data.frame(stim, dt[, list(stim, true_cat)])
colnames(learn) <- c(paste0("Dim", 1:3), "ID", "Cat")

observed <- data.frame(stim, dt[, list(stim, response)])
colnames(observed) <- c(paste0("Dim", 1:3), "ID", "Cat")

N <- iterative.frequencies(x = learn)
P <- make.prototype(x = learn, fun = cum.median.by.cat)
P.ID <- simplify2array(lapply(P, rownames))

result <- Max.likelihood.optimization.solnp(n.dim = 3,
                                            Observedset = observed, 
                                            Learningset = learn,
                                            Model = "GCM", 
                                            Metric = "Attr",
                                            ignore = 0,
                                            N = N,
                                            P = P,
                                            P.ID = P.ID)
result$optimal.parameters

# Fitting with gcm
dt <- data.table(stim, dt[, list(true_cat, response)])

formula <- response ~ V1 + V2 + V3

m <- gcm$new(formula, data = dt, cat = ~ true_cat, metric = "d", fixed = c(r = 1, p = 1))
m$parm
m$fit()

# dt <- data.table(c = c(0, 1, 1),
#                  f1 = c(2, 1, 2),
#                  f2 = c(1, 3, 2),
#                  true_cat = c(1, 0, 1))
# 
# formula <- c ~ f1 + f2
