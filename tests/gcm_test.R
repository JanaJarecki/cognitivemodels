library(data.table)

setwd("~/cogscimodels/tests")
source("../models/gcm/gcm.R", chdir = TRUE)

files <- list.files(path = "~/Masterarbeit/Experiment_Python/Categorization Experiment/data/pretest/csv/", pattern = "", full.names = TRUE) # *.csv$
dt <- rbindlist(lapply(files, fread, fill = TRUE))
stim <- matrix(as.numeric(t(dt[, strsplit(as.character(stim), split = "")])), ncol = 3)
dt <- cbind(stim, dt)

# Fitting with gcm
fit_ll <- function(data, metric){
  m <- gcm(formula = response ~ V1 + V2 + V3, data = data, cat = ~ true_cat, metric = metric, fixed = c(r = 1, p = 1), choicerule = "soft", discount = 8)
  return(as.list(c(m$parm, loglik = m$logLik(), AIC = m$AIC())))
}

predict_gcm <- function(data, metric) {
  m <- gcm(formula = response ~ V1 + V2 + V3, 
           data = data[block == "training"], cat = ~ true_cat, metric = metric, 
           fixed = c(r = 1, p = 1, w1 = data$w1[1], w2 = data$w2[1], w3 = data$w3[1], c = data$c[1], tau = data$tau[1]), 
           choicerule = "soft")
  c(m$predict(), m$predict(newdata = data[block == "test"]))
}

# Fit Parameters of discrete and Minkowsi model
fitted_parm_disc <- dt[block == "training", fit_ll(.SD, metric = "d"), by = subj_id]
fwrite(fitted_parm_disc, file = "../data/Categorization/processed/fitted_parm_disc.csv")
fitted_parm_mink <- dt[block == "training", fit_ll(.SD, metric = "m"), by = subj_id]
fwrite(fitted_parm_mink, file = "../data/Categorization/processed/fitted_parm_mink.csv")
setkey(fitted_parm_disc, subj_id)
setkey(fitted_parm_mink, subj_id)

# Predict for discrete and Minkowski model
setkey(dt, subj_id)
dt <- dt[fitted_parm_disc]
dt[block != "familiarization", pred_disc := predict_gcm(data = .SD, metric = "d"), by = subj_id]
dt[, colnames(fitted_parm_disc)[-1] := NULL]
dt <- dt[fitted_parm_mink]
dt[block != "familiarization", pred_mink := predict_gcm(data = .SD, metric = "m"), by = subj_id]
dt[, colnames(fitted_parm_mink)[-1] := NULL]


# dt <- data.table(c = c(0, 1, 1),
#                  f1 = c(2, 1, 2),
#                  f2 = c(1, 3, 2),
#                  true_cat = c(1, 0, 1))
# 
# formula <- c ~ f1 + f2
