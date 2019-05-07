source("../models/rsfft/lwei.R", chdir = TRUE)
library(data.table)

set.seed(123)
n <- 10
data <- data.table(a = sample(0:2, n, rep = T),
          b = sample(0:2, n, rep = T),
          c = sample(0:2, n, rep = T)
          )
data[, y := rbinom(.N, 1, choicerule(.5 * a + 1 * b - .5 * c, "soft", tau = 1))]

fml <- y ~  a + b + c

self <- Lwei$new(formula = fml, data = data, choicerule = NULL)

self