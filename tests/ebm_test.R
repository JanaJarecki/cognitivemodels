context("EBM values")
library(cogscimodels)
# Test data for binary case with risky gambles
# column 1 - 3: risky gamble values
# column 4: binary chose, don't chose feedback
# column 5: choices people made, to fit free model parameter from
# Juslin Olsson & Olsson (2003)
d <- matrix(c(1,1,0,0,57,
              1,0,1,0,56,
              0,0,0,1,51,
              1,1,1,0,56), 4, 5, T)
d <- as.data.frame(d)
d$x = 0
model <- ebm(formula = x ~ V1 + V2 + V3 + V4 | V5, data = d, discount = 0, fixed = c(r = 2, V4 = .25, lambda = 10))
d <- as.data.frame(matrix(1, 1, 4))
model$predict(newdata = as.data.frame(matrix(1, 1, 4)))

model$fit('solnp')

model$predict('c')
self <- model
gof(model$obs, model$predict, 'log', na.rm=T)

cogsciutils::gof(model$obs, model$predict(), 'mse', resp="c", na.rm=TRUE)


model$fit('solnp')
model$logLik()
model$MSE()
model$discount


# Make the model, similar to lm(), but diffrent
model <- ebm(formula = y ~ x1 + x2 + x3 | truey, data = d, discount = 1, fixed = c(r = 2))
self$fit('solnp')
model

gof(obs = model$obs, pred = model$predict(), resp = model$response, na.rm = TRUE)

class(model$obs)

model$predict()
model$fit()
ebm_cpp(1, c(1,2), c(.5,.5), 1, 1, 1, 1, 1)

RMSE(model)
model$fit(c("grid", "solnp"))
model$logLik()

model$predict()
model$logLik()

model$logLik()


model$print()

self <- model

self <- model

model$fit("grid")

logLik(model)
SSE(model)
MSE(model)
RMSE(model)

library(ggplot2); library(themejj); theme_set(themejj())
barcolors <- c("#FFFFFF", "#F2F1ED", "#E3E1DC", "#D9D6CE", "#C4BFAB")
pointcolors <- c("#FFD719", "#450738", "#FF00CA", "#14CCC6", "#044A48")

ggplot(cbind(d, pred = predict(model)), aes(x = interaction(x1,x2,x3, sep=", "), y = y)) +geom_bar(aes(fill = interaction(x1,x2,x3)), stat = "summary", fun = mean, color = barcolors[2], fill = barcolors[2], size = 1, width = .4) +geom_point(aes(y = pred), color = pointcolors[2], size = 4)


str(model)

m <- cognitiveModel$new(data = d, formula = formula, parameter = c(w=1,c=2), fixed = NULL)
m$which.free()
m
str(m)
m$parameter
model$parameter
model$freeparameter
model$initialparameter


model$setFreeparameter(c(lambda = 1))

model$data
model$parm
predict(model)
logLik(model)

d = matrix(c(
   0,0,91,
   1,1,49,
   1,0,49,
   0,1,0), nc = 3, byrow = T)
colnames(d) <- c(paste0('x', 1:2), "y")
obj <- Mem(y ~ x1 + x2, data = d)

w.space <-  RowSumMatrix(rs = 1, nc = obj$n.dim, regular = T, intervall = .25)
y <- obj$data[,1]
sigma.space <- round(seq(0.01, max(y)/2, (max(y)-min(y)) / 10))
parm.space <- list(
   sigma = sigma.space,
   q = 1,
   r = 1,
   lambda = seq(1,10,3),
    w = w.space
   )
parm.space <- lapply(parm.space, as.matrix)
parm.ids <- do.call(expand.grid, lapply(parm.space, function(x) seq_len(nrow(x))))




ttt <- ParameterRecovery.cognitiveModel(obj, parm.space = parm.space, parm.ids = parm.ids[1:2,])
ttt
########## TODO
#
# Predictions are based on the values from the data, implement an iterative predict thingy

library(ggplot2); library(themejj); theme_set(themejj())

ggplot(ttt[ttt$parm == "w1", ], aes(x = as.factor(parm))) +geom_hline(aes(yintercept = true)) +geom_point(aes(y = fitted)) +geom_violin(aes(y = fitted)) 