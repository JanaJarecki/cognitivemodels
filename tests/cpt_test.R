rm(list=ls(all=T)) # Empty workspace
source("../models/cpt/cpt.R", chdir = TRUE)


# Following
# Tversky, A., & Kahneman, D. (1992). Advances in prospect theory: cumulative representation of uncertainty. Journal of Risk and Uncertainty, 5, 297–323. doi:10.1007/BF00122574
dt <- data.frame(
  x1 = c(100, -100),
  px = 1,
  x2 = 0,
  y1 = c(200, -200),
  py = c(.71,.64),
  y2 = 0,
  rp = 1)

model <- cpt(rp ~ x1 + x2 + px + I(1-px) | y1 + y2 + py + I(1-py), nopt = 2, nout = 2, ref = 0, choicerule = NULL, data = dt, fixed = c(alpha = 0.88, beta = 0.88, lambda = 2.25, gammap = 0.61, gamman = 0.69))

model$predict()


dt <- data.frame(
  x1 = c(-2),
  px = c(.1),
  x2 = c(-3),
  y1 = c(5),
  py = c(.9),
  y2 = c(-5),
  rp = 0)

model <- cpt(rp ~ x1 + x2 + px + I(1-px) | y1 + y2 + py + I(1-py), nopt = 2, nout = 2, ref = 0, choicerule = NULL, data = dt, fixed = c(alpha = 0.88, beta = 0.88, lambda = 2.25, gammap = 0.61, gamman = 0.69))

model$predict("v", 1:2)


dt <- data.frame(
  x1 = c(5),
  px = c(.1),
  x2 = c(-1),
  y1 = c(0),
  py = c(.6),
  y2 = c(-1),
  rp = 0)

model <- cpt(rp ~ x1 + x2 + px + I(1-px) | y1 + y2 + py + I(1-py), nopt = 2, nout = 2, ref = 0, choicerule = NULL, data = dt, fixed = c(alpha = 0.88, beta = 0.88, lambda = 2.25, gammap = 0.61, gamman = 0.69))





# Following
# Tversky, A., & Kahneman, D. (1992). Advances in prospect theory: cumulative representation of uncertainty. Journal of Risk and Uncertainty, 5, 297–323. doi:10.1007/BF00122574
dt <- data.frame(
  x1 = 15, # 100, -100
  px = .1,
  x2 = 9,
  y1 = 10, # 200, -200
  py = .6,
  y2 = 9,
  rp = 1)

model <- cpt(rp ~ x1 + x2 + px + I(1-px) | y1 + y2 + py + I(1-py), nopt = 2, nout = 2, ref = 40, choicerule = NULL, data = dt, fixed = c(alpha = 0.88, beta = 0.88, lambda = 2.25, gammap = 0.61, gamman = 0.69))

model$predict()



library(ggplot2)
ps <- cbind(seq(0,1,.01), 1 - seq(0,1,.01))
xs <- cbind(rep(1,101),rep(1,101))
ggplot(data.frame(x = ps[,1]), aes(x=x)) +
  geom_line(data= data.frame(x=ps[,1], y = model$wfun(x = -xs, p = ps, gamman = 0.69, gammap = 0.61)[,1]), aes(x=x,y=y), linetype = 2) +
  geom_line(data= data.frame(x=ps[,1], y = model$wfun(x = xs, p = ps, gamman = 0.69, gammap = 0.61)[,1]), aes(y=y), linetype = 3) +
  geom_segment(x=0,xend=1,y=0,yend=1) +
  scale_x_continuous("p", breaks = seq(0,1,.2)) +
  scale_y_continuous("w(p)", breaks = seq(0,1,.2)) +
  ggtitle("Weighting Function")