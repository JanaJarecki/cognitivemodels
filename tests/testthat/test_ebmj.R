# Juslin, P., Olsson, H., & Olsson, A.-C. (2003). Exemplar effects in categorization and multiple-cue judgment. Journal of Experimental Psychology: General, 132, 133-156. doi: 10.1037/0096-3445.132.1.133
context("ebm")

# 0. Data set, standard parameters, and tolerance
dt <- data.frame(
  f1 = c(1, 1, 2, 2),
  f2 = c(1, 2, 1, 2),
  rp = c(12, 14, 14, 16),
  c = c(10, 20, 5, 15)) 
par <- c(f1 = .5, f2  = .5, r = 1.5, q = 1.5, lambda = 1)
M <- ebm_j(rp ~ f1 + f2, ~c, data = dt, fix = par)
tol <- .01 

r <- par["r"]
q <- par["q"]
lambda <- par["lambda"]
w <- par[c("f1", "f2")]
x <- dt[, 1:2]
pred <- sapply(1:nrow(dt), function(i) {
  p <- unlist(x[i, ])
  dist <- as.matrix(abs(sweep(x, 2, p)))
  dist <- ((dist^r) %*% w)^(1/r)
  sim <- exp(-lambda * dist^q)
  (dt[, "c"] %*% sim) / sum(sim)
})
pred == M$predict()  
