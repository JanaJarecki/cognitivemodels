library(Rcpp)

minkowski(c(1,1), c(1,1), c(.5,.5), 1)

sim <- function(x,y,lambda = log(10), r = 1, q = 1) {
  exp(lambda * -1 * sum(abs(x-y)))
}

sim(c(1,1,0,0), c(1,1,1,1))
sim(c(1,0,1,0), c(1,1,1,1))
sim(c(0,0,0,1), c(1,1,1,1))
sim(c(1,1,1,0), c(1,1,1,1))

