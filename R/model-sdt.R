ds <- seq(0,3,.5)
sigmas <- c(1,1)
means <- c(0,2)
n <- 10000

# Manually
res <- sapply(ds, function(d) mean(rnorm(n, 0+d, sig) > rnorm(n, 0, sig)))
plot(data.frame(d=ds,r=res))

# Linear amplifier model (transducer)
# mu(c) = beta * c
# beta * ln(c)
# (beta * c)^gamma
# Integration
nafmax <- function(x, ms, ss, mn, sn) {
  # average prob that x is greater than signal given x jointly from all noise responses
  # p(< x | signal), under the signal distribution ~ N(ms, sds)
  p_leq_signal <- dnorm(x, ms, ss)
  # Joint p(x | noises), given all noise responses ~ N(mn, sn)
  joint_p_noises <- prod(pnorm(x, mn, sn))^(length(mn) - 1)
  
  return(p_leq_signal * joint_p_noises)
  # p(x under noise) * p(x > signal)
  #return(dnorm(x, m1, sd1) * (1-pnorm(x, m2, sd2)))
}

predict <- function(means, sigmas) {
  y <- integrate(
    f = nafmax,
    lower = means[1]-5*sigmas[1],
    upper = means[2]+5*sigmas[2],
    ms = means[1],
    ss = sigmas[1],
    mn = means[2:length(means)],
    sn = sigmas[2:length(sigmas)]
  )
  return(y$value)
}

predict(means, sigmas)

res <- sapply(ds, function(d) predict(c(0, 0+d), sigmas=sigmas))
plot(data.frame(d=ds,r=res))
