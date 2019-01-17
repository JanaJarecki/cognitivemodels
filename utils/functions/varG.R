# Exact variance between two gambles (without N-1 correction)
varG <- function(p, x) {
  if ( is.matrix(p)) {
    return(sapply(1:dim(p)[1], function(i) varG(p[i, ], x[i, ])))
  }
  ev <- c(x %*% p)
  p %*% ((x - ev)^2)
}