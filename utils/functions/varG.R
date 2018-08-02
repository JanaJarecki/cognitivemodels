# Exact variance between two gambles (without N-1 correction)
varG <- function(p, x) {
   ev <- c(x %*% p)
   p %*% ((x - ev)^2)
}