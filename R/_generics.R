nobs <- function(x) {
  UseMethod("nobs")
}
logLik <- function(x) {
  UseMethod("logLik")
}
MSE <- function(x) {
  UseMethod("MSE")
}
AIC <- function(x) {
  UseMethod("AIC")
}
AICc <- function(x) {
  UseMethod("AICc")
}
BIC <- function(x) {
  UseMethod("BIC")
}
add_model <- function(x) {
  UseMethod("add_model", x)
}
fit <- function(x) {
  UseMethod("fit", x)
}
fun <- function(x) {
  UseMethod("fun", x)
}

