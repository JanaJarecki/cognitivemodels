nobs <- function(x) {
  UseMethod("nobs")
}
npar <- function(x) {
  UseMethod("npar")
}
logLik <- function(x) {
  UseMethod("logLik")
}
SSE <- function(x) {
  UseMethod("SSE")
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
add_model <- function(x, ...) {
  UseMethod("add_model", x)
}
#' @export
fun <- function(x, ...) {
  UseMethod("fun", x)
}
end <- function(x, ...) {
  UseMethod("end", x)
}
`%+%` <- function(x, ...) {
  UseMethod("%+%", x)
}