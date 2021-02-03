# TODO
# => update getCall() to also return the data object

# Nosofsky, R. M. (1989). Further tests of an exemplar-similarity approach to relating identification and categorization. Perception & Psychophysics, 45, 279–290. doi:10.3758/BF03204942
# From Table 3 and Figure 2

test_that("Anova Table", {
  data(nosofsky1989)
  d <- nosofsky1989[nosofsky1989$condition == "angle" & !is.na(nosofsky1989$true_cat), ]
  fitdata <- nosofsky1989[nosofsky1989$condition == "angle", ]
  fml <- pobs ~ angle + size
  n <- fitdata$N
  # no constraints
  m1 <- gcm(fml, data = d, criterion = ~true_cat, fix = list(q = 2, r = 2),
    options = list(fit_args = list(n = n), fit_data=fitdata),
    discount = 0, choicerule = "none")
  # weights constrained
  m2 <- ebm(fml, data = d, criterion = ~true_cat,
    fix = list(q = 2, r = 2, angle = .5, size = .5),
    options = list(fit_data = fitdata, fit_args = list(n = n)),
    discount = 0, choicerule = "none")
  # bias constrained
  m3 <- gcm(fml, data = d, criterion = ~true_cat,
    fix = list(q = 2, r = 2, b0 = .5, b1 = .5),
    options = list(fit_args = list (n = n), fit_data = fitdata),
    discount = 0, choicerule = "none")
  # lm model
  options(warn=-1)
  mlm <- lm(pobs ~ angle + size, data = fitdata)
  options(warn=0)
  do.call(anova, list(m1,m2,m3))
})






# models <- list(A = m1, B = m2, C = m3)
# digits <- 2
# res <- sapply(models, function(x) c(
#   npar = x$npar('free'),
#   LogLik = x$logLik(),
#   AIC = x$AIC(),
#   BIC = x$BIC(),
#   `w[akaike]` = NA))
# res['w[akaike]', ] <- c(akaikeweight(res['AIC', , drop = FALSE]))
# colnames(res) <- abbreviate(colnames(res))
# round(t(res), digits)


