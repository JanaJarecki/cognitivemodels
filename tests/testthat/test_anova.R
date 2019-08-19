context("anova")

##
# TODO
#
# => update getCall() to also return the data object

# Nosofsky, R. M. (1989). Further tests of an exemplar-similarity approach to relating identification and categorization. Perception & Psychophysics, 45, 279–290. doi:10.3758/BF03204942
# From Table 3 and Figure 2
data(nosofsky1989)
d <- nosofsky1989[nosofsky1989$condition == 'angle', ]
fitdata <- d[!is.na(d$true_cat), ]
nn <- d$N
fml <- pobs ~ angle + size | true_cat
# no constraints
m1 <- ebm(fml,
           data = fitdata,
           fixed = list(q = 2, r = 2),
           type = 'choice',
           fit.options = list(n = nn, newdata = d),
           discount = 0)
# weights constrained
m2 <- ebm(fml,
         data = fitdata,
         fixed = list(q = 2, r = 2, angle = .5, size = .5),
         type = 'choice',
         fit.options = list(n = nn, newdata = d),
         discount = 0)
# bias constrained
m3 <- ebm(fml,
          data = fitdata,
          fixed = list(q = 2, r = 2, b0 = .5, b1 = .5),
          type = 'choice',
          fit.options = list(n = nn, newdata = d),
          discount = 0)
# lm model
mlm <- lm(pobs ~ angle + size, data = fitdata)
mglm <- glm(fml, data = fitdata, family = 'binomial')

print(anova(m1,m2,m3), signif.legend = FALSE)
print(anova(m1,m2,m3))

anova(mglm,mlm)
anova(mlm,mglm)
do.call(anova, list(m1,m2))







# models <- list(A = m1, B = m2, C = m3)
# digits <- 2
# res <- sapply(models, function(x) c(
#   npar = x$nparm('free'),
#   LogLik = x$logLik(),
#   AIC = x$AIC(),
#   BIC = x$BIC(),
#   `w[akaike]` = NA))
# res['w[akaike]', ] <- c(akaikeweight(res['AIC', , drop = FALSE]))
# colnames(res) <- abbreviate(colnames(res))
# round(t(res), digits)


