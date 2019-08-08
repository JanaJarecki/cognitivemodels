context("bayeslearn")
library(cogscimodels)

D <- data.frame(obs1 = c(0,0,1), obs2 = c(1,1,0), obs3 = c(0,0,0))
DC <- as.data.frame(apply(D, 2, cumsum))

fp <- list(prior.obs1=1, prior.obs2=1, prior.obs3=1, prior.obs1.i=1)

test_that('Predicted Values of Bayesian Updating', {
  m <- bayeslearn(~ obs1 + obs2, data = D, fixed = fp[1:2])
  mc <- bayeslearn(~ obs1 + obs2, data = DC, fixed = fp[1:2], format = 'count')
  expect_equal(m$predict('map'), cbind(obs1 = c(1/3, 1/4, 2/5), obs2 = c(2/3, 3/4, 3/5)))
  expect_equal(mc$predict('map'), cbind(obs1 = c(1/3, 1/4, 2/5), obs2 = c(2/3, 3/4, 3/5)))

  m <- bayeslearn(~ obs1, data = D, fixed = fp[c(1,4)])
  mc <- bayeslearn(~ obs1, data = DC, format = 'count', fixed = fp[c(1,4)])
  expect_equal(m$predict('map'), cbind(c(1/3, 1/4, 2/5), c(2/3, 3/4, 3/5)))
  expect_equal(mc$predict('map'), cbind(c(1/3, 1/4, 2/5), c(2/3, 3/4, 3/5)))

  m <- bayeslearn(~ obs1 + obs2 + obs3, data = D, fixed = fp[1:3])
  mc <- bayeslearn(~ obs1 + obs2 + obs3, data = DC, fixed = fp[1:3], format = 'count')
  expect_equal(m$predict('map'), cbind(obs1=c(1/4, 1/5, 2/6), obs2=c(2/4, 3/5, 3/6), obs3=c(1/4, 1/5, 1/6)))
  expect_equal(mc$predict('map'), cbind(obs1=c(1/4, 1/5, 2/6), obs2=c(2/4, 3/5, 3/6), obs3=c(1/4, 1/5, 1/6)))
})

test_that('Model types', {
  m <- bayeslearn(~ obs1 + obs2, data = D, fixed = fp[c(1,2)])
  expect_equal(m$type, 'binomial')
  m <- bayeslearn(~ obs1, data = D, fixed = fp[c(1,4)])
  expect_equal(m$type, 'binomial')
  m <- bayeslearn(~ obs1 + obs2 + obs3, data = D, fixed = fp[1:3])
  expect_equal(m$type, 'multinomial')
})

test_that('Error handling', {
  expect_error(bayeslearn(~ obs1 + obs2, data = D, fixed = list(prior.x1=1, prior.obs2=1)))
  expect_error(bayeslearn(~ obs1 + obs2, data = D, fixed = list(prior=1)))
  expect_error(bayeslearn(~ obs1 + obs2, data = DC, format = 'raw'))
  expect_error(bayeslearn(~ obs1, data = DC + 2, format = 'count'))
})


D <- data.frame(y = c(0), sd = c(.3), a = c(0,0), b = c(5,6), c = c(10,11), d = c(0,0))
m <- bayeslearn(y + sd ~ a + b, D, format = 'count', fixed = list(prior.a = 1, prior.b = 1), fit.options = list(options = list(pdf = 'dnorm')))
m$predict('hyperparm')






library(themejj)
theme_set(themejj(base_family = 'Arial'))
theme_update(axis.text = element_text(size = rel(.6)))
library(data.table)
library(patchwork)


T <- c(5, 30)
priors <- list(c(1,1), c(.5,.5), c(1.99,.01), c(.01,1.99))

plot_density <- function(T, ngains, priors, title) {
  Modellist <- lapply(seq_along(T), function(t) lapply(1:length(priors), function(p) {
    D <- data.frame(y=1, gain = ngains[t], loss =  T[t] - ngains[t])
    betapar <- priors[[p]]
    bayeslearn(y ~ gain + loss, D, fixed = list(prior.gain = betapar[1], prior.loss = betapar[2]), format = 'count')
  }))

  Ms <- lapply(Modellist, function(x) lapply(x, function(m) m$predict('pmean')['gain']))
  Means <- data.table::rbindlist(Ms, id = 'trials')
  Means <- melt(Means, 1, value.name = 'M', variable.name = 'priors')
  Means[, trials := factor(trials, labels = paste0(T, ' (', ngains, ')'))]
  Means[, priors := factor(priors, labels = paste('Beta ~', gsub('\\(|c|\\)','',..priors)))]

  xx <- round(seq(0,1,.001), 3)
  Ds <- lapply(Modellist, function(x) lapply(x, function(m) {
    shapes <- as.vector(m$predict('hyperparm'))
    pred <- dbeta(xx, shapes[1], shapes[2])
  }))
  Draws <- data.table::rbindlist(Ds, id = 'trials')
  Draws[, xx := rep(..xx, length(T))]
  Draws <- melt(Draws, id.vars = c('xx', 'trials'), value.name = 'dens', variable.name = 'priors')
  Draws[, trials := factor(trials, labels = paste0(T, ' (', ngains, ')'))]
  Draws[, priors := factor(priors, labels = paste('Beta ~', gsub('\\(|c|\\)','',..priors)))]

  Means[, Mround := round(M, 3)]
  Means <- Draws[Means, on = c('trials', 'priors', 'xx'='Mround')]

  true <- ngains[1]/T[1]

  p <- ggplot(Draws, aes(xx, dens)) +
    geom_path(aes(linetype = trials)) +
    geom_segment(data = Means, aes(linetype = trials, x = M, xend = M, y = 0, yend = dens)) +
    geom_segment(x=true, xend=true, y=-Inf, yend=0, color = 'red', size = 1.5) +
    geom_point(data=Means, aes(M, 0.15, shape = trials, fill = trials), size = 1.6) +
    scale_x_continuous('Posterior beliefs about Pr(gain) given different prior beliefs', expand = c(0,0), limits = c(-0.1,1.1),  breaks = c(0,true,0.5,1), labels = scales::percent) +
    scale_y_continuous('Density', expand = c(0,0), limits = c(-0.5, NA)) +facet_wrap(~priors, nrow = 1) +
    theme(panel.spacing.x = unit(5, 'mm'),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
      axis.line = element_blank(),
      plot.title = element_text(hjust = -.03),
      legend.position = 'right',
      legend.direction = 'vertical',
      legend.key.width = unit(5, 'mm')) +
    scale_fill_manual('Sample Size (# gains)', values = c('white', 'black')) +
    labs(title = title) +
    scale_shape_manual('Sample Size (# gains)', values = c(21,19))
    if (length(T) > 1)
      p <- p +
      scale_linetype_manual('Sample Size (# gains)', values = c(2,1))
    return(p)
}

plota <- plot_density(T = 0, ngains = 0, priors = priors, '(a) Prior') +theme(axis.title.x = element_blank(), legend.position = 'none')
plotb <- plot_density(T = T, ngains = 0.2*T, priors = priors, '(b) Posterior, True Pr(gain) = 20%') +theme(axis.title.x = element_blank(), strip.text = element_blank())
plotc <- plot_density(T = T, ngains = 0.8*T, priors = priors, '(c) Posterior, True Pr(gain) = 80%') +theme(legend.title = element_blank(), strip.text = element_blank())
plota + plotb + plotc +plot_layout(nrow = 3)