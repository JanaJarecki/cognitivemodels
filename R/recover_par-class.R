# ==========================================================================
# Package: Cognitivemodels
# File: recover_par-class.R
# Author: Jana B. Jarecki
# ==========================================================================

# ==========================================================================
# Class for recovering parameters
# ==========================================================================


#' Class for parameter recovery objects
#' 
#' Parameter recovery for a model works as follows: First, select a set of true model parameters \eqn{\theta}. Second, simulate data from a cognitive model, i.e. an object of class `cm`, with the true parameters \eqn{\theta}. Third, use the simulated data to estimate the cognitive model's free parameters, store the best-fitting parameter estimates \eqn{\theta*}. Repeat step 1-2 a number of runs Lastly, diagnose the results if the recovered parameter equal the true parameter.
#' 
#' @import data.table
#' @param model An object of class cm.
#' @param nruns (optional, default 100) Integer, how often to repeat the recovery per set of true parameter?
#' @param nsteps (optional, default 3) list with steps by which to vary each parameter.
#' @param seed (optional) Random seed for reproducibility.
#' @details Conducts a parameter recovery from  cognitive model objects (\link{Cm} objects).
#' @author Jana B. Jarecki, \email{jj@janajarecki.com}
#' @examples 
#' No examples yet
#' @noRd
recover_par <- function(model, nruns = 100, nsteps = list(), seed = 98634) {
  .args <- as.list(rlang::call_standardise(match.call())[-1])
   return(do.call(what = Recover_par$new, args = .args, envir = parent.frame()))
}

#' @noRd
Recover_par <- R6Class(
  'recover_par',
  public = list(
    model = NULL,
    parameter = NULL,
    nruns = NULL,
    nsteps = NULL,
    seed = NULL,
    true = NULL,
    recovered = NULL,
    initialize = function(model, nruns = NULL, nsteps = NULL, seed = NULL) {
      if (!is.null(seed)) {
        set.seed(seed)
      }
      self$model <- model$model
      self$nruns <- nruns
      self$seed <- seed
      self$recover(model, nsteps, nruns)
      self$true <- data.table(self$true, key = 'id')
      self$recovered <- data.table(self$recovered, key = 'id')
      self$parameter <- names(self$true)[-1]
    },
    recover = function(model, nsteps, nruns) {
      oldpar <- model$get_par()
      oldpred <- model$pred
      oldobs <- model$res
      pargrid <- model$make_pargrid(nsteps = nsteps)
      parid <- pargrid$ids
      truepar <- pargrid$par
      npar <- length(truepar)
      nparset <- nrow(parid)
      recovered <- sapply(seq_len(nrow(parid)), function(id, pargrid) {
        model$set_par(get_id_in_grid(id, pargrid))
          runs <- sapply(seq_len(nruns), function(x) {
            model$res[] <- model$simulate()
            model$fit()
            unlist(model$get_par("free"), use.names = FALSE)
          })
          unlist(runs)        
      }, pargrid = pargrid)
      recovered <- matrix(c(recovered), nc = length(truepar), byrow = TRUE, dimnames = list(NULL, model$get_parnames('free')))
      recovered <- cbind(id = rep(seq_len(nparset), each = nruns),
        run = rep(seq_len(nruns), times = nparset),
        recovered)

      true <- t(sapply(seq_len(nrow(parid)), function(id, paggrid) {
        get_id_in_grid(id, pargrid)
      }))
      true <- cbind(id = seq_len(nparset), true)

      model$set_par(oldpar)
      model$pred <- oldpred

      self$true <- as.data.table(true)
      self$recovered <- as.data.table(recovered)
    },
    summary = function() {
    },
    plot = function(color = 'blue', R = TRUE) {
      dl <- self$long()
      p <- ggplot(dl, aes(x = true, y = recovered)) +geom_abline(linetype = 2, size = .7) +geom_point(shape = 21) +facet_wrap(~variable, labeller = label_parsed, scales = 'free') +theme(aspect.ratio = 1, panel.background = element_rect(color = 'grey'), panel.grid = element_blank(), strip.text = element_text(face = 2)) +geom_smooth(method = 'lm', se = FALSE, size = .7, color = color) +xlab('True parameter') + ylab('Recovered parameter')
      if (R) {
        labels <- dl[, .(label = paste0('r = ', sprintf('%.2f', round(cor(true,recovered),2)))), by=variable]
        p <- p +geom_label(data = labels, aes(label = label, x= -Inf, y=Inf), hjust = -0.1, vjust = 1.05, alpha = .8, label.padding = unit(1, 'mm'), label.r = unit(0, 'mm'), label.size = 0, size = 2.5, color = color)
        }
      return(p)
    },
    plotmarginal = function(color = 'red') {
      if (length(self$parameter) != 2) {
        stop('Margina plots are only implemented for two parameters.')
      }

      parameter <- self$parameter[1]
      xpar <- setdiff(self$parameter, parameter)
      ids <- self$recovered$id      
      d <- self$true[id %in% ids][self$recovered[id %in% ids, c('id','run',..parameter)]]
      p1 <- ggplot(d, aes_string(paste0('factor(',xpar,')'), paste0('i.',parameter))) +geom_hline(data = self$true, aes_string(yintercept = parameter), linetype = 2)  +geom_jitter(shape = 21, color = 'grey', width = 0.1) +geom_violin(scale = 'width', width = .3, fill = NA) +geom_point(stat = 'summary', fun.y = 'mean', color = color) +facet_wrap(as.formula(paste("~", parameter)), ncol = 1, labeller = label_parsed) +xlab(parse(text=paste0('True~', xpar))) + ylab(parse(text=paste0('Recovered~', parameter))) +ggtitle(parse(text=parameter)) +theme(plot.title=element_text(hjust=.5))

      parameter2 <- self$parameter[2]
      xpar2 <- setdiff(self$parameter, parameter2)
      ids <- self$recovered$id
      d <- self$true[id %in% ids][self$recovered[id %in% ids, c('id','run',..parameter2)]]
      p2 <- ggplot(d, aes_string(paste0('factor(',xpar2,')'), paste0('i.',parameter2))) +geom_hline(data = self$true, aes_string(yintercept = parameter2), linetype = 2)  +geom_jitter(shape = 21, color = 'grey', width = 0.1) +geom_violin(scale = 'width', width = .3, fill = NA) +geom_point(stat = 'summary', fun.y = 'mean', color = color) +facet_wrap(as.formula(paste("~", parameter2)), ncol = 1, labeller = label_parsed) +xlab(parse(text=paste0('True~', xpar2))) + ylab(parse(text=paste0('Recovered~', parameter2))) +ggtitle(parse(text=parameter2)) +theme(plot.title=element_text(hjust=.5))
        p1 + p2
    },
    long = function() {
      data.table::melt(self$recovered, 1:2, value = 'recovered')[data.table::melt(self$true, 1, value = 'true'), on = c('id', 'variable')]
    },
    print = function(digits = 2) {
      cat('Parameter recovery for model: ', self$model, '\n')
      cat('  Runs: ', max(self$recovered$run), '\n')
      cat('  Parameters: ', self$parameter, '\n')
    })
  )