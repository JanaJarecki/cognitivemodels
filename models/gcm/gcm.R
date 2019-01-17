library(R6)
library(Rsolnp)
library(cogsciutils)
source("../../../cogscimodels/utils/classes/cognitiveModel.R")

Gcm <- R6Class("gcm",
               inherit = cognitiveModel,
               public = list(
                 obs = NULL,
                 input = NULL,
                 cat = NULL,
                 parm = NULL,
                 constr = NULL,
                 metric = NULL,
                 ndim = NULL,
                 input_id = NULL,
                 stimulus_frequencies = NULL,
                 fixed = NULL, 
                 gofvalue = NULL,
                 initialize = function(formula, data, cat, metric = c("minkowski", "discrete"), fixed, choicerule, discount) {
                   self$obs <- model.frame(formula, data)[, 1] # observed response
                   self$input <- model.frame(formula, data)[, -1] # feature values
                   self$cat <- model.frame(cat, data)[, 1] # true cat
                   self$ndim <- ncol(self$input)
                   self$metric <- match.arg(metric)
                   self$input_id <- apply(self$input, 1, paste0, collapse = "")
                   self$stimulus_frequencies <- self$calc_stimulus_frequencies()
                   self$fixed <- fixed
                   
                   # w = vector with attention weights, sum = 1
                   # c = scalar, sensitivity that discriminate the items on the dimensions
                   # p = scalar for decay function, 1 (exponential) for readily discriminable stimuli or 2 (gaussian) for highly confusable stimuli
                   # r = scalar, distance metric, 1 = city-block for separable-dimension stimuli, 2 = euclidean for integral-dimension stimuli
                   allowedparm <- matrix(0, ncol = 3, nrow = self$ndim + 3, dimnames = list(c(self$make_weight_names(), "c", "p", "r"),
                                                                                            c("ll", "ul", "init")))
                   allowedparm[, "ll"] <- c(rep(0, self$ndim), 0.0001, 1, 1)
                   allowedparm[, "ul"] <- c(rep(1, self$ndim), 5, 1, 1)
                   allowedparm[, "init"] <- c(rep(1/self$ndim, self$ndim), 2.5, 1, 1)
                   
                   super$initialize(formula = formula, data = data, fixedparm = fixed, model = "gcm", discount = discount, choicerule = choicerule, allowedparm = allowedparm)
                 },
                 make_weight_names = function(ndim = self$ndim) {
                   return(c(paste0("w", 1:ndim)))
                 },
                 calc_stimulus_frequencies = function() {
                   id <- self$input_id
                   id <- factor(id, levels = unique(id))
                   cat <- factor(self$cat)
                   res <- sapply(1:nrow(self$input), function(z) {
                     table(id[1:z], cat[1:z])
                   })
                   rownames(res) <- rep(levels(id), 2)
                   
                   res <- array(res, dim = c((nrow(res)/2), 2, ncol(res)), dimnames = list(levels(id), c(0,1), 1:ncol(res)))
                   return(aperm(res, c(1, 3, 2)))
                 },
                 calc_similarity_matrix = function(x, y){
                   # x: matrix with features of every trial, e.g., learningset
                   # y: matrix with feature combinations to which the similarity should be calculated; e.g., prototypes
                   x <- as.data.frame(unique(x))
                   rownames(x) <- apply(x, 1, paste0, collapse = "")
                   
                   if(missing(y)) {
                     # if no y, calculate similarity of x to x
                     identical <- TRUE
                     y <- x
                     pairs <- as.matrix(combn(nrow(x), 2))
                     sim_mat <- diag(x = nrow(x))
                   } else {
                     # if y, calculate similarity between x and y
                     identical <- FALSE
                     y <- as.data.frame(unique(y))
                     rownames(y) <- apply(unique(y), 1, paste0, collapse = "")
                     pairs <- rbind(rep(1:nrow(x), each = nrow(y)), rep(1:nrow(y), times = nrow(x)))
                     sim_mat <- matrix(0, nrow = nrow(x), ncol = nrow(y))
                   }
                   dimnames(sim_mat) <- list(rownames(x), rownames(y))
                   for(pair in 1:ncol(pairs)) {
                     z <- pairs[, pair]
                     probe <- x[z[1], ]
                     exemplar <- y[z[2], ]
                     distance <- self$calc_distance(probe = probe, exemplar = exemplar)
                     
                     c <- self$parm["c"]
                     p <- self$parm["p"]
                     similarity <- exp(-c * distance^p)
                     sim_mat[row.names(probe), row.names(exemplar)] <- similarity
                   }
                   
                   if(identical) sim_mat[lower.tri(sim_mat)] <- t(sim_mat)[lower.tri(sim_mat)]
                   
                   gc() # garbage collector
                   
                   return(sim_mat)
                 },
                 predict = function(newdata = NULL){
                   if(!is.null(newdata)) {
                     input <- model.frame(self$formula, newdata, na.action = NULL)[, -1]
                     n <- nrow(input)
                     input_id <- apply(input, 1, paste0, collapse = "")
                     stimulus_frequencies <- self$stimulus_frequencies[, nrow(self$input), , drop = FALSE]
                     names <- dimnames(stimulus_frequencies)
                     names[[2]] <- 1:n
                     stimulus_frequencies <- aperm(array(rep(stimulus_frequencies, n), dim = c(8, 2, n), dimnames = names[c(1, 3, 2)]), perm = c(1, 3, 2))
                   } else {
                     stimulus_frequencies <- self$stimulus_frequencies
                     input <- self$input
                     input_id <- self$input_id
                     n <- nrow(input)
                   }
                   ### ONLY FOR GCM
                   sim_mat <- self$calc_similarity_matrix(x = rbind(self$input, input), y = self$input)
                   res <- sapply(1:n, function(trial) {
                     if(trial == 1 & is.null(newdata)) {
                       return(1/length(unique(self$cat)))
                     }
                     id <- input_id[trial]
                     effective_trial <- trial - is.null(newdata)
                     sim_to_cat1 <- stimulus_frequencies[, effective_trial, 2] %*% sim_mat[id, ]
                     
                     total_sim <- rowSums(stimulus_frequencies[, effective_trial, ]) %*% sim_mat[id, ]
                     
                     sim_to_cat1/total_sim
                   })
                   res <- self$applychoicerule(res)
                   return(res)
                 },
                 calc_distance = function(probe, exemplar, r = self$parm["r"], w = self$parm[1:self$ndim]){
                   dist <- as.numeric(probe - exemplar)
                   if(self$metric == "minkowski"){
                     dist <- sum( w*abs(dist)^r )^(1/r)
                   } else {
                     dist <- w %*% as.numeric(dist != 0) # Vector multiplication
                   }
                   return(dist)
                 },
                 fit = function(unidim) {
                   allowedparm <- self$allowedparm
                   LB <- allowedparm[self$freenames, 'll'] 
                   UB <- allowedparm[self$freenames, 'ul']  
                   par0 <- allowedparm[self$freenames, 'init']
                   fun <- function(parm, self) {
                     self$setparm(parm)
                     -cogsciutils::gof(obs = self$obs, pred = self$predict(), type = 'log', response = 'discrete')
                   }
                   
                   if (unidim == FALSE) {
                     
                     equal <- function(parameter, ...) {
                       return(sum(parameter[1:self$ndim]))
                     }
                     fit <- solnp(pars = par0, fun = fun, eqfun = equal, eqB = 1, LB = LB, UB = UB, self = self)
                     pars <- fit$pars
                     negloglik <- tail(fit$values, 1)
                     
                   } else {
                     
                     negloglik <- Inf
                     for(i in 1:self$ndim) {
                       # make all weights to 0 and then make wi to 1
                       self$parm[1:self$ndim] <- 0
                       self$parm[i] <- 1 
                       
                       fit <- solnp(pars = par0, fun = fun, LB = LB, UB = UB, self = self)
                       if(tail(fit$values, 1) < negloglik) {
                         pars <- c(self$parm[1:self$ndim], fit$pars)
                         negloglik <- tail(fit$values, 1)
                       }
                     }
                     
                   }

                   self$setparm(pars)
                   self$gofvalue <- negloglik
                 }
               )
)

gcm <- function(formula, data, cat, metric = c("minkowski", "discrete"), fixed, choicerule, discount = 0) {
  obj <- Gcm$new(formula = formula, data = data, cat = cat, metric = metric, fixed = fixed, choicerule = choicerule, discount = discount)
  if(length(obj$freenames) > 0) {
    obj$fit(unidim = FALSE)
  }
  return(obj)
}
