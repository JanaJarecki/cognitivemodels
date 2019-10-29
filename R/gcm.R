#' @export
Gcm <- R6Class("gcm",
               inherit = Cogscimodel,
               public = list(
                 obs = NULL,
                 input = NULL,
                 cat = NULL,
                 parm = NULL,
                 constr = NULL,
                 metric = NULL,
                 ndim = NULL,
                 nval = NULL,
                 input_id = NULL,
                 stimulus_frequencies = NULL,
                 fixed = NULL, 
                 gofvalue = NULL,
                 initialize = function(formula, data, cat, metric = c("minkowski", "discrete", "threshold", "mahalanobis"), fixed, choicerule, discount) {
                   self$cat <- get_all_vars(cat, data)[, 1] # true cat
                   self$metric <- match.arg(metric)
                   self$ndim <- length(attr(terms(formula), "term.labels"))
                   f <- Formula(formula)
                   self$nval <- length(unique(as.vector(as.matrix(get_all_vars(formula(f, lhs=0, rhs=NULL), data)))))
                   
                   # w = vector with attention weights, sum = 1
                   # c = scalar, sensitivity that discriminate the items on the dimensions
                   # p = scalar for decay function, 1 (exponential) for readily discriminable stimuli or 2 (gaussian) for highly confusable stimuli
                   # r = scalar, distance metric, 1 = city-block for separable-dimension stimuli, 2 = euclidean for integral-dimension stimuli
                   allowedparm <- matrix(0, ncol = 4, nrow = self$ndim + 3, dimnames = list(c(self$make_weight_names(), "c", "p", "r"),
                                                                                            c("ll", "ul", "init", "na")))
                   allowedparm[, "ll"] <- c(rep(0, self$ndim), 0.0001, 1, 1)
                   allowedparm[, "ul"] <- c(rep(1, self$ndim), 5, 1, 1)
                   allowedparm[, "init"] <- c(rep(1/self$ndim, self$ndim), 2.5, 1, 1)
                   allowedparm[, "na"] <- c(rep(1/self$ndim, self$ndim), 1, 1, 1)
                   if(self$metric == "threshold") {
                     allowedparm <- rbind(allowedparm, gamma = c(0, self$nval - 2, 1, 0))
                   }
                   
                   super$initialize(formula = formula, data = data, fixed = as.list(fixed), model = "gcm", discount = discount, choicerule = choicerule, allowedparm = allowedparm, response = "discrete")
                   
                   self$input_id <- apply(self$input, 1, paste0, collapse = "")
                   self$stimulus_frequencies <- self$calc_stimulus_frequencies()
                 },
                 make_weight_names = function(ndim = self$ndim) {
                   return(c(paste0("w", 1:ndim)))
                 },
                 cum_mean_by_cat = function(z, c, c_value) {
                   return( round(cumsum(z * (c == c_value))/cumsum(c == c_value), 2) )
                 },
                 cum_median_by_cat = function(z, c, c_value) {
                   return( sapply(1:length(z), function(i) median(z[1:length(z) <= i & c == c_value])) )
                 },
                 make_prototype = function(f = self$input, c = self$cat, fun = self$cum_median_by_cat, last_only = TRUE) {
                   # f = feature values; c = categories; fun = aggregation function
                   x1 <- apply(f, 2, function(z) {
                     do.call(fun, list(z = z, c = c, c_value = 1))
                   })
                   
                   x0 <- apply(f, 2, function(z) {
                     do.call(fun, list(z = z, c = c, c_value = 0))
                   })
                   
                   # rownames(x1) <- apply(x1, 1, paste0, collapse = "")
                   # rownames(x0) <- apply(x0, 1, paste0, collapse = "")
                   gc()

                   if(last_only) {
                     return( as.data.table(rbind(tail(x0, 1), tail(x1, 1))) )
                   } else {
                     return( list(x0, x1) )
                   }
                 },
                 calc_cov = function(f = self$input, c = self$cat) {
                   return(list(var(f[c == 0, ]), var(f[c == 1, ])))
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
                 calc_similarity_matrix = function(x, y, cov = NULL){
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
                     if(!is.null(cov)) {
                       distance <- self$calc_distance(probe = probe, exemplar = exemplar, cov = cov[[z[2]]])
                     } else {
                       distance <- self$calc_distance(probe = probe, exemplar = exemplar)
                     }
                     
                     c <- self$parm[["c"]]
                     p <- self$parm[["p"]]
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
                   if(self$metric == "mahalanobis") {
                     means <- self$make_prototype()
                     cov <- self$calc_cov()
                     sim_mat <- self$calc_similarity_matrix(x = rbind(self$input, input), y = means, cov = cov)
                   } else {
                     sim_mat <- self$calc_similarity_matrix(x = rbind(self$input, input), y = self$input)
                   }
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
                 calc_distance = function(probe, exemplar, r = self$parm[["r"]], w = self$parm[1:self$ndim], gamma = NULL, cov = NULL){
                   w <- unlist(w)
                   dist <- as.numeric(probe - exemplar)
                   if(self$metric == "minkowski"){
                     dist <- sum( w*abs(dist)^r )^(1/r)
                   } 
                   if(self$metric == "discrete"){
                     dist <- w %*% as.numeric(dist != 0) # Vector multiplication
                   }
                   if(self$metric == "threshold"){
                     gamma <- self$parm[["gamma"]]
                     dist <- w %*% as.numeric(dist > gamma) # Vector multiplication
                   }
                   if(self$metric == "mahalanobis"){
                     dist <- mahalanobis(x = probe, center = unlist(c(exemplar)), cov = cov)
                   }
                   return(dist)
                 },
                 eqfun = function() {
                   return(list(
                     eqfun = function(pars, self) {
                       sum(pars[1:self$ndim])
                     },
                     eqB = 1
                   )
                   )
                 },
                 parGrid = function(offset) {
                   if(self$metric != "threshold"){
                     grid <- MakeGridList(
                       names = self$freenames,
                       ll = self$allowedparm[, 'll'] + offset,
                       ul = self$allowedparm[, 'ul'] - offset,
                       nsteps = list(w = 4, c = 4, tau = 4),
                       sumto = list('w' = paste0("w", 1:self$ndim)),
                       regular = TRUE,
                       offset = 0)
                   } 
                   # else {
                   #   grid <- MakeGridList(
                   #     names = self$freenames,
                   #     ll = self$allowedparm[, 'll'] + offset,
                   #     ul = self$allowedparm[, 'ul'] - offset,
                   #     nsteps = list(w = 4, c = 4, tau = 4, gamma = 2),
                   #     sumto = list('w' = paste0("w", 1:self$ndim)),
                   #     regular = TRUE,
                   #     offset = 0)
                   # }
                   return(grid)
                 }
                 # fit = function(type = "solnp") {
                 #   super$fit(type = type, eqfun = eqfun, eqB = eqB)
                 # }
               )
)

#' @export
gcm <- function(formula, data, cat, metric = c("minkowski", "discrete", "threshold"), fixed, choicerule, discount = 0) {
  obj <- Gcm$new(formula = formula, data = data, cat = cat, metric = metric, fixed = fixed, choicerule = choicerule, discount = discount)
  if(length(obj$freenames) > 0) {
    if(obj$metric == "threshold") {
      gofs <- vector("numeric", length = obj$nval - 2)
      parms <- matrix(nrow = obj$nval - 1, ncol = length(obj$parm), dimnames = list(NULL, names(obj$parm)))
      for(i in 1:(obj$nval - 2)) {
        obj$parm$gamma <- i - 1
        obj$fit(type = "solnp")
        gofs[i] <- obj$gofvalue
        parms[i, ] <- unlist(obj$parm)
      }
      obj$setparm(parms[which.min(gofs), ])
      obj$gofvalue <- min(gofs)
    } else {
      obj$fit(type = "solnp") # c("grid", "solnp")
    }
  }
  return(obj)
}

Gcm$set("active", "super_", function() super) # colinfay.me/r6-parent-exposure/; super$super doesn't work if we want to expose function at grandparent level from grandchild
