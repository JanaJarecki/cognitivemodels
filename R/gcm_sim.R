Gcm_sim <- R6Class("gcm_sim",
               inherit = Cogscimodel,
               public = list(
                 obs = NULL,
                 input = NULL,
                 cat = NULL,
                 parm = NULL,
                 constr = NULL,
                 metric = NULL,
                 nobj = NULL,
                 ndim = NULL,
                 nval = NULL,
                 input_id_l = NULL,
                 input_id_r = NULL,
                 fixed = NULL, 
                 gofvalue = NULL,
                 initialize = function(formula, data, metric = c("minkowski", "discrete", "threshold"), fixed, choicerule, discount) {
                   self$metric <- match.arg(metric)
                   self$nobj <- length(strsplit(attr(terms(formula), "term.labels"), split = "\\|")[[1]]) # how many objects per trial?
                   self$ndim <- length(all.vars(formula)[-1]) / self$nobj # how many dimensions per object?
                   f <- Formula(formula)
                   self$nval <- length(unique(as.vector(as.matrix(get_all_vars(formula(f, lhs=0, rhs=NULL), data))))) # how many values per dimension?

                   # w = vector with attention weights, sum = 1
                   # c = scalar, sensitivity that discriminate the items on the dimensions
                   # p = scalar for decay function, 1 (exponential) for readily discriminable stimuli or 2 (gaussian) for highly confusable stimuli
                   # r = scalar, distance metric, 1 = city-block for separable-dimension stimuli, 2 = euclidean for integral-dimension stimuli
                   # gamma = scalar, threshold indicating maximal difference still perceived as being no difference
                   allowedparm <- matrix(0, ncol = 4, nrow = self$ndim + 3, dimnames = list(c(self$make_weight_names(), "c", "p", "r"),
                                                                                            c("ll", "ul", "init", "na")))
                   allowedparm[, "ll"] <- c(rep(0, self$ndim), 0.0001, 1, 1)
                   allowedparm[, "ul"] <- c(rep(1, self$ndim), 5, 1, 1)
                   allowedparm[, "init"] <- c(rep(1/self$ndim, self$ndim), 2.5, 1, 1)
                   allowedparm[, "na"] <- c(rep(1/self$ndim, self$ndim), 1, 1, 1)
                   if(self$metric == "threshold") {
                     allowedparm <- rbind(allowedparm, gamma = c(0, self$nval - 2, 1, 0))
                   }
                   super$initialize(formula = formula, data = data, fixed = as.list(fixed), model = "gcm_sim", discount = discount, choicerule = choicerule, allowedparm = allowedparm, response = "continuous")
                   self$input_id_l <- apply(self$input[, 1:3], 1, paste0, collapse = "") # makes a new column for each dimension of each stimulus
                   self$input_id_r <- apply(self$input[, 4:6], 1, paste0, collapse = "")
                 },
                 make_weight_names = function(ndim = self$ndim) {
                   return(c(paste0("w", 1:ndim)))
                 },
                 calc_similarity_matrix = function(x, y, metric = self$metric){
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
                     input <- model.frame(self$formula, newdata, na.action = NULL)[, -1] # makes input id for new data
                     input_id_l <- apply(input[, 1:3], 1, paste0, collapse = "")
                     input_id_r <- apply(input[, 4:6], 1, paste0, collapse = "")
                   } else {
                     input <- self$input # makes input ids for training data
                     input_id_l <- self$input_id_l
                     input_id_r <- self$input_id_r
                   }
                   n <- nrow(input)
                   input <- rbind(self$input, input)
                   sim_mat <- self$calc_similarity_matrix(x = input[, 1:3], y = input[, 4:6])
                   res <- sapply(1:n, function(trial) {
                     id_l <- input_id_l[trial]
                     id_r <- input_id_r[trial]
                     sim_mat[id_l, id_r]
                   })
                   return(res)
                 },
                 calc_distance = function(probe, exemplar, r = self$parm[["r"]], w = self$parm[1:self$ndim], gamma = NULL, cov = NULL){
                   w <- unlist(w)
                   dist <- as.numeric(probe - exemplar)
                   if(self$metric == "minkowski"){
                     dist <- sum( w*abs(dist)^r )^(1/r)
                   } 
                   if(self$metric == "discrete"){
                     dist <- w %*% as.numeric(dist != 0) 
                   }
                   if(self$metric == "threshold"){
                     gamma <- self$parm[["gamma"]]
                     dist <- w %*% as.numeric(dist > gamma)
                   }
                   return(dist)
                 },
                 eqfun = function() {
                   if ( all(paste0("w", 1:self$ndim) %in% self$fixednames) ) {
                     return(list(NULL, NULL))
                   } else {
                     return(list(
                     eqfun = function(pars, self) {
                       sum(pars[1:self$ndim])
                     },
                     eqB = 1))
                     }
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
                   return(grid)
                 }
               )
)

#' @export
gcm_sim <- function(formula, data, metric = c("minkowski", "discrete", "threshold"), fixed, choicerule, discount = 0) {
  obj <- Gcm_sim$new(formula = formula, data = data, metric = metric, fixed = fixed, choicerule = choicerule, discount = discount)
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

Gcm_sim$set("active", "super_", function() super) # colinfay.me/r6-parent-exposure/; super$super doesn't work if we want to expose function at grandparent level from grandchild
