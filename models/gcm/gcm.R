library(R6)

gcm <- R6Class("gcm",
               public = list(
                 obs = NULL,
                 input = NULL,
                 cat = NULL,
                 parm = NULL,
                 metric = NULL,
                 ndim = NULL,
                 input_id = NULL,
                 stimulus_frequencies = NULL,
                 initialize = function(formula, data, cat, metric = c("minkowski", "discrete")) {
                   self$obs <- model.frame(formula, data)[, 1] # observed response
                   self$input <- model.frame(formula, data)[, -1] # feature values
                   self$cat <- model.frame(cat, data)[, 1] # true cat
                   self$ndim <- ncol(self$input)
                   self$metric <- match.arg(metric)
                   self$input_id <- apply(self$input, 1, paste0, collapse = "")
                   self$stimulus_frequencies <- self$calc_stimulus_frequencies()
                   
                   # w = vector with attention weights, sum = 1
                   # c = scalar, sensitivity that discriminate the items on the dimensions
                   # p = scalar for decay function, 1 (exponential) for readily discriminable stimuli or 2 (gaussian) for highly confusable stimuli
                   # r = scalar, distance metric, 1 = city-block for separable-dimension stimuli, 2 = euclidean for integral-dimension stimuli
                   allowedparm <- matrix(0, ncol = 3, nrow = self$ndim + 3, dimnames = list(c(paste0("w", 1:self$ndim), "c", "p", "r"),
                                                                                            c("ll", "ul", "init")))
                   allowedparm[, "ll"] <- c(rep(0, self$ndim), 0.0001, 1, 1)
                   allowedparm[, "ul"] <- c(rep(1, self$ndim), 5, 1, 1)
                   allowedparm[, "init"] <- c(rep(1/self$ndim, self$ndim), 2.5, 1, 1)
                   self$parm <- allowedparm[, "init"]
                 },
                 calc_stimulus_frequencies = function() {
                   id <- self$input_id
                   print(id)
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
                   x <- as.data.frame(unique(self$input))
                   rownames(x) <- self$input_id
                   
                   if(missing(y)) {
                     # if no y, calculate similarity of x to x
                     identical <- TRUE
                     y <- x
                     pairs <- as.matrix(combn(nrow(x), 2))
                     sim_mat <- diag(x = nrow(x))
                   } else {
                     # if y, calculate similarity between x and y
                     identical <- FALSE
                     y <- y[, grepl("^Dim", colnames(y))]
                     y <- as.data.frame(unique(y))
                     rownames(y) <- apply(y, 1, paste0, collapse = "")
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
                 predict = function(){
                   ### ONLY FOR GCM
                   sim_mat <- self$calc_similarity_matrix(x = self$input)
                   
                   res <- sapply(1:nrow(self$input), function(trial) {
                     if(trial == 1) {
                       return(1/self$ndim)
                     }
                     id <- self$input_id[trial]
                     sim_to_cat1 <- self$stimulus_frequencies[, (trial-1), 2] %*% sim_mat[id, ]
                       
                     total_sim <- rowSums(self$stimulus_frequencies[, (trial-1), ]) %*% sim_mat[id, ]
                     
                     sim_to_cat1/total_sim
                   })
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
                 }
               )
)