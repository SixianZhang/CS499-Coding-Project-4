#' Linear Model with L1 regularization
#'
#' This algorithm takes the penalty vector as input and iterate through each value in the vector using LinearModelL1
#'
#' @param X.mat
#' @param y.vec
#' @param penalty.vec
#' @param step.size
#'
#' @return
#' @export
#'
#' @examples
LinearModelL1penalties <-
  function(X.mat, y.vec, penalty.vec, step.size) {
    # Check type and dimension
    if (!all(is.numeric(X.mat), is.matrix(X.mat))) {
      stop("X.mat must be a numeric matrix")
    }
    
    if (!all(is.numeric(y.vec),
             is.vector(y.vec),
             length(y.vec) == nrow(X.mat))) {
      stop("y.vec must be a numeric vector of length nrow(X.mat)")
    }
    
    is.decending <- function(vec) {
      result <- all(diff(vec) < 0)
      return(result)
    }
    
    if (!all(
      is.vector(penalty.vec),
      is.numeric(penalty.vec),
      penalty.vec >= 0,
      is.decending(penalty.vec)
    )) {
      stop("penalty.vec must be a non-negative decreasing numeric vector")
    }
    
    if (!all(is.numeric(opt.thresh),
             length(opt.thresh) == 1,
             opt.thresh > 0)) {
      stop("opt.thresh must be a positive numeric scalar")
    }
    
    # Initializing
    n.train <- nrow(X.mat)
    n.features <- ncol(X.mat) # features is p here
    n.penalties <- length(penalty.vec)
    
    # Scale X.mat with m = 0, sd = 1
    feature.mean.vec <- colMeans(X.mat)
    feature.sd.vec <-
      sqrt(rowSums((t(X.mat) - feature.mean.vec) ^ 2) / n.train)
    
    # column with zero variance will become zero at the end
    feature.sd.vec[feature.sd.vec == 0] <- 1
    
    feature.sd.mat <- diag(1 / feature.sd.vec)
    
    X.scaled.mat <-
      t((t(X.mat) - feature.mean.vec) / feature.sd.vec)
    
    initial.weight.vec <- rep(0, n.features + 1)
    
    W.mat <- matrix(0, nrow = n.features + 1, ncol = n.penalties)
    # W.temp.mat <- W.mat
    
    for (i.penalty in c(1:n.penalties)) {
      W.mat[, i.penalty] <-
        LinearModelL1(X.scaled,
                      y.vec,
                      penalty.vec[i.penalty],
                      opt.thresh,
                      initial.weight.vec,
                      step.size)
      
      initial.weight.vec <-
        W.mat[, i.penalty] 
    }
    
    intercept.vec <-
      -feature.mean.vec %*% feature.sd.mat %*% W.mat[-1,] + W.mat[1,] # W.mat is the beta.vec
    W.mat <- rbind(intercept.vec, feature.sd.mat %*% W.mat[-1,])
    
    return(W.mat)
  }