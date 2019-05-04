#' Linear Model with L1 regularization
#'
#' This algorithm takes the penalty vector as input and iterate through each value in the vector using LinearModelL1
#'
#' @param X.mat unscaled training input matrix, with size n x p 
#' @param y.vec correspond training input matrix, with size n x 1
#' @param penalty.vec a vector with descending numeric number, default as seq(0.5, 0, length.out = 100)
#' @param step.size a scalar with numeric value, default as 0.01
#'
#' @return W.mat matrix, with the size ((feaure.size + 1)  x  n.step.size.length)
#' @export
#'
#' @examples
#' library(L1LinearModel)
#' data(prostate, package = "ElemStatLearn")
#' prostate <- list(features = as.matrix(prostate[, 1:8]), labels = prostate$lpsa, is.01 = FALSE)
#' data.set <- prostate
#' X.mat <- data.set$features
#' y.vec <- data.set$labels
#' LinearModelL1penalties(X.mat, y.vec, seq(0.4, 0.01, -0.01), 0.01)
LinearModelL1penalties <-
  function(X.mat, y.vec, penalty.vec=seq(0.5, 0, length.out = 100), step.size=0.01) {
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
    
    if(!all(is.numeric(step.size),
            length(step.size) == 1,
            step.size > 0
    )){
      stop("step.size must be a positive number")
    }
    
    is.binary <- ifelse((all(y.vec %in% c(0, 1))), TRUE, FALSE)
    
    # Initializing
    n.train <- nrow(X.mat)
    n.features <- ncol(X.mat) # features is p here
    
    # Scale X.mat with m = 0, sd = 1
    feature.mean.vec <- colMeans(X.mat)
    feature.sd.vec <-
      sqrt(rowSums((t(X.mat) - feature.mean.vec) ^ 2) / n.train)
    
    # column with zero variance will become zero at the end
    feature.sd.vec[feature.sd.vec == 0] <- 1
    
    feature.sd.mat <- diag(1 / feature.sd.vec)
    
    X.scaled.mat <-
      t((t(X.mat) - feature.mean.vec) / feature.sd.vec)
    
    initial.weight.vec <- rnorm(n.features + 1)
    
    W.mat <- matrix(0, nrow = n.features + 1, ncol = n.penalties)
    # W.temp.mat <- W.mat
    
    opt.thresh = 0.01;
    
    for (i.penalty in c(1:n.penalties)) {
      W.mat[, i.penalty] <-
        LinearModelL1(X.scaled.mat = X.scaled.mat,
                      y.vec = y.vec,
                      penalty = penalty.vec[i.penalty],
                      opt.thresh = opt.thresh,
                      initial.weight.vec = initial.weight.vec,
                      step.size = step.size)
      
      initial.weight.vec <-
        W.mat[, i.penalty] 
    }
    
    intercept.vec <-
      -feature.mean.vec %*% feature.sd.mat %*% W.mat[-1,] + W.mat[1,] # W.mat is the beta.vec
    W.mat <- rbind(intercept.vec, feature.sd.mat %*% W.mat[-1,])
    
    return(W.mat)
  }