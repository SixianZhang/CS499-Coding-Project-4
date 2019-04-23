#' Linear Model algorithm with L1 regularization
#'
#' This algorithm takes one penalty value
#'
#' @param X.scaled.mat
#' @param y.vec
#' @param penalty
#' @param opt.thresh
#' @param initial.weight.vec
#' @param step.size
#'
#' @return
#' @export
#'
#' @examples
LinearModelL1 <-
  function(X.scaled.mat,
           y.vec,
           penalty,
           opt.thresh,
           initial.weight.vec,
           step.size) {
    # Check type and dimension
    if (!all(is.numeric(X.scaled.mat), is.matrix(X.scaled.mat))) {
      stop("X.scaled.mat must be a numeric matrix")
    }
    
    if (!all(is.numeric(y.vec),
             is.vector(y.vec),
             length(y.vec) == nrow(X.scaled.mat))) {
      stop("y.vec must be a numeric vector of lenght nrow(X.scaled.mat).")
    }
    
    if (!all(is.numeric(penalty), length(penalty) == 1, penalty >= 0)) {
      stop("penalty must be a non-negative numeric scalar")
    }
    
    if (!all(is.numeric(opt.thresh),
             length(opt.thresh) == 1,
             opt.thresh > 0)) {
      stop("opt.thresh must be a positive numeric scalar")
    }
    
    if (!all(
      is.numeric(initial.weight.vec),
      is.vector(initial.weight.vec),
      length(initial.weight.vec) == ncol(X.scaled.mat) + 1
    )) {
      stop("initial.weight.vec must be a numeric vector of length ncol(X.scaled.mat) + 1") # <- Change here
    }
    
    sigmoid <- function(x) {
      return(1 / 1 + exp(-x))
    }
    
    sign <- function(w){
      if (w > 0)
        return(1)
      else if (w = 0)
        return(0)
      else 
        return(-1)
    }

    soft <- function(w, lambda) {
      l <- abs(w) - lambda
      return(sign(w) * ifelse(l > 0, l, 0))
    }
    
    # Initializing
    is.binary <- ifelse(y.vec %in% c(0, 1), TRUE, FALSE)
    max.iteration <- 10000L
    
    if (is.binary) {
      y.vec <- ifelse(y.vec == 0, -1, 1)
    }
    
    n.features <- ncol(X.scaled.mat)   # p 
    n.trains <- nrow(X.scaled.mat)  # n 

    X.train <- cbind(1,X.scaled.mat) # n x (p+1)    
    w.vec <- rnorm(n.features) # p x 1
    intercept <- rnorm(1)

    n.iteration <- 0
    
    while (all(norm(abs(w.gradient.vec)) > opt.thresh, n.iteration <= max.iteration,
               abs(intercept.gradient) > opt.thresh)) {
      
      n.iteration <- n.iteration + 1
      
      if (is.binary) {
        # do logistic
        w.gradient.vec <-
          -t(X.train) %*% (y.vec / (1 + exp(y.vec * (
            X.train %*% w.vec
          ))))
        
        u.vec <- w.vec - step.size * w.gradient.vec
        
        w.vec <- c(u.vec[1], soft(u.vec[-1], step.size * penalty))
        
      } else{
        # do linear square loss
        w.gradient.vec <- -t(X.train) %*% 
          (X.train %*% w.vec + rep(1,n.trains) * intercept - y.vec)
        
        intercept.gradient <- -t(rep(1,n.trains)) %*% 
          (X.train %*% w.vec + rep(1,n.trains) * intercept - y.vec)
        
        intercept <- intercept + step.size * intercept.gradient
        
        u.vec <- w.vec + step.size*w.gradient.vec
        
        w.vec <- soft(u.vec, step.size * penalty)
      }
    }
    
    return(w.vec)
  }

