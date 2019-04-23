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
    
    positive.part <- function() {
      
    }
    
    soft <- function() {
      
    }
    
    # Initializing
    is.binary <- ifelse(y.vec %in% c(0, 1), TRUE, FALSE)
    max.iteration <- 10000L
    
    if (is.binary) {
      y.vec <- ifelse(y.vec == 0, -1, 1)
    }
    
    n.features <- ncol(X.scaled.mat)
    n.trains <- nrow(X.scaled.mat)

    X.train = cbind(1,X.scaled.mat)    
    w.vec <- rnorm(n.features + 1)

    n.iteration <- 0
    
    while (norm(abs(W.gradient.vec)) > opt.thresh &&
           n.iteration <= max.iteration) {
      
      n.iteration = n.iteration + 1
      
      if (is.binary) {
        # do logistic
        W.gradient.vec <-
          -t(X.train) %*% (y.vec / (1 + exp(y.vec * (
            X.train %*% w.vec
          ))))
        
        u.vec <- w.vec - step.size * W.gradient.vec
        
        w.vec <- c(u.vec[1], soft(u.vec[-1], step.size * penalty))
        
      } else{
        # do linear
        
      }
    }
    
    
    return(w.vec)
  }