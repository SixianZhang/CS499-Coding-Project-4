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
      return(1 / (1 + exp(-x)))
    }
    
    positive <- function(x){
      return(ifelse(x > 0, x, 0))
    }
    
    soft <- function(w, lambda) {
      l <- abs(w) - lambda
      return(sign(w) * ifelse(l > 0, l, 0))
    }
    
    # Initializing
    is.binary <- all(ifelse(y.vec %in% c(0, 1), TRUE, FALSE))
    step.factor <- 2L
    
    if (is.binary) {
      y.vec <- ifelse(y.vec == 0, -1, 1)
    }
    
    n.features <- ncol(X.scaled.mat)   # p 
    n.trains <- nrow(X.scaled.mat)  # n 
    max.iter <- 50L
    X.train <- cbind(1,X.scaled.mat) # n x (p+1)    
    # w.vec <- initial.weight.vec[-1] # p x 1
    # intercept <- initial.weight.vec[1]
    
    loss <- function(lst){
      if (is.binary)
        mean(log(1+exp(-y.vec * (X.train %*% lst$W.vec))))
      else
        mean((X.train %*% lst$W.vec - y.vec)^2)
    }

    iter.learn <- function(initial.weight.vec, step.size){
      if (is.binary){ 
        # do logistic
        w.gradient.vec <-
          t(X.train) %*% (y.vec / (1 + exp(-y.vec * (
            X.train %*% initial.weight.vec))))
      }else{
        # do linear square loss
        w.gradient.vec <- -t(X.train) %*% 
          (X.train %*% initial.weight.vec - y.vec)
      }
      
      u.vec <- initial.weight.vec + step.size * w.gradient.vec / n.trains
      initial.weight.vec.new <- c(u.vec[1], soft(u.vec[-1], step.size * penalty))

      ret <- list(
        W.vec = initial.weight.vec.new,
        gradient.vec = w.gradient.vec)
      return(ret)
    }

    norm.gradient <- function(gradient, w){
        ifelse(w==0, positive(abs(gradient) - penalty),
               abs(gradient - sign(w) * penalty))
    }
    
    iter <-  0
    while (1) {
      while (loss(iter.learn(initial.weight.vec, step.size/2))
             < loss(iter.learn(initial.weight.vec, step.size))){
        step.size <- step.size / step.factor
      }
      
      while(loss(iter.learn(initial.weight.vec, step.size*2))
            < loss(iter.learn(initial.weight.vec, step.size))){
        step.size <- step.size * step.factor
      }
      
      lst.n <- iter.learn(initial.weight.vec, step.size)
      initial.weight.vec <- lst.n$W.vec
      loss(lst.n)
      criterion <- c(lst.n$gradient.vec[1],
                     norm.gradient(lst.n$gradient.vec[-1], initial.weight.vec[-1]))
      iter <- iter + 1
      if ((norm(as.matrix(abs(criterion)),'2') < opt.thresh) || (iter >= max.iter))
        break;
    }
    return(initial.weight.vec)
  }
