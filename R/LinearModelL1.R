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
    
    
    # inner methods
    sigmoid <- function(x) {
      return(1 / 1 + exp(-x))
    }

    soft <- function(w, lambda) {
      l <- abs(w) - lambda
      return(sign(w) * pos.part(l))
    }
    
    pos.part <- function(l){
      ifelse(l > 0, l, 0)
    }
    
    l1opt <- function(w.vec, d){
      ifelse(
        w.vec == 0,
        pos.part(abs(d) - lambda),
        abs(d - sign(w.vec)*lambda)
      )
    }
    
    # Initializing
    is.binary <- all(ifelse(y.vec %in% c(0, 1), TRUE, FALSE))
    max.iteration <- 30L
    
    if (is.binary) {
      y.vec <- ifelse(y.vec == 0, -1, 1)
    }
    
    n.features <- ncol(X.scaled.mat)
    n.trains <- nrow(X.scaled.mat)

    X.train = cbind(1,X.scaled.mat)    
    w.vec <- rnorm(n.features + 1)
    if (is.binary) {
      # do logistic
      w.gradient.vec <-
        -t(X.train) %*% (y.vec / (1 + exp(y.vec * (
          X.train %*% w.vec
        ))))
    
    } else{
      # do linear
      w.gradient.vec <- t(X.train)%*%(X.train %*% w.vec - y.vec)
    }
    
    w.gradient.vec <- w.gradient.vec / n.trains
    
    
    n.iteration <- 0
    
    
    # Iteration starts here
    while (norm(abs(w.gradient.vec)) > opt.thresh &&  # <-- Change the criterion
           n.iteration <= max.iteration) {
      
      n.iteration = n.iteration + 1
      
      if (is.binary) {
        # do logistic
        w.gradient.vec <-
          -t(X.train) %*% (y.vec / (1 + exp(y.vec * (
            X.train %*% w.vec
          ))))
        w.gradient.vec <- w.gradient.vec / n.trains
        
        # u.vec <- w.vec - step.size * w.gradient.vec
        # 
        # w.vec <- c(u.vec[1], soft(u.vec[-1], step.size * penalty))
        
      } else{
        # do linear
        w.gradient.vec <- t(X.train)%*%(X.train %*% w.vec - y.vec)
        w.gradient.vec <- w.gradient.vec / n.trains
        
        # u.vec <- w.vec - step.size * w.gradient.vec
        # 
        # w.vec <- c(u.vec[1], soft(u.vec[-1], step.size * penalty))
      }
      
      
      
      crit.vec <- c(abs(grad.vec[1]), l1opt(w.vec[-1], -w.gradient.vec[-1])) # This line why
      lmax <- max(abs(grad.vec[-1])) # This line why
      
      while(cost.step(step.size/2) < cost.step(step.size)){
        step.size <- step.size/2
      }
      
      while(cost.step(step.size*2) < cost.step(step.size)){
        step.size <- step.size*2
      }
      
      cost.weight <- function(w.vec){
        pred.vec <- X.int %*% w.vec
        loss.vec <- log(1 + exp(- pred.vec * y.tilde))
        mean(loss.vec) + lambda*sum(abs(w.vec[-1]))
      }
      
      cost.step <- function(step){
        new.w <- w.step(step)
        cost.weight(new.w)
      }
      
      w.step <- function(step){
        u.vec <- w.vec - step.size * w.gradient.vec
        c(u.vec[1], soft(u.vec[-1], step.size * penalty))
      }
      
      w.vec <- w.step(step.size)
      
    }
    
    return(w.vec)
  }

