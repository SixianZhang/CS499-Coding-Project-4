#' Linear Model algorithm with L1 regularization
#'
#' This algorithm takes one penalty value
#'
#' @param X.scaled.mat the scaled trainning input matrix (n x p) 
#' @param y.vec the training training labels with size (n x 1)
#' @param penalty a non-neagtive numeric scalar, default value 0.5, usually does not work :) 
#' @param opt.thresh a numeric scalar, default value as 0.5
#' @param initial.weight.vec initial weight with size (p+1 x 1), default value as a normal distribution warm up.  
#' @param step.size a numeric scalar, default value as 0.01
#'
#' @return optimal weight vector (with p+1 elements, first element is the bias/intercept b)
#'  for the given penalty parameter.
#' @export
#'
#' @examples
#' library(L1LinearModel)
#' data(prostate, package = "ElemStatLearn")
#' prostate <- list(features = as.matrix(prostate[, 1:8]), labels = prostate$lpsa, is.01 = FALSE)
#' data.set <- prostate
#' X.mat <- data.set$features
#' y.vec <- data.set$labels
#' LinearModelL1(X.mat,y.vec,0.2,0.1,rnorm(ncol(X.mat)+1),0.1)
LinearModelL1 <-
  function(X.scaled.mat,
           y.vec,
           penalty=0.5,
           opt.thresh=0.5,
           initial.weight.vec=rnorm(ncol(X.scaled.mat)+1),
           step.size=0.01) {
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

    if (!all(length(step.size) == 1, is.numeric(step.size))){
      stop("step.size must be a numeric scalar.")
    }
    
    
    # inner methods
    sigmoid <- function(x) {
      return(1 /(1 + exp(-x)))
    }
    
    soft <- function(w, penalty) {
      l <- abs(w) - penalty
      return(sign(w) * pos.part(l))
    }
    
    pos.part <- function(l) {
      ifelse(l > 0, l, 0)
    }
    
    l1opt <- function(w.vec, d) {
      ifelse(w.vec == 0,
             pos.part(abs(d) - penalty),
             abs(d - sign(w.vec) * penalty))
    }
    
    
    cost.weight <- function(w.vec) {
      if (is.binary) {
        pred.vec <- X.train %*% w.vec
        loss.vec <- log(1 + exp(-pred.vec * y.vec))
        mean(loss.vec) + penalty * sum(abs(w.vec[-1]))
      }
      else{
        pred.vec <- X.train %*% w.vec
        loss.vec <- sum((pred.vec - y.vec) ^ 2)
        mean(loss.vec) + penalty * sum(abs(w.vec[-1]))
      }
    }
    
    cost.step <- function(step) {
      new.w <- w.step(step)
      cost.weight(new.w)
    }
    
    w.step <- function(step) {
      u.vec <- w.vec - step.size * w.gradient.vec
      c(u.vec[1], soft(u.vec[-1], step.size * penalty))
    }
    
    # Initializing
    is.binary <- all(ifelse(y.vec %in% c(0, 1), TRUE, FALSE))
    max.iteration <- 100L
    
    if (is.binary) {
      y.vec <- ifelse(y.vec == 0, -1, 1)
    }
    
    n.features <- ncol(X.scaled.mat)
    n.trains <- nrow(X.scaled.mat)
    
    X.train = cbind(1, X.scaled.mat)
    w.vec <- initial.weight.vec
    if (is.binary) {
      # do logistic
      w.gradient.vec <-
        -t(X.train) %*% (y.vec / (1 + exp(y.vec * (
          X.train %*% w.vec
        ))))
      
    } else{
      # do linear
      w.gradient.vec <- t(X.train) %*% (X.train %*% w.vec - y.vec)
    }
    
    w.gradient.vec <- w.gradient.vec / n.trains
    
    
    n.iteration <- 0
    
    
    # Iteration starts here
    while (norm(abs(w.gradient.vec)) > opt.thresh &&
           # <-- Change the criterion
           n.iteration <= max.iteration) {
      n.iteration = n.iteration + 1
      
      if (is.binary) {
        # do logistic
        w.gradient.vec <-
          -t(X.train) %*% (y.vec / (1 + exp(y.vec * (
            X.train %*% w.vec
          ))))
        w.gradient.vec <- w.gradient.vec / n.trains
      } else{
        # do linear
        w.gradient.vec <- t(X.train) %*% (X.train %*% w.vec - y.vec)
        w.gradient.vec <- w.gradient.vec / n.trains
      }
      
      crit.vec <-
        c(abs(w.gradient.vec[1]), l1opt(w.vec[-1], -w.gradient.vec[-1])) # This line why
      
      lmax <- max(abs(w.gradient.vec[-1])) # This line why
      
      while (cost.step(step.size / 2) < cost.step(step.size)) {
        step.size <- step.size / 2
      }
      
      while (cost.step(step.size * 2) < cost.step(step.size)) {
        step.size <- step.size * 2
      }
      
      w.vec <- w.step(step.size)
      
    }
    
    return(w.vec)
  }
