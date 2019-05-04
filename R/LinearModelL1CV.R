#' Linear Model with L1 regularization using cross validation
#'
#' This algorithm splits the data into several folds and apply LinealModelL1penalites to each fold
#'
#' @param X.mat a numeric feature matrix of size n x p
#' @param y.vec a numeric labe vector of length nrow(X.mat)
#' @param fold.vec a numeric vector of lenght nrow(X.mat)
#' @param n.folds an integer greater than 1, which indicate number of folds, default is 5
#' @param penalty.vec a non-negative numeric decreasing penalty vector, default is 1 to 0.1 with 0.1 decreament
#' @param step.size a positive numeric value, default is 0.1
#'
#' @return
#' @export
#'
#' @examples
LinearModelL1CV <-
  function(X.mat,
           y.vec,
           fold.vec = sample(rep(1:n.folds, l = length(y.vec))),
           n.folds = 5L,
           penalty.vec = seq(0.3, 0, by = -0.03),
           step.size = 0.01) {
    # Check type and dimension
    if (!all(is.numeric(X.mat), is.matrix(X.mat))) {
      stop("X.mat must be a numeric matrix")
    }
    
    if (!all(is.numeric(y.vec),
             is.vector(y.vec),
             length(y.vec) == nrow(X.mat))) {
      stop("y.vec must be a numeric vector of length nrow(X.mat)")
    }
    
    if (!all(is.numeric(fold.vec),
             is.vector(fold.vec),
             length(fold.vec) == nrow(X.mat))) {
      stop("fold.vec must be a numeric vector of length nrow(X.mat)")
    }
    
    if(!all(is.numeric(n.folds),
            is.integer(n.folds),
            length(n.folds) == 1,
            n.folds > 1,
            n.folds == length(fold.vec))){
      stop("n.folds must be an interger greater than 1 and equal to the length of fold.vec")
    }
    
    if (!all(
      is.vector(penalty.vec),
      is.numeric(penalty.vec),
      penalty.vec >= 0,
      diff(penalty.vec) < 0
    )) {
      stop("penalty.vec must be a non-negative decreasing numeric vector")
    }
    
    if(!all(is.numeric(step.size),
            length(step.size) == 1,
            step.size > 0
    )){
      stop("step.size must be a positive number")
    }
    
    sigmoid <- function(x) {
      return(1 /(1 + exp(-x)))
    }
    
    # Initiallize
    is.binary <- ifelse((all(y.vec %in% c(0, 1))), TRUE, FALSE)
    
    n.features <- ncol(X.mat)
    train.loss.mat <-
      matrix(0, nrow = n.folds, ncol = length(penalty.vec))
    validation.loss.mat <-
      matrix(0, nrow = n.folds, ncol = length(penalty.vec))
    
    # Iterating folds
    for (i.fold in seq(n.folds)) {
      train.vec <- (fold.vec != i.fold)
   
      W.mat <-
        LinearModelL1penalties(X.mat[train.vec,], y.vec[train.vec], penalty.vec, step.size)
      
      set.list <- list(train = train.vec, validation = (!train.vec))
      for (set.name in names(set.list)) {
        index <- get(set.name, set.list)

        if (is.binary) {
          # Do 0-1 loss
          predict <- sigmoid(cbind(1, X.mat[index,]) %*% W.mat)
          
          predict <- ifelse(predict > 0.5, 1, 0)
          loss.vec <-
            colMeans((ifelse(predict == y.vec[index], 0, 1)))
        } else{
          # Do square loss
          predict <- cbind(1, X.mat[index,]) %*% W.mat
          loss.vec <-
            colMeans((predict - y.vec[index]) ^ 2)
        }
        
        if (set.name == "train") {
          train.loss.mat[i.fold, ] <- loss.vec
        } else{
          validation.loss.mat[i.fold, ] <- loss.vec
        }
      }
    }
    mean.train.loss.vec <- colMeans(train.loss.mat)
    mean.validation.loss.vec <- colMeans(validation.loss.mat)
    selected.penalty.index <- which.min(mean.validation.loss.vec)
    
    W.opt <- 
      LinearModelL1penalties(X.mat, y.vec, penalty.vec, step.size)
    
    weight.vec <- W.opt[, selected.penalty.index]
    
    predict <- function(testX.mat) {
      # Check type and dimension
      if (!all(is.numeric(testX.mat),
               is.matrix(testX.mat),
               ncol(testX.mat) == n.features)) {
        stop("testX.mat must be a numeric matrix with n.features columns")
      }
      
      # prediction.vec <- ifelse(cbind(1,testX.mat) %*% t(weight.vec) > 0.5, 1, -1)
      if(is.binary)
        prediction.vec <- sigmoid(cbind(1, testX.mat) %*% weight.vec)
      else
        prediction.vec <- cbind(1, testX.mat) %*% weight.vec

      return(prediction.vec)
    }
    
    result.list <- list(
      mean.validation.loss.vec = mean.validation.loss.vec,
      mean.train.loss.vec = mean.train.loss.vec,
      penalty.vec = penalty.vec,
      selected.penalty = penalty.vec[selected.penalty.index],
      weight.vec = weight.vec,
      predict = predict
    )
    
    return(result.list)
  }