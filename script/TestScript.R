library(ElemStatLearn)
library(NeuralNetwork)

data(spam, package = "ElemStatLearn")
data(SAheart, package = "ElemStatLearn")
data(zip.train, package = "ElemStatLearn")
zip.train <- zip.train[zip.train[, 1] %in% c(0, 1),]
data(prostate, package = "ElemStatLearn")
data(ozone, package = "ElemStatLearn")

data.list <- list(
  # spam = list(   
  #   features = as.matrix(spam[, 1:57]),
  #   labels = ifelse(spam$spam == "spam", 1, 0),
  #   is.01 = TRUE,
  #   step.size = 0.1,
  #   lmax = 0.3
  # ),
  # 
  SAheart = list(
    features = as.matrix(SAheart[, c(1:4, 6:9)]),
    labels = SAheart$chd,
    is.01 = TRUE,
    step.size = 0.1,
    lmax = 0.2
  # ),
  # 
  # zip.train = list(
  #   features = as.matrix(zip.train[, -1]),
  #   labels = zip.train[, 1],
  #   is.01 = TRUE,
  #   step.size = 0.1,
  #   lmax = 0.5
  # ),
  # 
  # 
  # prostate = list(
  #   features = as.matrix(prostate[, 1:8]),
  #   labels = prostate$lpsa,
  #   is.01 = FALSE,
  #   step.size = 0.1,
  #   lmax = 1
  # ),
  # 
  # ozone = list(
  #   features = as.matrix(ozone[,-1]),
  #   labels = ozone[, 1],
  #   is.01 = FALSE,
  #   step.size = 0.01,
  #   lmax = 21
  )
)

n.folds <- 5L
n.hidden.units <- 10L
max.iterations <- 500L

for (data.name in names(data.list)) {
  data.set <- data.list[[data.name]]
  
  test.loss.mat <- matrix(0, nrow = n.folds, ncol = 2)
  step.size <- data.set$step.size
  
  penalty.vec = seq(data.set$lmax, data.set$lmax / 100, length.out = 100)
  
  
  #Check data type here:
  
  set.seed(250)
  
  fold.vec <- sample(rep(1:n.folds, l = length(data.set$labels)))
  
  for (i.fold in (1:n.folds)) {
    train.index <- fold.vec != i.fold
    test.index <- fold.vec == i.fold
    
    X.mat <- data.set$features
    y.vec <- data.set$labels
    
    X.train <- data.set$features[train.index,]
    y.train <- data.set$labels[train.index]
    X.test <- data.set$features[test.index,]
    y.test <- data.set$labels[test.index]
    
    result.list <- LinearModelL1CV(
      X.mat = X.train,
      y.vec = y.train,
      # fold.vec = sample(rep(1:n.folds, l = length(y.vec))),
      n.folds = 5L,
      penalty.vec = penalty.vec,
      step.size = step.size
    )
    
    if (data.set$is.01) {
      # binary data
      L1.predict <-
        ifelse(result.list$predict(X.test) > 0.5, 1, 0)
      L1.loss <- mean(ifelse(L1.predict == y.test, 0, 1))
      
      baseline.predict <- ifelse(mean(y.test) > 0.5, 1, 0)
      baseline.loss <- mean(ifelse(baseline.predict == y.test, 0, 1))
      
    } else{
      # regression data
      L1.predict <- result.list$predict(X.test)
      L1.loss <- mean((L1.predict - y.test) ^ 2)
      
      baseline.predict <- mean(y.test)
      baseline.loss <- mean((baseline.predict - y.test) ^ 2)
    }
    
    test.loss.mat[i.fold,] = c(L1.loss, baseline.loss)
  }
  
  colnames(test.loss.mat) <- c("Linear Model L1", "Baseline")
  
  
  # plot result
  if (!data.set$is.01) {
    barplot(
      test.loss.mat,
      main = c("Square Regression: ", data.name),
      xlab = "mean loss value",
      legend = (rownames(test.loss.mat)),
      beside = TRUE
    )
  } else{
    barplot(
      test.loss.mat,
      main = c("Binary Classification: ", data.name),
      xlab = "mean loss value",
      legend = (rownames(test.loss.mat)),
      beside = TRUE
    )
  }
  
  model.list <- LinearModelL1CV(
    X.mat = X.mat,
    y.vec = y.vec,
    # fold.vec = sample(rep(1:n.folds, l = length(y.vec))),
    n.folds = 5L,
    penalty.vec = penalty.vec,
    step.size = step.size
  )
  
  
  
  matplot(
    x = penalty.vec,
    y = cbind(
      model.list$mean.validation.loss.vec,
      model.list$mean.train.loss.vec
    ),
    xlab = "penalties",
    ylab = "mean loss value",
    type = "l",
    lty = 1:2,
    pch = 15,
    col = c(17)
  )
  
  
  dot.x <- model.list$selected.penalty
  dot.y <- model.list$mean.validation.loss.vec[which(penalty.vec == dot.x)]
  
  matpoints(x = dot.x,
            y = dot.y,
            col = 2,
            pch = 19)
  legend(
    x = 0,
    y = max(
      cbind(
        model.list$mean.validation.loss.vec,
        model.list$mean.train.loss.vec
      )
    ),
    c("Validation loss", "Train loss"),
    lty = 1:2,
    xjust = 0,
    yjust = 1
  )
}




# X.mat <- data.set$features
# y.vec <- data.set$labels
#

# legend(
#   x = 0,
#   y = max(
#     cbind(
#       model.list$mean.validation.loss.vec,
#       model.list$mean.train.loss.vec
#     )
#   ),
#   c("Validation loss", "Train loss"),
#   lty = 1:2,
#   xjust = 0,
#   yjust = 1
# )
