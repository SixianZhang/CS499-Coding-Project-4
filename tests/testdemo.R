library(L1LinearModel)

data(spam, package = "ElemStatLearn")
data(SAheart, package = "ElemStatLearn")
data(zip.train, package = "ElemStatLearn")
zip.train <- zip.train[zip.train[,1] %in% c(0,1),]
data(prostate, package = "ElemStatLearn")
data(ozone, package = "ElemStatLearn")

data.list <- list(
  spam = list(
    features = as.matrix(spam[, 1:57]),
    labels = ifelse(spam$spam == "spam", 1, 0),
    is.01 = TRUE
  ),

  SAheart = list(
    features = as.matrix(SAheart[, c(1:4,6:9)]),
    labels = SAheart$chd,
    is.01 = TRUE
  ),

  zip.train = list(
    features = as.matrix(zip.train[, -1]),
    labels = zip.train[, 1],
    is.01 = TRUE
  ),

  prostate = list(features = as.matrix(prostate[, 1:8]),
                  labels = prostate$lpsa,
                  is.01 = FALSE),

  ozone = list(features = as.matrix(ozone[,-1]),
               labels = ozone[, 1],
               is.01 = FALSE)
)

data.set <- data.list$prostate
X.mat <- data.set$features
y.vec <- data.set$labels
# W.mat <- LinearModelL1penalties(X.mat,y.vec,seq(1, 0.1, -0.1))

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

head(LinearModelL1(X.scaled.mat,y.vec,0.2,0.1,initial.weight.vec,0.1))

