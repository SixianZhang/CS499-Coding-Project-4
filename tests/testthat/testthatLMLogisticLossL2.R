library(testthat)
library(LinearModel)
data(spam, package = "ElemStatLearn")
X.mat <- data.matrix(spam[,-ncol(spam)])
y.vec <- as.vector(ifelse(spam$spam == 'spam',1,0))
penalty <- 0.8
opt.thresh <- 0.5
initial.weight.vec <- c(rep(0,dim(X.mat)[2] + 1))

test_that(
  "For valid inputs, your function returns an output of the expected type/dimension",
  {
    W.mat <-
      LinearModelL1(X.mat, y.vec, penalty, opt.thresh, initial.weight.vec)
    expect_true(is.numeric(W.mat))
    expect_true(is.matrix(W.mat))
    expect_equal(nrow(W.mat), ncol(cbind(1, X.mat)))
  }
)

test_that(
  "For an invalid input, your function stops with an informative error message.",
  {
    expect_error(
      opt.weight.vec <- 
        LinearModelL1(as.data.frame(X.mat), y.vec, penalty, opt.thresh, initial.weight.vec),
      "X.scaled.mat must be a numeric matrix",
      fixed = TRUE
    )
    expect_error(
      opt.weight.vec <-
        LinearModelL1(X.mat, y.vec[-1], penalty, opt.thresh, initial.weight.vec),
      "y.vec must be a numeric vector of lenght nrow(X.scaled.mat).",
      fixed = TRUE
    )
    expect_error(
      opt.weight.vec <-
        LinearModelL1(X.mat, y.vec, c(rep(penalty,2)), opt.thresh, initial.weight.vec),
      "penalty must be a non-negative numeric scalar",
      fixed = TRUE
    )    
    expect_error(
      opt.weight.vec <-
        LinearModelL1(X.mat, y.vec, penalty, c(rep(opt.thresh,2)), initial.weight.vec),
      "opt.thresh must be a positive numeric scalar",
      fixed = TRUE
    )    
    expect_error(
      opt.weight.vec <-
        LinearModelL1(X.mat, y.vec, penalty, opt.thresh, initial.weight.vec[-1]),
      "initial.weight.vec must be a numeric vector of length ncol(X.scaled.mat) + 1",
      fixed = TRUE
    )
  }
)

