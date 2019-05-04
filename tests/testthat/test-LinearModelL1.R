library(testthat)
library(LinearModel)
data(spam, package = "ElemStatLearn")
X.mat <- data.matrix(spam[,-ncol(spam)])
y.vec <- as.vector(ifelse(spam$spam == 'spam',1,0))
penalty <- 0.8
opt.thresh <- 0.5
initial.weight.vec <- rnorm(ncol(X.mat) + 1)

test_that(
  "For valid inputs, your function returns an output of the expected type/dimension",
  {
    opt.weight.vec <-
      LinearModelL1(X.mat,y.vec,penalty,opt.thresh,initial.weight.vec,step.size)
    expect_true(is.numeric(opt.weight.vec))
    expect_true(is.vector(opt.weight.vec))
    expect_equal(length(opt.weight.vec), ncol(cbind(1, X.mat)))
  }
)

test_that(
  "For an invalid input, your function stops with an informative error message.",
  {
    expect_error(
      opt.weight.vec <-
        LinearModelL1(as.data.frame(X.mat),y.vec,penalty,opt.thresh,initial.weight.vec,step.size),
      "X.scaled.mat must be a numeric matrix",
      fixed = TRUE
    )
    expect_error(
      opt.weight.vec <-
        LinearModelL1(X.mat,y.vec[-1],penalty,opt.thresh,initial.weight.vec,step.size),
      "y.vec must be a numeric vector of lenght nrow(X.scaled.mat).",
      fixed = TRUE
    )
    expect_error(
      opt.weight.vec <-
        LinearModelL1(X.mat,y.vec,penalty = -1,opt.thresh,initial.weight.vec,step.size),
      "penalty must be a non-negative numeric scalar",
      fixed = TRUE
    )    
    expect_error(
      opt.weight.vec <-
        LinearModelL1(X.mat,y.vec,penalty,opt.thresh = 0,initial.weight.vec,step.size),
      "opt.thresh must be a positive numeric scalar",
      fixed = TRUE
    )    
    expect_error(
      opt.weight.vec <-
        LinearModelL1(X.mat,y.vec,penalty,opt.thresh,initial.weight.vec[-1],step.size),
      "initial.weight.vec must be a numeric vector of length ncol(X.scaled.mat) + 1",
      fixed = TRUE
    )
    expect_error(
    opt.weight.vec <-
      LinearModelL1(X.mat,y.vec,penalty,opt.thresh,initial.weight.vec,step.size = 0),
    "step.size must be a numeric scalar",
    fixed = TRUE
)
  }
)

