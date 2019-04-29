library(testthat)
library(LinearModel)
data(spam, package = "ElemStatLearn")
X.mat <- data.matrix(spam[,-ncol(spam)])
y.vec <- as.vector(ifelse(spam$spam == 'spam',1,0))
penalty.vec <- seq(5, 0.1, by = -0.1)
# X.mat, y.vec, penalty.vec

test_that(
  "For valid inputs, your function returns an output of the expected type/dimension",
  {
    W.mat <-
      LinearModelL1penalties(X.mat, y.vec, penalty.vec£¬0.01)
    expect_true(is.numeric(W.mat))
    expect_true(is.matrix(W.mat))
    expect_equal(nrow(W.mat), ncol(cbind(1, X.mat)))
  }
)

test_that(
  "For an invalid input, your function stops with an informative error message.",
  {
    expect_error(
      W.mat <- 
        LinearModelL1penalties(as.data.frame(X.mat), y.vec, penalty.vec, 0.01),
      "X.mat must be a numeric matrix.",
      fixed = TRUE
    )
    expect_error(
      W.mat <-
        LinearModelL1penalties(X.mat, y.vec[-1], penalty.vec, 0.01),
      "y.vec must be a numeric vector of the same number of rows as X.mat.",
      fixed = TRUE
    )
    expect_error(
      W.mat <-
        LinearModelL1penalties(X.mat, y.vec, seq(0.1, 5, by = 0.1), 0.01),
      "penalty.vec must be a non-negative decreasing numeric vector",
      fixed = TRUE
    )
  }
)

