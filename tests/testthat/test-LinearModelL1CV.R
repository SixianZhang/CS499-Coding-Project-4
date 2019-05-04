library(testthat)
library(LinearModelL1)
data(spam, package = "ElemStatLearn")
X.mat <- data.matrix(spam[,-ncol(spam)])
y.vec <- as.vector(ifelse(spam$spam == 'spam', 1, 0))
fold.vec <- sample(rep(1:2, l = length(y.vec)))
penalty.vec <- seq(1, 0.1, by = -0.1)
n.fold = 2L

test_that("For valid inputs, your function returns an output of the expected type/dimension",
          {
            result.list <-
              LinearModelL1CV(X.mat,y.vec,fold.vec,n.folds,penalty.vec,step.size = 0.01)
            expect_true(is.list(result.list))
          })

test_that("For an invalid input, your function stops with an informative error message.",
          {
            expect_error(
              result.list <-
                LinearModelL1CV(as.data.frame(X.mat),y.vec,fold.vec,n.folds,penalty.vec,step.size = 0.01),
              "X.mat must be a numeric matrix",
              fixed = TRUE
            )
            expect_error(
              result.list <-
                LinearModelL1CV(X.mat,y.vec[-1],fold.vec,n.folds,penalty.vec,step.size = 0.01),
              "y.vec must be a numeric vector of length nrow(X.mat)",
              fixed = TRUE
            )
            expect_error(
              result.list <-
                LinearModelL1CV(X.mat,y.vec,fold.vec,n.folds = 0,penalty.vec,step.size = 0.01),
              "n.folds must be an interger greater than 1 and equal to the number of unique element of fold.vec",
              fixed = TRUE   
            )
            expect_error(
              result.list <-
                LinearModelL1CV(X.mat,y.vec,fold.vec[-1],n.folds,penalty.vec,step.size = 0.01),
              "fold.vec must be a numeric vector of length nrow(X.mat)",
              fixed = TRUE
            )
            expect_error(
              result.list <-
                LinearModelL1CV(X.mat,y.vec,fold.vec,n.folds,penalty.vec = seq(0.1, 1, by = 0.1),step.size = 0.01),
              "penalty.vec must be a non-negative decreasing numeric vector",
              fixed = TRUE
            )
            expect_error(
              result.list <-
                LinearModelL1CV(X.mat,y.vec,fold.vec,n.folds,penalty.vec,step.size = -1),
              "step.size must be a positive number",
              fixed = TRUE
            )
          })
