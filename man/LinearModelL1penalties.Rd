% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LinearModelL1penalties.R
\name{LinearModelL1penalties}
\alias{LinearModelL1penalties}
\title{Linear Model with L1 regularization}
\usage{
LinearModelL1penalties(X.mat, y.vec, penalty.vec = seq(0.5, 0, length.out
  = 100), step.size = 0.01)
}
\arguments{
\item{X.mat}{unscaled training input matrix, with size n x p}

\item{y.vec}{correspond training input matrix, with size n x 1}

\item{penalty.vec}{a vector with descending numeric number, default as seq(0.5, 0, length.out = 100)}

\item{step.size}{a scalar with numeric value, default as 0.01}
}
\value{
W.mat matrix, with the size ((feaure.size + 1)  x  n.step.size.length)
}
\description{
This algorithm takes the penalty vector as input and iterate through each value in the vector using LinearModelL1
}
\examples{
library(LinearModelL1)
data(prostate, package = "ElemStatLearn")
prostate <- list(features = as.matrix(prostate[, 1:8]), labels = prostate$lpsa, is.01 = FALSE)
data.set <- prostate
X.mat <- data.set$features
y.vec <- data.set$labels
LinearModelL1penalties(X.mat, y.vec, seq(0.4, 0.01, -0.01), 0.01)
}
