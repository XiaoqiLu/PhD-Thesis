% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/learn.R
\name{BatchGradientQ}
\alias{BatchGradientQ}
\title{Batch Gradient Q-Learning}
\usage{
BatchGradientQ(
  phis,
  discount,
  method = "FQI",
  loss = NULL,
  lambda = 0,
  alpha = 1,
  theta = NULL,
  learning_rate = 1,
  max_iter = 1000,
  tol = 0.001,
  accelerate = TRUE
)
}
\arguments{
\item{phis}{a list of processed outcome from \code{SARS2Phis()}.}

\item{discount}{a numeric number between 0 and 1.}

\item{method}{Q-learning method, choice of "FQI", "GGQ", and "BEM"}

\item{loss}{loss function for evaluation, choice of "MSPBE" and "MSBE"}

\item{lambda}{regularization coefficient}

\item{alpha}{elastic net mixing parameter between 0 (ridge) and 1 (lasso)}

\item{theta}{a numeric vector as model parameter.}

\item{learning_rate}{learning rate for gradient descent}

\item{max_iter}{maximum number of iteration}

\item{tol}{tolerance level for convergence}

\item{accelerate}{if \code{TRUE}, use accelerated proximal gradient method; otherwise
use proximal gradient method.}
}
\value{
a list of model fitting results
}
\description{
Batch Gradient Q-Learning
}
\seealso{
\code{\link{SARS2Phis}}, \code{\link{MSPBE}}, \code{\link{GradientFQI}}
}
