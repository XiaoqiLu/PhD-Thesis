#' LogSumExp
#'
#' @description
#' `LogSumExp()` is a utility function to compute LogSumExp (aka SoftMax), using
#' the shifting trick to increase numerical stability.
#'
#' @param x a numeric object
#'
#' @return LogSumExp of x
#' @export
#'
#' @examples
#' LogSumExp(1:3)
LogSumExp <- function(x) {
  m <- max(x)
  m + log(sum(exp(x - m)))
}

#' Greedy Action
#'
#' @description
#' `Greedy()` is a utility function that returns action indexes of maximal values.
#'
#' @param values a numeric vector or matrix for values of actions. For numeric vector,
#' it is values of all actions; for numeric matrix, each row is a set of values.
#'
#' @return an integer vector for chosen action indexes
#' @export
#'
#' @family value-based action functions
#'
#' @examples
#' Greedy(c(2, 1, 3, 7, 5))
#' Greedy(matrix(c(1, 2, 2, 1), 2, 2, byrow = TRUE))
Greedy <- function(values) {
  if (is.vector(values)) {
    return(which.max(values))
  }
  if (is.matrix(values)) {
    return(apply(values, 1, Greedy))
  }
  stop("values should be either a vector or a matrix")
}

#' Random Action
#'
#' @description
#' `Random()` is a utility function that returns randomly picked action indexes, with
#' probabilities determined by input values.
#'
#' @inheritParams Greedy
#'
#' @return an integer vector for chosen action indexes
#' @export
#'
#' @family value-based action functions
#'
#' @examples
#' Random(c(2, 1, 3, 7, 5))
#' Random(matrix(c(1, 2, 2, 1), 2, 2, byrow = TRUE))
Random <- function(values) {
  if (is.vector(values)) {
    return(sample(seq_along(values), 1, prob = values))
  }
  if (is.matrix(values)) {
    return(apply(values, 1, Random))
  }
  stop("values should be either a vector or a matrix")
}

#' Epsilon-Greedy Action
#'
#' @description
#' `EpsilonGreedy()` is a utility function that performs "epsilon-greedy" actions.
#'
#' @param epsilon a numeric value between 0 and 1. It is the probability of choosing
#' uniformly random actions instead of greedy actions.
#' @inheritParams Greedy
#'
#' @return an integer vector for chosen action indexes
#' @export
#'
#' @family value-based action functions
#'
#' @examples
#' EpsilonGreedy(c(2, 1, 3, 7, 5))
#' EpsilonGreedy(matrix(c(1, 2, 2, 1), 2, 2, byrow = TRUE), epsilon = 0.2)
EpsilonGreedy <- function(values, epsilon = 0.1) {
  if (is.vector(values)) {
    return(ifelse(stats::runif(1) < epsilon, Random(rep(
      1, length(values)
    )), Greedy(values)))
  }
  if (is.matrix(values)) {
    return(apply(values, 1, EpsilonGreedy, epsilon = epsilon))
  }
  stop("values should be either a vector or a matrix")
}

#' Gibbs Action
#'
#' @description
#' `Gibbs()` is a utility function that samples actions from Gibbs distribution.
#'
#' @param temperature a numeric value as temperature.
#' @inheritParams Greedy
#'
#' @return an integer vector for chosen action indexes
#' @export
#'
#' @family value-based action functions
#'
#' @details
#' The Gibbs (or Boltzmann) distribution has p.d.f. of the following form
#' \deqn{p(x) \propto \exp(x / T),}{p(x) = C exp(x / T),}
#' where temperature \eqn{T} controls the trade-off between greedy (exploitation)
#' and uniformly random (exploration): a high temperature makes the distribution
#' more even, while a lower temperature makes the distribution more concentrated
#' (to favor higher values).
#'
#' @note
#' A negative \eqn{T} can be used to favor actions with lower values. In fact, the
#' standard formulation of Gibbs distribution has a negative sign before \eqn{x / T},
#' thus lower values have higher chances of being sampled.
#'
#' @examples
#' Gibbs(c(2, 1, 3, 7, 5))
#' Gibbs(matrix(c(1, 2, 2, 1), 2, 2, byrow = TRUE), temperature = 0.01)
Gibbs <- function(values, temperature = 1.0) {
  if (is.vector(values)) {
    scaled_values <- values / temperature
    return(Random(exp(
      scaled_values - LogSumExp(scaled_values)
    )))
  }
  if (is.matrix(values)) {
    return(apply(values, 1, Gibbs, temperature = temperature))
  }
  stop("values should be either a vector or a matrix")
}

#' Cumulative Reward
#'
#' @description
#' `CumReward()` is a utility function that computes cumulative reward for discounted
#' MDP.
#'
#' @param rewards a numeric vector of reward sequence
#' @param discount a numeric number between 0 and 1
#'
#' @return a numeric number for cumulative reward
#' @export
#'
#' @examples
#' CumReward(c(1, 1, 0, 1), 0.9)
CumReward <- function(rewards, discount) {
  sum(rewards * discount^(seq_along(rewards) - 1))
}

#' Polynomial Basis
#'
#' @description
#' `Poly()` is a utility function for polynomial transform.
#'
#' @param x a data matrix, where rows are observations and columns are dimensions.
#' @param degree degree of polynomial.
#' @param interaction_only if `TRUE`, only interactions (products of distinct features)
#' are produced.
#' @param include_bias if `TRUE`, include a column of ones
#'
#' @return transformed matrix
#' @export
#'
#' @examples
#' x <- cbind(c(0, 1, 0, 1), 1 : 4)
#' Poly(x, degree = 2)
Poly <- function(x, degree=1, interaction_only=TRUE, include_bias=TRUE){
  if (!is.matrix(x)) {
    stop("`x` should be a matrix")
  }
  n <- nrow(x)
  m <- ncol(x)
  if (include_bias){
    z <- matrix(1, n, 1)
  } else{
    z <- NULL
  }
  .Power <- function(idx){
    return(apply(x[, idx, drop = FALSE], 1, prod))
  }
  for (d in 1 : degree){
    combs <- arrangements::combinations(m, d, replace = !interaction_only)
    z <- cbind(z, matrix(apply(combs, 1, .Power), nrow = n))
  }
  return(z)
}

#' Row-Wise Kronecker
#'
#' @description
#' `RowWiseKronecker()` is a row-wise Kronecker utility function for feature engineering.
#'
#' @param x the first data matrix
#' @param y the second data matrix, should have the same number of rows as `x`
#'
#' @return transformed matrix
#' @export
#'
#' @examples
#' x <- cbind(rep(1, 3), 1 : 3)
#' y <- matrix(c(1, 0,
#'               0, 1,
#'               1, 1), 3, 2, byrow = TRUE)
#'  RowWiseKronecker(x, y)

RowWiseKronecker <- function(x, y){
  if (!is.matrix(x)) {
    stop("`x` should be a matrix")
  }
  if (!is.matrix(y)) {
    stop("`y` should be a matrix")
  }
  if (nrow(x) != nrow(y)) {
    stop("`x` and `y` should have the same number of rows")
  }
  nx <- ncol(x)
  ny <- ncol(y)
  return(x[, rep(seq(nx), times = ny), drop = FALSE] * y[, rep(seq(ny), each = nx), drop = FALSE])
}

#' Radial Kernels
#'
#' @description
#' Utility functions for radial kernels.
#'
#' @describeIn Gaussian \eqn{\exp(- r^2)}
#'
#' @param r a positive numeric object
#' @param scale a positive number for scaling
#'
#' @return an object of the same size of `r`
#' @export
#'
#' @examples
#' Gaussian(0 : 6, scale = 2)
Gaussian <- function(r, scale = 1) {
  z <- r / scale
  exp(- z^2)
}

#' @describeIn Gaussian \eqn{\sqrt{1 + r^2}}
#' @export
Multiquadric <- function(r, scale = 1) {
  z <- r / scale
  sqrt(1 + z^2)
}

#' @describeIn Gaussian \eqn{1 / (1 + r^2)}
#' @export
InverseQuadratic <- function(r, scale = 1) {
  z <- r / scale
  1 / (1 + z^2)
}

#' @describeIn Gaussian \eqn{1 / \sqrt{1 + r^2}}
#' @export
InverseMultiquadric <- function(r, scale = 1) {
  z <- r / scale
  1 / sqrt(1 + z^2)
}

#' @describeIn Gaussian \eqn{\exp(- 1 / (1 - r^2)))} when \eqn{r < 1}, and \eqn{0} otherwise.
#' Note that it is not scaled (`Bump(0)` < 1) and it is compactly supported.
#' @export
Bump <- function(r, scale = 1) {
  z <- r / scale
  ifelse(z < 1, exp(- 1 / (1 - z^2)), 0)
}

#' Radial Basis Function
#'
#' @description
#' `RBF()` is a utility function for RBF transform.
#'
#' @param x a data matrix, where rows are observations and columns are dimensions.
#' @param centers a matrix for cluster centers
#' @param Kernel radial functions
#' @param scale scale parameter, or bandwidth
#' @param include_bias if `TRUE`, include a column of ones
#'
#' @return transformed matrix
#' @export
#'
#' @seealso \code{\link{Gaussian}}
#'
#' @examples
#' x <- matrix(rnorm(20), 10, 2)
#' centers <- kmeans(x, 2)$centers
#' RBF(x, centers)
RBF <- function(x, centers, Kernel = Gaussian, scale = 1, include_bias=TRUE) {
  if (!is.matrix(x)) {
    stop("`x` should be a matrix")
  }
  if (!is.matrix(centers)) {
    stop("`centers` should be a matrix")
  }
  if (ncol(x) != ncol(centers)) {
    stop("`x` and `centers` should have the same number of columns")
  }
  n <- nrow(x)
  m <- ncol(x)
  k <- nrow(centers)

  phi <- matrix(NA, n, k)
  for (j in 1 : k) {
    phi[, j] <- apply(x - matrix(centers[j, ], n, m, byrow = TRUE), 1,
                      function(z){Kernel(sqrt(sum(z^2)), scale = scale)})
  }

  if (include_bias) {
    phi <- cbind(1, phi)
  }

  return(phi)
}

#' Soft Thresholding
#'
#' @param z input numeric object
#' @param lambda threshold
#'
#' @return output numeric object
#' @export
#'
#' @examples
#' Soft(c(-1, 0, 2, 3), 1)
Soft <- function(z, lambda=0){
  return(sign(z) * pmax(abs(z) - lambda, 0))
}

#' Proximal Mapping for Elastic Net
#'
#' @param z input numeric object
#' @param lambda regularization coefficient
#' @param alpha elastic net mixing parameter between 0 (ridge) and 1 (lasso)
#'
#' @return values after proximal mapping
#' @export
#'
#' @examples
#' ProximalElastic(c(-1, 0, 2, 3), 1)
ProximalElastic <- function(z, lambda=0, alpha=1){
  return(Soft(z, lambda * alpha) / (1 + lambda * (1 - alpha)))
}
