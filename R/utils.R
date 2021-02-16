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
  nx <- ncol(x)
  ny <- ncol(y)
  return(x[, rep(seq(nx), times = ny), drop = FALSE] * y[, rep(seq(ny), each = nx), drop = FALSE])
}
