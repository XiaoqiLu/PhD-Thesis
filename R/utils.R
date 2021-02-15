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
