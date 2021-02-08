#' LogSumExp
#'
#' @description
#' `.logsumexp()` is an internal function to compute LogSumExp (aka SoftMax), using
#' the shifting trick to increase numerical stability.
#'
#' @param x a numeric object
#'
#' @return LogSumExp of x
#' @export
#'
#' @keywords internal
#'
#' @examples
#' .logsumexp(1:3)
.logsumexp <- function(x) {
  m <- max(x)
  m + log(sum(exp(x - m)))
}

#' Greedy Action
#'
#' @description
#' `greedy()` is a utility function that returns action indexes of maximal values.
#'
#' @param values a numeric vector or matrix for values of actions. For numeric vector,
#' it is values of all actions; for numeric matrix, each row is a set of values.
#'
#' @return an integer vector for chosen action indexes
#' @export
#'
#' @examples
#' greedy(c(2, 1, 3, 7, 5))
#' greedy(matrix(c(1, 2, 2, 1), 2, 2, byrow = TRUE))
greedy <- function(values) {
  if (is.vector(values)) {
    return(which.max(values))
  }
  if (is.matrix(values)) {
    return(apply(values, 1, greedy))
  }
  stop("values should be either a vector or a matrix")
}

#' Random Action
#'
#' @description
#' `random()` is a utility function that returns randomly picked action indexes, with
#' probabilities determined by input values.
#'
#' @inheritParams greedy
#'
#' @return an integer vector for chosen action indexes
#' @export
#'
#' @examples
#' random(c(2, 1, 3, 7, 5))
#' random(matrix(c(1, 2, 2, 1), 2, 2, byrow = TRUE))
random <- function(values) {
  if (is.vector(values)) {
    return(sample(seq_along(values), 1, prob = values))
  }
  if (is.matrix(values)) {
    return(apply(values, 1, random))
  }
  stop("values should be either a vector or a matrix")
}

#' Epsilon-Greedy Action
#'
#' @description
#' `epsilon_greedy()` is a utility function that performs "epsilon-greedy" actions.
#'
#' @param epsilon a numeric value between 0 and 1. It is the probability of choosing
#' uniformly random actions instead of greedy actions.
#' @inheritParams greedy
#'
#' @return an integer vector for chosen action indexes
#' @export
#'
#' @examples
#' epsilon_greedy(c(2, 1, 3, 7, 5))
#' epsilon_greedy(matrix(c(1, 2, 2, 1), 2, 2, byrow = TRUE), epsilon = 0.2)
epsilon_greedy <- function(values, epsilon = 0.1) {
  if (is.vector(values)) {
    return(ifelse(stats::runif(1) < epsilon, random(rep(1, length(values))), greedy(values)))
  }
  if (is.matrix(values)) {
    return(apply(values, 1, epsilon_greedy, epsilon = epsilon))
  }
  stop("values should be either a vector or a matrix")
}

#' Gibbs Action
#'
#' @description
#' `gibbs()` is a utility function that samples actions from Gibbs distribution.
#'
#' @param temperature a numeric value as temperature.
#' @inheritParams greedy
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
#' gibbs(c(2, 1, 3, 7, 5))
#' gibbs(matrix(c(1, 2, 2, 1), 2, 2, byrow = TRUE), temperature = 0.01)
gibbs <- function(values, temperature = 1.0) {
  if (is.vector(values)) {
    return(random(exp((values - max(values)) / temperature)))
  }
  if (is.matrix(values)) {
    return(apply(values, 1, gibbs, temperature = temperature))
  }
  stop("values should be either a vector or a matrix")
}
