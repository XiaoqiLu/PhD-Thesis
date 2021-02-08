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
epsilon_greedy <- function(values, epsilon = 0.1) {
  if (is.vector(values)) {
    return(ifelse(stats::runif(1) < epsilon, random(rep(1, length(values))), greedy(values)))
  }
  if (is.matrix(values)) {
    return(apply(values, 1, epsilon_greedy, epsilon = epsilon))
  }
  stop("values should be either a vector or a matrix")
}
