#' Greedy Action
#'
#' @description
#' The function `greedy()` is a utility function that returns action indexes of maximal
#' values.
#'
#' @param values a numeric vector or matrix for values of actions. For numeric vector,
#' returns the index of the maximum; for numeric matrix, returns the indexes of the
#' maximums for each row.
#'
#' @return an integer vector
#' @export
#'
#' @examples
#' greedy(c(2, 1, 3, 7, 5))
#' greedy(matrix(c(1, 2, 2, 1), 2, 2, byrow = TRUE))
greedy <- function(values) {
  if (is.vector(values)) return(which.max(values))
  if (is.matrix(values)) return(apply(values, 1, which.max))
  stop("values should be either a vector or a matrix")
}
