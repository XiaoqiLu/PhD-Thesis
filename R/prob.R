#' Generates a sample from zero-inflated Poisson
#'
#' @param n sample size
#' @param p probability of extra zeros
#' @param lambda rate of Poisson distribution
#'
#' @return a vector of length n
#' @export
#'
#' @examples
#' RZeroInflatedPoisson(5, 0.4, 5)
RZeroInflatedPoisson <- function(n, p, lambda){
  return((stats::runif(n) > p) * stats::rpois(n, lambda))
}
