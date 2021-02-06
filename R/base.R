#' SARS Object
#'
#' @description
#' The function `sars()` creates a SARS object for discrete-time Markov Decision
#' Process (MDP) data.

#'
#' @param states a matrix, the number of rows equals sample size
#' @param actions a matrix, the number of rows equals sample size
#' @param rewards a column vector, the number of rows equals sample size
#' @param states_next a matrix, the number of rows equals sample size
#'
#' @return a SARS object (`class = "sars"`)
#' @export
#'
#' @details
#' SARS stands for \eqn{S} (state), \eqn{A} (action), \eqn{A} (reward), and
#' \eqn{S'} (next state), a basic unit of MDP.
#'
#' SARS objects are designed to store more than one units. A typical use case is
#' MDP trajectories of the form
#' \deqn{S_1, A_1, R_1, S_2, A_2, R_2, \ldots, S_n, A_n, R_n, S_{n+1}}
#' which can be rearranged into units \eqn{(S_1, A_1, R_1, S'_1=S_2)}, \eqn{(S_2, A_2, R_2, S'_2=S_3)},
#' and so on. Elements across all units are then stacked together into matrices of
#' `states`, `actions`, `rewards`, and `states_next`. For example, if each \eqn{S}
#' is a \eqn{p}-vector, then `state` is a \eqn{n}-by-\eqn{p} matrix.
#'
#' This structure is not a compact representation for trajectory use-case, because
#' `states_next` would be a duplicate for 1 time step lagged `states`. However,
#' it has compatibility over more than one trajectories: simply stacking matrices
#' from different trajectories together. This single-matrix representation provides
#' some computational advantages.
#'
#' @note
#' For 1D arguments (e.g. reward as a real number), a column vector (\eqn{n}-by-\eqn{1} matrix)
#' is expected.
#'
#' @examples
sars <- function(states, actions, rewards, states_next) {
  states <- as.matrix(states)
  actions <- as.matrix(actions)
  rewards <- as.matrix(rewards)
  states_next <- as.matrix(states_next)

  # determine sample size from reward (always 1D)
  if (ncol(rewards) > 1) {
    if (nrow(rewards) > 1) stop("size of `rewards` is wrong, expecting column vector")
    warning("`rewards` is transposed to column vector")
    rewards <- t(rewards)
  }
  n <- nrow(rewards)

  # check dimensions of other inputs
  if ((nrow(states) != n) || (nrow(actions) != n) || (nrow(states_next) != n)) stop("inconsistent number of rows")
  if (ncol(states) != ncol(states_next)) stop("inconsistent number of columns")

  structure(list(
    states = states,
    actions = actions,
    rewards = rewards,
    states_next = states_next,
    n = n
  ),
  class = "sars"
  )
}
