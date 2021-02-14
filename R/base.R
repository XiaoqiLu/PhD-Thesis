#' SARS Object
#'
#' @description
#' The function `SARS()` creates a SARS object for discrete-time Markov Decision
#' Process (MDP) data.

#'
#' @param states a numeric matrix for states, each row for each time step.
#' @param actions a numeric matrix for actions.
#' @param rewards a numeric column vector for rewards.
#' @param states_next a numeric matrix for next states.
#'
#' @return a SARS object (`class = "SARS"`)
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
#' states <- matrix(c(1, 2, 3, 4), 2, 2)
#' actions <- matrix(c(1, 0), 2, 1)
#' rewards <- matrix(c(1, 2), 2, 1)
#' states_next <- matrix(c(2, 3, 4, 5), 2, 2)
#' ss <- SARS(states, actions, rewards, states_next)
#' ss
SARS <- function(states, actions, rewards, states_next) {
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

  # check sizes of other inputs
  if ((nrow(states) != n) || (nrow(actions) != n) || (nrow(states_next) != n)) stop("inconsistent number of rows")
  if (ncol(states) != ncol(states_next)) stop("inconsistent number of columns")

  structure(list(
    states = states,
    actions = actions,
    rewards = rewards,
    states_next = states_next,
    n = n
  ),
  class = "SARS"
  )
}

#' Trajectory Object
#'
#' @description
#' The function `Traj()` creates a trajectory object for discrete-time RL data.
#'
#' @param observations a list for observations, the number of elements should be
#' one more than that of `actions`.
#' @param actions a list for actions, each element for each time step.
#'
#' @details
#' The trajectory object is designed to represent typical RL trajectory data:
#' \deqn{O_1, A_1, O_2, A_2, \ldots, O_n, A_n, O_{n+1}}
#'
#' This representation is compatible with many RL data generator (for example the
#' data structure of OpenAI Gym library). With user-defined interpreter, observations
#' and actions (or their history) can be encoded/converted to states, rewards and
#' actions under Markov Decision Process (MDP) framework.
#'
#' @return a trajectory object (`class = "Traj"`)
#' @export
#'
#' @examples
#' observations <- list(0, -1, -1, 0, 1, 1)
#' actions <- list(-1, 1, 1, 0, -1)
#' tj <- Traj(observations, actions)
#' tj
Traj <- function(observations, actions) {
  if (!is.list(observations)) stop("`observations` should be a list")
  if (!is.list(actions)) stop("`actions` should be a list")

  # determine sample size from actions
  n <- length(actions)
  if (length(observations) != n + 1) stop("`observations` should have one more elements than `actions`")

  structure(list(
    observations = observations,
    actions = actions,
    n = n
  ), class = "Traj")
}

#' Environment Object
#'
#' @param internal_state the internal state ("everything is Markov")
#' @param rng_state the RNG state for non-deterministic evolution
#'
#' @return an environment object (`class = "env"`)
#' @export
#'
#' @note
#' This is an abstract class, and should not be used directly.
Env <- function(internal_state = NULL, rng_state = .Random.seed) {
  structure(list(internal_state = internal_state, rng_state = rng_state), class = "Env")
}

#' Observe
#'
#' @description
#' `Observe()` is a generic method that aims to extract an observation from an object.
#'
#' @param x environment-like object
#' @param ... extra arguments
#'
#' @return an observation
#' @export
#'
#' @note
#' `Observe()` is not meant to be used directly on an environment object. For developers,
#' it is best practice to define an inheritance of environment class, then define
#' corresponding methods.
#'
#'
#' @examples
Observe <- function(x) {
  UseMethod("Observe", x)
}

#' @describeIn Observe Default behavior (error)
#' @export
Observe.default <- function(x) {
  stop("default method not defined")
}

#' @describeIn Observe Internal state from an environment object
#' @export
Observe.Env <- function(x) {
  return(x$internal_state)
}

Seed <- function(x, seed = NULL) {
  UseMethod("Seed", x)
}

Seed.Env <- function(x, seed = NULL) {
  current_state <- .Random.seed
  withr::defer(.Random.seed <<- current_state)
  set.seed(seed)
  x$rng_state <- .Random.seed
  return(x)
}
