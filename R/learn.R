#' Convert SARS to Basis Representation
#'
#' @description
#' The function `SARS2Phis()` converts a SARS object to basis representation. The
#' motivation is to preprocess data and prepare for faster training.
#'
#' @param sars a SARS object (`class = "SARS"`).
#' @param action_space a list of feasible actions.
#' @param Feature feature function, taking states and actions as input, outputs basis.
#'
#' @return a list containing the following components:
#' \describe{
#'   \item{`phi`}{data matrix of \eqn{\phi(state, action)}}
#'   \item{`phi_next_list`}{a list of data matrices, each matrix has the same size
#'   as `phi` and represents the next time-step data matrix with each possible action}
#'   \item{`r`}{rewards}
#'   \item{`n`}{sample size}
#' }
#' @export
#'
#' @examples
#' states <- matrix(c(1, 2, 3, 4), 2, 2)
#' actions <- matrix(c(1, 0), 2, 1)
#' rewards <- matrix(c(1, 2), 2, 1)
#' states_next <- matrix(c(2, 3, 4, 5), 2, 2)
#' sars <- SARS(states, actions, rewards, states_next)
#' Feature <- function(states, actions) {
#'   phi <- RowWiseKronecker(Poly(states, 2), Poly(actions, 1))
#'   return(phi)
#' }
#' SARS2Phis(sars, list(0, 1), Feature)
SARS2Phis <- function(sars, action_space, Feature){
  if (!inherits(sars, "SARS")) {
    stop("`sars` should be a `SARS` object")
  }

  n <- nrow(sars$states)
  phi <- Feature(sars$states, sars$actions)
  phi_next_list <- vector("list", length(action_space))
  for (i_action in seq_along(action_space)){
    action_next <- action_space[[i_action]]
    actions_next <- matrix(rep(action_next, n), nrow = n, byrow = TRUE)
    phi_next_list[[i_action]] <- Feature(sars$states_next, actions_next)
  }
  r <- sars$rewards
  return(list(phi = phi, phi_next_list = phi_next_list, r = r, n = n))
}

#' Objective functions for Q-learning
#'
#' @describeIn MSPBE Mean Squared Projected Bellman Error
#'
#' @param theta a numeric vector as model parameter.
#' @param phis a list of processed outcome from `SARS2Phis()`.
#' @param discount a numeric number between 0 and 1.
#'
#' @return a (non-negative) number
#' @export
MSPBE <- function(theta, phis, discount){
  q <- phis$phi %*% theta
  qs_next <- lapply(phis$phi_next_list,
                    function(phi_next){phi_next %*% theta})
  q_next <- do.call(pmax, qs_next)
  delta <- phis$r + discount * q_next - q

  w <- MASS::ginv(phis$phi) %*% delta
  d <- t(phis$phi) %*% delta / phis$n
  return(as.numeric(t(d) %*% w))
}

#' @describeIn MSPBE Mean Squared Bellman Error
#' @export
MSBE <- function(theta, phis, discount){
  q <- phis$phi %*% theta
  qs_next <- lapply(phis$phi_next_list,
                    function(phi_next){phi_next %*% theta})
  q_next <- do.call(pmax, qs_next)
  delta <- phis$r + discount * q_next - q

  return(mean(delta^2))
}

#' Gradient functions for Q-learning
#'
#' @describeIn GradientFQI Fitted Q Iteration (objective function: MSPBE)
#'
#' @inheritParams MSPBE
#'
#' @return gradient
#' @export
GradientFQI <- function(theta, phis, discount){
  q <- phis$phi %*% theta
  qs_next <- lapply(phis$phi_next_list,
                    function(phi_next){phi_next %*% theta})
  q_next <- do.call(pmax, qs_next)

  g <- - t(phis$phi) %*% (phis$r + discount * q_next - phis$phi %*% theta) / phis$n
  return(g)
}

#' @describeIn GradientFQI Greedy Gradient-Q (objective function: MSPBE)
#' @export
GradientGGQ <- function(theta, phis, discount){
  q <- phis$phi %*% theta
  qs_next <- sapply(phis$phi_next_list,
                    function(phi_next){phi_next %*% theta})
  idx_next <- apply(qs_next, 1, which.max)
  q_next <- matrix(qs_next[cbind(seq_along(idx_next), idx_next)], ncol = 1)
  phi_next <- t(apply(cbind(seq_along(idx_next), idx_next), 1,
                      function(ii){return(phis$phi_next_list[[ii[2]]][ii[1], ])}))
  delta <- phis$r + discount * q_next - q

  w <- MASS::ginv(phis$phi) %*% delta
  g <- - (t(phis$phi) %*% delta - discount * (t(phi_next) %*% (phis$phi %*% w))) / phis$n
  return(g)
}

#' @describeIn GradientFQI Bellman Error Minimization (objective function: MSBE)
#' @export
GradientBEM <- function(theta, phis, discount){
  q <- phis$phi %*% theta
  qs_next <- sapply(phis$phi_next_list,
                    function(phi_next){phi_next %*% theta})
  idx_next <- apply(qs_next, 1, which.max)
  q_next <- matrix(qs_next[cbind(seq_along(idx_next), idx_next)], ncol = 1)
  phi_next <- t(apply(cbind(seq_along(idx_next), idx_next), 1,
                      function(ii){return(phis$phi_next_list[[ii[2]]][ii[1], ])}))
  delta <- phis$r + discount * q_next - q

  g <- t(discount * phi_next - phis$phi) %*% delta / phis$n
  return(g)
}
