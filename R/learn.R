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
SARS2Phis <- function(sars, action_space, Feature) {
  if (!inherits(sars, "SARS")) {
    stop("`sars` should be a `SARS` object")
  }

  n <- nrow(sars$states)
  phi <- Feature(sars$states, sars$actions)
  phi_next_list <- vector("list", length(action_space))
  for (i_action in seq_along(action_space)) {
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
MSPBE <- function(theta, phis, discount) {
  q <- phis$phi %*% theta
  qs_next <- lapply(
    phis$phi_next_list,
    function(phi_next) {
      phi_next %*% theta
    }
  )
  q_next <- do.call(pmax, qs_next)
  delta <- phis$r + discount * q_next - q

  w <- MASS::ginv(phis$phi) %*% delta
  d <- t(phis$phi) %*% delta / phis$n
  return(as.numeric(t(d) %*% w))
}

#' @describeIn MSPBE Mean Squared Bellman Error
#' @export
MSBE <- function(theta, phis, discount) {
  q <- phis$phi %*% theta
  qs_next <- lapply(
    phis$phi_next_list,
    function(phi_next) {
      phi_next %*% theta
    }
  )
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
GradientFQI <- function(theta, phis, discount) {
  q <- phis$phi %*% theta
  qs_next <- lapply(
    phis$phi_next_list,
    function(phi_next) {
      phi_next %*% theta
    }
  )
  q_next <- do.call(pmax, qs_next)

  g <- -t(phis$phi) %*% (phis$r + discount * q_next - phis$phi %*% theta) / phis$n
  return(g)
}

#' @describeIn GradientFQI Greedy Gradient-Q (objective function: MSPBE)
#' @export
GradientGGQ <- function(theta, phis, discount) {
  q <- phis$phi %*% theta
  qs_next <- sapply(
    phis$phi_next_list,
    function(phi_next) {
      phi_next %*% theta
    }
  )
  idx_next <- apply(qs_next, 1, which.max)
  q_next <- matrix(qs_next[cbind(seq_along(idx_next), idx_next)], ncol = 1)
  phi_next <- t(apply(
    cbind(seq_along(idx_next), idx_next), 1,
    function(ii) {
      return(phis$phi_next_list[[ii[2]]][ii[1], ])
    }
  ))
  delta <- phis$r + discount * q_next - q

  w <- MASS::ginv(phis$phi) %*% delta
  g <- -(t(phis$phi) %*% delta - discount * (t(phi_next) %*% (phis$phi %*% w))) / phis$n
  return(g)
}

#' @describeIn GradientFQI Bellman Error Minimization (objective function: MSBE)
#' @export
GradientBEM <- function(theta, phis, discount) {
  q <- phis$phi %*% theta
  qs_next <- sapply(
    phis$phi_next_list,
    function(phi_next) {
      phi_next %*% theta
    }
  )
  idx_next <- apply(qs_next, 1, which.max)
  q_next <- matrix(qs_next[cbind(seq_along(idx_next), idx_next)], ncol = 1)
  phi_next <- t(apply(
    cbind(seq_along(idx_next), idx_next), 1,
    function(ii) {
      return(phis$phi_next_list[[ii[2]]][ii[1], ])
    }
  ))
  delta <- phis$r + discount * q_next - q

  g <- t(discount * phi_next - phis$phi) %*% delta / phis$n
  return(g)
}

#' Batch Gradient Q-Learning
#'
#' @param method Q-learning method, choice of "FQI", "GGQ", and "BEM"
#' @param loss loss function for evaluation, choice of "MSPBE" and "MSBE"
#' @param lambda regularization coefficient
#' @param alpha elastic net mixing parameter between 0 (ridge) and 1 (lasso)
#' @param learning_rate learning rate for gradient descent
#' @param max_iter maximum number of iteration
#' @param tol tolerance level for convergence
#' @param accelerate if `TRUE`, use accelerated proximal gradient method; otherwise
#' use proximal gradient method.
#' @inheritParams MSPBE
#'
#' @return a list of model fitting results
#' @export
#'
#' @seealso \code{\link{SARS2Phis}}, \code{\link{MSPBE}}, \code{\link{GradientFQI}}
BatchGradientQ <- function(phis, discount, method = "FQI", loss = NULL, lambda = 0, alpha = 1,
                           theta = NULL, learning_rate = 1.0, max_iter = 1000, tol = 1e-3,
                           accelerate = TRUE) {
  Gradient <- switch(method,
    "FQI" = function(theta) {
      return(GradientFQI(theta, phis, discount))
    },
    "GGQ" = function(theta) {
      return(GradientGGQ(theta, phis, discount))
    },
    "BEM" = function(theta) {
      return(GradientBEM(theta, phis, discount))
    }
  )
  if (is.null(loss)) {
    loss <- switch(method,
      "FQI" = "MSPBE",
      "GGQ" = "MSPBE",
      "BEM" = "MSBE",
    )
  }
  Loss <- switch(loss,
    MSPBE = function(theta) {
      return(MSPBE(theta, phis, discount))
    },
    MSBE = function(theta) {
      return(MSBE(theta, phis, discount))
    }
  )
  if (is.null(theta)) {
    theta <- matrix(0, ncol(phis$phi), 1)
  }

  n_iter <- 0
  value <- Inf
  step_size <- learning_rate
  momentum <- 0 * theta
  t_acc <- 0
  convergence <- 1 # maxit reached
  while (n_iter < max_iter) {
    n_iter <- n_iter + 1
    theta_prev <- theta
    value_prev <- value
    if (accelerate) {
      t_acc <- (1 + sqrt(1 + 4 * t_acc^2)) / 2
      weight_acc <- (t_acc - 1) / (t_acc + 1)
    } else {
      weight_acc <- 0
    }
    theta_acc <- theta + weight_acc * momentum
    g_acc <- Gradient(theta_acc)
    theta <- ProximalElastic(
      theta_acc - step_size * g_acc,
      step_size * lambda,
      alpha
    )
    momentum <- theta - theta_prev
    value <- Loss(theta)
    if (sqrt(sum(momentum^2)) > 1 / tol) {
      convergence <- 2 # diverged
      break
    } else if (abs(value - value_prev) < tol * (abs(value_prev) + tol)) {
      convergence <- 0 # converged
      break
    }
  }

  return(list(
    theta = theta, value = value, method = method, loss = loss,
    n_iter = n_iter, convergence = convergence
  ))
}

#' #' Batch Gradient Q-Learning with Cross-Validation
#' #'
#' #' @param phis
#' #' @param discount
#' #' @param method
#' #' @param loss
#' #' @param lambdas
#' #' @param alpha
#' #' @param n_fold
#' #' @param se
#' #' @param learning_rate
#' #' @param max_iter
#' #' @param tol
#' #' @param accelerate
#' #'
#' #' @return
#' #' @export
#' BatchGradientQCV <- function(phis, discount, method="FQI", loss=NULL,
#'                              lambdas=10^seq(0,-3,length.out=31), alpha=1, n_fold=5, se=0,
#'                              learning_rate=1.0, max_iter=1000, tol=1e-3, accelerate=TRUE){
#'   if (is.null(loss)){
#'     loss <- switch(method,
#'                    FQI = "MSPBE",
#'                    GGQ = "MSPBE",
#'                    BEM = "MSBE"
#'     )
#'   }
#'   LossVal <- switch(loss,
#'                     MSPBE = function(theta, phis){return(MSPBE(theta, phis, discount))},
#'                     MSBE = function(theta, phis){return(MSBE(theta, phis, discount))},
#'                     NEU = function(theta, phis){return(NEU(theta, phis, discount))}
#'   )
#'   lambdas <- sort(lambdas, decreasing = TRUE)
#'   losses_tr <- losses_val <- matrix(NA, length(lambdas), n_fold)
#'
#'   n_traj <- length(traj)
#'   idx_traj <- sample.int(n_traj)
#'   for (i_fold in seq_len(n_fold)){
#'     is_tr <- (idx_traj %% n_fold != (i_fold - 1))
#'     traj_tr <- traj[is_tr]
#'     traj_val <- traj[!is_tr]
#'     phis_val <- SARS2Phis(Traj2SARS(traj_val, burn_in), action_space, Feature)
#'
#'     theta <- matrix(0, ncol(phis_val$phi), 1)
#'     for (i_lambda in seq_along(lambdas)){
#'       lambda <- lambdas[i_lambda]
#'       fit <- BatchGradientQ(traj_tr, discount, action_space, Feature, burn_in,
#'                             method, loss, lambda, alpha, theta,
#'                             learning_rate, max_iter, tol, accelerate)
#'       theta <- fit$theta
#'       losses_tr[i_lambda, i_fold] <- fit$value
#'       losses_val[i_lambda, i_fold] <- LossVal(theta, phis_val)
#'     }
#'   }
#'
#'   mean_losses_val <- apply(losses_val, 1, mean)
#'   se_losses_val <- apply(losses_val, 1, sd) / sqrt(n_fold)
#'   i_best_lambda <- which.min(mean_losses_val)
#'   se_rule <- mean_losses_val[i_best_lambda] + se * se_losses_val[i_best_lambda]
#'   i_best_lambda <- which.max(mean_losses_val <= se_rule)
#'   best_lambda <- lambdas[i_best_lambda]
#'   best_fit <- BatchGradientQ(traj, discount, action_space, Feature, burn_in,
#'                              method, loss, best_lambda, alpha, NULL,
#'                              learning_rate, max_iter, tol, accelerate)
#'
#'   return(list(lambdas = lambdas, losses_tr = losses_tr, losses_val = losses_val, best_lambda = best_lambda, best_fit = best_fit))
#' }
