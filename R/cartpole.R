#' CartPole Object
#'
#' @description
#' The function `CartPole()` creates a CartPole object. The `CartPole` class is inherited
#' from the `Env` class.
#'
#' @param internal_state vector of size 4, representing cart position, cart velocity,
#' pole angle, and pole angular velocity
#' @param setting a list of constants that controls the setting of the CartPole object,
#' see details.
#' @inheritParams Env
#'
#' @details
#' The internal state of a CartPole object includes four elements (following OpenAI
#' Gym convention):
#'
#' | Index | Observation |
#' | ----- | ----------- |
#' | 1     | Cart Position |
#' | 2     | Cart Velocity |
#' | 3     | Pole Angle    |
#' | 4     | Pole Angular Velocity |
#'
#' Note that the \eqn{\theta} (index 3 and 4) has the opposite sign to OpenAI Gym.
#' In our definition, anti-clockwise is positive direction.
#'
#' @return a CartPole object (`class = c("CartPole", "Env")`)
#' @export
#'
#' @examples
#' cp <- CartPole()
CartPole <- function(internal_state = vector("numeric", 4),
                     rng_state = .Random.seed,
                     setting = list()) {
  # setting
  cons <-
    list(
      gravity = 9.8,
      mass_cart = 1.0,
      mass_pole = 0.1,
      length_pole = 1.0,
      force_magnitude = 10.0,
      time_delta = 0.02
    )
  names_cons <- names(cons)
  names_setting <- names(setting)
  cons[names_setting] <- setting
  names_extra <- names_setting[!(names_setting %in% names_cons)]
  if (length(names_extra) > 0) {
    warning("unknown constants: ", paste(names_extra, collapse = ", "))
  }
  cons["mass_total"] <- cons$mass_cart + cons$mass_pole
  cons["masslength_pole"] <- cons$mass_pole * cons$length_pole
  setting <- cons

  structure(
    list(
      internal_state = internal_state,
      rng_state = rng_state,
      setting = setting
    ),
    class = c("CartPole", "Env")
  )
}

#' @inheritParams Step
#' @describeIn CartPole evolve given left (-1) or right (+1) action
#' @export
Step.CartPole <- function(x, action) {
  global_rng_state <- .Random.seed
  withr::defer(.Random.seed <<- global_rng_state)
  .Random.seed <<- x$rng_state

  pos <- x$internal_state[1]
  pos_dot <- x$internal_state[2]
  theta <- x$internal_state[3]
  theta_dot <- x$internal_state[4]

  force <- action * x$setting$force_magnitude
  tmp <- force - x$setting$masslength_pole * (theta_dot^2) * sin(theta) / 2
  theta_dotdot <-
    (tmp * cos(theta) + x$setting$mass_total * x$setting$gravity * sin(theta)) /
      ((x$setting$mass_total * 2 / 3 - x$setting$mass_pole * cos(theta)^2 / 2) * x$setting$length_pole)
  pos_dotdot <- (tmp + x$setting$masslength_pole * theta_dotdot * cos(theta) / 2) / x$setting$mass_total

  pos <- pos + pos_dot * x$setting$time_delta
  pos_dot <- pos_dot + pos_dotdot * x$setting$time_delta
  theta <- theta + theta_dot * x$setting$time_delta
  theta_dot <- theta_dot + theta_dotdot * x$setting$ time_delta

  x$internal_state <- c(pos, pos_dot, theta, theta_dot)

  x$rng_state <- .Random.seed
  return(x)
}

#' @inheritParams Reset
#' @describeIn CartPole reset by uniformly sample from U\[-0.05, +0.05\]
#' @export
Reset.CartPole <- function(x) {
  global_rng_state <- .Random.seed
  withr::defer(.Random.seed <<- global_rng_state)
  .Random.seed <<- x$rng_state

  x$internal_state <- stats::runif(4, -0.05, +0.05)

  x$rng_state <- .Random.seed
  return(x)
}
