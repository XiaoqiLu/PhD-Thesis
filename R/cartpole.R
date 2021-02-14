cartpole <- function(constants = list()) {
  # environment setup
  cons <- list(gravity = 9.8, mass_cart = 1.0, mass_pole = 0.1, half_length_pole = 0.5, force = 10.0)
  names_cons <- names(cons)
  names_constants <- names(constants)
  cons[names_constants] <- constants
  names_constants_extra <- names_constants[!(names_constants %in% names_cons)]
  if (length(names_constants_extra) > 0) warning("unknown constants: ", paste(names_constants_extra, collapse = ", "))

  # seed setup
  rng_state <- .Random.seed
  withr::defer(.Random.seed <<- rng_state)
  set.seed(seed)

  # internal state of cart-pole
  state <- NULL

  structure(list(state = state, constants = constants, rng_state = rng_state), class = "cartpole")
}

