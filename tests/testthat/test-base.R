test_that("SARS() works", {
  states <- matrix(c(1, 2, 3, 4), 2, 2)
  actions <- matrix(c(1, 0), 2, 1)
  rewards <- matrix(c(1, 2), 2, 1)
  states_next <- matrix(c(2, 3, 4, 5), 2, 2)
  expect_s3_class(SARS(states, actions, rewards, states_next), "SARS")
})

test_that("SARS() works if rewards is vector", {
  states <- matrix(c(1, 2, 3, 4), 2, 2)
  actions <- matrix(c(1, 0), 2, 1)
  rewards <- 1:2
  states_next <- matrix(c(2, 3, 4, 5), 2, 2)
  expect_warning(SARS(states, actions, rewards, states_next), NA)
  expect_s3_class(SARS(states, actions, rewards, states_next), "SARS")
})

test_that("SARS() gives warning if rewards is row vector (1-by-n matrix)", {
  states <- matrix(c(1, 2, 3, 4), 2, 2)
  actions <- matrix(c(1, 0), 2, 1)
  rewards <- matrix(c(1, 2), 1, 2)
  states_next <- matrix(c(2, 3, 4, 5), 2, 2)
  expect_warning(
    SARS(states, actions, rewards, states_next),
    "`rewards` is transposed to column vector"
  )
})

test_that("SARS() gives error if rewards can not be converted to column vector", {
  states <- matrix(c(1, 2, 3, 4), 2, 2)
  actions <- matrix(c(1, 0), 2, 1)
  rewards <- matrix(c(1, 2, 1, 2), 2, 2)
  states_next <- matrix(c(2, 3, 4, 5), 2, 2)
  expect_error(
    SARS(states, actions, rewards, states_next),
    "`rewards` should be a column vector"
  )
})

test_that("SARS() gives error if sample sizes are not consistent", {
  states <- matrix(c(1, 2, 3, 4), 2, 2)
  actions <- matrix(c(1, 0, 3), 3, 1)
  rewards <- matrix(c(1, 2), 2, 1)
  states_next <- matrix(c(2, 3, 4, 5), 2, 2)
  expect_error(
    SARS(states, actions, rewards, states_next),
    "inconsistent number of rows"
  )
})

test_that("SARS() gives error if dimensions of states and states_next are not consistent", {
  states <- matrix(c(1, 2, 3, 4), 2, 2)
  actions <- matrix(c(1, 0), 2, 1)
  rewards <- matrix(c(1, 2), 2, 1)
  states_next <- matrix(c(2, 3, 4, 5, 6, 7), 2, 3)
  expect_error(
    SARS(states, actions, rewards, states_next),
    "inconsistent number of columns"
  )
})

test_that("Traj() works", {
  observations <- list(0, -1, -1, 0, 1, 1)
  actions <- list(-1, 1, 1, 0, -1)
  expect_s3_class(Traj(observations, actions), "Traj")
})

test_that("Traj() gives error if non-list objects are given", {
  observations <- list(0, -1, -1, 0, 1, 1)
  actions <- c(-1, 1, 1, 0, -1)
  expect_error(Traj(observations, actions), "`actions` should be a list")
})

test_that("Traj() gives error if length is not compatible", {
  observations <- list(0, -1, -1, 0, 1, 1, 2)
  actions <- list(-1, 1, 1, 0, -1)
  expect_error(
    Traj(observations, actions),
    "`observations` should have one more elements than `actions`"
  )
})

test_that("Traj2SARS() works", {
  observations <- list(0, -1, -1, 0, 1, 1)
  actions <- list(-1, 1, 1, 0, -1)
  traj <- Traj(observations, actions)
  Interpreter <- function(actions, observations) {
    n <- length(actions)
    if (n > 0) {
      state <- c(observations[[n]], observations[[n + 1]])
      reward <- 1 - state[2]^2
    } else {
      state <- reward <- NULL
    }
    return(list(state = state, reward = reward))
  }
  sars <- Traj2SARS(traj, Interpreter, skip = 1)
  expect_s3_class(sars, "SARS")
  expect_equal(sars$states[2, ], c(-1, -1))
  expect_equal(sars$actions[2, ], 1)
  expect_equal(as.vector(sars$rewards), c(0, 1, 0, 0))
})

test_that("Env() works", {
  global_rng_state <- .Random.seed
  expect_s3_class(Env(internal_state = 1), "Env")
  expect_equal(Env(internal_state = 1)$internal_state, 1)
  expect_equal(Env(internal_state = 1)$rng_state, global_rng_state)
})

test_that("Observe.Env() works", {
  expect_equal(Observe(Env(internal_state = 1)), 1)
})

test_that("Seed.Env() works", {
  set.seed(1)
  global_rng_state <- .Random.seed
  ev <- Seed(Env(internal_state = 1), 1)
  expect_equal(ev$rng_state, global_rng_state)
  expect_equal(.Random.seed, global_rng_state)
})

test_that("Step.Env() works", {
  set.seed(1)
  global_rng_state <- .Random.seed
  ev <- Env(internal_state = 0)
  ev2 <- Step(ev, 0)
  x <- stats::rnorm(1)
  expect_equal(ev$rng_state, global_rng_state)
  expect_equal(ev2$internal_state, x)
  expect_equal(ev2$rng_state, .Random.seed)
})

test_that("Reset.Env() works", {
  ev <- Env(internal_state = 0)
  ev <- Step(ev, 2e5)
  ev <- Reset(ev)
  expect_true(ev$internal_state < 1e5)
})
