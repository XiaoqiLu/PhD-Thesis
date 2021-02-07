test_that("sars() works", {
  states <- matrix(c(1, 2, 3, 4), 2, 2)
  actions <- matrix(c(1, 0), 2, 1)
  rewards <- matrix(c(1, 2), 2, 1)
  states_next <- matrix(c(2, 3, 4, 5), 2, 2)
  expect_s3_class(sars(states, actions, rewards, states_next), "sars")
})

test_that("sars() works if rewards is vector", {
  states <- matrix(c(1, 2, 3, 4), 2, 2)
  actions <- matrix(c(1, 0), 2, 1)
  rewards <- 1:2
  states_next <- matrix(c(2, 3, 4, 5), 2, 2)
  expect_warning(sars(states, actions, rewards, states_next), NA)
  expect_s3_class(sars(states, actions, rewards, states_next), "sars")
})

test_that("sars() gives warning if rewards is row vector (1-by-n matrix)", {
  states <- matrix(c(1, 2, 3, 4), 2, 2)
  actions <- matrix(c(1, 0), 2, 1)
  rewards <- matrix(c(1, 2), 1, 2)
  states_next <- matrix(c(2, 3, 4, 5), 2, 2)
  expect_warning(sars(states, actions, rewards, states_next), "`rewards` is transposed to column vector")
})

test_that("sars() gives error if rewards can not be converted to column vector", {
  states <- matrix(c(1, 2, 3, 4), 2, 2)
  actions <- matrix(c(1, 0), 2, 1)
  rewards <- matrix(c(1, 2, 1, 2), 2, 2)
  states_next <- matrix(c(2, 3, 4, 5), 2, 2)
  expect_error(sars(states, actions, rewards, states_next), "size of `rewards` is wrong, expecting column vector")
})

test_that("sars() gives error if sample sizes are not consistent", {
  states <- matrix(c(1, 2, 3, 4), 2, 2)
  actions <- matrix(c(1, 0, 3), 3, 1)
  rewards <- matrix(c(1, 2), 2, 1)
  states_next <- matrix(c(2, 3, 4, 5), 2, 2)
  expect_error(sars(states, actions, rewards, states_next), "inconsistent number of rows")
})

test_that("sars() gives error if dimensions of states and states_next are not consistent", {
  states <- matrix(c(1, 2, 3, 4), 2, 2)
  actions <- matrix(c(1, 0), 2, 1)
  rewards <- matrix(c(1, 2), 2, 1)
  states_next <- matrix(c(2, 3, 4, 5, 6, 7), 2, 3)
  expect_error(sars(states, actions, rewards, states_next), "inconsistent number of columns")
})

test_that("trajectory() works", {
  observations <- list(0, -1, -1, 0, 1, 1)
  actions <- list(-1, 1, 1, 0, -1)
  expect_s3_class(trajectory(observations, actions), "trajectory")
})

test_that("trajectory() gives error if non-list objects are given", {
  observations <- list(0, -1, -1, 0, 1, 1)
  actions <- c(-1, 1, 1, 0, -1)
  expect_error(trajectory(observations, actions), "`actions` should be a list")
})

test_that("trajectory() gives error if length is not compatible", {
  observations <- list(0, -1, -1, 0, 1, 1, 2)
  actions <- list(-1, 1, 1, 0, -1)
  expect_error(trajectory(observations, actions), "`observations` should have one more elements than `actions`")
})
