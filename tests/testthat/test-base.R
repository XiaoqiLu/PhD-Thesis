test_that("sars object constructor works", {
  states <- matrix(c(1, 2, 3, 4), 2, 2)
  actions <- matrix(c(1, 0), 2, 1)
  rewards <- matrix(c(1, 2), 2, 1)
  states_next <- matrix(c(2, 3, 4, 5), 2, 2)
  expect_s3_class(sars(states, actions, rewards, states_next), "sars")
})

test_that("sars object constructor works if rewards is vector", {
  states <- matrix(c(1, 2, 3, 4), 2, 2)
  actions <- matrix(c(1, 0), 2, 1)
  rewards <- 1:2
  states_next <- matrix(c(2, 3, 4, 5), 2, 2)
  expect_warning(sars(states, actions, rewards, states_next), NA)
  expect_s3_class(sars(states, actions, rewards, states_next), "sars")
})

test_that("sars object constructor gives warning if rewards is row vector (1-by-n matrix)", {
  states <- matrix(c(1, 2, 3, 4), 2, 2)
  actions <- matrix(c(1, 0), 2, 1)
  rewards <- matrix(c(1, 2), 1, 2)
  states_next <- matrix(c(2, 3, 4, 5), 2, 2)
  expect_warning(sars(states, actions, rewards, states_next), "`rewards` is transposed to column vector")
})

test_that("sars object constructor gives error if rewards can not be converted to column vector", {
  states <- matrix(c(1, 2, 3, 4), 2, 2)
  actions <- matrix(c(1, 0), 2, 1)
  rewards <- matrix(c(1, 2, 1, 2), 2, 2)
  states_next <- matrix(c(2, 3, 4, 5), 2, 2)
  expect_error(sars(states, actions, rewards, states_next), "size of `rewards` is wrong, expecting column vector")
})

test_that("sars object constructor gives error if sample sizes are not consistent", {
  states <- matrix(c(1, 2, 3, 4), 2, 2)
  actions <- matrix(c(1, 0, 3), 3, 1)
  rewards <- matrix(c(1, 2), 2, 1)
  states_next <- matrix(c(2, 3, 4, 5), 2, 2)
  expect_error(sars(states, actions, rewards, states_next), "inconsistent number of rows")
})

test_that("sars object constructor gives error if dimensions of states and states_next are not consistent", {
  states <- matrix(c(1, 2, 3, 4), 2, 2)
  actions <- matrix(c(1, 0), 2, 1)
  rewards <- matrix(c(1, 2), 2, 1)
  states_next <- matrix(c(2, 3, 4, 5, 6, 7), 2, 3)
  expect_error(sars(states, actions, rewards, states_next), "inconsistent number of columns")
})

