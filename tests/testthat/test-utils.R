test_that("greedy() works for vector input", {
  expect_equal(greedy(1:5), 5)
  expect_equal(greedy(c(2, 1, 3, 7, 5)), 4)
})

test_that("greedy() works for matrix input", {
  expect_equal(greedy(matrix(c(1, 2, 2, 1), 2, 2, byrow = TRUE)), c(2, 1))
  expect_equal(greedy(matrix(1:3)), rep(1, 3))
  expect_equal(greedy(matrix(1:3, 1, 3)), 3)
})

test_that("greedy() gives error for wrong input", {
  expect_error(greedy(array(dim = c(2, 2, 2))), "values should be either a vector or a matrix")
})
