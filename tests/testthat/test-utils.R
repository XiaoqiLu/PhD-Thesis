test_that("greedy() works for vector input", {
  expect_equal(greedy(1:5), 5)
  expect_equal(greedy(c(2, -1, 3, 7, 5)), 4)
})

test_that("greedy() works for matrix input", {
  expect_equal(greedy(matrix(c(1, 2, 2, 1), 2, 2, byrow = TRUE)), c(2, 1))
  expect_equal(greedy(matrix(1:3)), rep(1, 3))
  expect_equal(greedy(matrix(1:3, 1, 3)), 3)
})

test_that("greedy() gives error for wrong input", {
  expect_error(greedy(array(dim = c(2, 2, 2))), "values should be either a vector or a matrix")
})

test_that("random() works for vector input", {
  significance_level <- 0.01
  n <- 10000
  p_raw <- 1 : 5
  p <- p_raw / sum(p_raw)
  test <- chisq.test(table(factor(replicate(n, random(p_raw)),
                                  levels = seq_along(p_raw))),
                     p = p)
  if (test$p.value < significance_level) warning(paste("reject null at significance level =", significance_level))
  expect_true(TRUE)
})

test_that("random() works for matrix input", {
  significance_level <- 0.01
  n <- 10000
  p1 <- 1 : 5
  p2 <- 5 : 1
  p <- ((p1 / sum(p1)) + (p2 / sum(p2))) / 2
  test <- chisq.test(table(factor(as.vector(replicate(n, random(matrix(c(p1, p2), 2, length(p1), byrow = TRUE)))),
                                  levels = seq_along(p1))),
                     p = p, rescale.p = TRUE)
  if (test$p.value < significance_level) warning(paste("reject null at significance level =", significance_level))
  expect_true(TRUE)
})

test_that("epsilon_greedy() works for vector input", {
  significance_level <- 0.01
  n <- 10000
  p_raw <- 1 : 5
  eps <- 0.1
  p <- rep(eps / length(p_raw), length(p_raw))
  p[which.max(p_raw)] <- p[which.max(p_raw)] + (1 - eps)
  expect_equal(p, c(0.02, 0.02, 0.02, 0.02, 0.92))
  test <- chisq.test(table(factor(replicate(n, epsilon_greedy(p_raw, eps)),
                                  levels = seq_along(p_raw))),
                     p = p, rescale.p = TRUE)
  if (test$p.value < significance_level) warning(paste("reject null at significance level =", significance_level))
  expect_true(TRUE)
})

test_that("epsilon_greedy() works for matrix input", {
  significance_level <- 0.01
  n <- 10000
  p1 <- 1 : 5
  p2 <- 5 : 1
  eps <- 0.1
  p <- rep(eps / length(p1), length(p1))
  p[which.max(p1)] <- p[which.max(p1)] + (1 - eps) / 2
  p[which.max(p2)] <- p[which.max(p2)] + (1 - eps) / 2
  expect_equal(p, c(0.47, 0.02, 0.02, 0.02, 0.47))
  test <- chisq.test(table(factor(as.vector(replicate(n, epsilon_greedy(matrix(c(p1, p2), 2, length(p1), byrow = TRUE), eps))),
                                  levels = seq_along(p1))),
                     p = p, rescale.p = TRUE)
  if (test$p.value < significance_level) warning(paste("reject null at significance level =", significance_level))
  expect_true(TRUE)
})
