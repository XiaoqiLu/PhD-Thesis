test_that("LogSumExp() works", {
  x <- 1:3
  expect_equal(LogSumExp(x), log(sum(exp(x))))
})

test_that("Greedy() works for vector input", {
  expect_equal(Greedy(1:5), 5)
  expect_equal(Greedy(c(2, -1, 3, 7, 5)), 4)
})

test_that("Greedy() works for matrix input", {
  expect_equal(Greedy(matrix(c(1, 2, 2, 1), 2, 2, byrow = TRUE)), c(2, 1))
  expect_equal(Greedy(matrix(1:3)), rep(1, 3))
  expect_equal(Greedy(matrix(1:3, 1, 3)), 3)
})

test_that("Greedy() gives error for wrong input", {
  expect_error(Greedy(array(dim = c(2, 2, 2))), "values should be either a vector or a matrix")
})

test_that("Random() works for vector input", {
  significance_level <- 0.01
  n <- 10000
  p_raw <- 1:5
  p <- p_raw / sum(p_raw)
  test <-
    chisq.test(table(factor(replicate(n, Random(
      p_raw
    )), levels = seq_along(p_raw))), p = p)
  if (test$p.value < significance_level) {
    warning(paste("reject null at significance level =", significance_level))
  }
  expect_true(TRUE)
})

test_that("Random() works for matrix input", {
  significance_level <- 0.01
  n <- 10000
  p1 <- 1:5
  p2 <- 5:1
  p <- ((p1 / sum(p1)) + (p2 / sum(p2))) / 2
  test <-
    chisq.test(table(factor(as.vector(
      replicate(n, Random(matrix(
        c(p1, p2), 2, length(p1),
        byrow = TRUE
      )))
    ),
    levels = seq_along(p1)
    )),
    p = p, rescale.p = TRUE
    )
  if (test$p.value < significance_level) {
    warning(paste("reject null at significance level =", significance_level))
  }
  expect_true(TRUE)
})

test_that("EpsilonGreedy() works for vector input", {
  significance_level <- 0.01
  n <- 10000
  p_raw <- 1:5
  eps <- 0.1
  p <- rep(eps / length(p_raw), length(p_raw))
  p[which.max(p_raw)] <- p[which.max(p_raw)] + (1 - eps)
  expect_equal(p, c(0.02, 0.02, 0.02, 0.02, 0.92))
  test <-
    chisq.test(table(factor(
      replicate(n, EpsilonGreedy(p_raw, eps)),
      levels = seq_along(p_raw)
    )),
    p = p, rescale.p = TRUE
    )
  if (test$p.value < significance_level) {
    warning(paste("reject null at significance level =", significance_level))
  }
  expect_true(TRUE)
})

test_that("EpsilonGreedy() works for matrix input", {
  significance_level <- 0.01
  n <- 10000
  p1 <- 1:5
  p2 <- 5:1
  eps <- 0.1
  p <- rep(eps / length(p1), length(p1))
  p[which.max(p1)] <- p[which.max(p1)] + (1 - eps) / 2
  p[which.max(p2)] <- p[which.max(p2)] + (1 - eps) / 2
  expect_equal(p, c(0.47, 0.02, 0.02, 0.02, 0.47))
  test <-
    chisq.test(table(factor(as.vector(
      replicate(n, EpsilonGreedy(matrix(
        c(p1, p2), 2, length(p1),
        byrow = TRUE
      ), eps))
    ),
    levels = seq_along(p1)
    )),
    p = p, rescale.p = TRUE
    )
  if (test$p.value < significance_level) {
    warning(paste("reject null at significance level =", significance_level))
  }
  expect_true(TRUE)
})

test_that("Gibbs() works for vector input", {
  expect_equal(Gibbs(1:5, temperature = 1e-8), 5)
  significance_level <- 0.01
  n <- 10000
  test <-
    chisq.test(table(factor(replicate(
      n, Gibbs(1:5, temperature = 1e8)
    ),
    levels = 1:5
    )),
    p = rep(0.2, 5)
    )
  if (test$p.value < significance_level) {
    warning(paste("reject null at significance level =", significance_level))
  }
  expect_true(TRUE)
})

test_that("CumReward() works", {
  expect_equal(CumReward(c(1, 1, 0, 1), 0.9), 2.629)
})

test_that("Poly() works", {
  x <- cbind(c(0, 1, 0, 1), 1 : 4)
  expect_equal(Poly(x, degree = 1, interaction_only = FALSE, include_bias = FALSE), x)
  expect_equal(Poly(x, degree = 1, interaction_only = TRUE, include_bias = FALSE), x)
  expect_equal(Poly(x, degree = 1, include_bias = TRUE), cbind(1, x))
  expect_equal(Poly(x, degree = 2, interaction_only = TRUE), cbind(1, x, x[, 1] * x[, 2]))
  expect_equal(Poly(x, degree = 2, interaction_only = FALSE), cbind(1, x, x[, 1]^2, x[, 1] * x[, 2], x[, 2]^2))
})

test_that("RowWiseKronecker() works", {
  x <- cbind(rep(1, 3), 1 : 3)
  y <- matrix(c(1, 0,
                0, 1,
                1, 1), 3, 2, byrow = TRUE)
  expect_equal(RowWiseKronecker(x, y), matrix(c(1, 1, 0, 0,
                                                0, 0, 1, 2,
                                                1, 3, 1, 3), 3, 4, byrow = TRUE))
})

test_that("Radial Kernels work", {
  expect_equal(Gaussian(c(0, 2), scale = 2), c(1, exp(-1)))
  expect_equal(Multiquadric(c(0, 2), scale = 2), c(1, sqrt(2)))
  expect_equal(InverseQuadratic(c(0, 2), scale = 2), c(1, 1 / 2))
  expect_equal(InverseMultiquadric(c(0, 2), scale = 2), c(1, 1 / sqrt(2)))
  expect_equal(Bump(c(0, 1, 2, 4), scale = 2), c(exp(-1), exp(- 4 / 3), 0, 0))
})

test_that("RBF() works", {
  expect_equal(RBF(matrix(c(0, 1, 3, 4), 4, 1),
                   matrix(c(0.5, 3.5), 2, 1),
                   Kernel = Bump, scale = 2, include_bias = FALSE),
               matrix(c(exp(- 16 / 15), 0,
                        exp(- 16 / 15), 0,
                        0, exp(- 16 / 15),
                        0, exp(- 16 / 15)), 4, 2, byrow = TRUE))
})

test_that("Soft() works", {
  expect_equal(Soft(c(-1, 0, 2, 3), lambda = 1), c(0, 0, 1, 2))
})

test_that("ProximalElastic() works", {
  expect_equal(ProximalElastic(c(-1, 0, 2, 3), lambda = 1, alpha = 1), c(0, 0, 1, 2))
  expect_equal(ProximalElastic(c(-1, 0, 2, 3), lambda = 1, alpha = 0), c(-1, 0, 2, 3) / 2)
})
