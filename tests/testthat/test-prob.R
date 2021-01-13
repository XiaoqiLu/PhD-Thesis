test_that("RZeroInflatedPoisson() is consistent with MOM", {
  n <- 10000
  tol <- 10 / sqrt(n)
  p <- 0.4
  lambda <- 5
  x <- RZeroInflatedPoisson(n, p, lambda)
  m <- mean(x)
  m2 <- m ^ 2
  v <- var(x)
  lambda_mom <- (v + m2) / m - 1
  p_mom <- (v - m) / (v + m2 - m)
  expect_equal(lambda_mom, lambda, tolerance = tol)
  expect_equal(p_mom, p, tolerance = tol)
})
