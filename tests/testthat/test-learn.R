test_that("SARS2Phis() works", {
  states <- matrix(c(1, 2, 3, 4), 2, 2)
  actions <- matrix(c(1, 0), 2, 1)
  rewards <- matrix(c(1, 2), 2, 1)
  states_next <- matrix(c(2, 3, 4, 5), 2, 2)
  sars <- SARS(states, actions, rewards, states_next)
  Feature <- function(states, actions) {
    phi <- RowWiseKronecker(Poly(states, 2), Poly(actions, 1))
    return(phi)
  }
  phis <- SARS2Phis(sars, list(0, 1), Feature)
  expect_equal(phis$r, rewards)
  expect_equal(phis$phi[2, ], c(1, 2, 4, 8, 0, 0, 0, 0))
  expect_equal(phis$phi_next_list[[2]][1, ], c(1, 2, 4, 8, 1, 2, 4, 8))
})
