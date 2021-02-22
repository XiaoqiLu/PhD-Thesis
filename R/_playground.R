n_traj <- 100
max_step <- 200

cp <- CartPole()
AgentRandom <- function(observation) {
  ifelse(stats::runif(1) < 0.5, -1, 1)
}

AgentQ <- function(observation) {
  phin <- RowWiseKronecker(Poly(matrix(observation, nrow = 1), 3, interaction_only = TRUE),
                           outer(-1, c(-1, 1), "=="))
  phip <- RowWiseKronecker(Poly(matrix(observation, nrow = 1), 3, interaction_only = TRUE),
                           outer(1, c(-1, 1), "=="))
  c(-1, 1)[EpsilonGreedy(c(phin %*% res$theta, phip %*% res$theta), epsilon = 0.02)]
}

trajs <- vector("list", n_traj)

IsTerminated <- function(observation) {
  (abs(observation[1]) > 2.4) || (abs(observation[3]) > 12 / 180 * pi)
}

Interpreter <- function(actions, observations) {
  n <- length(actions)
  state <- observations[[n + 1]]
  reward <- Gaussian(abs(state[1]), 2.4) * Gaussian(abs(state[3]), 12 / 180 * pi)
  return(list(state = state, reward = reward))
}

for (i_traj in 1 : n_traj) {
  cp <- Reset(cp)
  obs <- Observe(cp)
  traj <- Traj(list(obs), list())
  for (n_step in 1 : max_step) {
    action <- AgentQ(obs)
    cp <- Step(cp, action)
    obs <- Observe(cp)
    traj$actions[[n_step]] <- action
    traj$observations[[n_step + 1]] <- obs
    if (IsTerminated(obs)) {
      break
    }
  }
  traj$n <- n_step
  trajs[[i_traj]] <- traj
}

ids <- NULL
sars <- vector("list", n_traj)
for (i_traj in 1 : n_traj) {
  sars[[i_traj]] <- Traj2SARS(trajs[[i_traj]], Interpreter, skip = 0)
}

ss <- BindSARS(sars, 1 : n_traj)


Feature <- function(states, actions) {
  phi <- RowWiseKronecker(Poly(states, 3, interaction_only = TRUE),
                          outer(as.vector(actions), c(-1, 1), "=="))
  return(phi)
}

phis <- SARS2Phis(ss, list(-1, 1), Feature)

res <- BatchGradientQ(phis, discount = 1, method = "GGQ", lambda = 0,
                      theta = res$theta, learning_rate = 0.1)
print(res$theta)
print(res$value)
print(ss$n / n_traj)
