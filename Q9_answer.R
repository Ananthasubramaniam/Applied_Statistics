# Q9: Monte-Carlo simulation to estimate beta

mu_true <- 105
sigma <- 15
n <- 36
mu0 <- 100
alpha <- 0.05

critical_x_bar <- mu0 + qnorm(1 - alpha) * (sigma / sqrt(n))

num_simulations <- 10000
samples <- rnorm(num_simulations * n, mean = mu_true, sd = sigma)
sample_means <- colMeans(matrix(samples, nrow = n))

# Estimate beta
beta_hat <- mean(sample_means <= critical_x_bar)
print(paste("Estimated beta from simulation:", beta_hat))

# Theoretical beta
beta_theoretical <- pnorm((critical_x_bar - mu_true) / (sigma / sqrt(n)))
print(paste("Theoretical beta:", beta_theoretical))

# Comparison
print(paste("Difference:", abs(beta_hat - beta_theoretical)))
