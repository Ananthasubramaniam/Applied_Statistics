# Q8: Right-tailed hypothesis test

mu0 <- 100
sigma <- 15
n <- 36
alpha <- 0.05
mu_true <- 105

# (a) Determine the critical region
critical_z <- qnorm(1 - alpha)
critical_x_bar <- mu0 + critical_z * (sigma / sqrt(n))
print(paste("Critical region: x_bar >", critical_x_bar))

# (b) Compute the Type-II error probability (beta)
beta <- pnorm((critical_x_bar - mu_true) / (sigma / sqrt(n)))
print(paste("Type-II error probability (beta):", beta))

# (c) Compute the Power of the test
power <- 1 - beta
print(paste("Power of the test:", power))
