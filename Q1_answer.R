# Q1: Compute the probability P(-1.25 < Z <= 2.10)

# Using pnorm()
prob_pnorm <- pnorm(2.10) - pnorm(-1.25)
print(paste("Probability using pnorm():", prob_pnorm))

# Using numerical integration
integrand <- function(x) {
  dnorm(x)
}
prob_integrate <- integrate(integrand, lower = -1.25, upper = 2.10)$value
print(paste("Probability:", prob_integrate))

# Comparison
print(paste("Difference:", abs(prob_pnorm - prob_integrate)))
