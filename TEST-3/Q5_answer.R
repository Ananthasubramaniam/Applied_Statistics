  # Q5: Perform a left-tailed Z-test

mu0 <- 1200
x_bar <- 1172
sigma <- 100
n <- 50
alpha <- 0.05

# Z-statistic
z_statistic <- (x_bar - mu0) / (sigma / sqrt(n))
print(paste("Z-statistic:", z_statistic))

# p-value
p_value <- pnorm(z_statistic)
print(paste("p-value:", p_value))

# Conclusion
if (p_value < alpha) {
  print("Reject the null hypothesis. There is sufficient evidence to conclude that the mean lifetime of the battery is less than 1200 hours.")
} else {
  print("Fail to reject the null hypothesis. There is not sufficient evidence to conclude that the mean lifetime of the battery is less than 1200 hours.")
}
