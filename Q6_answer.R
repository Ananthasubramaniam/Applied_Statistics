# Q6: Two-sample Z-test

n1 <- 45
x1_bar <- 52.3
sigma1 <- 2.1

n2 <- 55
x2_bar <- 51.5
sigma2 <- 1.8

alpha <- 0.01

# Z-statistic
z_statistic <- (x1_bar - x2_bar) / sqrt((sigma1^2 / n1) + (sigma2^2 / n2))
print(paste("Z-statistic:", z_statistic))

# p-value
p_value <- 2 * (1 - pnorm(abs(z_statistic)))
print(paste("p-value:", p_value))

# Conclusion
if (p_value < alpha) {
  print("Reject the null hypothesis as there is a significant difference in the mean cable strength between the two factories.")
} else {
  print("Fail to reject the null hypothesis as there is no significant difference in the mean cable strength between the two factories.")
}
