# Q2: Compute a 95% confidence interval for the true mean

x <- c(9.8, 10.1, 9.9, 10.2, 9.7, 10.0, 9.9, 9.8)
sigma <- 0.8
n <- length(x)
x_bar <- mean(x)
alpha <- 0.05
z_alpha_by_2 <- qnorm(1 - alpha / 2)
margin_of_error <- z_alpha_by_2 * (sigma / sqrt(n))
confidence_interval <- c(x_bar - margin_of_error, x_bar + margin_of_error)

print("95% Confidence Interval:")
print(confidence_interval)
