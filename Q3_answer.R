# Q3: Compute a 98% confidence interval for the true defective proportion

# (a) using the Z-based manual formula
n <- 300
x <- 21
p_hat <- x / n
alpha <- 0.02
z_alpha_by_2 <- qnorm(1 - alpha / 2)
margin_of_error <- z_alpha_by_2 * sqrt(p_hat * (1 - p_hat) / n)
confidence_interval_manual <- c(p_hat - margin_of_error, p_hat + margin_of_error)

print("98% Confidence Interval (Manual):")
print(confidence_interval_manual)

# (b) using prop.test(correct = FALSE)
prop_test_result <- prop.test(x, n, conf.level = 0.98, correct = FALSE)
confidence_interval_prop_test <- prop_test_result$conf.int

print("98% Confidence Interval (prop.test):")
print(confidence_interval_prop_test)

# Comparison
print("The manual formula and prop.test give very similar results")