# Q4: Generate a table of critical Z-values for two-sided confidence levels

confidence_levels <- c(0.90, 0.95, 0.99, 0.995, 0.999)
alpha_levels <- 1 - confidence_levels
critical_z_values <- qnorm(1 - alpha_levels / 2)

results <- data.frame(
  Confidence_Level = paste0(confidence_levels * 100, "%"),
  Critical_Z_Value = critical_z_values
)

print("Critical Z-values for two-sided confidence levels:")
print(results)
