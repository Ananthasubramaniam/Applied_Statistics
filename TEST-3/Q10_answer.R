# Q10: Function to compute and plot two-sided p-value

plot_p_value <- function(z_scores) {
  p_values <- 2 * (1 - pnorm(abs(z_scores)))
  
  plot(z_scores, p_values, type = "l", main = "Two-sided p-value vs. Z-score", xlab = "Z-score", ylab = "p-value")
  abline(h = 0.05, col = "red", lty = 2)
  
  # Interpretation
  cat("The plot shows the p-value for different Z-scores. The red dashed line represents an alpha level of 0.05.
Any Z-score where the p-value (black line) is below the red line would lead to rejecting the null hypothesis.
This corresponds to Z-scores with an absolute value greater than", qnorm(1 - 0.05 / 2), "\n")
}

z_scores <- seq(-3.5, 3.5, by = 0.1)
plot_p_value(z_scores)

