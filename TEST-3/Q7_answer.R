# Q7: Plot a standard normal curve and shade the rejection regions

alpha <- 0.05
critical_z <- qnorm(1 - alpha / 2)

x <- seq(-4, 4, length.out = 1000)
y <- dnorm(x)

plot(x, y, type = "l", main = "Standard Normal Curve with Rejection Regions", xlab = "Z", ylab = "Density")

# Shade rejection regions
x_reject_pos <- seq(critical_z, 4, length.out = 100)
y_reject_pos <- dnorm(x_reject_pos)
polygon(c(critical_z, x_reject_pos, 4), c(0, y_reject_pos, 0), col = "red")

x_reject_neg <- seq(-4, -critical_z, length.out = 100)
y_reject_neg <- dnorm(x_reject_neg)
polygon(c(-4, x_reject_neg, -critical_z), c(0, y_reject_neg, 0), col = "red")

# Label critical Z-values
text(critical_z, 0.05, paste("z =", round(critical_z, 2)), pos = 4)
text(-critical_z, 0.05, paste("z =", round(-critical_z, 2)), pos = 2)
