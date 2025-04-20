#Question 3.7 Part e: 
# Set up
df <- 62 # degrees of freedom
t_val <- 0.569
alpha <- 0.05
t_crit <- qt(1 - alpha / 2, df) # two-tailed critical value

# Plot the t-distribution
curve(dt(x, df),
    from = -4, to = 4,
    main = "Two-Tail t-Test for Intercept = 10",
    ylab = "Density", xlab = "t", lwd = 2
)

# Add vertical lines at ±t-statistic
abline(v = t_val, col = "blue", lty = 2)
abline(v = -t_val, col = "blue", lty = 2)

# Add vertical lines at critical values
abline(v = t_crit, col = "red", lty = 3)
abline(v = -t_crit, col = "red", lty = 3)

# Shade p-value area (both tails)
polygon(c(seq(t_val, 4, 0.01), 4),
    c(dt(seq(t_val, 4, 0.01), df), 0),
    col = rgb(0, 0, 1, 0.3), border = NA
)

polygon(c(-4, seq(-4, -t_val, 0.01)),
    c(0, dt(seq(-4, -t_val, 0.01), df)),
    col = rgb(0, 0, 1, 0.3), border = NA
)

# Shade rejection regions in red
polygon(c(seq(t_crit, 4, 0.01), 4),
    c(dt(seq(t_crit, 4, 0.01), df), 0),
    col = rgb(1, 0, 0, 0.3), border = NA
)

polygon(c(-4, seq(-4, -t_crit, 0.01)),
    c(0, dt(seq(-4, -t_crit, 0.01), df)),
    col = rgb(1, 0, 0, 0.3), border = NA
)

# Add legend
legend("topright",
    legend = c("p-value", "Rejection Region", "t = ±0.569"),
    col = c("blue", "red", "blue"), lty = c(1, 1, 2), lwd = 2
)

#Question 7. Part f
# Given values
b1 <- 1.029
se_b1 <- 0.09572093
beta_null <- 1 
df <- 49 
# Calculate t-statistic
t_stat <- (b1 - beta_null) / se_b1
t_stat

# Get critical value for two-tailed test at alpha = 0.05
t_crit <- qt(0.975, df)
t_crit

#Question 17
#Part a
slope_hat <- 2.46
slope_null <- 1.80
se <- 0.16
df <- 986 - 2

t_stat <- (slope_hat - slope_null) / se
p_value <- 1 - pt(t_stat, df)
t_critical <- qt(0.95, df)

cat("t-statistic:", t_qt(stat, "\n")
cat("p-value:", p_value, "\n")
cat("t-critical (one-tailed, alpha=0.05):", t_critical, "\n")

if (t_stat > t_critical) {
    cat("Reject H0: There is evidence that slope > 1.80\n")
} else {
    cat("Do not reject H0\n")
}

x_vals <- seq(-4, 6, length.out = 1000)
y_vals <- dt(x_vals, df)

plot(x_vals, y_vals,
    type = "l", lwd = 2, col = "black",
    main = "Right-Tailed t-Test (Critical Region)",
    ylab = "Density", xlab = "t value"
)

x_crit <- seq(t_critical, max(x_vals), length.out = 100)
y_crit <- dt(x_crit, df)
polygon(c(x_crit, rev(x_crit)), c(y_crit, rep(0, length(y_crit))),
    col = "red", border = NA
)

abline(v = t_critical, col = "red", lty = 2, lwd = 2)
abline(v = t_stat, col = "blue", lty = 2, lwd = 2)

legend("topright",
    legend = c("Critical Value", "Test Statistic"),
    col = c("red", "blue"), lty = 2, lwd = 2
)

