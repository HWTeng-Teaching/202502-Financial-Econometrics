# e
x <- c(3, 2, 1, -1, 0)
y <- c(4, 2, 3, 1, 0)

plot(x, y, main = "Scatter Plot with Regression Line", xlab = "x", ylab = "y", pch = 19)

model <- lm(y ~ x)

abline(model, col = "red", lwd = 2)

legend("topleft", legend = paste("y =", round(coef(model)[1], 2), "+", round(coef(model)[2], 2), "x"), col = "red", lwd = 2)


x_bar <- mean(x)
y_bar <- mean(y)

y_hat_bar <- predict(model, newdata = data.frame(x = x_bar))

# f
cat("Mean (x, y): (", x_bar, ",", y_bar, ")\n")
print("Yes")

points(x_bar, y_bar, col = "blue", pch = 19, cex = 1.5)
text(x_bar, y_bar, labels = "Mean (x_hat, y_hat)", pos = 4, col = "blue")

# g
b1 <- coef(model)[1]  # 截距 b1
b2 <- coef(model)[2]  # 斜率 b2

y_bar_pred <- b1 + b2 * x_bar

cat("Computed y_bar:", y_bar, "\n")
cat("Predicted y_bar from regression:", y_bar_pred, "\n")
print("Yes")