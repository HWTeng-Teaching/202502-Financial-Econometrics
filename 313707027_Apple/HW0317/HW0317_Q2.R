# 4.28
data = data("wa_wheat")
TIME <- wa_wheat$time
YIELD <- wa_wheat$northampton

# (a)
models <- list(
  lm(YIELD ~ TIME),
  lm(YIELD ~ log(TIME)),
  lm(YIELD ~ I(TIME^2)),
  lm(log(YIELD) ~ TIME)
)

titles <- c("Model 1: Linear", 
            "Model 2: Log", 
            "Model 3: Quadratic", 
            "Model 4: Log-Linear")

# (i) plots of the fitted equations
plot(TIME, YIELD, pch = 16, cex = 0.7,
     xlab = 'TIME', ylab = 'YIELD', ylim = c(-0.5,2.5))
colors <- c("red", "blue", "green", "orange")
for (i in 1:4) {
  lines(TIME, fitted(models[[i]]), col = colors[[i]], lwd = 2)
}
legend("topleft", legend = titles, col = colors, lwd = 2, cex = 0.8, bty = "n")

# (ii) plots of the residuals
par(mfrow = c(2, 2))
for (i in 1:4) {
  plot(TIME, residuals(models[[i]]), 
      xlab = "TIME", ylab = "Residuals",  ylim = c(-1.2,1),
      pch = 16, cex = 0.7, main = titles[i])
  abline(h = 0, col = "red", lwd = 2, lty = 2)
}

# (iii) error normality tests (by Shapiro-Wilk Test)
for (i in 1:4) {
  print(paste("Shapiro-Wilk test for", titles[i]))
  print(shapiro.test(residuals(models[[i]])))
}

# values for R²
r_squared <- sapply(models, function(model) summary(model)$r.squared)
names(r_squared) <- titles
print(r_squared)


# (b)
chosen_model <- models[[3]] 
summary(chosen_model)

# (c)
diagnostics <- list(
  list(name = "Studentized Residuals", values = rstudent(chosen_model), threshold = c(-2, 2)),
  list(name = "LEVERAGE", values = hatvalues(chosen_model), threshold = 2 * 2 / 48),
  list(name = "DFFITS", values = dffits(chosen_model), threshold = c(-2 * sqrt(2 / 48), 2 * sqrt(2 / 48))),
  list(name = "DFBETAS", values = dfbetas(chosen_model), threshold = c(-2 / sqrt(48), 2 / sqrt(48)))
)

par(mfrow = c(2, 2))  

for (k in diagnostics) {
  if (k$name == "DFBETAS") {
    matplot(k$values, type = "h", pch = 16, cex = 0.7,
             main = k$name, xlab = "INDEX", ylab = k$name)
  } else {
    plot(k$values, pch = 16, cex = 0.7,
         main = k$name, xlab = "INDEX", ylab = k$name)
  }
  abline(h = k$threshold, col = "red", lwd = 2, lty = 2)
}

par(mfrow = c(1, 1))

# (d)
train_data <- wa_wheat[1:47, ]
model_train <- lm(YIELD ~ I(TIME^2), data = train_data)
(prediction <- predict(model_train, newdata = data.frame(TIME = 48),
                      interval = "prediction", level = 0.95))
(origin <- wa_wheat[48, 1])


