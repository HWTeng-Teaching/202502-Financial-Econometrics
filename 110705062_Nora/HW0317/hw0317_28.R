url <- "https://www.principlesofeconometrics.com/poe5/data/rdata/wa_wheat.rdata"
file_path <- "wa_wheat.rdata"
download.file(url, file_path, mode = "wb")
load(file_path)
ls()
head(wa_wheat)
install.packages("tseries")


## 4.28.1
northampton <- data.frame(
  YIELD = wa_wheat$northampton,
  TIME = wa_wheat$time
)

# --- 建立四個模型 ---
model1 <- lm(YIELD ~ TIME, data = northampton)                # 線性模型
model2 <- lm(YIELD ~ log(TIME), data = northampton)           # log(TIME)
model3 <- lm(YIELD ~ I(TIME^2), data = northampton)           # TIME^2
model4 <- lm(log(YIELD) ~ TIME, data = northampton)           # log(YIELD)

# --- 畫出實際與擬合值 ---
plot(northampton$TIME, northampton$YIELD,
     main = "Northampton Shire: YIELD over TIME with Fitted Models",
     xlab = "TIME (Year since 1950)", ylab = "Wheat Yield (tonnes/ha)",
     pch = 16)

lines(northampton$TIME, fitted(model1), col = "blue", lwd = 2)
lines(northampton$TIME, fitted(model2), col = "green", lwd = 2)
lines(northampton$TIME, fitted(model3), col = "red", lwd = 2)
lines(northampton$TIME, exp(fitted(model4)), col = "purple", lwd = 2)

legend("topleft", legend = c("Model 1: Linear", "Model 2: Log TIME",
                             "Model 3: TIME^2", "Model 4: Log(YIELD)"),
       col = c("blue", "green", "red", "purple"), lwd = 2)

# --- 殘差圖 ---
par(mfrow = c(2, 2))

plot(resid(model1), main = "Residuals: Model 1", ylab = "Residuals")
abline(h = 0, lty = 2)

plot(resid(model2), main = "Residuals: Model 2", ylab = "Residuals")
abline(h = 0, lty = 2)

plot(resid(model3), main = "Residuals: Model 3", ylab = "Residuals")
abline(h = 0, lty = 2)

plot(resid(model4), main = "Residuals: Model 4", ylab = "Residuals")
abline(h = 0, lty = 2)

par(mfrow = c(1, 1))

# --- 常態性檢定---
library(tseries)

models <- list(model1, model2, model3, model4)
model_names <- c("Model 1", "Model 2", "Model 3", "Model 4")

cat("Jarque-Bera Normality Test (p-values):\n")
for (i in 1:4) {
  jb_p <- jarque.bera.test(resid(models[[i]]))$p.value
  cat(model_names[i], ": p =", round(jb_p, 5), ifelse(jb_p > 0.05, "normal\n", "not Normal\n"))
}

# --- R-squared 比較 ---
cat("\nR-squared Values:\n")
cat("Model 1 (Linear):", summary(model1)$r.squared, "\n")
cat("Model 2 (Log TIME):", summary(model2)$r.squared, "\n")
cat("Model 3 (TIME^2):", summary(model3)$r.squared, "\n")
cat("Model 4 (Log YIELD):", summary(model4)$r.squared, "\n")


## 4.28.2
summary(model3)

## 4.28.3
model3 <- lm(YIELD ~ I(TIME^2), data = northampton)
n <- nrow(northampton)
k <- 1
r_thresh <- 2
h_thresh <- 2 * (k + 1) / n
dfb_thresh <- 2 / sqrt(n)
dffits_thresh <- 2 * sqrt((k + 1) / n)

student_res <- rstudent(model3)
leverage <- hatvalues(model3)
dfb <- dfbetas(model3)
dffits_vals <- dffits(model3)

diagnostics <- data.frame(
  Obs = 1:n,
  TIME = northampton$TIME,
  YIELD = northampton$YIELD,
  Studentized_Residual = student_res,
  Leverage = leverage,
  DFBETA_TIME2 = dfb[, 2],
  DFFITS = dffits_vals
)

print_flagged <- function(df, condition, label) {
  flagged <- df[condition, ]
  if (nrow(flagged) > 0) {
    cat(paste0("\n no observations flagged by ", label, ":\n"))
    print(round(flagged, 4), row.names = FALSE)
  } else {
    cat(paste0("\n no observations flagged by ", label, "\n"))
  }
}

print_flagged(diagnostics, abs(diagnostics$Studentized_Residual) > r_thresh, "Studentized Residual > 2")
print_flagged(diagnostics, diagnostics$Leverage > h_thresh, paste0("Leverage > ", round(h_thresh, 4)))
print_flagged(diagnostics, abs(diagnostics$DFBETA_TIME2) > dfb_thresh, paste0("DFBETAS > ", round(dfb_thresh, 4)))
print_flagged(diagnostics, abs(diagnostics$DFFITS) > dffits_thresh, paste0("DFFITS > ", round(dffits_thresh, 4)))

## 4.28.4
train_data <- subset(northampton, TIME <= 47)
model3_train <- lm(YIELD ~ I(TIME^2), data = train_data)
new_data <- data.frame(TIME = 48)
prediction <- predict(model3_train,
                      newdata = new_data,
                      interval = "prediction",
                      level = 0.95)
actual_1997 <- northampton$YIELD[northampton$TIME == 48]
cat("95% Prediction Interval for YIELD in 1997 (TIME = 48):\n")
print(round(prediction, 4))

cat("\nActual YIELD in 1997:", actual_1997, "\n")
if (actual_1997 >= prediction[1, "lwr"] && actual_1997 <= prediction[1, "upr"]) {
  cat("\nThe actual yield is within the 95% prediction interval.\n")
} else {
  cat("\nThe actual yield is not within the 95% prediction interval.\n")
}

