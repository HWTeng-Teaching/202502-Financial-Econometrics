# 清除工作環境
rm(list = ls())

##############################
# (1) 載入資料
##############################
# 假設資料檔案 "cps5_small.csv" 存放在工作目錄中
# The file "cps5_small.csv" should contain variables such as:
#   WAGE  - Hourly wage rate
#   EDUC  - Years of education
#   SEX   - Gender (e.g., "male", "female") [if not a factor, we convert it]
#   RACE  - Race (e.g., "black", "white")
cps5 <- read.csv("cps5_small.csv", header = TRUE)
str(cps5)  # 查看資料結構

# Convert SEX and RACE to factors if they are not already
cps5$SEX <- as.factor(cps5$SEX)
cps5$RACE <- as.factor(cps5$RACE)

##############################
# (a) Summary Statistics & Histograms for WAGE and EDUC
##############################
# (a) 取得 WAGE 與 EDUC 的描述性統計和直方圖

# Summary statistics for WAGE and EDUC
summary_wage <- summary(cps5$WAGE)
summary_educ <- summary(cps5$EDUC)
cat("【(a) Descriptive Statistics】\n")
cat("WAGE Summary:\n"); print(summary_wage)
cat("\nEDUC Summary:\n"); print(summary_educ)

# Plot histograms
par(mfrow = c(1, 2))
hist(cps5$WAGE, breaks = 30, col = "lightblue",
     main = "Histogram of WAGE",
     xlab = "WAGE (Hourly wage)")
hist(cps5$EDUC, breaks = 30, col = "lightgreen",
     main = "Histogram of EDUC",
     xlab = "EDUC (Years of education)")
par(mfrow = c(1, 1))

# Discussion (in your report, discuss characteristics such as skewness, outliers, etc.)

##############################
# (b) Linear Regression: WAGE = β1 + β2 EDUC + e
##############################
# (b) 估計線性回歸模型並討論結果
lm_linear <- lm(WAGE ~ EDUC, data = cps5)
summary_lm_linear <- summary(lm_linear)
cat("\n【(b) Linear Regression Results】\n")
print(summary_lm_linear)

# Interpretation:
# β2 represents the average change in WAGE (hourly wage rate) for an additional year of education.
# β1 is the predicted wage when EDUC = 0.

##############################
# (c) Residual Analysis: Plot Residuals vs. EDUC
##############################
# (c) 計算最小平方法殘差並繪製殘差與 EDUC 的關係圖
residuals_linear <- residuals(lm_linear)
plot(cps5$EDUC, residuals_linear,
     xlab = "EDUC (Years of education)",
     ylab = "Residuals",
     main = "Residuals vs. EDUC (Linear Model)",
     pch = 16, col = "purple")
abline(h = 0, lty = 2)
cat("\n【(c) Residual Plot】\n")
cat("Examine the plot to see if residuals are randomly scattered (as expected under SR1–SR5).\n")

##############################
# (d) Separate Regressions by Subgroups: Males, Females, Blacks, Whites
##############################
# (d) 針對男性、女性、黑人、白人分別估計回歸模型

# For Males
lm_male <- lm(WAGE ~ EDUC, data = subset(cps5, SEX == "male"))
summary_male <- summary(lm_male)
cat("\n【(d) Regression for Males】\n")
print(summary_male)

# For Females
lm_female <- lm(WAGE ~ EDUC, data = subset(cps5, SEX == "female"))
summary_female <- summary(lm_female)
cat("\n【(d) Regression for Females】\n")
print(summary_female)

# For Blacks
lm_black <- lm(WAGE ~ EDUC, data = subset(cps5, RACE == "black"))
summary_black <- summary(lm_black)
cat("\n【(d) Regression for Blacks】\n")
print(summary_black)

# For Whites
lm_white <- lm(WAGE ~ EDUC, data = subset(cps5, RACE == "white"))
summary_white <- summary(lm_white)
cat("\n【(d) Regression for Whites】\n")
print(summary_white)

# Compare the estimated coefficients and discuss differences among subgroups.

##############################
# (e) Quadratic Regression: WAGE = α1 + α2 EDUC² + e, and Marginal Effects
##############################
# (e) 估計二次迴歸模型並計算不同教育水準下的邊際效應
lm_quad <- lm(WAGE ~ I(EDUC^2), data = cps5)
summary_lm_quad <- summary(lm_quad)
cat("\n【(e) Quadratic Regression Results】\n")
print(summary_lm_quad)

# Marginal effect of an extra year of education is the derivative:
# dWAGE/dEDUC = 2 * α2 * EDUC
alpha2 <- coef(lm_quad)["I(EDUC^2)"]

# For a person with 12 years of education:
marginal_effect_12 <- 2 * alpha2 * 12
# For a person with 16 years of education:
marginal_effect_16 <- 2 * alpha2 * 16

cat("\nMarginal effect at 12 years of education =", marginal_effect_12, "\n")
cat("Marginal effect at 16 years of education =", marginal_effect_16, "\n")
cat("Compare these values with the linear model's estimated β2 (from part (b)).\n")

##############################
# (f) Plot Fitted Values from Both Models
##############################
# (f) 在同一圖上繪製線性回歸與二次回歸的擬合線，以及原始資料點
plot(cps5$EDUC, cps5$WAGE,
     xlab = "EDUC (Years of education)",
     ylab = "WAGE (Hourly wage)",
     main = "Fitted Lines: Linear vs. Quadratic Models",
     pch = 16, col = "blue")
# Fitted linear model line
educ_range <- seq(min(cps5$EDUC, na.rm = TRUE), max(cps5$EDUC, na.rm = TRUE), length.out = 300)
fitted_linear <- predict(lm_linear, newdata = data.frame(EDUC = educ_range))
lines(educ_range, fitted_linear, col = "red", lwd = 2)

# Fitted quadratic model values
fitted_quad <- predict(lm_quad, newdata = data.frame(EDUC = educ_range))
lines(educ_range, fitted_quad, col = "green", lwd = 2, lty = 2)

legend("topleft", legend = c("Data Points", "Linear Fit", "Quadratic Fit"),
       col = c("blue", "red", "green"), pch = c(16, NA, NA),
       lty = c(NA, 1, 2), lwd = c(NA, 2, 2))

cat("\n【(f) Plot Comparison】\n")
cat("Examine the plot to decide which model appears to fit the data better.\n")
