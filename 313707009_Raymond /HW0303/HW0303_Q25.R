if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("ccolonescu/POE5Rdata", force = TRUE)

library(POE5Rdata)
data("cex5_small")
data <- cex5_small

# (a) 繪製 FOODAWAY 直方圖與敘述統計量以及25,75百分位
hist(data$foodaway, main = "Histogram of FOODAWAY", xlab = "FOODAWAY", col = "lightblue", border = "black")
summary(data$foodaway)
quantile(data$foodaway, probs = c(0.25, 0.75))

# (b) 計算不同學歷類別的 FOODAWAY 均值與中位數
num_advanced <- sum(data$advance)
num_college <- sum(data$college)
num_no_degree <- nrow(data) - num_advanced - num_college

mean_advanced <- mean(data$foodaway[data$advance == 1])
median_advanced <- median(data$foodaway[data$advance == 1])
mean_college <- mean(data$foodaway[data$college == 1])
median_college <- median(data$foodaway[data$college == 1])
mean_no_degree <- mean(data$foodaway[data$advance == 0 & data$college == 0])
median_no_degree <- median(data$foodaway[data$advance == 0 & data$college == 0])

cat("Mean FOODAWAY for households with an advanced degree:", mean_advanced, "\n")
cat("Median FOODAWAY for households with an advanced degree:", median_advanced, "\n")
cat("Mean FOODAWAY for households with a college degree:", mean_college, "\n")
cat("Median FOODAWAY for households with a college degree:", median_college, "\n")
cat("Mean FOODAWAY for households with no degree:", mean_no_degree, "\n")
cat("Median FOODAWAY for households with no degree:", median_no_degree, "\n")


# (c) 繪製 ln(FOODAWAY) 直方圖與統計摘要
data$ln_foodaway <- log(data$foodaway)
hist(data$ln_foodaway, main = "Histogram of ln(FOODAWAY)", xlab = "ln(FOODAWAY)", col = "lightgreen", border = "black")
summary(data$ln_foodaway)

# (d) 估計線性回歸 ln(FOODAWAY) ~ INCOME
data <- data[data$ln_foodaway != -Inf,]
lm_model <- lm(ln_foodaway ~ income, data = data)
summary(lm_model)

# (e) 繪製 ln(FOODAWAY) vs. INCOME 散佈圖與回歸線
plot(data$income, data$ln_foodaway, 
     main = "ln(FOODAWAY) vs. INCOME", 
     xlab = "Household Monthly Income (in $100 units)", 
     ylab = "ln(FOODAWAY)", 
     col = "blue", pch = 16)
abline(lm_model, col = "red", lwd = 2)

# (f) 計算並繪製殘差圖
residuals_lm <- resid(lm_model)
plot(data$income, residuals_lm, 
     main = "Residuals of Linear Regression Model", 
     xlab = "Household Monthly Income", 
     ylab = "Residuals", 
     col = "purple", pch = 16)
abline(h = 0, col = "black", lty = 2)