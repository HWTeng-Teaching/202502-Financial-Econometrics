########################################################################################
#程式碼解釋
#(a) – (c)：
#我們先建立一個包含五個觀測值的資料框，並計算各觀測值與平均值的差（偏差值）。接著，
#計算偏差平方、偏差乘積等欄位，並驗證一些數學關係（例如：Σ(x − x̄)² = Σx² − n·x̄²）。

#(b)：
#透過簡單線性迴歸的公式，利用手動計算方式求出迴歸係數，其中斜率 b₂ 為 Σ(x − x̄)(y − ȳ)
#除以 Σ(x − x̄)²，而截距 b₁ 則為 ȳ − b₂·x̄。並在程式中列印出這些估計值以及對其意義的說明。

#(d)：
#程式計算出每筆資料的預測值 (ŷ)，再依據 ŷ 與實際值 y 的差計算出殘差 (e)、殘差的平方
#(e²) 與 x 乘以殘差 (x·e)。並將這些數值的總和印出，以檢查計算正確性。
#額外統計量：
#計算了 y 與 x 的樣本變異數、標準差、樣本共變異數以及樣本相關係數。此外，也計算了 x 的
#變異係數 (CV) 以及 x 的中位數。

#(e)：
#使用基礎繪圖函數繪製 x 與 y 的散佈圖，再將以迴歸方程式計算出的預測迴歸線 (紅色) 疊加
#在散佈圖上。

#(f)：
#在圖中標出資料的中心點（也就是 (x̄, ȳ)），並檢查該迴歸線是否通過該中心點。

#(g) 與 (h)：
#程式驗證了數值關係：ȳ = b₁ + b₂·x̄ 以及所有預測值的平均值等於 ȳ。

#(i) 與 (j)：
#計算了誤差變異數 σ̂²（自由度為 n - 2），並依此求出斜率 b₂ 的條件變異數及標準誤。
########################################################################################


# Clear the workspace
rm(list = ls())

##############################
# Data: 5 Observations
##############################
data <- data.frame(
  Obs = 1:5,
  x = c(3, 2, 1, -1, 0),
  y = c(4, 2, 3, 1, 0)
)

n <- nrow(data)

##############################
# (a) Complete the Table & Compute Sample Means
##############################
# Calculate sample means
xbar <- mean(data$x)
ybar <- mean(data$y)

# Compute deviations and related columns
data$x_minus_mean <- data$x - xbar
data$sq_x_minus_mean <- (data$x - xbar)^2
data$y_minus_mean <- data$y - ybar
data$x_y_dev <- (data$x - xbar) * (data$y - ybar)

# Sum up columns for the table's last row
sum_row <- c(
  x = sum(data$x),
  y = sum(data$y),
  x_minus_mean = sum(data$x_minus_mean),
  sq_x_minus_mean = sum(data$sq_x_minus_mean),
  y_minus_mean = sum(data$y_minus_mean),
  x_y_dev = sum(data$x_y_dev)
)

cat("===== (a) Table and Sample Means =====\n")
print(data)
cat("\nSum row:\n")
print(sum_row)
cat("\nSample means: x̄ =", xbar, ", ȳ =", ybar, "\n\n")

##############################
# (b) Compute Regression Coefficients b1 and b2
##############################
# Using formulas: 
#   b2 = sum((x - x̄)(y - ȳ)) / sum((x - x̄)^2)
#   b1 = ȳ - b2*x̄
b2 <- sum(data$x_y_dev) / sum(data$sq_x_minus_mean)
b1 <- ybar - b2 * xbar

cat("===== (b) Regression Coefficients =====\n")
cat("Intercept, b1 =", b1, "\n")
cat("Slope, b2 =", b2, "\n")
cat("Interpretation:\n")
cat(" - b2 indicates that for each one-unit increase in x, y increases by", b2, "units on average.\n")
cat(" - b1 is the predicted y value when x = 0.\n\n")

##############################
# (c) Verify Σ(x−x̄)² and Σ(x−x̄)(y−ȳ) using alternative formulas
##############################
sum_x2 <- sum(data$x^2)
sum_xy <- sum(data$x * data$y)

cat("===== (c) Verification of Alternative Formulas =====\n")
cat("Sum x² =", sum_x2, "\n")
cat("n*x̄² =", n * xbar^2, "\n")
cat("Σ(x − x̄)² =", sum(data$sq_x_minus_mean), "\n")
cat("Thus, Σ(x − x̄)² = Σx² − n*x̄²\n\n")

cat("Sum x*y =", sum_xy, "\n")
cat("n*x̄*ȳ =", n * xbar * ybar, "\n")
cat("Σ(x − x̄)(y − ȳ) =", sum(data$x_y_dev), "\n")
cat("Thus, Σ(x − x̄)(y − ȳ) = Σxy − n*x̄*ȳ\n\n")

##############################
# (d) Compute Fitted Values, Residuals, and Additional Quantities
##############################
# Fitted regression line: ŷ = b1 + b2*x
data$fitted <- b1 + b2 * data$x
data$residuals <- data$y - data$fitted
data$resid_sq <- data$residuals^2
data$x_resid <- data$x * data$residuals

cat("===== (d) Fitted Values and Residuals =====\n")
print(data[, c("x", "y", "fitted", "residuals", "resid_sq", "x_resid")])
cat("\nSums:\n")
cat("Σx =", sum(data$x), "\n")
cat("Σy =", sum(data$y), "\n")
cat("Σŷ =", sum(data$fitted), "\n")
cat("Σe =", sum(data$residuals), "\n")
cat("Σe² =", sum(data$resid_sq), "\n")
cat("Σ(x*e) =", sum(data$x_resid), "\n\n")

# Compute additional statistics:
# Sample variance of y and x, covariance, correlation, CV, and median of x.
s2_y <- sum((data$y - ybar)^2) / (n - 1)
s2_x <- sum((data$x - xbar)^2) / (n - 1)
s_y <- sqrt(s2_y)
s_x <- sqrt(s2_x)
s_xy <- sum((data$x - xbar) * (data$y - ybar)) / (n - 1)
r_xy <- s_xy / (s_x * s_y)
CV_x <- 100 * (s_x / xbar)
median_x <- median(data$x)

cat("===== Additional Statistics =====\n")
cat("s_y² =", s2_y, "\n")
cat("s_x² =", s2_x, "\n")
cat("Sample covariance, s_xy =", s_xy, "\n")
cat("Sample correlation, r_xy =", r_xy, "\n")
cat("Coefficient of Variation of x, CV_x =", CV_x, "%\n")
cat("Median of x =", median_x, "\n\n")

##############################
# (e) Plot the Data and Fitted Regression Line
##############################
cat("===== (e) Plotting Data and Fitted Regression Line =====\n")
plot(data$x, data$y, pch = 16, col = "blue",
     xlab = "x", ylab = "y",
     main = "Scatterplot with Fitted Regression Line")
abline(a = b1, b = b2, col = "red", lwd = 2)

##############################
# (f) Plot the Centroid and Check if the Fitted Line Passes Through It
##############################
points(xbar, ybar, col = "darkgreen", pch = 17, cex = 1.5)
text(xbar, ybar, labels = "Centroid", pos = 4, col = "darkgreen")
cat("===== (f) Centroid =====\n")
cat("The centroid (x̄, ȳ) is (", xbar, ",", ybar, ")\n")
cat("The fitted line passes through the centroid if ȳ = b1 + b2*x̄.\n\n")

##############################
# (g) Verify ȳ = b1 + b2*x̄
##############################
cat("===== (g) Verification =====\n")
cat("b1 + b2*x̄ =", b1 + b2 * xbar, "\n")
cat("ȳ =", ybar, "\n\n")

##############################
# (h) Verify that the Average Fitted Value Equals ȳ
##############################
avg_fitted <- mean(data$fitted)
cat("===== (h) Average Fitted Value =====\n")
cat("Average fitted value =", avg_fitted, "\n")
cat("ȳ =", ybar, "\n\n")

##############################
# (i) Compute σ̂² (Error Variance)
##############################
# Degrees of freedom = n - 2 for simple regression
sigma2_hat <- sum(data$resid_sq) / (n - 2)
cat("===== (i) σ̂² =====\n")
cat("σ̂² =", sigma2_hat, "\n\n")

##############################
# (j) Compute var(b₂|x) and se(b₂)
##############################
var_b2 <- sigma2_hat / sum(data$sq_x_minus_mean)
se_b2 <- sqrt(var_b2)
cat("===== (j) Variance and Standard Error of b₂ =====\n")
cat("var(b₂|x) =", var_b2, "\n")
cat("se(b₂) =", se_b2, "\n")

