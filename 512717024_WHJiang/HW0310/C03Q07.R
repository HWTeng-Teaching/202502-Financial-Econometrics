#----------------------------
# C03Q07 Analysis in R
#----------------------------

# 觀察值數量與自由度
n <- 51
df <- n - 2

# (a) 估計截距的計算
# 已知截距的 t 值與標準誤
t_int <- 4.31
se_int <- 2.672
# 根據公式：截距 = t值 * 標準誤
intercept_est <- t_int * se_int
cat("Estimated intercept (a) =", round(intercept_est, 2), "\n")  # 約 11.53

# (b) 畫出估計的迴歸關係圖
# 迴歸方程式：INCOME = intercept_est + 1.029 * BACHELOR
slope_est <- 1.029
# 為了示意，設定 BACHELOR 百分比範圍 (例如 20% ~ 40%)
BACHELOR <- seq(20, 40, length.out = 100)
INCOME_est <- intercept_est + slope_est * BACHELOR

# 繪製線性圖
plot(BACHELOR, INCOME_est, type = "l", lwd = 2, col = "blue",
     xlab = "BACHELOR (%)", ylab = "INCOME (in thousands of dollars)",
     main = "Estimated Regression: INCOME = a + 1.029 * BACHELOR")
grid()
# 加上數據點標示
points(BACHELOR, INCOME_est, pch = 16, col = "red")

# (c) 計算斜率標準誤
# 已知斜率的 t 值 = 10.75
t_slope <- 10.75
se_slope <- slope_est / t_slope
cat("Standard error of the slope =", round(se_slope, 4), "\n")  # 約 0.0958

# (d) 檢定截距是否等於10
# 虛無假說：截距 = 10
t_test_int <- (intercept_est - 10) / se_int
cat("t-statistic for testing intercept = 10:", round(t_test_int, 3), "\n")

# (e) 繪製截距檢定的 t 分布圖及 p 值區域
alpha <- 0.05  # 顯著水準
t_crit <- qt(1 - alpha/2, df)  # 雙尾檢定臨界值
cat("Critical t-value for alpha = 0.05 (two-tailed):", round(t_crit, 3), "\n")

# 計算 p-value (雙尾)
p_value_int <- 2 * (1 - pt(abs(t_test_int), df))
cat("p-value for intercept test =", round(p_value_int, 3), "\n")

# 使用 ggplot2 畫出 t 分布與拒絕域
if(!require(ggplot2)) { install.packages("ggplot2"); library(ggplot2) }
t_vals <- seq(-4, 4, length.out = 1000)
dens <- dt(t_vals, df)
df_plot <- data.frame(t_vals = t_vals, density = dens)
ggplot(df_plot, aes(x = t_vals, y = density)) +
  geom_line() +
  geom_area(data = subset(df_plot, t_vals >= t_crit), aes(y = density),
            fill = "red", alpha = 0.5) +
  geom_area(data = subset(df_plot, t_vals <= -t_crit), aes(y = density),
            fill = "red", alpha = 0.5) +
  geom_vline(xintercept = t_test_int, color = "blue", linetype = "dashed", size = 1) +
  labs(title = "t-Distribution with Rejection Regions",
       subtitle = paste("Observed t =", round(t_test_int, 3),
                        "; Critical t =", round(t_crit, 3))) +
  theme_minimal()

# (f) 建構 99% 信賴區間 (斜率)
# 99% 信賴區間使用 df = 49
t_crit_99 <- qt(0.995, df)  # 0.995 為雙尾 (0.5%在每尾)
margin_error <- t_crit_99 * se_slope
ci_lower <- slope_est - margin_error
ci_upper <- slope_est + margin_error
cat("99% Confidence Interval for the slope: (", round(ci_lower, 3),
    ",", round(ci_upper, 3), ")\n")

# (g) 檢定斜率是否等於1
# 虛無假說 H0: slope = 1
t_test_slope <- (slope_est - 1) / se_slope
cat("t-statistic for testing slope = 1:", round(t_test_slope, 3), "\n")
p_value_slope <- 2 * (1 - pt(abs(t_test_slope), df))
cat("p-value for slope test =", round(p_value_slope, 3), "\n")

if(abs(t_test_slope) > t_crit) {
  cat("Reject H0: The slope is significantly different from 1.\n")
} else {
  cat("Fail to reject H0: The slope is not significantly different from 1.\n")
}
