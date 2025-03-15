if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("ccolonescu/POE5Rdata", force = TRUE)
install.packages("ggplot2")
install.packages("dplyr") 
library(ggplot2)
library(dplyr)
library(POE5Rdata)
data("motel")
motel_data <- motel

# (a) 畫出旅館與競爭者的趨勢圖
p <- ggplot(motel_data, aes(x = time)) +
  geom_line(aes(y = motel_pct, color = "motel")) +
  geom_line(aes(y = comp_pct, color = "competitor")) +
  labs(title = "motel vs competitor occupancy rates over time",
       x = "time", y = "occupancy percentage") +
  theme_minimal() +
  scale_color_manual(name = "legend", values = c("motel" = "blue", "competitor" = "red"))
print(p)

# (a) 執行線性回歸
model <- lm(motel_pct ~ comp_pct, data = motel_data)
summary(model)

# (a) 計算 95% 信賴區間
confint(model, level = 0.95)

# (b) comp_pct = 70 時的 motel_pct（90% 信賴區間）
new_data <- data.frame(comp_pct = 70)
predict(model, newdata = new_data, interval = "confidence", level = 0.90)

# (c) t 檢定：檢驗 h0: β2 = 0 對應 h1: β2 > 0，顯著水準 0.01
t_value <- coef(summary(model))[2, "t value"]
p_value <- coef(summary(model))[2, "Pr(>|t|)"]
alpha <- 0.01
if (p_value < alpha) {
  print("reject h0: there is a significant positive relationship.")
} else {
  print("fail to reject h0: no significant relationship detected.")
}

# (d) t 檢定：檢驗 h0: β2 = 1 對應 h1: β2 ≠ 1
beta2 <- coef(model)[2]
se_beta2 <- coef(summary(model))[2, "Std. Error"]
t_test_stat <- (beta2 - 1) / se_beta2
df <- nrow(motel_data) - 2
t_critical <- qt(0.005, df, lower.tail = FALSE)
if (abs(t_test_stat) > t_critical) {
  print("reject h0: the slope significantly differs from 1.")
} else {
  print("fail to reject h0: the slope is not significantly different from 1.")
}

# (e) 計算殘差並繪製殘差圖
motel_data$residuals <- resid(model)
ggplot(motel_data, aes(x = time, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "residuals vs time", x = "time", y = "residuals") +
  theme_minimal()
# (e) 特定期間的殘差圖(17~23)
ggplot(motel_data %>% filter(time >= 17 & time <= 23), aes(x = time, y = residuals)) +
  geom_point(color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "residuals vs time (July 2004 - January 2005)", x = "time", y = "residuals") +
  theme_minimal()
