# 載入必要套件
remotes::install_github("ccolonescu/POE5Rdata", force = TRUE)
library(POE5Rdata)
library(ggplot2)

# 載入資料
data("motel")

# 3.19 a: 檢查資料摘要
summary(motel)

# 繪製 MOTEL_PCT 和 COMP_PCT 隨著 TIME 變化的折線圖
ggplot(motel, aes(x = time)) +
  geom_line(aes(y = motel_pct, color = "MOTEL_PCT"), size = 1.2, linetype = "solid") +
  geom_line(aes(y = comp_pct, color = "COMP_PCT"), size = 1.2, linetype = "dashed") +
  scale_color_manual(values = c("MOTEL_PCT" = "darkorange", "COMP_PCT" = "steelblue")) +
  labs(
    title = "MOTEL_PCT and COMP_PCT over TIME",
    x = "Time",
    y = "Occupancy Percentage",
    color = "Legend"
  ) +
  theme_minimal(base_size = 14)

# 3.19 b: 簡單線性回歸模型
model <- lm(motel_pct ~ comp_pct, data = motel)

# 使用 comp_pct = 70 預測 motel_pct，並給出90%信賴區間
new_data <- data.frame(comp_pct = 70)
pred <- predict(model, new_data, interval = "confidence", level = 0.90)
print(pred)

# 3.19 c: t檢定（H0: b2 <= 0）
summary(model)
b2 <- coef(summary(model))["comp_pct", "Estimate"]
se_b2 <- coef(summary(model))["comp_pct", "Std. Error"]

t_value <- b2 / se_b2
df <- nrow(motel) - 2
critical_value <- qt(0.99, df = df)
p_value <- 1 - pt(t_value, df = df)

cat("b2 =", b2, "\n")
cat("SE(b2) =", se_b2, "\n")
cat("t-統計量 =", t_value, "\n")
cat("臨界值 =", critical_value, "\n")
cat("p 值 =", p_value, "\n")

if (t_value > critical_value) {
  cat("結論：拒絕 H0，b2 顯著大於 0。\n")
} else {
  cat("結論：無法拒絕 H0，b2 可能小於或等於 0。\n")
}

# 3.19 d: t檢定（H0: b2 = 1）
t_value <- (b2 - 1) / se_b2
critical_value <- qt(0.995, df = df)
p_value <- 2 * (1 - pt(abs(t_value), df = df))

cat("b2 =", b2, "\n")
cat("SE(b2) =", se_b2, "\n")
cat("t-統計量 =", t_value, "\n")
cat("臨界值 =", critical_value, "\n")
cat("p 值 =", p_value, "\n")

if (abs(t_value) > critical_value) {
  cat("結論：拒絕 H0，b2 顯著不同於 1。\n")
} else {
  cat("結論：無法拒絕 H0，b2 可能等於 1。\n")
}

# 3.19 e: 殘差圖
motel$residuals <- residuals(model)

ggplot(motel, aes(x = time, y = residuals)) +
  geom_line(color = "darkgreen", size = 1) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "red", size = 1) +
  labs(
    title = "Residuals of MOTEL_PCT Regression on COMP_PCT",
    x = "Time",
    y = "Residuals"
  ) +
  theme_minimal(base_size = 14)
