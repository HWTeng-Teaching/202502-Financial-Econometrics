# 給定的估計值和標準誤
b2 <- 0.01309        # 迴歸係數估計值
se_b2 <- 0.00215     # 係數的標準誤
n <- 64              # 國家數量
df <- n - 2        # 自由度 (n - 2)

# 計算 t 統計量
t_stat <- b2 / se_b2
cat("t 統計量 =", round(t_stat, 3), "\n")

# 設定顯著水準 (1%) 並計算臨界值（單尾檢定，右尾）
alpha <- 0.01
critical_value <- qt(1 - alpha, df)
cat("1% 顯著水準下的臨界值 =", round(critical_value, 3), "\n")

# 決策：若 t_stat 大於臨界值，則拒絕虛無假說
if (t_stat > critical_value) {
  cat("結論：拒絕虛無假說，支持 GDP 與獎牌數之間存在正向關係。\n")
} else {
  cat("結論：無法拒絕虛無假說，無充分證據支持 GDP 與獎牌數之間存在正向關係。\n")
}

# 計算單尾檢定的 p 值
p_value <- 1 - pt(t_stat, df)
cat("單尾檢定的 p 值 =", p_value, "\n")
