# 執行 Microsoft 的 CAPM 迴歸
msft_model <- lm(msft_rf ~ mkt_rf, data = capm5)

# 顯示回歸結果
summary(msft_model)

# 繪製回歸圖
ggplot(capm5, aes(x = mkt_rf, y = msft_rf)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  ggtitle("📈 Microsoft CAPM 回歸分析") +
  xlab("市場風險溢酬") +
  ylab("Microsoft 超額報酬") +
  theme_minimal()
