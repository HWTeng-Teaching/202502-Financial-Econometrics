library(ggplot2)

# 定義公司名稱
company <- c("ge", "ibm", "ford", "msft", "dis", "xom")

# 計算市場超額報酬
capm5$Market_Excess <- capm5$mkt - capm5$riskfree

# 儲存結果的資料框
beta_comparison <- data.frame(Company = company, Beta_With_Intercept = NA, Beta_No_Intercept = NA)

# 進行迴歸分析
for (i in company) {
  capm5$Stock_Excess <- capm5[[i]] - capm5$riskfree

  # 1. 估計 CAPM（包含截距）
  model_with_intercept <- lm(Stock_Excess ~ Market_Excess, data = capm5)

  # 2. 估計 CAPM（假設 α = 0）
  model_no_intercept <- lm(Stock_Excess ~ Market_Excess - 1, data = capm5)

  # 3. 存入結果
  beta_comparison[beta_comparison$Company == i, "Beta_With_Intercept"] <- coef(model_with_intercept)[2]
  beta_comparison[beta_comparison$Company == i, "Beta_No_Intercept"] <- coef(model_no_intercept)[1]
}

# 顯示 beta 值比較結果
print(beta_comparison)

# 計算 beta 變化程度
beta_comparison$Beta_Change <- beta_comparison$Beta_No_Intercept - beta_comparison$Beta_With_Intercept

# 繪製 Beta 變化比較圖
ggplot(beta_comparison, aes(x = Company)) +
  geom_bar(aes(y = Beta_With_Intercept, fill = "With Intercept"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = Beta_No_Intercept, fill = "No Intercept"), stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("With Intercept" = "blue", "No Intercept" = "red")) +
  theme_minimal() +
  labs(title = "Comparison of Beta Estimates (With vs. Without Intercept)",
       y = "Beta Value", x = "Company") +
  geom_text(aes(y = Beta_With_Intercept, label = round(Beta_With_Intercept, 2)), vjust = -0.5, color = "blue") +
  geom_text(aes(y = Beta_No_Intercept, label = round(Beta_No_Intercept, 2)), vjust = 1, color = "red")
