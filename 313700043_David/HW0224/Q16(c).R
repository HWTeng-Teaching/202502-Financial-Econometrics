msft_excess <- capm5$msft - capm5$riskfree

market_excess <- capm5$MKT_excess

model_msft <- lm(msft_excess ~ market_excess)

library(ggplot2)
alpha_msft <- results$Alpha[4]
beta_msft <- results$Beta[4]
print(alpha_msft)
print(beta_msft)

capm5$msft_excess <- capm5$msft - capm5$riskfree

ggplot(capm5, aes(x = MKT_excess, y = msft_excess)) +
  geom_point(color = "blue") + 
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(
    x = expression(R[m] - R[f]),
    y = expression(R[MSFT] - R[f]),
    title = bquote(
      "CAPM for Microsoft: " ~ (R[MSFT] - R[f]) == .(round(alpha_msft,4))
      + .(round(beta_msft,4)) * (R[m] - R[f])
    )
  )

results_no_intercept <- data.frame(
  Firm = firms,
  Beta = numeric(length(firms))) 
for (i in seq_along(firms)) {
  
  # 公司超額報酬 (R_i - R_f)
  firm_excess <- capm5[[firms[i]]] - capm5$riskfree
  
  # 無截距迴歸：(R_i - R_f) = β(R_m - R_f)
  # 注意 formula 寫法：firm_excess ~ 0 + capm5$MKT_excess
  model_no_intercept <- lm(firm_excess ~ 0 + capm5$MKT_excess)
  
  # 提取 β，因為模型只有一個係數，所以 coef(model_no_intercept)[1] 即 β
  results_no_intercept$Beta[i] <- coef(model_no_intercept)[1]
}

# 檢視結果
print(results_no_intercept)
