library(dplyr)
library(ggplot2)
library(lmtest)


# 計算市場與個股的超額報酬
capm5 <- capm5 %>%
  mutate(
    excess_market = mkt - riskfree,   # 市場超額報酬
    excess_GE = ge - riskfree ,
    excess_IBM = ibm - riskfree,
    excess_Ford = ford - riskfree,
    excess_MSFT = msft - riskfree,
    excess_Disney = dis - riskfree,
    excess_Exxon = xom - riskfree
  )


# b.
#建立回歸

run_capm <- function(stock_returns, market_returns) {
  model <- lm(stock_returns ~ market_returns)  # 執行線性回歸
  return(summary(model))  # 回傳回歸結果
}



models <- list(
  "GE" = run_capm(capm5$excess_GE, capm5$excess_market),
  "IBM" = run_capm(capm5$excess_IBM, capm5$excess_market),
  "Ford" = run_capm(capm5$excess_Ford, capm5$excess_market),
  "Microsoft" = run_capm(capm5$excess_MSFT, capm5$excess_market),
  "Disney" = run_capm(capm5$excess_Disney, capm5$excess_market),
  "ExxonMobil" = run_capm(capm5$excess_Exxon, capm5$excess_market)
)


#找出B值，看哪間公司最保守與激進


betas <- sapply(models, function(m) coef(m)[2])  # 提取 beta 值
alphas <- sapply(models, function(m) coef(m)[1]) # 提取 alpha 值

most_aggressive <- names(which.max(betas))  # 最大 beta（進取型股票）
most_defensive <- names(which.min(betas))  # 最小 beta（防禦型股票）

cat("最具進取性的股票:", most_aggressive, "\n")
cat("最保守的股票:", most_defensive, "\n")



# c.

# 提取 alpha（截距）和 p-value
alpha_values <- sapply(models, function(m) coef(m)[1])  # 提取 α
alpha_p_values <- sapply(models, function(m) coef(m)[1,4])  # α 的 p-value

# 顯示 α 和 p-value
alpha_results <- data.frame(Stock = names(models), Alpha = alpha_values, P_Value = alpha_p_values)
print(alpha_results)

# 檢查哪些 α 顯著 ≠ 0（p < 0.05）
significant_alphas <- alpha_results %>% filter(P_Value < 0.05)
print(significant_alphas)

# 🔹 繪製 Microsoft (MSFT) 回歸圖
ggplot(capm5, aes(x = excess_market, y = excess_MSFT)) +
  geom_point(alpha = 0.6, color = "blue") +  # 散點圖
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # 加上回歸線
  labs(title = "Microsoft excess return vs. Market excess return ",
       x = "Market excess return",
       y = "Microsoft excess return") +
  theme_minimal()


#d. 

# 🔹 強制 α = 0 的回歸
run_capm_no_alpha <- function(stock_returns, market_returns) {
  model <- lm(stock_returns ~ market_returns - 1)  # 去掉截距
  return(summary(model))
}

# 🔹 重新估計 β（不包含 α）
models_no_alpha <- list(
  "GE" = run_capm_no_alpha(capm5$excess_GE, capm5$excess_market),
  "IBM" = run_capm_no_alpha(capm5$excess_IBM, capm5$excess_market),
  "Ford" = run_capm_no_alpha(capm5$excess_Ford, capm5$excess_market),
  "Microsoft" = run_capm_no_alpha(capm5$excess_MSFT, capm5$excess_market),
  "Disney" = run_capm_no_alpha(capm5$excess_Disney, capm5$excess_market),
  "ExxonMobil" = run_capm_no_alpha(capm5$excess_Exxon, capm5$excess_market)
)

# 🔹 比較 β 值變化
beta_old <- sapply(models, function(m) coef(m)[2])  # 原本的 β
beta_new <- sapply(models_no_alpha, function(m) coef(m)[1])  # 新的 β（去掉 α）

# 🔹 建立比較表格
beta_comparison <- data.frame(
  Stock = names(models),
  Beta_Original = beta_old,
  Beta_No_Alpha = beta_new
)

# 顯示比較結果
print(beta_comparison)






