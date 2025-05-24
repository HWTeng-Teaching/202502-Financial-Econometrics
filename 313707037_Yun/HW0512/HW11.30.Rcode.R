if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("ccolonescu/POE5Rdata", force = TRUE)
library(POE5Rdata)
data(klein)
head(klein)

#(a)
investment_ols <- lm(i ~ p + plag + klag, data = klein)
summary(investment_ols)

#(b)
klein_complete <- na.omit(klein)
profit_rf <- lm(p ~ g + w2 + tx + time + plag + klag + elag, data = klein_complete)
summary(profit_rf)

# 取得 fitted values 與殘差
klein_complete$P_hat <- fitted(profit_rf)
klein_complete$v_hat <- resid(profit_rf)

#(C)
investment_hausman <- lm(i ~ p + plag + klag + v_hat, data = klein_complete)
summary(investment_hausman)

#(d)
# 載入必要套件
install.packages("ivreg")
library(ivreg)  # 或 library(AER)

# 假設你的資料框名稱為 klein_data
# 投資方程式：It = β1 + β2Pt + β3Pt-1 + β4Kt-1 + e2t
# 內生變數：Pt
# 工具變數（IVs）：所有八個外生與預定變數（例如：g, w2, tx, time, plag, klag, elag, 1）

# 2SLS估計
investment_2sls <- ivreg(i~ p+plag + klag, 
                         instruments = ~ g + w2 + tx + time + plag + klag + elag, 
                         data = klein_complete)

# 查看估計結果
summary(investment_2sls)

# 比較OLS結果（假設已經存在）
summary(investment_ols)

#(e)
# 假設你已經有第一階段的預測值 P_hat（來自簡約型回歸）
# klein_data 是你的資料框

# 將 P_hat 加入資料
klein_complete$P_hat <-fitted(profit_rf)  # P_hat 應為第一階段回歸 fitted 值

# 第二階段OLS回歸：以 P_hat 取代 P
investment_2nd_stage <- lm(i~ fitted(profit_rf) + plag + klag, data = klein_complete)

# 查看結果
summary(investment_2nd_stage)

# 與2SLS結果比較（假設你已經有 investment_2sls 物件）
summary(investment_2sls)

#f
# 假設你已經有2SLS模型 investment_2sls
# 1. 取得2SLS殘差
sargan_resid <- residuals(investment_2sls)

# 2. 用殘差對所有外生與預定變數做回歸
sargan_lm <- lm(sargan_resid ~ g + w2 + tx + time + plag + klag + elag, data = klein_complete)

# 3. 取得R平方
R2 <- summary(sargan_lm)$r.squared

# 4. 計算檢定統計量 TR^2
n <- nrow(klein_complete)  # 樣本數
TR2 <- n * R2

# 5. 計算臨界值（自由度 = 工具變數數量 - 內生變數數量）
df <- 7 - 1  # 7個IV，1個內生變數
critical_value <- qchisq(0.95, df)

# 6. 輸出檢定結果
cat("Sargan test statistic (TR^2):", TR2, "\n")
cat("Critical value (chi-squared, df =", df, "):", critical_value, "\n")
if (TR2 < critical_value) {
  cat("結論：無法拒絕工具變數有效性假設，工具變數設定合理。\n")
} else {
  cat("結論：拒絕工具變數有效性假設，工具變數設定可能有問題。\n")
}

