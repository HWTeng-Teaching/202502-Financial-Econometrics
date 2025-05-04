library(POE5Rdata)
data("capm5")

#a.------------------------------------------------------
# 計算 Microsoft 的超額報酬
capm5$excess_msft <- capm5$msft - capm5$riskfree
# 計算 市場 的超額報酬
capm5$excess_mkt <- capm5$mkt - capm5$riskfree
# 執行 OLS 回歸：
capm_model <- lm(excess_msft ~ excess_mkt, data = capm5)
summary(capm_model)

#b.------------------------------------------------------
# 建立 RANK 變數：將 excess_mkt 由小到大排序後給 1~n
capm5$RANK <- rank(capm5$excess_mkt)
# 第一階段回歸：用 RANK 預測 excess_mkt
first_stage <- lm(excess_mkt ~ RANK, data = capm5)
summary(first_stage)

#c.------------------------------------------------------
# 取得第一階段殘差（v_hat）
capm5$v_hat <- resid(first_stage)
# 擴充的 CAPM 模型，加上 v_hat
augmented_model <- lm(excess_msft ~ excess_mkt + v_hat, data = capm5)
summary(augmented_model)

#d.------------------------------------------------------
library(AER)
iv_model <- ivreg(excess_msft ~ excess_mkt | RANK, data = capm5)
summary(iv_model)

#e.------------------------------------------------------
# 建立 POS 變數：當市場超額報酬 > 0 時為 1，否則為 0
capm5$POS <- ifelse(capm5$excess_mkt > 0, 1, 0)
first_stage_e <- lm(excess_mkt ~ RANK + POS, data = capm5)
summary(first_stage_e)

library(car)
# 檢定 H0: RANK = 0 且 POS = 0（聯合顯著性檢定）
linearHypothesis(first_stage_e, c("RANK = 0", "POS = 0"))

# f.------------------------------------------------------
capm5$v_hat2 <- resid(first_stage_e)
# Augmented regression：加上殘差來進行 Hausman 檢定
hausman_model <- lm(excess_msft ~ excess_mkt + v_hat2, data = capm5)
summary(hausman_model)

# g.------------------------------------------------------
iv_model_2iv <- ivreg(excess_msft ~ excess_mkt | RANK + POS, data = capm5)
summary(iv_model_2iv)

# h.------------------------------------------------------
# 取得 2SLS 模型的殘差
resid_iv <- resid(iv_model_2iv)
# 對這些殘差用工具變數 RANK 和 POS 進行回歸
sargan_test <- lm(resid_iv ~ capm5$RANK + capm5$POS)
summary(sargan_test)

# 計算 Sargan 統計量：n * R^2
n <- nrow(capm5)
R2_sargan <- summary(sargan_test)$r.squared
sargan_stat <- n * R2_sargan

# 計算 p 值，使用卡方分配，df = 1（2 個工具 − 1 個內生變數）
p_value <- 1 - pchisq(sargan_stat, df = 1)
# 輸出結果
cat("Sargan statistic =", sargan_stat, "\n")
cat("p-value =", p_value, "\n")
# 判斷結論
if (p_value < 0.05) {
  cat("在 5% 顯著水準下，拒絕 H0：有工具變數可能無效\n")
} else {
  cat("在 5% 顯著水準下，無法拒絕 H0：工具變數可視為有效\n")
}
